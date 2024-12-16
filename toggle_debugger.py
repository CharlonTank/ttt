#!/usr/bin/env python3

import os
import subprocess
import random
import string
import webbrowser


def check_toggling_state():
    return os.path.exists("src/Debuggy/App.elm")


def toggle_debugger_backend(toggling_on=False):
    file_path = "src/Backend.elm"
    with open(file_path, "r") as file:
        lines = file.readlines()

    if toggling_on:
        # Add Debuggy.App import if it doesn't exist
        if "import Debuggy.App" not in "".join(lines):
            lines.insert(1, "import Debuggy.App\n")

        # Replace Lamdera.backend with Debuggy.App.backend
        new_token = "".join(random.choices(string.ascii_letters + string.digits, k=16))
        for i, line in enumerate(lines):
            if "Lamdera.backend" in line:
                lines[i] = line.replace(
                    "Lamdera.backend",
                    f'Debuggy.App.backend NoOpBackendMsg "{new_token}"',
                )

        print(f"New token generated: {new_token}")
        webbrowser.open(f"https://backend-debugger.lamdera.app/{new_token}")
    else:
        # Remove Debuggy.App import
        lines = [
            line for line in lines if not line.strip().startswith("import Debuggy.App")
        ]

        # Replace Debuggy.App.backend with Lamdera.backend
        for i, line in enumerate(lines):
            if "Debuggy.App.backend" in line:
                lines[i] = line.replace(
                    "Debuggy.App.backend NoOpBackendMsg", "Lamdera.backend"
                )
                lines[i + 1] = ""

    with open(file_path, "w") as file:
        file.writelines(lines)

    subprocess.run(["elm-format", file_path, "--yes"])


def toggle_debugger_app(toggling_on):
    file_path = "src/Debuggy/App.elm"
    if toggling_on:
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w") as file:
            file.write(
                """module Debuggy.App exposing (backend)

import Http
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import Task
import Time


backend :
    backendMsg
    -> String
    ->
        { init : ( backendModel, Cmd backendMsg )
        , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
        , subscriptions : backendModel -> Sub backendMsg
        }
    ->
        { init : ( backendModel, Cmd backendMsg )
        , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
        , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
        , subscriptions : backendModel -> Sub backendMsg
        }
backend backendNoOp sessionName { init, update, updateFromFrontend, subscriptions } =
    { init =
        let
            ( model, cmd ) =
                init
        in
        ( model
        , Cmd.batch
            [ cmd
            , sendToViewer
                backendNoOp
                (Init { sessionName = sessionName, model = Debug.toString model })
            ]
        )
    , update =
        \\msg model ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            ( newModel
            , Cmd.batch
                [ cmd
                , if backendNoOp == msg then
                    Cmd.none

                  else
                    sendToViewer
                        backendNoOp
                        (Update
                            { sessionName = sessionName
                            , msg = Debug.toString msg
                            , newModel = Debug.toString newModel
                            }
                        )
                ]
            )
    , updateFromFrontend =
        \\sessionId clientId msg model ->
            let
                ( newModel, cmd ) =
                    updateFromFrontend sessionId clientId msg model
            in
            ( newModel
            , Cmd.batch
                [ cmd
                , sendToViewer
                    backendNoOp
                    (UpdateFromFrontend
                        { sessionName = sessionName
                        , msg = Debug.toString msg
                        , newModel = Debug.toString newModel
                        , sessionId = sessionId
                        , clientId = clientId
                        }
                    )
                ]
            )
    , subscriptions = subscriptions
    }


type DataType
    = Init { sessionName : String, model : String }
    | Update { sessionName : String, msg : String, newModel : String }
    | UpdateFromFrontend { sessionName : String, msg : String, newModel : String, sessionId : String, clientId : String }


sendToViewer : msg -> DataType -> Cmd msg
sendToViewer backendNoOp data =
    Time.now
        |> Task.andThen
            (\\time ->
                Http.task
                    { method = "POST"
                    , headers = []
                    , url = "http://localhost:8001/https://backend-debugger.lamdera.app/_r/data"
                    , body = Http.jsonBody (encodeDataType time data)
                    , resolver = Http.bytesResolver (\\_ -> Ok ())
                    , timeout = Just 10000
                    }
            )
        |> Task.attempt (\\_ -> backendNoOp)


encodeTime : Time.Posix -> Json.Encode.Value
encodeTime time =
    Time.posixToMillis time |> Json.Encode.int


encodeDataType : Time.Posix -> DataType -> Json.Encode.Value
encodeDataType time data =
    Json.Encode.list
        identity
        (case data of
            Init { sessionName, model } ->
                [ Json.Encode.int 0
                , Json.Encode.string sessionName
                , Json.Encode.string model
                , Json.Encode.null
                , encodeTime time
                ]

            Update { sessionName, msg, newModel } ->
                [ Json.Encode.int 1
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.null
                , encodeTime time
                ]

            UpdateFromFrontend { sessionName, msg, newModel, sessionId, clientId } ->
                [ Json.Encode.int 2
                , Json.Encode.string sessionName
                , Json.Encode.string msg
                , Json.Encode.string newModel
                , Json.Encode.string sessionId
                , Json.Encode.string clientId
                , Json.Encode.null
                , encodeTime time
                ]
        )
"""
            )

    else:
        if os.path.exists(file_path):
            os.remove(file_path)


def main():
    toggling_on = not check_toggling_state()
    toggle_debugger_backend(toggling_on=toggling_on)
    toggle_debugger_app(toggling_on)
    print(f"Debugger {'enabled' if toggling_on else 'disabled'}.")


if __name__ == "__main__":
    main()
