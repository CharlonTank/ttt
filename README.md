# Ultimate Tic-tac-toe

## 🎯 Game Rules

Ultimate Tic-tac-toe is an advanced version of the classic game where each cell is itself a small Tic-tac-toe game. To win, you must align three won small Tic-tac-toe games on the large board.

## 🎮 Features

- **Multiple Game Modes**
  - Player vs Player online (with matchmaking)
  - Player vs Player offline
  - Player vs Bot (4 difficulty levels: Easy, Medium, Hard, Elite)

- **Modern User Interface**
  - Dark / Light mode
  - Multilingual support (French / English)
  - Responsive and intuitive interface

- **Advanced Features**
  - Move history (Undo/Redo)
  - First player selection (Human/Bot/Random)
  - Integrated debug mode
  - User preferences saving (language, dark mode)
  - Tutorial mode
  - "Play for me" feature against bots

## 🛠 Technologies

- **Frontend**: Elm
- **Backend**: Lamdera (Elm fullstack)
- **State Management**: Elm Architecture
- **Persistence**: LocalStorage for user preferences

## 🚀 Getting Started

### Prerequisites
- Install Lamdera by following the instructions at [Lamdera.com](https://lamdera.com/)
- Git for cloning the repository

### Setup and Development
1. Clone the repository:
   ```bash
   git clone https://github.com/CharlonTank/ttt.git
   cd ttt
   ```

2. Start the development server with auto-reload:
   First, make sure you have:
   - `fswatch` installed (`brew install fswatch` on macOS)

   Then run:
   ```bash
   ./lamdera-dev-watch.sh
   ```
   The application will be available at `http://localhost:8000` and will automatically reload when elm-pkg-js files are modified.

## 📁 Debugging

### Frontend Debug Panel
- Click 4 times on the French flag to activate the debug panel
- The debug panel shows:
  - Local storage content
  - Current language
  - Dark mode status
  - Game state
- The panel is draggable and resizable

### Backend Debugger
To enable backend debugger:
```bash
./toggle_debugger.py
```
This will set up the necessary code for communicating with the Martin Stewart Lamdera debugger and will also open the debugger in your browser.

## 📁 Project Structure

- `src/`
  - `Frontend.elm` - User interface and frontend logic
  - `Backend.elm` - Server logic
  - `Types.elm` - Shared types and data structures
  - `Bot.elm` - Bot logic and AI algorithms
  - `I18n.elm` - Internationalization
  - `Color.elm` - Color theme definitions
  - `Env.elm` - Environment variables
  - `Debugger.elm` - Frontend debug panel
  - `Tutorial/` - Tutorial mode implementation
    - `Tutorial.elm` - Tutorial logic
    - `View.elm` - Tutorial UI components
    - `Types.elm` - Tutorial-specific types
  - `Evergreen/` - Lamdera migrations

## 🔧 Contributing

Contributions are welcome! Feel free to open an issue or submit a pull request.
