#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m' # No Color

PORT=8000

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --port=*) PORT="${1#*=}" ;;
        *) echo -e "${RED}Unknown parameter: $1${NC}"; exit 1 ;;
    esac
    shift
done

export GREEN BLUE YELLOW CYAN RED NC PORT

show_banner() {
    clear
    echo -e "${GREEN}"
    echo "╔═══════════════════════════════════════════╗" 
    echo "║         Lamdera Development Server        ║"
    echo "╚═══════════════════════════════════════════╝"
    echo -e "${NC}"
}
export -f show_banner

file_changed_message() {
    echo -e "\n${YELLOW}File changed: ${CYAN}$1${NC}"
    echo -e "${BLUE}Restarting Lamdera server...${NC}\n"
}
export -f file_changed_message

start_lamdera() {
    if [ -f /tmp/lamdera.pid ]; then
        local pid=$(cat /tmp/lamdera.pid)
        kill $pid 2>/dev/null || true
        sleep 0.2
        kill -9 $pid 2>/dev/null || true
        rm -f /tmp/lamdera.pid
    fi

    lamdera live --port=$PORT > /tmp/lamdera.log 2>&1 &
    echo $! > /tmp/lamdera.pid
    
    for i in {1..10}; do
        if lsof -ti:$PORT >/dev/null 2>&1; then
            echo -e "${GREEN}Server started on ${CYAN}http://localhost:$PORT${NC}\n"
            return 0
        fi
        sleep 0.1
    done
}
export -f start_lamdera

stop_lamdera() {
    if [ -f /tmp/lamdera.pid ]; then
        local pid=$(cat /tmp/lamdera.pid)
        kill $pid 2>/dev/null || true
        sleep 0.2
        kill -9 $pid 2>/dev/null || true
        rm -f /tmp/lamdera.pid
    fi
}
export -f stop_lamdera

cleanup() {
    stop_lamdera
    rm -f /tmp/lamdera.pid /tmp/lamdera.log
    exit 0
}

trap cleanup SIGINT SIGTERM EXIT

show_banner
echo -e "${BLUE}Watching for changes in elm-pkg-js/*.js${NC}\n"
start_lamdera

if command -v inotifywait >/dev/null 2>&1; then
    while true; do
        inotifywait -q -e modify -r elm-pkg-js/ 2>/dev/null | while read -r directory events filename; do
            if [[ "$filename" == *.js ]]; then
                show_banner
                file_changed_message "$directory$filename"
                start_lamdera
            fi
        done
    done
else
    touch /tmp/last_check
    while true; do
        find elm-pkg-js -name "*.js" -newer /tmp/last_check 2>/dev/null | while read file; do
            if [ -n "$file" ]; then
                show_banner
                file_changed_message "$file"
                start_lamdera
            fi
        done
        touch /tmp/last_check
        sleep 0.2
    done
fi