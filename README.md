# Ultimate Tic-tac-toe

## 🎯 Game Rules

Ultimate Tic-tac-toe is an advanced version of the classic game where each cell is itself a small Tic-tac-toe game. To win, you must align three won small Tic-tac-toe games on the large board.

## 🎮 Features

- **Multiple Game Modes**
  - Player vs Player locally
  - Player vs Bot (4 difficulty levels: Easy, Medium, Hard, Elite)

- **Modern User Interface**
  - Dark / Light mode
  - Multilingual support (French / English)
  - Responsive and intuitive interface

- **Advanced Features**
  - Move history (Undo/Redo)
  - First player selection
  - Integrated debug mode
  - User preferences saving

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
   git clone https://github.com/your-username/ttt.git
   cd ttt
   ```

2. Start the development server:
   ```bash
   lamdera live
   ```
   The application will be available at `http://localhost:8000`

## 📁 Debugging

- Click 4 times on the French flag to activate the debug panel
- The debug panel shows:
  - Local storage content
  - Current language
  - Dark mode status
  - Game state
- The panel is draggable and resizable

## 📁 Project Structure

- `src/`
  - `Frontend.elm` - User interface and frontend logic
  - `Backend.elm` - Server logic
  - `Types.elm` - Shared types and data structures
  - `Bot.elm` - Bot logic and AI algorithms
  - `I18n.elm` - Internationalization
  - `Debugger.elm` - Debugging tools
  - `Env.elm` - Environment variables
  - `Evergreen/` - Lamdera migrations

## 🔧 Contributing

Contributions are welcome! Feel free to open an issue or submit a pull request.
