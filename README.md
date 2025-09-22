```
██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗
██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗
╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
 ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
  ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
```

**[ Vibes + Emacs = Maximum Flow ]**

Terminal-first Emacs configuration for mobile coding with a modular plugin architecture. Perfect for coding on tablets, SSH sessions, and anywhere you need a full development environment in a terminal.

## ✨ Features

### 🎨 Visual Vibes
- **Rainbow animated logo** - VIBEMACS banner with cycling colors
- **Matrix rain animations** - Multiple animation modes (matrix, aquarium, starfield, scrolling logo)
- **Zone.el screensavers** - Built-in trippy screensavers for idle time
- **Custom Treemacs colors** - Cyan folders, purple dotfiles, vibey file tree
- **Transparent terminal theme** - Clean, minimal aesthetic

### 🏗️ Modular Architecture
- **Auto-discovery panels** - Drop `.el` files in `panel-modules/` for instant integration
- **Auto-discovery animations** - Add new animations without touching core code
- **Custom ordering** - Control panel/animation order with `order.el` files
- **Self-registering modules** - Plugins automatically expose keybindings and metadata

### 🖥️ VS Code-Style Layout
- **Left**: Treemacs file explorer with git integration
- **Center**: Main editor with bottom terminal panel
- **Right**: AI assistant panels (Claude, GPT, Gemini, System info)
- **Smart window cycling** - `C-c o` cycles through work areas, skips sidebar

### 🤖 AI Integration
- **Multiple AI terminals** - Dedicated terminals for Claude, GPT, Gemini
- **Tab-based switching** - Easy switching between AI assistants
- **System info panel** - Screenfetch system information display

## 🚀 Quick Start

```bash
git clone https://github.com/Jacob-Stokes/vibemacs.git ~/.emacs.d
```

Packages auto-install on first run. Designed for terminal use - perfect for:
- SSH sessions to remote servers
- Coding on tablets via terminal apps
- Lightweight development environments
- Mobile coding workflows

## ⌨️ Key Bindings

### Core Navigation
- `C-c l` - Setup/reset VibEmacs layout
- `C-c o` - Cycle through main windows (editor → panels → terminal)
- `C-c v t` - Jump to Treemacs file explorer
- `M-o` - Ace-window (jump to any window by letter)

### AI Panels (Right Side)
- `C-c c` - Claude assistant
- `C-c g` - GPT assistant
- `C-c v c` - Gemini assistant
- `C-c y` - System info panel

### Animations & Fun
- `C-c n` - Switch animation mode
- `C-c v z` - Instant screensaver (zone.el)
- Auto-screensaver activates after 5 minutes idle

### Project Management
- `C-c p p` - Switch project (Projectile)
- `C-c p f` - Find file in project
- `C-x t t` - Toggle Treemacs

## 🛠️ Extending VibEmacs

### Adding New Panels
1. Create `.el` file in `modules/panel-modules/`
2. Define panel metadata and functions
3. Restart Emacs - auto-discovered and keybound!

### Adding New Animations
1. Create `.el` file in `modules/animation-modules/`
2. Implement animation functions
3. Auto-discovered and added to rotation

### Custom Ordering
- Create `order.el` in module folders to control display order
- Panels/animations not listed appear alphabetically after ordered ones

## 🎯 Design Philosophy

VibEmacs prioritizes:
- **Terminal-first** - Works perfectly over SSH, mobile apps, web terminals
- **Extensible** - True plugin architecture with zero-config module loading
- **Aesthetic** - Functional but fun, serious development with personality
- **Mobile-friendly** - Optimized for coding on tablets and remote sessions

Perfect for developers who want VS Code familiarity with Emacs power, especially for mobile and remote coding workflows.