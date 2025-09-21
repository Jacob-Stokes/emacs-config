# Emacs Configuration

VS Code-like layout for Emacs with file tree, terminals, and IDE features.

## Layout

- Left: Treemacs file explorer
- Center top: Main editor
- Center bottom: Terminal
- Right: Claude/GPT terminal switcher

## Packages

- **treemacs** - File tree sidebar
- **projectile** - Project management
- **company** - Autocomplete
- **doom-modeline** - Status bar
- **which-key** - Shows available keybindings

## Key Bindings

- `C-x t t` - Toggle file tree
- `C-c l` - Reset to default layout
- `C-c c` - Switch right pane to Claude terminal
- `C-c g` - Switch right pane to GPT terminal
- `C-c p p` - Switch project
- `C-c p f` - Find file in project

## Features

- Transparent background
- Relative line numbers
- Auto-save to dedicated directories
- Multiple terminal support
- Tab bar for multiple workspaces

## Installation

Clone and symlink:
```bash
git clone https://github.com/Jacob-Stokes/emacs-config.git ~/.emacs.d
```

Packages auto-install on first run.