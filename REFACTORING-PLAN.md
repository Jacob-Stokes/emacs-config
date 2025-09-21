# Init.el Refactoring Plan

## Current State Analysis
- **Size**: 911 lines (too large for a single file)
- **Structure**: Monolithic with mixed concerns
- **Maintainability**: Difficult to navigate and modify

## Identified Logical Modules

### 1. **Core Setup** (`vibes-core.el`)
- Package management (MELPA, use-package)
- Basic Emacs settings (startup messages, backups, auto-save)
- UI cleanup (menu bar, tool bar, scroll bar)
- Default directory settings

### 2. **Theme & Appearance** (`vibes-theme.el`)
- Transparent background setup
- Color theme configuration
- Face customization
- Line numbers configuration
- Highlight current line
- Show matching parentheses

### 3. **Package Configurations** (`vibes-packages.el`)
- Doom-modeline
- All-the-icons
- Treemacs setup
- Projectile
- Company
- Which-key
- Ace-window

### 4. **Vibe Features** (`vibes-aesthetic.el`)
- Beacon (cursor flash)
- Rainbow-mode
- Indent guides
- Nyan mode
- Rainbow delimiters
- Fireplace
- Imenu-list

### 5. **Dashboard & Welcome** (`vibes-dashboard.el`)
- Dashboard configuration
- Welcome buffer creation
- ASCII art banners
- Rainbow logo animation

### 6. **Animation System** (`vibes-animations.el`)
- Matrix rain animation
- Aquarium animation
- Animation switcher
- Animation timers and controls
- All animation-related variables and functions

### 7. **AI Terminal System** (`vibes-ai-terminals.el`)
- Claude terminal setup
- GPT/Codex terminal setup
- Gemini terminal setup
- Terminal switching functions
- Right pane header updates

### 8. **Layout Management** (`vibes-layout.el`)
- VS Code-like layout setup
- Window configuration
- Panel management
- Layout reset functions

### 9. **Keybindings** (`vibes-keybindings.el`)
- All global keybindings
- Terminal switching keys
- Animation control keys
- Layout control keys

## Proposed File Structure
```
~/.emacs.d/
├── init.el                    # Main loader (50-100 lines)
├── modules/
│   ├── vibes-core.el          # Core settings
│   ├── vibes-theme.el         # Theme and appearance
│   ├── vibes-packages.el     # Package configurations
│   ├── vibes-aesthetic.el    # Visual enhancements
│   ├── vibes-dashboard.el    # Dashboard and welcome
│   ├── vibes-animations.el   # Animation system
│   ├── vibes-ai-terminals.el # AI terminal management
│   ├── vibes-layout.el       # Layout management
│   └── vibes-keybindings.el  # All keybindings
├── custom.el                  # Custom-set variables
└── early-init.el             # Early initialization (optional)
```

## Implementation Strategy

### Phase 1: Setup Module System
1. Create `modules/` directory
2. Create new init.el that loads modules
3. Add module loading helpers

### Phase 2: Extract Modules (Order matters!)
1. **Core** - Extract first (others depend on it)
2. **Theme** - Visual settings
3. **Packages** - Third-party package configs
4. **Aesthetic** - Vibe features
5. **Animations** - All animation code
6. **AI Terminals** - Terminal management
7. **Dashboard** - Welcome and dashboard
8. **Layout** - Window layout functions
9. **Keybindings** - Extract last (references other modules)

### Phase 3: Testing & Refinement
1. Test each module loads correctly
2. Check for circular dependencies
3. Ensure proper load order
4. Add provide/require statements

## Benefits of Refactoring
1. **Easier Maintenance**: Find and modify specific features quickly
2. **Better Organization**: Clear separation of concerns
3. **Selective Loading**: Can disable entire modules easily
4. **Faster Debugging**: Issues isolated to specific modules
5. **Collaboration**: Multiple people can work on different modules
6. **Performance**: Can lazy-load some modules if needed

## Module Template
```elisp
;;; vibes-[name].el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: Your Name

;;; Commentary:
;; This module provides...

;;; Code:

;; Module content here

(provide 'vibes-[name])
;;; vibes-[name].el ends here
```

## Next Steps
1. Create backup of current init.el
2. Create modules directory
3. Start extracting modules one by one
4. Test after each extraction
5. Update documentation