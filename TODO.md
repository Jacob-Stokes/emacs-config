# VibEmacs TODO

## Phase 1: Core Modular Systems

### 1. Modular Terminal Panel System (HIGH PRIORITY)
- [ ] Create `vibe-terminal.el` module for terminal panel management
- [ ] Build auto-discovery system for terminal modules in `terminal-modules/` subfolder
- [ ] Create self-registering terminal modules with metadata registration
- [ ] Implement dynamic tab switching for bottom terminal panel
- [ ] Add keybinding auto-registration (`C-c t [key]` pattern)
- [ ] Support custom ordering via `terminal-modules/order.el`
- [ ] Create terminal header with tab indicators and current terminal display

### 2. Essential Terminal Modules (HIGH PRIORITY)
- [ ] **bash.el** - Standard bash shell
- [ ] **docker.el** - Container shell selector with running container list
- [ ] **ssh.el** - SSH connection manager with saved hosts
- [ ] **git.el** - Git status/log viewer with interactive commands
- [ ] **python.el** - Python REPL with IPython detection
- [ ] **node.el** - Node.js REPL
- [ ] **htop.el** - System monitor

### 3. Modular Treemacs Panel System (MEDIUM PRIORITY)
- [ ] Research Treemacs extensibility options
- [ ] Create `vibe-treemacs.el` module if possible
- [ ] Investigate if Treemacs supports plugin architecture
- [ ] Explore alternative: custom file explorer with modular views
- [ ] Implement different file tree views (standard, git-focused, project-focused)

## Implementation Notes

### Terminal Panel Architecture
- Follow same pattern as side panels and animations
- Each terminal module should register:
  - `name` - Display name
  - `command` - Shell command to run
  - `key` - Keybinding suffix
  - `color` - Tab color theme
  - `setup-function` - Custom initialization

### Proposed Terminal Modules

#### Core Shells
- **bash** - Standard bash shell (C-c t b)
- **zsh** - Zsh with oh-my-zsh themes (C-c t z)
- **fish** - Fish shell with syntax highlighting (C-c t f)

#### Language REPLs
- **python** - Python REPL with IPython if available (C-c t p)
- **node** - Node.js REPL (C-c t n)
- **ruby** - Ruby IRB (C-c t r)
- **lua** - Lua interpreter (C-c t l)

#### Development Tools
- **docker** - Docker container shell selector (C-c t d)
- **ssh** - SSH connection manager (C-c t s)
- **git** - Git status/log viewer (C-c t g)
- **htop** - System monitor (C-c t h)

#### Specialized Environments
- **claude** - Claude AI terminal (C-c t c)
- **vim** - Vim editor in terminal (C-c t v)
- **emacs** - Nested Emacs terminal session (C-c t e)
- **tmux** - Tmux session manager (C-c t m)

#### System Utils
- **logs** - System log viewer (journalctl/tail) (C-c t L)
- **files** - File browser (ranger/mc if available) (C-c t F)
- **network** - Network tools (ping/curl/wget) (C-c t N)

### Treemacs Considerations
- Treemacs may be less modular than other components
- Consider hybrid approach: keep Treemacs core, add modular views
- Alternative: build custom file explorer using same auto-discovery pattern

## Phase 2: Enhanced Features

### 4. Additional Terminal Modules (MEDIUM PRIORITY)
- [ ] **zsh.el** - Zsh with oh-my-zsh themes
- [ ] **fish.el** - Fish shell with syntax highlighting
- [ ] **ruby.el** - Ruby IRB
- [ ] **lua.el** - Lua interpreter
- [ ] **vim.el** - Vim editor in terminal
- [ ] **tmux.el** - Tmux session manager
- [ ] **logs.el** - System log viewer (journalctl/tail)
- [ ] **files.el** - File browser (ranger/mc)
- [ ] **network.el** - Network tools (ping/curl/wget)

### 5. Visual Enhancements (LOW PRIORITY)
- [ ] Zone-out screensavers with custom animations
- [ ] Particle effects following cursor movement
- [ ] Weather widget in modeline with ASCII art
- [ ] Different ASCII logo themes (retro, synthwave, cyberpunk)
- [ ] Mood-based color themes by time of day

### 6. Interactive Features (LOW PRIORITY)
- [ ] ASCII pet in terminal that reacts to typing speed
- [ ] Code streak tracking with celebrations
- [ ] Random motivational quotes rotation
- [ ] GitHub activity widget in ASCII
- [ ] Pomodoro timer with visual animations
- [ ] Achievement system with sound effects

## Phase 3: Polish & Distribution

### 7. Quality of Life
- [ ] Terminal session persistence across restarts
- [ ] Terminal history management
- [ ] Project-specific terminal configurations
- [ ] Terminal multiplexer integration improvements
- [ ] Configuration wizard for first-time setup
- [ ] Performance optimizations for animations
- [ ] Mobile/tablet optimization settings

### 8. Documentation & Distribution
- [ ] Complete README with screenshots
- [ ] Installation guide for different platforms
- [ ] Configuration documentation
- [ ] Plugin development guide
- [ ] Video demo/walkthrough
- [ ] Package for MELPA distribution
- [ ] Create VibEmacs website/landing page

## Technical Questions to Investigate
1. Can Treemacs be extended with custom modules?
2. Should we replace Treemacs with custom file explorer?
3. How to handle terminal session management across layout resets?
4. Performance impact of multiple animations running simultaneously?
5. Best practices for terminal module error handling?
6. Mobile terminal app compatibility testing needed?