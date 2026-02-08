# 🤖 AI-Native Emacs Configuration

A modern, AI-first Emacs setup with Vim keybindings, LSP support, and native Claude Code integration.

**Built for:** Developers who want the power of Emacs with the ergonomics of Vim and AI assistance.

---

## ✨ Features

### 🎮 **Vim Experience**
- **Evil mode** with `jk` escape and `;` for command mode
- **Leader key** (`SPC`) matching nvchad conventions
- **which-key** for keybinding discovery
- Vim-style navigation (`gd`, `gr`, `K`, `[d`, `]d`)

### 🔍 **Modern UI**
- **Tokyo Night** theme with transparency support
- **Vertico + Consult** for fuzzy finding (Telescope equivalent)
- **Doom modeline** with Nerd Icons
- **Relative line numbers**, 2-space indent, 120-column width

### 🛠️ **LSP & Development**
- **Eglot** (built-in LSP client) with auto-setup for:
  - Python (pyright)
  - TypeScript/JavaScript (typescript-language-server)
  - Go (gopls), Rust (rust-analyzer), C/C++ (clangd), Lua
- **Corfu** inline completion (blink.cmp equivalent)
- **Flymake** diagnostics with fringe indicators
- **Tree-sitter** for better syntax highlighting

### 🌳 **File Management**
- **dired-sidebar** file tree (NvimTree/Oil.nvim feel)
- **Projectile** for project management
- **consult-ripgrep** for live grep

### 🐙 **Git Integration**
- **Magit** - best Git interface (better than LazyGit)
- **diff-hl** - inline git status in fringe
- **git-timemachine** - browse file history
- Vim-style hunk navigation (`[g`, `]g`)

### 🤖 **AI Integration** (Phase 3)
- **claude-code-ide.el** - Native Claude Code integration
- **Ediff workflow** for reviewing AI suggestions
- **MCP tools** - Claude can access LSP, diagnostics, xref
- **File tracking** - Claude knows your context

---

## 🚀 Quick Start

### 1. Install Dependencies

**macOS (Homebrew):**
```bash
# Core tools
brew install ripgrep fd

# Nerd Fonts
brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono-nerd-font

# vterm dependencies
brew install cmake libtool libvterm

# Language servers (pick what you need)
npm install -g pyright typescript-language-server typescript
brew install gopls rust-analyzer lua-language-server llvm

# Claude CLI (optional)
npm install -g @anthropic-ai/claude-code
```

**See [SETUP.md](SETUP.md) for detailed installation instructions for all platforms.**

---

### 2. Install Configuration

```bash
# Backup existing config
mv ~/.emacs.d ~/.emacs.d.backup

# Clone this repo
git clone https://github.com/sudoish/emacs-ai-config.git ~/.emacs.d

# Start Emacs
emacs
```

**First launch:** Packages will auto-install (~1-2 minutes). Tree-sitter grammars will prompt for installation.

---

## 📚 Documentation

- **[SETUP.md](SETUP.md)** - System dependencies, language servers, troubleshooting
- **[PROGRESS.md](PROGRESS.md)** - Build phases and implementation status
- **[REVIEW.md](REVIEW.md)** - Configuration audit, recommendations, testing checklist

---

## ⌨️ Key Bindings Cheat Sheet

### Leader Key: `SPC` (Space)

#### Files & Search
- `SPC f f` - Find file
- `SPC f g` - Live grep (ripgrep)
- `SPC f b` - Switch buffer
- `SPC f r` - Recent files
- `SPC s s` - Search in current buffer

#### Buffers & Windows
- `SPC b b` - Switch buffer
- `SPC b d` - Kill buffer
- `SPC w v` - Split vertical
- `SPC w s` - Split horizontal
- `C-h/j/k/l` - Navigate windows

#### Git
- `SPC g g` - Magit status
- `SPC g b` - Git blame
- `SPC g l` - Git log
- `SPC g d` - Git diff
- `[g` / `]g` - Prev/next git hunk

#### LSP/Code
- `gd` - Go to definition
- `gr` - Find references
- `K` - Hover docs
- `SPC c a` - Code actions
- `SPC c r` - Rename symbol
- `SPC c f` - Format buffer
- `[d` / `]d` - Prev/next diagnostic

#### AI (Claude Code)
- `C-c '` - Open Claude Code menu
- `SPC a c` - Send buffer to Claude
- `SPC a s` - Send selection to Claude

#### Project
- `SPC p p` - Switch project
- `SPC p f` - Find file in project
- `SPC p g` - Grep in project

#### Other
- `SPC e` - Toggle file explorer
- `SPC q` - Quit buffer/Emacs
- `jk` - Exit insert mode (in Evil)
- `;` - Command mode (in Evil normal)

---

## 🏗️ Build Phases

### ✅ Phase 1: Foundation (Complete)
Core editor with Evil mode, leader keys, theme, and basic navigation.

### ✅ Phase 2: Advanced Editor (Complete)
Fuzzy finding (Telescope equivalent), LSP, Git integration, file tree.

### ✅ Phase 3: AI Integration (Complete)
Claude Code IDE with ediff workflow for reviewing AI suggestions.

**Status:** Production-ready (Score: 9/10)

### ⏳ Phase 4: AI-Native Workflow (Planned)
Ideas: gptel chat, Aider integration, AI code review, context builders.

### ⏳ Phase 5: Polish & Automation (Planned)
Ideas: Custom snippets, project templates, automated workflows.

---

## 🔧 Configuration Structure

```
~/.emacs.d/
├── early-init.el     # Performance optimizations (loads first)
├── init.el           # Main configuration
├── custom.el         # Emacs custom-set-* (auto-generated)
├── PROGRESS.md       # Build tracker
├── REVIEW.md         # Configuration audit
└── SETUP.md          # Installation guide
```

---

## 🐛 Troubleshooting

### Icons not showing?
1. Install a Nerd Font (see SETUP.md)
2. Set it in Emacs: `(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-12")`
3. Restart Emacs

### LSP not starting?
1. Check language server is installed: `which pyright` (or `gopls`, etc.)
2. Check eglot events: `M-x eglot-events-buffer`
3. Manually start: `M-x eglot`

### Claude Code not loading?
1. Install CLI: `npm install -g @anthropic-ai/claude-code`
2. Set API key: `export ANTHROPIC_API_KEY="..."`
3. Verify: `claude --version`

**See [SETUP.md](SETUP.md) for full troubleshooting guide.**

---

## 📦 Requirements

- **Emacs:** 29.1+ (30+ recommended)
- **Tools:** ripgrep, fd (optional), Nerd Fonts
- **LSP:** Language servers for your languages
- **AI:** Claude CLI (optional, for Phase 3)

---

## 🤝 Credits

**Inspired by:**
- [nvchad](https://nvchad.com/) - Vim keybindings and conventions
- [doom-emacs](https://github.com/doomemacs/doomemacs) - Theme and modeline
- [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el) - AI integration

**Built with:**
- [Evil](https://github.com/emacs-evil/evil) - Vim emulation
- [Eglot](https://github.com/joaotavora/eglot) - LSP client
- [Magit](https://magit.vc/) - Git interface
- [Vertico](https://github.com/minad/vertico) + [Consult](https://github.com/minad/consult) - Completion

---

## 📄 License

MIT License - See LICENSE file for details.

---

## 🚀 Contributing

Contributions welcome! This is an evolving configuration.

**Areas for improvement:**
- Additional language support
- More AI integrations (gptel, Aider)
- Custom keybindings
- Documentation improvements

Open an issue or PR!

---

**Built by:** [@sudoish](https://github.com/sudoish)  
**Powered by:** Emacs + Evil + AI 🤖
