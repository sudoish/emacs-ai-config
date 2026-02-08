# Setup Guide

This document lists all system dependencies required for this Emacs configuration to work properly.

---

## Required: Emacs Version

**Minimum:** Emacs 29.1 (for tree-sitter, eglot, use-package :vc)  
**Recommended:** Emacs 30+ (for improved performance and features)

Check your version:
```bash
emacs --version
```

---

## Required: Core Tools

### 1. **ripgrep** (for fast file search)
Used by `consult-ripgrep` for live grep (`SPC f g`, `SPC p g`)

**Install:**
```bash
# macOS
brew install ripgrep

# Ubuntu/Debian
sudo apt install ripgrep

# Arch
sudo pacman -S ripgrep

# Fedora
sudo dnf install ripgrep
```

**Verify:**
```bash
rg --version
```

---

### 2. **fd** (optional, for faster file finding)
Used by `consult-find` (fallback to `find` if not available)

**Install:**
```bash
# macOS
brew install fd

# Ubuntu/Debian
sudo apt install fd-find
sudo ln -s $(which fdfind) /usr/local/bin/fd  # Create alias

# Arch
sudo pacman -S fd

# Fedora
sudo dnf install fd-find
```

**Verify:**
```bash
fd --version
```

---

### 3. **Nerd Fonts** (for icons in UI)
Required for `nerd-icons`, `nerd-icons-dired`, `nerd-icons-corfu`

**Install:**
Download from: https://www.nerdfonts.com/

**Recommended fonts:**
- JetBrains Mono Nerd Font
- Hack Nerd Font
- FiraCode Nerd Font

**macOS:**
```bash
brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono-nerd-font
```

**Linux:**
```bash
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts
wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/JetBrainsMono.zip
unzip JetBrainsMono.zip
fc-cache -fv
```

**Verify:**
```bash
fc-list | grep -i nerd
```

**In Emacs:** Set the font in your config or via:
```elisp
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-12")
```

---

## Required: Language Servers (LSP)

Eglot requires language servers for each language you want LSP support.

### Python
**Install:** `pyright` (recommended) or `python-lsp-server`
```bash
npm install -g pyright
# OR
pip install python-lsp-server
```

**Verify:**
```bash
pyright --version
```

---

### TypeScript/JavaScript
**Install:** `typescript-language-server`
```bash
npm install -g typescript-language-server typescript
```

**Verify:**
```bash
typescript-language-server --version
```

---

### Go
**Install:** `gopls`
```bash
go install golang.org/x/tools/gopls@latest
```

**Verify:**
```bash
gopls version
```

---

### Rust
**Install:** `rust-analyzer`
```bash
# Via rustup (recommended)
rustup component add rust-analyzer

# Via system package manager
# macOS
brew install rust-analyzer

# Arch
sudo pacman -S rust-analyzer
```

**Verify:**
```bash
rust-analyzer --version
```

---

### C/C++
**Install:** `clangd`
```bash
# macOS
brew install llvm

# Ubuntu/Debian
sudo apt install clangd

# Arch
sudo pacman -S clang

# Fedora
sudo dnf install clang-tools-extra
```

**Verify:**
```bash
clangd --version
```

---

### Lua
**Install:** `lua-language-server`
```bash
# macOS
brew install lua-language-server

# Arch
sudo pacman -S lua-language-server

# Manual install
git clone https://github.com/LuaLS/lua-language-server
cd lua-language-server
./make.sh
```

**Verify:**
```bash
lua-language-server --version
```

---

### Other Languages
Eglot auto-detects many language servers. Check the full list:
https://github.com/joaotavora/eglot#connecting-to-a-server

---

## Optional: AI Integration (Phase 3)

### Claude Code CLI
**Required for:** `claude-code-ide.el` integration

**Install:**
```bash
npm install -g @anthropic-ai/claude-code
```

**Verify:**
```bash
claude --version
```

**Setup API key:**
```bash
export ANTHROPIC_API_KEY="your-api-key-here"
# Add to ~/.bashrc or ~/.zshrc
```

**Note:** If `claude` CLI is not found, the config will skip loading `claude-code-ide` (graceful degradation).

---

## Optional: Terminal Emulation

### vterm (fast terminal in Emacs)
**Required for:** `claude-code-ide` terminal backend

**Dependencies:**
- CMake
- libtool
- libvterm

**Install:**
```bash
# macOS
brew install cmake libtool libvterm

# Ubuntu/Debian
sudo apt install cmake libtool-bin libvterm-dev

# Arch
sudo pacman -S cmake libtool libvterm

# Fedora
sudo dnf install cmake libtool libvterm-devel
```

**First-time setup:**
When you first use vterm, Emacs will compile the module automatically. This requires the above dependencies.

---

## Troubleshooting

### Issue: Icons not showing (squares/boxes instead)
**Fix:**
1. Install a Nerd Font (see above)
2. Set it in Emacs:
   ```elisp
   (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-12")
   ```
3. Restart Emacs

---

### Issue: LSP not starting for a language
**Fix:**
1. Check if language server is installed: `which pyright` (or `gopls`, `rust-analyzer`, etc.)
2. Check if server is in PATH: `echo $PATH`
3. Check eglot diagnostics: `M-x eglot-events-buffer`
4. Manually start server: `M-x eglot`

---

### Issue: `consult-ripgrep` not working
**Fix:**
1. Install ripgrep: `brew install ripgrep` (or equivalent)
2. Verify: `rg --version`
3. Restart Emacs

---

### Issue: `claude-code-ide` not loading
**Fix:**
1. Install Claude CLI: `npm install -g @anthropic-ai/claude-code`
2. Set API key: `export ANTHROPIC_API_KEY="..."`
3. Verify: `claude --version`
4. Restart Emacs

**Note:** If you don't want Claude integration, the config will skip it automatically.

---

### Issue: vterm compilation fails
**Fix:**
1. Install dependencies: `cmake`, `libtool`, `libvterm` (see above)
2. Remove vterm package: Delete `~/.emacs.d/elpa/vterm-*`
3. Reinstall: `M-x package-reinstall RET vterm RET`
4. Restart Emacs

---

## Quick Install Script (macOS/Homebrew)

```bash
#!/bin/bash
# install-deps.sh - Install all dependencies for Emacs AI config

echo "Installing core tools..."
brew install ripgrep fd

echo "Installing Nerd Fonts..."
brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono-nerd-font

echo "Installing vterm dependencies..."
brew install cmake libtool libvterm

echo "Installing language servers..."
npm install -g pyright typescript-language-server typescript
brew install gopls rust-analyzer lua-language-server llvm

echo "Installing Claude CLI (optional)..."
npm install -g @anthropic-ai/claude-code

echo "Done! Restart Emacs to apply changes."
```

**Usage:**
```bash
chmod +x install-deps.sh
./install-deps.sh
```

---

## Verification Checklist

After installation, verify everything works:

- [ ] Emacs version ≥ 29.1
- [ ] `rg --version` (ripgrep)
- [ ] `fd --version` (fd-find)
- [ ] Nerd Font installed and set in Emacs
- [ ] Language servers installed for your languages
- [ ] `claude --version` (if using AI integration)
- [ ] vterm compiles successfully

**Test in Emacs:**
1. Open a file: `SPC f f`
2. Live grep: `SPC f g` (requires ripgrep)
3. Open LSP file (e.g., Python): Should auto-connect to LSP
4. Test icons: Open dired (`SPC e`) - should show file icons
5. Test Claude (if installed): `C-c '`

---

## Next Steps

After installing dependencies:
1. Start Emacs
2. Wait for packages to install (first launch may take 1-2 minutes)
3. Install tree-sitter grammars when prompted: `y`
4. Test core features (see verification checklist above)

Enjoy your AI-native Emacs! 🚀
