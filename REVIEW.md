# Configuration Review & Optimization Plan

**Date:** 2026-02-08  
**Status:** Phase 3 Complete - Pre-Phase 4 Review

---

## ✅ What's Working Well

### 1. **Solid Foundation**
- Evil mode setup is clean (jk escape, ; for command)
- Leader key bindings match nvchad conventions
- Performance optimizations in `early-init.el` are appropriate

### 2. **Good Package Choices**
- **Eglot over lsp-mode** → Simpler, built-in, faster
- **Vertico/Consult over Helm** → Modern, lightweight
- **Magit** → Industry standard, best Git interface
- **claude-code-ide.el** → Native ediff integration is excellent

### 3. **Sensible Defaults**
- 2-space indent (matching stylua)
- 120 column width
- Relative line numbers
- Transparency support

---

## 🔍 Issues Found

### 1. **Duplicate Git Hunk Navigation** ⚠️
**Problem:** Conflicting keybindings for git hunks
```elisp
;; In general.el leader bindings:
"gn" '(diff-hl-next-hunk :which-key "next hunk")
"gp" '(diff-hl-previous-hunk :which-key "prev hunk")  ; ← Conflicts with "gp" (git push)

;; In normal mode bindings:
"[g" 'diff-hl-previous-hunk
"]g" 'diff-hl-next-hunk  ; ← Better approach
```

**Fix:** Remove `SPC g n` and `SPC g p`, keep only `[g` / `]g`

---

### 2. **LSP Hover Provider Disabled** ⚠️
**Problem:**
```elisp
(setq eglot-ignored-server-capabilities '(:hoverProvider))  ; Disables K hover!
```

**Impact:** The `K` keybinding for hover docs won't work because we disabled hover!

**Fix:** Remove this line. If hover is slow, use `:documentHighlightProvider` instead.

---

### 3. **Unused/Unnecessary Packages** 📦

#### `evil-collection` - KEEP (needed for dired, magit, etc.)
#### `evil-commentary` - REPLACE?
Could use **evil-nerd-commenter** for more features, but current one works fine.
**Verdict:** Keep (simple, does the job)

#### `cape` - Possibly Overkill?
Adds `cape-dabbrev`, `cape-file`, `cape-elisp-block` to completion.
**Check:** Does LSP completion already provide enough?
**Verdict:** Keep for now, but test if removing it breaks anything

#### `helpful` - KEEP (better help buffers are worth it)

---

### 4. **Tree-sitter Auto-Install** ⚠️
```elisp
(treesit-auto-install 'prompt)
```

**Problem:** Will prompt every time you open a new language.
**Better:** Set to `'ask` or pre-install common grammars once.

---

### 5. **Missing Dependencies Documentation**
The config assumes these are installed:
- `ripgrep` (for `consult-ripgrep`)
- Language servers (pyright, typescript-language-server, rust-analyzer, etc.)
- Nerd Fonts

**Fix:** Add a `SETUP.md` documenting required system dependencies.

---

### 6. **Claude Code IDE - No Error Handling**
```elisp
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :branch "main")
```

**Problem:** If package fetch fails or `claude` CLI not found, no graceful degradation.
**Fix:** Add `:if (executable-find "claude")` or error messages.

---

### 7. **Transparency Applied Twice** 🎨
**In `early-init.el`:**
```elisp
(push '(alpha-background . 90) default-frame-alist)
```

**In `init.el`:**
```elisp
(defun my/apply-transparency-to-faces ()
  (set-face-background 'default "unspecified-bg")
  ...
)
```

**Question:** Are both needed? The first sets frame alpha, the second unsets face backgrounds.
**Verdict:** Keep both (they do different things), but document why.

---

## 🚀 Optimization Opportunities

### 1. **Use Built-in `project.el` Instead of Projectile?**
Emacs 28+ has `project.el` built-in, which is lighter than Projectile.

**Pros:**
- One less package
- Faster (simpler codebase)
- Built-in

**Cons:**
- Less features (no `projectile-replace`, project templates, etc.)
- Consult-projectile would need rewriting

**Recommendation:** Keep Projectile for now (more mature, better ecosystem).

---

### 2. **Simplify Leader Bindings**
Current setup has:
- `SPC f f` - find files
- `SPC f p` - project files
- `SPC p f` - project files (duplicate!)

**Fix:** Consolidate:
- `SPC f f` → all files (project-aware via consult)
- `SPC p f` → remove (redundant)

---

### 3. **Add `envrc` for `.envrc` Support?**
If working on projects with direnv/`.envrc`, add:
```elisp
(use-package envrc
  :hook (after-init . envrc-global-mode))
```

**Decision:** Add only if needed. Not everyone uses direnv.

---

### 4. **Performance: Lazy Load More Packages**
Currently most packages load immediately. Could defer:
```elisp
(use-package magit
  :defer t  ; Only load when called
  :commands (magit-status magit-blame)
  ...)
```

**Impact:** Faster startup (~200ms saved), but negligible for most users.
**Recommendation:** Do this if startup time becomes a problem.

---

## 📋 Action Plan

### Priority 1: Critical Fixes
- [ ] Fix `SPC g p` conflict (git push vs prev hunk)
- [ ] Remove `eglot-ignored-server-capabilities` (breaks hover)
- [ ] Add `:if` check for `claude-code-ide` (graceful degradation)

### Priority 2: Documentation
- [ ] Create `SETUP.md` with system dependencies
- [ ] Document why both transparency methods are used
- [ ] Add troubleshooting section

### Priority 3: Testing
- [ ] Test LSP hover (`K` keybinding) after fixing eglot
- [ ] Verify `[g` / `]g` git hunk navigation works
- [ ] Test Claude Code integration (ediff workflow)
- [ ] Check if `cape` completion sources are actually useful

### Priority 4: Nice-to-Have
- [ ] Consolidate duplicate keybindings
- [ ] Add lazy loading for heavy packages (magit, vterm)
- [ ] Consider `envrc` if direnv is used

---

## 🧪 Testing Checklist

### Core Editor
- [ ] Open file with `SPC f f`
- [ ] Switch buffers with `SPC b b`
- [ ] Split windows (`SPC w v` / `SPC w s`)
- [ ] Navigate windows (`C-h`, `C-j`, `C-k`, `C-l`)
- [ ] Test evil mode (`jk` to escape, `;` for command mode)
- [ ] Test transparency toggle (`SPC t h`)

### LSP & Code Navigation
- [ ] Open TypeScript/Python file
- [ ] Verify LSP starts automatically
- [ ] Test `gd` (go to definition)
- [ ] Test `gr` (find references)
- [ ] Test `K` (hover docs) - **currently broken, needs fix**
- [ ] Test `[d` / `]d` (diagnostics navigation)
- [ ] Test `SPC c a` (code actions)

### Git Integration
- [ ] Open magit with `SPC g g`
- [ ] Test `[g` / `]g` (next/prev hunk)
- [ ] Test diff-hl fringe indicators
- [ ] Test git-timemachine (`SPC g t`)

### AI Integration (Claude Code)
- [ ] Open Claude Code menu (`C-c '`)
- [ ] Send buffer to Claude (`SPC a c`)
- [ ] Send selection (`SPC a s`)
- [ ] Test ediff accept/reject workflow

### File Management
- [ ] Open dired-sidebar (`SPC e`)
- [ ] Test `h` (up directory) and `l` (open file)
- [ ] Test projectile project switching (`SPC p p`)

---

## 🎯 Recommendations

### Keep As-Is ✅
- Evil mode setup
- Vertico/Consult/Orderless completion
- Magit + diff-hl
- claude-code-ide.el (best available option)
- Doom themes + transparency
- Eglot for LSP

### Fix Immediately ⚠️
1. Git keybinding conflict
2. LSP hover disabled
3. Claude Code graceful degradation

### Consider Later 🤔
- Lazy loading for faster startup
- Project.el instead of Projectile (only if minimalism is priority)
- envrc if direnv is used

---

## 📝 Next Steps

1. **Apply Priority 1 fixes** (critical bugs)
2. **Create SETUP.md** (system dependencies)
3. **Run testing checklist** (verify everything works)
4. **Update PROGRESS.md** (mark review complete)
5. **Decide:** Phase 4 or polish current setup?

---

**Overall Assessment:** 🎉 **Solid 8/10**

The config is well-structured, uses modern packages, and has a clear progression. The main issues are:
- A few conflicting keybindings
- One critical LSP setting that breaks hover
- Missing documentation for system dependencies

After fixing these, the setup will be production-ready!
