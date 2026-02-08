;;; init.el --- AI-Native Emacs Configuration -*- lexical-binding: t; -*-

;; Author: AI-Native Emacs Config
;; Description: Phase 1 - Core editor with vim keybindings matching nvchad

;;; Commentary:
;; This configuration aims to provide a modern, AI-native Emacs experience
;; with vim keybindings matching the nvchad configuration.

;;; Code:

;; ============================================================================
;; Package Management Setup
;; ============================================================================

;; Initialize package.el
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Install and configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)      ; Always install packages if not present
(setq use-package-always-defer nil)     ; Don't defer by default for predictable startup
(setq use-package-expand-minimally t)   ; Minimize expanded code for faster load

;; ============================================================================
;; Editor Settings (matching nvchad: 2-space indent, 120 column width)
;; ============================================================================

;; Indentation - 2 spaces (matching nvchad/stylua config)
(setq-default indent-tabs-mode nil)     ; Use spaces, not tabs
(setq-default tab-width 2)              ; Display tabs as 2 spaces
(setq-default standard-indent 2)        ; Standard indentation
(setq-default evil-shift-width 2)       ; Evil mode shift width

;; Line width - 120 columns (matching nvchad)
(setq-default fill-column 120)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Highlight current line (matching nvchad cursorline)
(global-hl-line-mode t)

;; Better scrolling
(setq scroll-margin 8)                  ; Keep 8 lines visible at edges
(setq scroll-conservatively 101)        ; Scroll smoothly
(setq scroll-preserve-screen-position t)

;; Word wrapping
(setq-default truncate-lines t)         ; Don't wrap long lines by default
(setq-default word-wrap t)              ; Wrap at word boundaries when enabled

;; Miscellaneous editor settings
(setq-default cursor-type 'bar)         ; Bar cursor
(setq ring-bell-function 'ignore)       ; Disable bell
(setq visible-bell nil)                 ; No visible bell
(setq create-lockfiles nil)             ; Don't create .#lock files
(setq make-backup-files nil)            ; Don't create backup~ files
(setq auto-save-default nil)            ; Disable auto-save

;; Better defaults
(fset 'yes-or-no-p 'y-or-n-p)           ; y/n instead of yes/no
(setq confirm-kill-emacs 'y-or-n-p)     ; Confirm before quitting
(setq require-final-newline t)          ; Newline at end of file
(setq sentence-end-double-space nil)    ; Single space after sentences

;; Remember cursor position
(save-place-mode 1)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Auto-pair brackets
(electric-pair-mode 1)

;; Delete selection when typing
(delete-selection-mode 1)

;; Revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; ============================================================================
;; Transparency Support (matching nvchad full transparency)
;; ============================================================================

(defun my/set-frame-transparency (value)
  "Set frame transparency to VALUE (0-100, where 100 is fully opaque)."
  (interactive "nTransparency (0-100): ")
  (set-frame-parameter nil 'alpha-background value))

(defun my/toggle-transparency ()
  "Toggle between transparent and opaque."
  (interactive)
  (let ((current (frame-parameter nil 'alpha-background)))
    (if (and current (< current 100))
        (my/set-frame-transparency 100)
      (my/set-frame-transparency 90))))

;; ============================================================================
;; Evil Mode - Vim Keybindings
;; ============================================================================

(use-package evil
  :init
  ;; Required for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)       ; Let evil-collection handle keybindings
  (setq evil-want-C-u-scroll t)         ; C-u scrolls up
  (setq evil-want-C-d-scroll t)         ; C-d scrolls down
  (setq evil-want-Y-yank-to-eol t)      ; Y yanks to end of line
  (setq evil-split-window-below t)      ; Horizontal splits below
  (setq evil-vsplit-window-right t)     ; Vertical splits right
  (setq evil-undo-system 'undo-redo)    ; Use native undo-redo (Emacs 28+)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  ;; jk to escape (matching nvchad)
  (define-key evil-insert-state-map (kbd "j")
    (lambda ()
      (interactive)
      (let ((next-char (read-event nil nil 0.2)))
        (if (eq next-char ?k)
            (evil-normal-state)
          (insert "j")
          (when next-char
            (setq unread-command-events (list next-char)))))))

  ;; ; to enter command mode (matching nvchad)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-visual-state-map (kbd ";") 'evil-ex))

;; Evil Collection - Better evil bindings for various modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil Commentary - gc to comment (matching vim-commentary)
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Evil Surround - ys, cs, ds for surrounding text
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ============================================================================
;; General.el - Leader Key Bindings (matching nvchad style)
;; ============================================================================

(use-package general
  :after evil
  :config
  (general-evil-setup t)

  ;; Create leader key definer (Space as leader, matching nvchad)
  (general-create-definer my/leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Define leader key bindings
  (my/leader-def
    ;; Top-level bindings
    "SPC" '(execute-extended-command :which-key "M-x")
    ":" '(eval-expression :which-key "eval")

    ;; Quit/close (matching nvchad <leader>q)
    "q" '(my/quit-buffer-or-emacs :which-key "quit buffer/emacs")

    ;; File operations (matching nvchad Telescope keybindings)
    "f" '(:ignore t :which-key "file/find")
    "ff" '(consult-find :which-key "find file")         ; SPC f f - like Telescope find_files
    "fg" '(consult-ripgrep :which-key "live grep")      ; SPC f g - like Telescope live_grep
    "fb" '(consult-buffer :which-key "buffers")         ; SPC f b - like Telescope buffers
    "fr" '(consult-recent-file :which-key "recent files")
    "fs" '(save-buffer :which-key "save file")
    "fR" '(rename-visited-file :which-key "rename file")
    "fp" '(consult-projectile :which-key "project files")
    "fP" '(projectile-switch-project :which-key "switch project")

    ;; Search operations
    "s" '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search in buffer")
    "sS" '(consult-line-multi :which-key "search all buffers")
    "si" '(consult-imenu :which-key "imenu")
    "sI" '(consult-imenu-multi :which-key "imenu all buffers")
    "so" '(consult-outline :which-key "outline")
    "sm" '(consult-mark :which-key "marks")
    "sr" '(consult-ripgrep :which-key "ripgrep")

    ;; Buffer operations
    "b" '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "kill buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "prev buffer")
    "bl" '(ibuffer :which-key "list buffers")
    "bs" '(consult-buffer-other-window :which-key "buffer other window")

    ;; Window operations
    "w" '(:ignore t :which-key "window")
    "wv" '(evil-window-vsplit :which-key "split vertical")
    "ws" '(evil-window-split :which-key "split horizontal")
    "wc" '(evil-window-delete :which-key "close window")
    "wo" '(delete-other-windows :which-key "only this window")
    "ww" '(other-window :which-key "other window")
    "wh" '(evil-window-left :which-key "window left")
    "wj" '(evil-window-down :which-key "window down")
    "wk" '(evil-window-up :which-key "window up")
    "wl" '(evil-window-right :which-key "window right")

    ;; Dired/Explorer (matching nvchad's NvimTree feel)
    "e" '(dired-sidebar-toggle-sidebar :which-key "file explorer")
    "E" '(dired-jump :which-key "dired here")

    ;; Toggle features
    "t" '(:ignore t :which-key "toggle")
    "th" '(my/toggle-transparency :which-key "toggle transparency")
    "tl" '(display-line-numbers-mode :which-key "line numbers")
    "tw" '(whitespace-mode :which-key "whitespace")
    "tt" '(my/load-theme-interactive :which-key "choose theme")

    ;; Help
    "h" '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hk" '(describe-key :which-key "describe key")
    "hm" '(describe-mode :which-key "describe mode")

    ;; Project operations (matching nvchad)
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(consult-projectile-find-file :which-key "find file in project")
    "pg" '(consult-ripgrep :which-key "grep in project")
    "pb" '(consult-project-buffer :which-key "project buffers")
    "pd" '(projectile-find-dir :which-key "find directory")
    "pr" '(projectile-recentf :which-key "recent project files")
    "pk" '(projectile-kill-buffers :which-key "kill project buffers")
    "pa" '(projectile-add-known-project :which-key "add project")

    ;; Git operations (matching nvchad LazyGit feel)
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit status")    ; SPC g g - main git interface
    "gG" '(magit-dispatch :which-key "magit dispatch")
    "gb" '(magit-blame :which-key "blame")
    "gl" '(magit-log-current :which-key "log")
    "gL" '(magit-log-buffer-file :which-key "file log")
    "gd" '(magit-diff-dwim :which-key "diff")
    "gD" '(magit-diff-buffer-file :which-key "file diff")
    "gf" '(magit-fetch :which-key "fetch")
    "gF" '(magit-pull :which-key "pull")
    "gp" '(magit-push :which-key "push")
    "gc" '(magit-commit :which-key "commit")
    "gs" '(magit-stage-file :which-key "stage file")
    "gS" '(magit-stage-modified :which-key "stage all")
    "gu" '(magit-unstage-file :which-key "unstage file")
    "gr" '(magit-refresh :which-key "refresh")
    "gt" '(git-timemachine :which-key "time machine")
    "gn" '(diff-hl-next-hunk :which-key "next hunk")
    "gp" '(diff-hl-previous-hunk :which-key "prev hunk")
    "gh" '(diff-hl-revert-hunk :which-key "revert hunk")

    ;; Code/LSP operations
    "c" '(:ignore t :which-key "code/lsp")
    "ca" '(eglot-code-actions :which-key "code actions")
    "cr" '(eglot-rename :which-key "rename")
    "cf" '(eglot-format :which-key "format")
    "cF" '(eglot-format-buffer :which-key "format buffer")
    "cd" '(xref-find-definitions :which-key "go to definition")
    "cD" '(xref-find-definitions-other-window :which-key "definition other window")
    "ci" '(eglot-find-implementation :which-key "find implementation")
    "ct" '(eglot-find-typeDefinition :which-key "find type definition")
    "cR" '(xref-find-references :which-key "find references")
    "cs" '(consult-eglot-symbols :which-key "workspace symbols")
    "ch" '(eldoc-doc-buffer :which-key "hover docs")
    "ce" '(consult-flymake :which-key "errors/diagnostics")
    "cq" '(eglot-shutdown :which-key "shutdown LSP")
    "cQ" '(eglot-reconnect :which-key "restart LSP")

    ;; AI (placeholder for future AI integration)
    "a" '(:ignore t :which-key "AI")
    "at" '(my/ai-task-placeholder :which-key "AI task"))

  ;; Window navigation with C-h/j/k/l (matching nvchad)
  (general-define-key
   :states '(normal visual motion)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)

  ;; LSP/Code navigation in normal mode (vim-style, matching nvchad)
  (general-define-key
   :states 'normal
   "gd" 'xref-find-definitions        ; Go to definition
   "gD" 'xref-find-definitions-other-window
   "gr" 'xref-find-references         ; Go to references
   "gi" 'eglot-find-implementation    ; Go to implementation
   "gt" 'eglot-find-typeDefinition    ; Go to type definition
   "K" 'eldoc-doc-buffer              ; Show hover docs
   "[d" 'flymake-goto-prev-error      ; Previous diagnostic
   "]d" 'flymake-goto-next-error      ; Next diagnostic
   "[g" 'diff-hl-previous-hunk        ; Previous git hunk
   "]g" 'diff-hl-next-hunk))

;; Helper function for leader-q (matching nvchad behavior)
(defun my/quit-buffer-or-emacs ()
  "Kill current buffer. If it's the last buffer, quit Emacs."
  (interactive)
  (if (one-window-p)
      (if (> (length (buffer-list)) 1)
          (kill-current-buffer)
        (save-buffers-kill-terminal))
    (kill-buffer-and-window)))

;; Placeholder for AI task (Phase 3+)
(defun my/ai-task-placeholder ()
  "Placeholder for AI task workflow (to be implemented in Phase 3)."
  (interactive)
  (message "AI task workflow will be implemented in Phase 3"))

;; Interactive theme loader
(defun my/load-theme-interactive ()
  "Interactively choose and load a theme."
  (interactive)
  (call-interactively 'load-theme))

;; ============================================================================
;; Which-Key - Show Available Keybindings
;; ============================================================================

(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)       ; Show quickly
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-add-column-padding 1)
  :config
  (which-key-mode))

;; ============================================================================
;; TokyoNight Theme with Transparency
;; ============================================================================

(use-package doom-themes
  :config
  ;; Global doom-themes settings
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)

  ;; Load the Tokyo Night theme
  (load-theme 'doom-tokyo-night t)

  ;; Enable visual bell flashing for errors
  (doom-themes-visual-bell-config)

  ;; Correct org-mode fontification
  (doom-themes-org-config))

;; Additional transparency for UI elements (matching nvchad's NONE backgrounds)
(defun my/apply-transparency-to-faces ()
  "Apply transparency to various faces for a cleaner look."
  (when (display-graphic-p)
    ;; Make various backgrounds transparent
    (set-face-background 'default "unspecified-bg")
    (set-face-background 'fringe "unspecified-bg")
    (set-face-background 'line-number "unspecified-bg")
    (set-face-background 'line-number-current-line "unspecified-bg")))

;; Apply after theme loads
(add-hook 'after-init-hook #'my/apply-transparency-to-faces)

;; ============================================================================
;; Doom Modeline - Modern Status Line
;; ============================================================================

(use-package doom-modeline
  :init
  (setq doom-modeline-height 28)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-window-width-limit nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-encoding nil)      ; Hide encoding
  (setq doom-modeline-indent-info nil)          ; Hide indent info
  (setq doom-modeline-minor-modes nil)          ; Hide minor modes
  (setq doom-modeline-vcs-max-length 20)
  :config
  (doom-modeline-mode 1))

;; Required for doom-modeline icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; ============================================================================
;; Recent Files
;; ============================================================================

(use-package recentf
  :ensure nil  ; Built-in
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 15)
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

;; ============================================================================
;; Better Minibuffer Completion (enhances find-file, switch-buffer, etc.)
;; ============================================================================

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 12))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

;; ============================================================================
;; Project Management with Projectile
;; ============================================================================

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/dev" "~/projects"))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)  ; Use external tools (faster)
  :config
  (projectile-mode +1))

;; ============================================================================
;; Consult - Powerful Search and Navigation (Telescope equivalent)
;; ============================================================================

(use-package consult
  :init
  ;; Use consult for xref locations
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  ;; Configure preview behavior
  (setq consult-preview-key "M-.")
  (setq consult-narrow-key "<")

  ;; Configure ripgrep arguments for live grep
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip"))

;; Consult integration with projectile
(use-package consult-projectile
  :after (consult projectile))

;; Embark - Actions on completion candidates (like Telescope actions)
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-consult integration
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; Dired - File Manager (Oil.nvim equivalent)
;; ============================================================================

(use-package dired
  :ensure nil  ; Built-in
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)           ; Suggest other dired buffer for operations
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)  ; Single dired buffer
  :config
  ;; Evil keybindings for dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "o" 'dired-find-file-other-window
    "gr" 'revert-buffer))

;; Dired-x for extra functionality
(use-package dired-x
  :ensure nil  ; Built-in
  :after dired
  :config
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\.DS_Store$\\|^\\.git$"))

;; Dired-sidebar - File tree like NvimTree/Oil
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  :config
  ;; Refresh sidebar when switching projects
  (setq dired-sidebar-should-follow-file t))

;; Nerd icons for dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; ============================================================================
;; Git Integration - Magit (LazyGit equivalent, arguably better)
;; ============================================================================

(use-package magit
  :commands (magit-status magit-blame magit-log-current magit-log-buffer-file)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)     ; Show word-level diff highlighting
  :config
  ;; Full-screen magit status
  (setq magit-bury-buffer-function #'magit-restore-window-configuration))

;; Git gutter - Show git diff in fringe (like gitsigns.nvim)
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-margin-symbols-alist
   '((insert . "│")
     (delete . "│")
     (change . "│")
     (unknown . "│")
     (ignored . "│")))
  :config
  ;; Use margin mode for terminal compatibility
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

;; Git time machine - Browse file history
(use-package git-timemachine
  :commands git-timemachine)

;; ============================================================================
;; LSP with Eglot (built-in, simpler than lsp-mode)
;; ============================================================================

(use-package eglot
  :ensure nil  ; Built-in in Emacs 29+
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (lua-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)          ; Shutdown server when last buffer closes
  (eglot-events-buffer-size 0)    ; Disable events buffer for performance
  (eglot-sync-connect nil)        ; Don't block on connect
  (eglot-connect-timeout 10)
  :config
  ;; Performance optimizations
  (setq eglot-ignored-server-capabilities '(:hoverProvider))  ; Disable hover for speed

  ;; Add language servers if needed
  ;; Most modern servers are auto-detected, but you can customize:
  ;; (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  )

;; Consult integration with eglot
(use-package consult-eglot
  :after (consult eglot)
  :commands consult-eglot-symbols)

;; ============================================================================
;; Corfu - Inline Completion (blink.cmp equivalent)
;; ============================================================================

(use-package corfu
  :custom
  (corfu-cycle t)                  ; Enable cycling through candidates
  (corfu-auto t)                   ; Enable auto-completion
  (corfu-auto-prefix 2)            ; Start completion after 2 characters
  (corfu-auto-delay 0.1)           ; Delay before showing completions
  (corfu-quit-no-match 'separator) ; Don't quit if no match
  (corfu-preview-current nil)      ; Don't preview current candidate
  (corfu-preselect 'prompt)        ; Don't preselect first candidate
  (corfu-on-exact-match nil)       ; Don't auto-insert on exact match
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Show docs after delay
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)  ; Show documentation popup
  :config
  ;; TAB cycle through candidates
  (define-key corfu-map (kbd "TAB") 'corfu-next)
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "S-TAB") 'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous)
  ;; RET to confirm
  (define-key corfu-map (kbd "RET") 'corfu-insert)
  ;; Escape to quit
  (define-key corfu-map (kbd "<escape>") 'corfu-quit))

;; Nerd icons for corfu
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ============================================================================
;; Cape - Completion At Point Extensions
;; ============================================================================

(use-package cape
  :init
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; Configure cape-dabbrev
  (setq cape-dabbrev-min-length 3)
  (setq cape-dabbrev-check-other-buffers t))

;; ============================================================================
;; Flymake - Diagnostics (works with eglot)
;; ============================================================================

(use-package flymake
  :ensure nil  ; Built-in
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)  ; Run checks after 0.5s of no changes
  (flymake-start-on-save-buffer t)
  :config
  ;; Show diagnostics in minibuffer
  (setq flymake-show-diagnostics-at-end-of-line t)  ; Emacs 30+, ignored otherwise

  ;; Custom fringe indicators
  (define-fringe-bitmap 'flymake-big-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00111100
            #b01111110
            #b01111110
            #b01111110
            #b00111100
            #b00000000
            #b00000000
            #b00000000)))

;; Consult integration for flymake
(use-package consult-flymake
  :after (consult flymake)
  :ensure nil  ; Part of consult
  :commands consult-flymake)

;; ============================================================================
;; Helpful - Better Help Buffers
;; ============================================================================

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

;; ============================================================================
;; Tree-sitter Support (Emacs 29+)
;; ============================================================================

;; Tree-sitter provides better syntax highlighting and code understanding
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)  ; Prompt before installing grammars
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; ============================================================================
;; Language Modes
;; ============================================================================

;; TypeScript/JavaScript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . eglot-ensure)
  :custom
  (typescript-indent-level 2))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-fontify-code-blocks-natively t))

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level 2))

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure)
  :custom
  (rust-format-on-save t))

;; ============================================================================
;; Custom File Location
;; ============================================================================

;; Keep custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; ============================================================================
;; Startup Message
;; ============================================================================

(defun my/display-startup-time ()
  "Display startup time in minibuffer."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;;; init.el ends here
