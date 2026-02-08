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

    ;; File operations
    "f" '(:ignore t :which-key "file")
    "fs" '(save-buffer :which-key "save file")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open-files :which-key "recent files")
    "fR" '(rename-visited-file :which-key "rename file")

    ;; Search (matching nvchad sf/sg patterns)
    "s" '(:ignore t :which-key "search")
    "sf" '(find-file :which-key "find file")
    "sg" '(grep :which-key "grep")
    "sb" '(switch-to-buffer :which-key "switch buffer")
    "sr" '(recentf-open-files :which-key "recent files")

    ;; Buffer operations
    "b" '(:ignore t :which-key "buffer")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "kill buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "prev buffer")
    "bl" '(ibuffer :which-key "list buffers")

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

    ;; Git (placeholder for future magit)
    "g" '(:ignore t :which-key "git")

    ;; AI (placeholder for future AI integration)
    "a" '(:ignore t :which-key "AI")
    "at" '(my/ai-task-placeholder :which-key "AI task"))

  ;; Window navigation with C-h/j/k/l (matching nvchad)
  (general-define-key
   :states '(normal visual motion)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right))

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
;; Helpful - Better Help Buffers
;; ============================================================================

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

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
