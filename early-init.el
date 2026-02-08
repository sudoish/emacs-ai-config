;;; early-init.el --- Early initialization for Emacs -*- lexical-binding: t; -*-

;; Author: AI-Native Emacs Config
;; Description: Performance optimizations that run before init.el

;;; Commentary:
;; This file is loaded before init.el and before package.el and GUI are initialized.
;; Use it for performance-critical settings that must run early.

;;; Code:

;; ============================================================================
;; Performance Optimizations
;; ============================================================================

;; Increase garbage collection threshold during startup
;; This dramatically improves startup time by reducing GC pauses
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Reset GC settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

;; Prevent package.el from loading packages prior to init.el
;; We use use-package with straight.el or built-in package.el instead
(setq package-enable-at-startup nil)

;; Disable expensive file handler operations during startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Inhibit resizing frame during startup
(setq frame-inhibit-implied-resize t)

;; Disable site-run-file
(setq site-run-file nil)

;; ============================================================================
;; UI Performance - Disable before loading
;; ============================================================================

;; Disable UI elements early for faster startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable menu bar, tool bar, and scroll bar
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ============================================================================
;; Transparency Support (matching nvchad config)
;; ============================================================================

;; Enable transparency - 95% opacity (matching tokyonight transparent feel)
;; Format: (active . inactive) where 100 is fully opaque
(push '(alpha-background . 90) default-frame-alist)

;; ============================================================================
;; Native Compilation Settings (Emacs 28+)
;; ============================================================================

(when (featurep 'native-compile)
  ;; Silence native compilation warnings
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Set native compilation cache directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; ============================================================================
;; Miscellaneous Early Settings
;; ============================================================================

;; Disable startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Disable scratch buffer message
(setq initial-scratch-message nil)

;; Start with *scratch* buffer
(setq initial-major-mode 'fundamental-mode)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(setq default-input-method nil)

;;; early-init.el ends here
