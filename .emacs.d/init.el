;; -----------------------------------------------------------------
;; GUI Customization

;; Enable mouse wheel scrolling
(mouse-wheel-mode 1)

;; Disable backup files
(setq make-backup-files nil)

;; Hide cursor in non-active buffers
(setq-default cursor-in-non-selected-widows nil)

;; Use a bar for the cursor
(setq-default cursor-type '(bar . 1))

;; Don't blink
(blink-cursor-mode 1)

;; Visual instead of audio bells
(setq visible-bell 1)

;; Turn off the toolbar and menubar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Turn off default startup messages
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Use system font
(setq font-use-system-font t)

;; ----------------------------------------------------------------
;; External Script Sources

;; Package Manager
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; User auload
(add-to-list 'load-path "~/.emacs.d/auto-load")

;; Color Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; ---------------------------------------------------------------
;; Text

(load-theme 'tango t)

;; Auto indent
(electric-indent-mode 1)
(electric-pair-mode 1)
(electric-layout-mode 1)

;; Auto Complete
(require 'auto-complete)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; ------------------------------------------------------------------
;; Extensions

;; rainbow-mode
;; Hilight them HTML colors
(require 'rainbow-mode)

;; web-mode
(autoload 'web-mode "web-mode" "Major mode for editing web templates." t)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdow files." t)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; extra ruby hooks
(autoload 'ruby-mode "ruby-mode" "Ruby Helpers." t)
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; YAML mode
(autoload 'yaml-mode "yaml-mode" "YAML editing mode." t)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Sass mode
;; Sass mode requires haml-mode
(autoload 'haml-mode "haml-mode" "Major mode for editing HAML files." t)
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(autoload 'sass-mode "sass-mode" "Sass major mode." t)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; Coffee Script Mode
(autoload 'coffee-mode "coffee-mode" "Coffee Script Mode." t)
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

;; jade-mode
(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; scala-mode
(add-to-list 'load-path "~/.emacs.d/scala-mode2")
(require 'scala-mode2)

;; colojure-mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; ------------------------------------------------------------------------------
;; EMACS-Apps

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;-----------------------------------------------------------------------------
;; SPELL CHECKING

(dolist (hook '(markdown-mode-hook text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; -----------------------------------------------------------------------------
;; HELPER FUNCTIONS CALLED VIA M-x

(defun make-unix-file ()
  "Change the current file encodind utf-8-unix"
  (interactiv)
  (set-buffer-file-coding-system 'utf-8-unix t))
