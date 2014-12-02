;; ----------------------------------------------------------------
;; External Script Sources

;; Package Manager
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; User auload
(add-to-list 'load-path "~/.emacs.d/auto-load")

;; -----------------------------------------------------------------
;; GUI Customization

;; Enable mouse wheel scrolling
(mouse-wheel-mode 1)

;; Turn off the toolbar and menubar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Color Theme
(load-theme 'flatui t)

;; Default window size
(add-hook 'before-make-frame-hook
	  #'(lambda ()
	      (add-to-list 'default-frame-alist '(left   . 0))
	      (add-to-list 'default-frame-alist '(top    . 0))
	      (add-to-list 'default-frame-alist '(height . 60))
	      (add-to-list 'default-frame-alist '(width  . 80))))

;; Hide cursor in non-active buffers
(setq-default cursor-in-non-selected-widows nil)

;; Use a bar for the cursor
(setq-default cursor-type '(bar . 2) )

;; Don't blink
(blink-cursor-mode 1)

;; Visual instead of audio bells
(setq visible-bell 1)

;; Turn off default startup messages
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Use system font
(setq font-use-system-font t)

;; Show line numbers
(line-number-mode)
(column-number-mode)

;; ----------------------------------------------------------------
;; Disable backup files
(setq make-backup-files nil)

;; ---------------------------------------------------------------
;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; --------------------------------------------------------------
;; Default File Encoding
(setq default-buffer-file-coding-system 'utf-8-unix)

;; ---------------------------------------------------------------
;; Enable Helm
(helm-mode 1)

;; --------------------------------------------------------------
;; Enable Git-Gutter mode
(global-git-gutter-mode 1)

;; --------------------------------------------------------------
;; Text Editing 

;; electric modes
(electric-pair-mode t)
(electric-indent-mode t)
;(electric-layout-mode t)

;; Whitespace cleanup
(global-whitespace-cleanup-mode)

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete)
(global-auto-complete-mode t)

;; Auto insert mode
;; TODO: Setup
(auto-insert-mode t)

;; Add coffee-mode to the list of auto complete modes
(add-to-list 'ac-modes 'coffee-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(flymake-coffee-coffeelint-configuration-file (expand-file-name "~/.emacs.d/coffeelint.json"))
 '(whitespace-cleanup-mode-ignore-modes (quote (special-mode view-mode comint-mode cider-repl-mode haskell-interactive-mode org-mode jade-mode))))

;; =============================================================================
;; Extensions
;;==============================================================================
;;--------------------------------------------------------------------
;; COLOR HIGHTLIGHTING

;; rainbow-mode
;; Hilight them HTML colors
(require 'rainbow-mode)
(rainbow-mode t)

;; -------------------------------------------------------------------
;; PROGRAMMING MODES

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
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
;; Enable spell check and long-ines-mode
(dolist (hook '(markdown-mode-hook text-mode-hook))
  (add-hook hook 'flyspell-mode)
  (add-hook hook 'longlines-mode)
  )


;; extra ruby hooks
(autoload 'ruby-mode "ruby-mode" "Ruby Helpers." t)
(require 'ruby-mode)
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;; YAML mode
(autoload 'yaml-mode "yaml-mode" "YAML editing mode." t)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Sass mode
;; Sass mode requires haml-mode
(autoload 'haml-mode "haml-mode" "Major mode for editing HAML files." t)
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-hook 'haml-mode-hook
          (lambda()
            (rainbow-mode t)))

(autoload 'sass-mode "sass-mode" "Sass major mode." t)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-hook 'sass-mode-hook
          (lambda()
            (auto-complete-mode t)
            (rainbow-mode t)))

;; Coffee Script Mode
(autoload 'coffee-mode "coffee-mode" "Coffee Script Mode." t)
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;; jade-mode
(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; scala-mode
(require 'scala-mode)

;; colojure-mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; puppet-mode
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; stylus-mode
(require 'stylus-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
(add-hook 'stylus-mode-hook
          (lambda()
            (rainbow-mode t)))

;; js-mode
(setq js-indent-level 2)
(add-hook 'js-mode-hook 'flymake-jshint-load)
(add-hook 'js-mode-hook (lambda ()
                          (imenu-add-menubar-index)
                          (hs-minor-mode t)))
;; ------------------------------------------------------------------------------
;; EMACS-Apps

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files (list "~/.org/work.org"
                              "~/.org/personal.org"
                              "~/.org/misc.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(p)" "DISCUSS" "|" "DONE(d!)" "CANCELED(c@)")))

;; ---------------------------------------------------------------------------
;; Key Bindings

;; Helm find
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; org-mode
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-switchb)

;; git-gutter
(global-set-key "\C-cgd" 'git-gutter:popup-diff)
(global-set-key "\C-cgu" 'git-gutter:revert-hunk)

;; -----------------------------------------------------------------------------
;; HELPER FUNCTIONS

(defun make-unix-file ()
  "Change the current file encodind utf-8-unix"
  (interactiv)
  (set-buffer-file-coding-system 'utf-8-unix t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-symbol-face ((t (:inherit font-lock-constant-face :foreground "DarkOrange3")))))
