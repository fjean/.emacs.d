;; Emacs config

;; ----------------------------------------------------------------------------
;; Initialization settings
;; ----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Update the package metadata is the local cache is missing
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Make sure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Reduce garbage collection at start, restore after started
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000)
            (setq gc-cons-percentage 0.1)))

;; Don't show warning from native comp
(setq native-comp-async-report-warnings-errors nil)

;; Only show errors
(setq warning-minimum-level :error)

;; Amount of data read from process (1Mb)
(setq read-process-output-max (* 1024 1024))

;; Always load newest byte code
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Enable local variables
(setq enable-local-variables :all)

;; ----------------------------------------------------------------------------
;; UI
;; ----------------------------------------------------------------------------

;; Inhibit X ressources, startup screen, and messahe
(setq inhibit-x-resources t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; Rendering otimizations
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 0
      scroll-preserve-screen-position 1)

;; Custom theme folder
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Enable / disable UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(global-hl-line-mode t)
(line-number-mode 1)
(column-number-mode 1)
;;(size-indication-mode 1)

;; Feedback options
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1)

;; UI options
(setq-default cursor-type 'bar)
(set-fringe-mode '(12 . 12))
(setq window-divider-default-right-width 3)
(window-divider-mode 1)
(setq frame-resize-pixelwise t)

;; Raise frame when it gets focus
(add-hook 'server-switch-hook #'raise-frame)

;; Set focus of new client frame
(defun conf/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'conf/focus-new-client-frame)

;; Fix for doom-modeline for new frames
(add-hook 'server-after-make-frame-hook (lambda () (setq doom-modeline-icon t)))

;; Set fonts
(defun conf/set-font-faces ()
  (set-face-attribute 'default nil :font "SourceCodePro Medium-13")
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "SourceCodePro Medium-13")
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular))
(add-hook 'server-after-make-frame-hook #'conf/set-font-faces)

(unless (daemonp)
  (conf/set-font-faces))

;; Default size and font for new frames
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(font . "SourceCodePro Medium-13"))

;; Set more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat (file-name-nondirectory (buffer-file-name)) " — Emacs")
                 "%b — Emacs"))))
(setq icon-title-format frame-title-format)

;; ----------------------------------------------------------------------------
;; Editor config
;; ----------------------------------------------------------------------------

;; Delete move files to trash
(setq delete-by-moving-to-trash t)

;; Set some mode options
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(global-so-long-mode)

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "" )

;; Clipboard settings
(setq select-enable-clipboard t
      select-enable-primary nil)
;;(setq mouse-drag-copy-region t)

;; Resize windows
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup files options
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; Default fill number of columns
(setq-default fill-column 79)

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; No completions buffer
(setq completion-auto-help nil)

;; Delete the selection with a keypress
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; UTF8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Remove trailing space when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Disable symbolic links expanding
;; (setq-default find-file-visit-truename nil)

;; Smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Make underscore character part of words
(modify-syntax-entry ?_ "w")

;; ----------------------------------------------------------------------------
;; General settings
;; ----------------------------------------------------------------------------

;; Set browser
(setq browse-url-browser-function 'browse-url-chrome)

;; Some utils functions
(defun kill-or-bury-other-buffer ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (if buffer-file-name
        (bury-buffer)
      (kill-buffer))))

;; Set some options to speed up Tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Compilation options
(setq compilation-read-command nil)
(setq compilation-scroll-output t)

;; ----------------------------------------------------------------------------
;; Glocal shortcuts
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-M-;") 'comment-dwim)
(global-set-key (kbd "M-<") 'delete-horizontal-space)
(global-set-key (kbd "C-é") 'undo)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "M-<up>") 'beginning-of-defun)
(global-set-key (kbd "M-<down>") 'end-of-defun)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-S-<left>") 'backward-list)
(global-set-key (kbd "M-S-<right>") 'forward-list)
(global-set-key (kbd "C-M-<return>") 'browse-url-at-point)
(global-set-key (kbd "C-x f") 'find-file-at-point)
(global-set-key (kbd "C-q") 'kill-or-bury-other-buffer)


;; ----------------------------------------------------------------------------
;; General packages configs
;; ----------------------------------------------------------------------------

(use-package no-littering
  :ensure t)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa" )
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "DISPLAY" "WAYLAND_DISPLAY" "WINDOWID" "XAUTHORITY"))
  (exec-path-from-shell-initialize))

(use-package page-break-lines
  :ensure t
  :config
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family))
  (global-page-break-lines-mode))

(use-package iedit
  :ensure t)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package saveplace
  :init
  (save-place-mode 1)
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; (use-package which-func
;;   :init
;;   (which-function-mode 1)
;;   :config
;;   (setq which-func-unknown "∅")
;;   (set-face-attribute 'which-func nil :foreground "white"))

(use-package savehist
  :init
  (savehist-mode))

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

;; (use-package beacon
;;   :ensure t
;;   :hook ((prog-mode text-mode lisp-mode) . beacon-mode)
;;   :custom
;;   (beacon-blink-when-point-moves-vertically 5)
;;   (beacon-size 50)
;;   (beacon-blink-delay 0.5))

;; ----------------------------------------------------------------------------
;; Editing packages configs
;; ----------------------------------------------------------------------------

(use-package cua-base
  :init
  (cua-mode 1)
  (define-key cua-global-keymap [C-return] nil))

(use-package crux
  :ensure t
  :init
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-k") 'crux-smart-kill-line)
  (global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)
  (global-set-key (kbd "S-<return>") 'crux-smart-open-line)
  (global-set-key (kbd "C-S-<return>") 'crux-smart-open-line-above))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo))

(use-package expand-region
  :ensure t
  :bind (("C-\\" . er/expand-region) ("C-<" . er/expand-region)))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-j" . ace-jump-mode))

;; (use-package smart-tabs-mode
;;  :ensure t
;;  :init
;;  (smart-tabs-insinuate 'c 'c++))

;; ----------------------------------------------------------------------------
;; Windows and UI related packages configs
;; ----------------------------------------------------------------------------

(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window))

(use-package winner
  :init
  (winner-mode))

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode))

(use-package nerd-icons
  :ensure t
  ;;:init
  ;;(nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; ----------------------------------------------------------------------------
;; Themes packages configs
;; ----------------------------------------------------------------------------

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;;(setq doom-themes-padded-modeline 1)
  (setq doom-dark+-blue-modeline 1)
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 35)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 20)
  (setq find-file-visit-truename t)
  (setq inhibit-compacting-font-caches t)
  :config
  ;;(set-face-attribute 'mode-line-inactive nil :box '(:line-width 1))
  ;;(set-face-attribute 'mode-line nil :box '(:line-width 1))
  :hook
  (after-init . doom-modeline-mode))

;; ----------------------------------------------------------------------------
;; Dashboard packages configs
;; ----------------------------------------------------------------------------

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-navigation-cycle t)
  (setq dashboard-set-file-icons t)
  :config
  (setq dashboard-banner-logo-title "")
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 10) (projects . 5)))
  (dashboard-setup-startup-hook))

;; ----------------------------------------------------------------------------
;; Dired
;; ----------------------------------------------------------------------------

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :init
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-laGh1v --group-directories-first --time-style long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

;; (use-package dired-single
;;   :ensure t
;;   :bind (([remap dired-find-file] . dired-single-buffer)
;;          ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
;;          ([remap dired-up-directory] . dired-single-up-directory))
;;   :commands (dired dired-jump))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook ((dired-mode . nerd-icons-dired-mode)))

;; ----------------------------------------------------------------------------
;; ibuffer
;; ----------------------------------------------------------------------------

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups (quote (("default" ("dired" (mode . dired-mode))))))
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	    '((mark modified read-only " "
		        (name 30 30 :left :elide)
		        " "
		        (size-h 9 -1 :right)
		        " "
		        (mode 16 16 :left :elide)
		        " "
		        filename-and-process))))

(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :functions nerd-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook (ibuffer . (lambda ()
                     (ibuffer-projectile-set-filter-groups)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :config
  (setq ibuffer-projectile-prefix
        (concat
         (nerd-icons-sucicon "nf-custom-folder_oct"
                             :face ibuffer-filter-group-name-face
                             :v-adjust 0.0
                             :height 1.0)
             " ")))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; ----------------------------------------------------------------------------
;; vterm
;; ----------------------------------------------------------------------------

(use-package vterm
  :ensure t
  :defer t
  :bind (("C-x t" . vterm)
         ("C-x T" . vterm-other-window)
         :map vterm-mode-map
         ("C-<return>" . vterm-copy-mode)
         ("C-S-v" . vterm-yank)
         ("C-S-y" . vterm-yank)
         ("C-M-y" . vterm-yank-pop)
         ("C-j"  . ace-jump-mode)
         ("M-<left>"  . previous-buffer)
         ("M-<right>" . next-buffer)
         )
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-timer-delay 0.01)
  (setq vterm-max-scrollback 100000))

;; ----------------------------------------------------------------------------
;; vertico
;; ----------------------------------------------------------------------------

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("C-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("C-r" . vertico-repeat))

;; ----------------------------------------------------------------------------
;; marginalia
;; ----------------------------------------------------------------------------

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; ----------------------------------------------------------------------------
;; orderless
;; ----------------------------------------------------------------------------

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator 'orderless-escapable-split-on-space))

;; (use-package prescient
;;   :ensure t)

;; ----------------------------------------------------------------------------
;; consult
;; ----------------------------------------------------------------------------

(defun conf/consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-S-s" . conf/consult-line-symbol-at-point)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-y" . consult-yank-pop)
         ("M-i" . consult-imenu)
         ("M-I" . consult-imenu-multi)
         ("C-x C-r" . consult-recent-file)

         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         :map isearch-mode-map
         ("C-s" . consult-line))                   ;; needed by consult-line to detect isearch

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; (kbd "C-+")
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
)

(use-package consult-projectile
  :ensure t
  :after projectile
  :bind (([remap projectile-find-dir] . consult-projectile-find-dir)
         ([remap projectile-find-file] . consult-projectile-find-file)
         ([remap projectile-recentf] . consult-projectile-recentf)
         ([remap projectile-switch-project] . consult-projectile-switch-project)
         ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer))
  :init
  (define-key projectile-command-map (kbd "SPC") #'consult-projectile))

;; ----------------------------------------------------------------------------
;; embark
;; ----------------------------------------------------------------------------

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter #'embark-completing-read-prompter)
  (setq embark-indicators '(embark-minimal-indicator))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ----------------------------------------------------------------------------
;; ag
;; ----------------------------------------------------------------------------

(use-package ag
  :ensure t)

(use-package consult-ag
  :ensure t
  :bind ("C-S-a" . consult-ag))

;; ----------------------------------------------------------------------------
;; nerd-icons-completion
;; ----------------------------------------------------------------------------

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; ----------------------------------------------------------------------------
;; company
;; ----------------------------------------------------------------------------

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :config
  (setq company-idle-delay 0.1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above nil)
  (setq company-format-margin-function 'company-vscode-dark-icons-margin)
  (set-face-background 'company-tooltip "gray16")
  (add-to-list 'company-backends '(company-files))
  (global-company-mode))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

;; ----------------------------------------------------------------------------
;; projectile
;; ----------------------------------------------------------------------------
(use-package projectile
  :ensure t
  ;; :bind (:map projectile-mode-map
  ;;             ("M-p" . projectile-command-map))

  :bind-keymap ("M-p" . projectile-command-map)

  :init
  (setq projectile-completion-system 'default)
  (setq projectile-find-dir-include-top-level t)
  (setq projectile-mode-line "Projectile")
  (defadvice projectile-on (around exlude-tramp activate)
    "This should disable projectile when visiting a remote file"
    (unless  (--any? (and it (file-remote-p it))
                     (list
                      (buffer-file-name)
                      list-buffers-directory
                      default-directory
                      dired-directory))
      ad-do-it))
  ;; (defun do-not-use-file-truename-in-projectile-project-root
  ;;   (old-fn &rest args)
  ;;   (cl-flet ((file-truename (d) d))
  ;;         (apply old-fn args)))
  ;; (advice-add 'projectile-project-root :around 'do-not-use-file-truename-in-projectile-project-root)
  (projectile-global-mode 1))

;; ----------------------------------------------------------------------------
;; magit
;; ----------------------------------------------------------------------------

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (setq auto-revert-check-vc-info t)
  (with-eval-after-load 'magit-status
    (define-key magit-mode-map (kbd "C-<tab>") nil)
    (define-key magit-mode-map (kbd "M-<down>") #'magit-section-forward-sibling)
    (define-key magit-mode-map (kbd "M-<up>") #'magit-section-backward-sibling)
    ))

;; ----------------------------------------------------------------------------
;; vdiff
;; ----------------------------------------------------------------------------

(use-package vdiff
  :ensure t
  :bind (:map vdiff-mode-map
              ("C-c" . vdiff-mode-prefix-map))
  :config
  (setq vdiff-auto-refine t))

(use-package vdiff-magit
  :ensure t
  :bind (:map magit-mode-map
              ("e" . vdiff-magit-dwim)
              ("E" . vdiff-magit))
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

;; ----------------------------------------------------------------------------
;; yasnippet
;; ----------------------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(use-package consult-yasnippet
  :ensure t
  :bind ("C-M-SPC" . consult-yasnippet))

;; ----------------------------------------------------------------------------
;; tree-sitter
;; ----------------------------------------------------------------------------

(use-package tree-sitter
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t)

;; ----------------------------------------------------------------------------
;; git modes
;; ----------------------------------------------------------------------------

(use-package git-modes
  :ensure t)

;; ----------------------------------------------------------------------------
;; Json, Yaml, Markdown, reStructuredText
;; ----------------------------------------------------------------------------

(use-package json-mode
  :ensure t
  :hook (json-mode . (lambda () (setq fill-column 100)))
  :mode ("\\.json\\'" . json-mode))

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . (lambda () (setq fill-column 100)))
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :hook (markdown-mode . (lambda () (setq fill-column 100)))
  :bind (:map markdown-mode-map
         ("M-p" . nil))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rst
  :ensure nil
  :hook (rst-mode . (lambda () (setq fill-column 100)))
  :mode (("\\.rst\\'" . rst-mode)))

;; ----------------------------------------------------------------------------
;; CMake
;; ----------------------------------------------------------------------------

(use-package cmake-mode
  :ensure t)

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; ----------------------------------------------------------------------------
;; Python
;; ----------------------------------------------------------------------------

(use-package numpydoc
  :ensure t
  :init
  (setq numpydoc-insertion-style nil)
  (setq numpydoc-insert-examples-block nil))

(use-package python
  :ensure nil
  :hook (python-mode . (lambda ()
                         (setq fill-column 100
                               python-indent-offset 4)))
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate))
  :config
  (setq python-indent-guess-indent-offset-verbose t))

(use-package pyvenv
  :ensure t
  :defer t
  :hook ((python-mode . pyvenv-mode)
         (pyvenv-post-activate . (lambda () (pyvenv-restart-python))))
  :bind (:map pyvenv-mode-map
              ("C-c v" . pyvenv-workon))
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/.local/miniconda/envs/"))
  (setq pyvenv-menu t))

(use-package python-pytest
  :ensure t
  :bind (("C-c t" . python-pytest-dispatch))
  :custom
  (python-pytest-confirm nil))

(use-package cython-mode
  :ensure t
  :hook (cython-mode . (lambda () (setq fill-column 100)))
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))

;; ----------------------------------------------------------------------------
;; C/C++
;; ----------------------------------------------------------------------------

(use-package cc-vars
  :ensure nil
  :hook (c-mode-common . (lambda ()
                           (setq fill-column 79
                                 c-basic-offset 4)))
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (other     . "bsd"))))

;; ----------------------------------------------------------------------------
;; flycheck
;; ----------------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; ----------------------------------------------------------------------------
;; pdf-Tools
;; ----------------------------------------------------------------------------

;; (use-package pdf-tools
;;   :pin manual
;;   :magic ("%PDF" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (setq pdf-annot-activate-created-annotations t)
;;   (setq pdf-view-resize-factor 1.1))

;; ----------------------------------------------------------------------------
;; treemacs
;; ----------------------------------------------------------------------------

(use-package treemacs
  :ensure t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         t
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("C-<menu>" . treemacs-select-window)
        ("C-S-<menu>" . treemacs)))

(use-package treemacs-projectile
  :ensure t
  :after projectile)

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

;; ----------------------------------------------------------------------------
;; LSP mode
;; ----------------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :defer t
  :bind (("C-c b" . compile)
         ("M-r" . lsp-find-references))
         ;; (:map lsp-mode-map
         ;;       ("<tab>" . company-indent-or-complete-common)))
  :hook ((lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
         (python-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c-or-c++-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-idle-delay 0.1)
  (setq lsp-log-io nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-imenu-index-symbol-kinds
        '(Namespace Function Class Struct Constructor Method Operator Property Interface Enum EnumMember))
  (setq lsp-imenu-sort-methods '(position kind))
  (setq lsp-completion-show-detail nil)
  (setq lsp-completion-show-label-description nil)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable nil)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	          ("C-c i" . lsp-ui-imenu)
              ("M-<return>" . lsp-describe-thing-at-point)))

(use-package lsp-treemacs
  :ensure t)

(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :bind ("C-c d" . dap-debug)
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (setq dap-python-debugger 'debugpy)
  (setq dap-auto-configure-features '(locals controls tooltip))
  (require 'dap-python)
  (dap-auto-configure-mode))

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :init
  (setq lsp-pyright-multi-root nil)
  :config
  (setq lsp-pyright-disable-organize-imports nil)
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-typechecking-mode "off")
  (setq lsp-pyright-venv-path "/home/fjean/.local/miniconda3/envs")
  (add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-restart-workspace)))
  (add-hook 'pyvenv-post-deactivate-hooks (lambda () (lsp-restart-workspace))))

;; ----------------------------------------------------------------------------
;; Custom set variables file definition and loading
;; ----------------------------------------------------------------------------
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
