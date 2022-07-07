;; Emacs config
;

;; ----------------------------------------------------------------------------
;; General config, not package related
;; ----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Inhibit X ressources
(setq inhibit-x-resources 't)

;; Reduce garbage collection at start, restore after started
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000)
            (setq gc-cons-percentage 0.1)))

;; Amount of data read from process (1Mb)
(setq read-process-output-max (* 1024 1024))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 0
      scroll-preserve-screen-position 1)

;; Rendering otimizations
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Custom theme folder
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Enable / disable some visual stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(global-hl-line-mode t)
(line-number-mode 1)
(column-number-mode 1)
;;(size-indication-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq-default fill-column 80)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "" )
(setq echo-keystrokes 0.1)
(setq-default cursor-type 'bar)
(set-fringe-mode '(12 . 12))
(add-hook 'server-switch-hook #'raise-frame)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(global-so-long-mode)
(setq window-divider-default-right-width 3)
(window-divider-mode 1)

;; Set focus of new client frame
(defun focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'focus-new-client-frame)

;; Delete move files to trash
(setq delete-by-moving-to-trash t)

;; Default size and font for new frames
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(font . "SourceCodePro Medium-12"))

;; Resize windows
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat (file-name-nondirectory (buffer-file-name)) " — Emacs")
                 "%b — Emacs"))))
(setq icon-title-format frame-title-format)

;; Set browser
(setq browse-url-browser-function 'browse-url-chrome)

;; Backup files options
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

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

;; Recent file options
(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")

;; Enable local variables
(setq enable-local-variables :all)

;; Unset global keys
;; (global-unset-key (kbd "C-x f"))

;; Some utils functions
(defun kill-or-bury-other-buffer ()
  (interactive)
  (save-selected-window
    (other-window 1)
    (if buffer-file-name
        (bury-buffer)
      (kill-buffer))))

;; Set some global keys
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

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; Clipboard settings
(setq select-enable-clipboard t
      select-enable-primary nil)
;;(setq mouse-drag-copy-region t)

;; Set some options to speed up Tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; ----------------------------------------------------------------------------
;; Packages configs
;; ----------------------------------------------------------------------------
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  ;;:custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-file)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-file-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-recentf)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-projectile-find-file)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-projectile-find-dir)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline 1)
  (setq doom-dark+-blue-modeline 1)
  (load-theme 'doom-dark+ t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 35)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-vcs-max-length 20)
  (setq find-file-visit-truename t)
  (setq inhibit-compacting-font-caches t)
    :config
  (set-face-attribute 'mode-line-inactive nil :box '(:line-width 1))
  :hook
  (after-init . doom-modeline-mode))

(use-package page-break-lines
  :ensure t
  :config
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family))
  (global-page-break-lines-mode))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-banner-logo-title "")
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 10) (projects . 5)))
  (dashboard-setup-startup-hook))

(use-package dired
  :init
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-laGh1v --group-directories-first --time-style long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

(use-package dired-x
  :hook ((dired-mode . dired-omit-mode))
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))
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
		        filename-and-process)))
  )

(use-package ibuffer-projectile
  :ensure t
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
  :config
  (setq ibuffer-projectile-prefix
        (concat
         (all-the-icons-octicon "file-directory"
                                :face ibuffer-filter-group-name-face
                                :v-adjust 0.0
                                :height 1.0)
             " ")))

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
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package which-func
  :init
  (which-function-mode 1)
  :config
  (setq which-func-unknown "∅")
  (set-face-attribute 'which-func nil :foreground "white"))

(use-package smex
  :ensure t)

;; (use-package windmove
;;   :config
;;   (windmove-default-keybindings 'meta))

(use-package ace-window
  :ensure t
  :init
  (global-set-key (kbd "M-o") 'ace-window))

(use-package winner
  :init
  (winner-mode))

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

(use-package vdiff
  :ensure t
  :defer t
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (setq vdiff-auto-refine t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (setq auto-revert-check-vc-info t)
  (with-eval-after-load 'magit-status
    (define-key magit-mode-map (kbd "C-<tab>") nil)))

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

;; (use-package gitconfig-mode
;;   :ensure t
;;   :defer t)

;; (use-package gitignore-mode
;;   :ensure t
;;   :defer t)

(use-package expand-region
  :ensure t
  :bind (("C-\\" . er/expand-region) ("C-<" . er/expand-region)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (add-to-list 'company-backends '(company-files))
  (global-company-mode))

(use-package ag
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-truncate-lines nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (global-set-key (kbd "C-r") 'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "C-<return>") 'ivy-insert-current)
  ;;(setf (alist-get t ivy-sort-matches-functions-alist) 'ivy--shorter-matches-first)
  (ivy-mode 1))

(use-package ivy-xref
  :ensure t
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package prescient
;;   :ensure t)

;; (use-package ivy-prescient
;;   :ensure t
;;   :config
;;   (ivy-prescient-mode t)
;;   (with-eval-after-load 'ivy-prescient
;;     (setq ivy-prescient-retain-classic-highlighting t)))

(use-package ivy-yasnippet
  :ensure t
  :bind ("C-M-SPC" . ivy-yasnippet))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-S-s") 'swiper-isearch-thing-at-point))

(use-package counsel
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-S-a") 'counsel-projectile-ag)
  (global-set-key (kbd "M-i") 'counsel-imenu)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :init
  (counsel-projectile-mode))

(use-package counsel-tramp
  :ensure t
  :after (counsel)
  :init
  (setq tramp-default-method "ssh")
  :config
  (define-key global-map (kbd "C-x C-l") 'counsel-tramp)
  (add-hook 'counsel-tramp-pre-command-hook '(lambda () (projectile-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda () (projectile-mode 1))))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-find-dir-include-top-level t)
  :config
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
  (setq projectile-mode-line "Projectile")
  (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
  (projectile-global-mode +1))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :bind ("C-j" . ace-jump-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; (add-hook 'python-mode-hook 'company-mode)
  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq warning-suppress-types '((python)
                                 (emacs))))

(use-package python-pytest
  :ensure t
  :bind (("C-c t" . python-pytest-dispatch))
  :custom
  (python-pytest-confirm nil))

(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))

(use-package numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate))
  :init
  (setq numpydoc-insertion-style nil)
  (setq numpydoc-insert-examples-block nil))

;; (use-package anaconda-mode
;;   :ensure t
;;   :bind ("C-c C-d" . anaconda-mode-show-doc))

;; (use-package company-anaconda
;;   :ensure t
;;   :config
;;   (require 'rx)
;;   :init
;;   (eval-after-load "company"
;;     '(add-to-list 'company-backends '(company-anaconda))))

;; (use-package python-docstring
;;   :ensure t
;;   :hook ((python-mode . python-docstring-mode)))

;; (use-package conda
;;   :ensure t
;;   :config
;;   (setq conda-anaconda-home (expand-file-name "~/.local/miniconda3"))
;;   (setq conda-env-home-directory (expand-file-name "~/.local/miniconda3"))
;;   (conda-env-autoactivate-mode t)
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell))

;; (use-package python-black
;;   :ensure t
;;   :after python)

;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-irony))
;; (use-package company-irony-c-headers
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-irony-c-headers))

;; (use-package irony
;;   :ensure t
;;   :config
;;   (setq c-default-style "linux"
;;         c-basic-offset 4
;;         tab-width 4
;;         indent-tabs-mode nil)
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;   ;; irony-mode's buffers by irony-mode's function
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package cc-vars
  :ensure nil
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (other     . "bsd")))
  (add-hook 'c-mode-common-hook
            (lambda () (setq indent-tabs-mode t)))
  (setq-default c-basic-offset 4))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  )

(use-package smart-tabs-mode
  :ensure t
  :init
  (progn (smart-tabs-insinuate 'c 'c++)))

;; (use-package realgud
;;   :ensure t
;;   :commands (realgud:gdb
;;              realgud:ipdb
;;              realgud:pdb))

;; (use-package dumb-jump
;;   :ensure t
;;   :bind (("M-g o" . dumb-jump-go-other-window)
;;          ("M-g j" . dumb-jump-go)
;;          ("M-g b" . dumb-jump-back)
;;          ("M-g q" . dumb-jump-quick-look)
;;          ("M-g x" . dumb-jump-go-prefer-external)
;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;;   :config (setq dumb-jump-selector 'ivy)
;;           (setq dumb-jump-force-searcher 'ag)
;;           (setq dumb-jump-quiet t)
;;           (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package logview
  :ensure t
  :mode ("\\.log\\'" . logview-mode)
  :init
  (setq logview-additional-submodes
        '(("training-log-submode"
           (format . "TIMESTAMP [LEVEL] MESSAGE")
           (levels . "training-log-level")
           ;; define timestamp if not one of standard
           (timestamp . "yyyy-MM-dd HH:mm:ss")
           (aliases "training-log"))))
  (setq logview-additional-level-mappings
        '(("training-log-level"
           (error "ERROR  ")
           (warning "WARNING")
           (information "INFO   ")
           (debug "DEBUG  ")))))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package cmake-mode
  :ensure t)

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package docker-tramp
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package engine-mode
  :ensure t
  :commands (enfine/wikipedia engine/search-google)
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-`"))
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"))

(use-package vterm
  :ensure t
  :defer t
  :bind (("C-x t" . vterm)
         ("C-x T" . vterm-other-window))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-timer-delay 0.01)
  (setq vterm-max-scrollback 100000)
  (define-key vterm-mode-map (kbd "C-c <return>") #'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-S-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-S-y") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-M-y") #'vterm-yank-pop)
  (define-key vterm-mode-map (kbd "C-j") #'ace-jump-mode)
  (define-key vterm-mode-map (kbd "M-<left>") 'previous-buffer)
  (define-key vterm-mode-map (kbd "M-<right>") 'next-buffer)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil))))

(use-package pdf-tools
  :pin manual
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1))

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
  :after treemacs projectile
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config
;;   (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
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
  (setq compilation-read-command nil)
  (setq compilation-scroll-output t)
  (global-set-key (kbd "M-r") 'lsp-find-references)
  :hook ((python-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c-or-c++-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind ("C-c b" . compile))

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

(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/.local/miniconda3/envs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
					                      (pyvenv-restart-python)))
  ;; (add-hook 'pyvenv-post-activate-hooks 'lsp-workspace-restart)
  :hook (python-mode . pyvenv-mode)
  :bind (:map pyvenv-mode-map
	          ("C-c v" . pyvenv-workon)))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol
  :bind (("C-c s" . lsp-ivy-workspace-symbol)
         ("C-c S" . (lambda ()
                      (interactive)
                      (let ((current-prefix-arg '(4)))
                        (call-interactively #'lsp-ivy-workspace-symbol))))
         ))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init
;;   (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp-deferred))))  ; or lsp-deferred

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
  (add-hook 'pyvenv-post-deactivate-hooks (lambda () (lsp-restart-workspace)))
)

(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

;; (use-package tree-sitter-indent
;;   :ensure t)

;; (use-package lsp-pyright
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq lsp-clients-python-library-directories '("/usr/" "~/.local/miniconda3/pkgs"))
;;   (setq lsp-pyright-disable-language-service nil
;; 	lsp-pyright-disable-organize-imports nil
;; 	lsp-pyright-auto-import-completions t
;; 	lsp-pyright-use-library-code-for-types t
;; 	lsp-pyright-venv-path "~/.local/miniconda3/envs")
;;   :hook ((python-mode . (lambda ()
;;                           (require 'lsp-pyright) (lsp-deferred)))))



;; ----------------------------------------------------------------------------
;; Custom set variables file definition and loading
;; ----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
