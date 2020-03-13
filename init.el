;; Emacs config
;;

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

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Custom theme folder
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; Enable / disable some visual stuff
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode t)
(line-number-mode 1)
(column-number-mode 1)
;;(size-indication-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq-default fill-column 80)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "-------------------------------- Scratch buffer --------------------------------\n")
(add-hook 'server-switch-hook #'raise-frame)

;; Delete move files to trash
(setq delete-by-moving-to-trash t)

;; Default size and font for new frames
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14:style=Regular"))

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

;; Change backup folders
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; UTF8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Remove trailing space when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Unset global keys
(global-unset-key (kbd "C-x f"))

;; Set some global keys
(global-set-key (kbd "C-M-;") 'comment-dwim)
(global-set-key (kbd "M-<") 'delete-horizontal-space)
(global-set-key (kbd "C-é") 'undo)

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;; ----------------------------------------------------------------------------
;; Packages configs
;; ----------------------------------------------------------------------------
(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts)))

;; (use-package all-the-icons-dired
;;   :ensure t
;;   :hook ((dired-mode . all-the-icons-dired-mode)))

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :after (all-the-icons ivy)
;;   ;;:custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
;;   :config
;;   (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-file)
;;   (add-to-list 'all-the-icons-ivy-file-commands 'counsel-file-jump)
;;   (add-to-list 'all-the-icons-ivy-file-commands 'counsel-recentf)
;;   (add-to-list 'all-the-icons-ivy-file-commands 'counsel-projectile-find-file)
;;   (add-to-list 'all-the-icons-ivy-file-commands 'counsel-projectile-find-dir)
;;   (all-the-icons-ivy-setup))

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
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-icon t)
  :hook
  (after-init . doom-modeline-mode))

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
  (setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode)))))))

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

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

(use-package smex
  :ensure t)

(use-package windmove
  :config
  (windmove-default-keybindings 'meta))

(use-package vdiff
  :ensure t
  :defer t
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (setq vdiff-auto-refine t))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

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

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-\\" . er/expand-region) ("C-<" . er/expand-region)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  (global-set-key (kbd "C-S-s") 'swiper-all))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-projectile
  :ensure t
  :after (counsel)
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

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
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package move-text
  :ensure t
  :defer t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'sphinx-doc-mode)
  (setq warning-suppress-types '((python)
                                 (emacs)))

  (use-package sphinx-doc
    :ensure t)

  (use-package anaconda-mode
    :ensure t
    :bind ("C-c C-d" . anaconda-mode-show-doc))

  (use-package company-anaconda
    :ensure t
    :config
    (require 'rx)
    :init
    (eval-after-load "company"
      '(add-to-list 'company-backends '(company-anaconda))))
)

(use-package conda
  :ensure t
  :defer t
  :config
  (setq conda-anaconda-home (expand-file-name "~/.local/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/.local/miniconda3"))
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

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

(use-package engine-mode
  :ensure t
  :commands (enfine/wikipedia engine/search-google)
  :config
  (engine-mode t)
  (engine/set-keymap-prefix (kbd "C-x /"))
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
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local global-hl-line-mode nil))) )

(use-package pdf-tools
  :pin manual
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1))


;; ----------------------------------------------------------------------------
;; Custom set variables file definition and loading
;; ----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
