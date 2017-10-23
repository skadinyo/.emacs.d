(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(defvar my-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters
    magit
    material-theme
    
    go-mode
    go-guru
    golint
    
    rjsx-mode))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package esup
  :defer t
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package highlight-symbol
  :ensure t
  :defer t
  :init (highlight-symbol-mode)
  :bind (("C-." . highlight-symbol-next)
         ("C-," . highlight-symbol-prev))
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 0))

;;;;;;;;;;
;; Functions
;;;;;;;;;;

(defun simulate-key-press (key)
  "Pretend that KEY was pressed.
KEY must be given in `kbd' notation."
  `(lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defun intellij-kill-current-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(defun intellij-send-top-form-to-repl ()
  (interactive)
  (cider-insert-last-sexp-in-repl -1)
  (cider-switch-to-last-clojure-buffer))

(defun intellij-reformat-code ()
  (interactive)
  (mark-whole-buffer)
  (indent-region (region-beginning) (region-end))
  (pop-global-mark))

(defun indent-marked-files ()
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer nil)))

(defun cider-dev>reset ()
  "Convenient function to reset my clojure development system."
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(dev/reset)")
  (cider-repl-return)
  ;;(cider-switch-to-last-clojure-buffer)
  )

(defun cider-dev>c.t.n.repl/refresh ()
  "Convenient function to reset my clojure development system."
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(clojure.tools.namespace.repl/refresh)")
  (cider-repl-return)
  ;;(cider-switch-to-last-clojure-buffer)
  )

(defun better-transpose-sexps-up (arg)
  "Mimic move form up cursive."
  (interactive "*p")
  (transpose-sexps arg)
  (paredit-backward)
  (paredit-backward)
  ;;(previous-line)
  )

(defun better-transpose-sexps-down (arg)
  "Mimic move form up cursive."
  (interactive "*p")
  (paredit-forward)
  (transpose-sexps arg)
  (paredit-backward)
  ;;(previous-line)
  )

(defun experiment-repair-all-unused-space ()
  "Experiment stuff."
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "[ ]+" nil t)
    (replace-match " "))
  (intellij-reformat-code))
 
;; comments
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun paredit-kill-region-or-backward-delete ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-delete)))

;;;;;;;;;;
;; Better default
;;;;;;;;;;
(require 'uniquify)

(require 'saveplace)

(electric-pair-mode 1)

(use-package diff-hl
  :defer t
  :ensure t
  :init (diff-hl-mode)
  )

(load-theme 'material t)
(blink-cursor-mode 0)
;; (global-linum-mode)
(show-paren-mode 1)
(toggle-indicate-empty-lines)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)

(use-package keyfreq
  :defer t
  :ensure t
  :init (keyfreq-mode 1)
  :config 
  (progn 
    (keyfreq-autosave-mode 1)))

(use-package recentf
  :defer t
  :ensure t
  :init (require 'recentf)
  :config
  (progn 
    (setq 
     recentf-save-file (concat user-emacs-directory ".recentf")
     recentf-max-menu-items 50)))

(use-package undo-tree
  :defer t
  :ensure t
  :init (global-undo-tree-mode 1)
  :bind (("C-z" . undo-tree-undo)
         ("M-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)
         ("M-Z" . undo-tree-redo)))

(use-package expand-region
  :defer t
  :ensure t
  :bind (("<M-up>" . er/expand-region)
         ("<M-down>" . er/contract-region)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(setq-default 
 frame-title-format "%b (%f)"
 indent-tabs-mode nil
 indicate-empty-lines t
 save-place t)
(global-linum-mode)
(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 
 ;; backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 auto-save-default nil
 ring-bell-function 'ignore
 electric-indent-mode nil
 create-lockfiles nil
 inhibit-startup-message t

 uniquify-buffer-name-style 'forward
 scroll-conservatively 10000
 scroll-preserve-screen-position t
 )

(use-package smex
  :ensure t
  :defer t
  :init (smex-initialize)
  :bind (("C-1" . smex))
  :config 
  (progn
    (setq
     smex-save-file (concat user-emacs-directory ".smex-items")
     save-place-file (concat user-emacs-directory "places"))))

(use-package ido
  :ensure t
  :defer t
  :init (ido-mode)
  :bind (("C-o" . ido-find-file)
         ("C-b" . ido-switch-buffer))
  :config
  (progn
    (setq 
     ido-enable-flex-matching t
     ido-use-filename-at-point nil
     ido-auto-merge-work-directories-length -1
     ido-use-virtual-buffers t)
    (add-to-list 'ido-ignore-files "\\.DS_Store"))
  )

(use-package flx-ido
  :ensure t
  :defer t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package ido-ubiquitous
  :ensure t
  :defer t
  :init (ido-ubiquitous-mode 1))

(use-package company
  :init (global-company-mode)
  :ensure t
  :defer t
  :config 
  (progn
    (setq company-idle-delay nil
          company-require-match nil))
  :bind (("M-TAB" . company-complete)
         ("TAB" . company-indent-or-complete-common)))

(use-package esup
  :ensure t
  :defer t)

(use-package flycheck
  :init (global-flycheck-mode)
  :ensure t
  :defer t)

;; (use-package tabbar
;;   :init (tabbar-mode t)
;;   :ensure t
;;   :defer t
;;   :bind (("C-{" . tabbar-backward)
;;          ("C-}" . tabbar-forward)
;;          ("M-{" . tabbar-backward)
;;          ("M-}" . tabbar-forward))
;;   )

;; (use-package tabbar-ruler
;;   :init (require 'tabbar-ruler)
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (tabbar-mode t)
;;     (setq tabbar-ruler-global-tabbar t)
;;     (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
;;     (tabbar-ruler-group-by-projectile-project))
;;   )

(use-package rg
 :ensure t
 :defer t
 :init (require 'rg)
 :bind (("C-S-f" . rg-project)))

(use-package google-this
 :ensure t
 :defer t
 :init (google-this-mode 1)
 :bind (("C-x C-f" . google-this))
 )

(use-package projectile
  :init (projectile-global-mode)
  :ensure t
  :defer t
  ;;FIXME Still don't know how to use bind for simulate-key-press function
  ;;:bind (("C-p" . (simulate-key-press "C-c p")))
  :config
  (progn
    (setq projectile-globally-ignored-directories
      (append '(
        ".git"
        ".svn"
        "out"
        "repl"
        "target"
        "venv"
        "node_modules"
        )
          projectile-globally-ignored-directories))
    (setq projectile-globally-ignored-files
      (append '(
        ".DS_Store"
        "*.gz"
        "*.pyc"
        "*.jar"
        "*.tar.gz"
        "*.tgz"
        "*.zip"
        )
          projectile-globally-ignored-files))
    (setq projectile-enable-caching t)
    (global-set-key (kbd "C-p") (simulate-key-press "C-c p")))
  )

;;;;;;;;;;
;; Global Kbds
;;;;;;;;;;
;;How do i manage this :( ??????
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'copy-region-as-kill)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "C-w") 'intellij-kill-current-buffer)
;; (global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-f") 'isearch-forward)
;; (global-set-key (kbd "C-F") 'isearch-forward-regexp)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; TODO move to clojure mode 
;; (global-set-key (kbd "C-S-l") 'cider-load-buffer)
;; (global-set-key (kbd "C-S-n") 'cider-repl-set-ns)
;; (global-set-key (kbd "C-S-p") 'intellij-send-top-form-to-repl)
;; (global-set-key (kbd "<f5>") 'cider-dev>reset)
;; (global-set-key (kbd "<f6>") 'cider-dev>c.t.n.repl/refresh)

;;;;;;;;;;
;; Elisp
;;;;;;;;;;

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

;;;;;;;;;;
;; Web Mode
;;;;;;;;;;

(use-package web-mode
  :ensure t
  :defer t
  :config
  (progn
    ;;Indentation
    (setq web-mode-markup-indent-offset 4) 
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-comment-style 4)
    
    (add-hook 'html-mode-hook 'subword-mode)
    
    ;;Use only that i know.
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
   
    ))

;;;;;;;;;;
;; Go Mode
;;;;;;;;;;
(setenv "GOPATH" "/Users/skadinyo/Projects/go")
(defun my-go-mode-hook ()
  (setq tab-width 2)
  (setq standard-indent 2) 
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'subword-mode))

;; 

;;TODO Explore go-eldoc

;; FIXME indentation won't work if put in use-package config
;; Don't know why.
(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go$"
  :bind (("M-." . godef-jump)
         ("M-," . godef-jump-other-window)
         ("M-p" . compile) 
         ("M-P" . recompile))
  :config
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'subword-mode)
  )

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))


(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)

;;;;;;;;;;
;; Paredit
;;;;;;;;;;


;;TODO Fix this $hit. it's so clutered :(
(use-package paredit
  :ensure t
  :defer t
  :config
  (progn
    (define-key paredit-mode-map (kbd "<backspace>") 'paredit-kill-region-or-backward-delete)
    (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward)
    (define-key paredit-mode-map (kbd "<M-left>")  'paredit-backward)
    (define-key paredit-mode-map (kbd "<M-up>") nil)
    (define-key paredit-mode-map (kbd "<M-down>")  nil)
    (define-key paredit-mode-map (kbd "<C-right>")  'move-end-of-line)
    (define-key paredit-mode-map (kbd "<C-left>")  'move-beginning-of-line)
    (define-key paredit-mode-map (kbd "M-k") 'paredit-kill)
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-s M-d") 'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-s M-a") 'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-b M-n") 'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-b M-v") 'paredit-backward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-w M-w") 'paredit-splice-sexp)
    (define-key paredit-mode-map (kbd "M-w M-q") 'paredit-splice-sexp-killing-backward)
    (define-key paredit-mode-map (kbd "M-w M-e") 'paredit-splice-sexp-killing-forward)
    (define-key paredit-mode-map (kbd "C-{") nil)
    (define-key paredit-mode-map (kbd "C-}") nil)
    ))

;;;;;;;;;;
;; Org-mode
;;;;;;;;;;

(use-package org
  :ensure t
  :defer t
  :mode "\\.org$"
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda))
  :config
  (progn
    (setq org-log-done t)
    (setq org-support-shift-select t)
    (setq org-todo-keywords
          '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

    ))

;; (find-file "~/Dropbox/documents/worknote.org")

;;;;;;;;;;
;; js-mode
;;;;;;;;;;

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :config
  (progn
    (subword-mode)
    (setq indent-tabs-mode nil)(setq js-indent-level 2)
    (setq js2-strict-missing-semi-warning nil)
    ;; (define-key rjsx-mode-map "<" nil)
    ;; (define-key rjsx-mode-map (kbd "C-d") nil)
    ;; (define-key rjsx-mode-map ">" nil)
  ))

(use-package json-reformat
  :ensure t
  :defer t)

(when (window-system)
  (set-default-font "Fira Code Retina"))

;;TODO Investigate ligature. It's slow AF
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))
