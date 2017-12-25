(x-focus-frame nil)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(defvar my-packages
  'paredit
  clojure-mode
  clojure-mode-extra-font-locking)

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

(use-package nginx-mode
  :defer t
  :ensure t)

;; TODO explore!!!
;; (use-package multi-term
;;   :defer t
;;   :ensure t
;;   )

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

(require 'uniquify)

(require 'saveplace)

(electric-pair-mode 1)

;; TODO make diff-hl only in go, js, elisp project!
;; (use-package diff-hl
;;   :defer t
;;   :ensure t
;;   :init (diff-hl-mode))

(use-package hl-todo
  :defer t
  :ensure t
  :init (global-hl-todo-mode))

(load-theme 'material t)
(blink-cursor-mode 0)
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

;; I think nlinum mode is more stable.
(use-package nlinum
  :defer t
  :ensure t
  :init (global-nlinum-mode))

;; TODO explore more configuration for this powerful package
(use-package expand-region
  :defer t
  :ensure t
  :bind (("<M-up>" . er/expand-region)
         ("<M-down>" . er/contract-region)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; TODO tidy up this setq
(setq-default 
 frame-title-format "%b (%f)"
 indent-tabs-mode nil
 indicate-empty-lines t
 save-place t)
(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 
 auto-save-default nil
 ring-bell-function 'ignore
 electric-indent-mode nil
 create-lockfiles nil
 inhibit-startup-message t
 
 uniquify-buffer-name-style 'forward
 scroll-conservatively 10000
 scroll-preserve-screen-position t
 )

;; (setq bidi-display-reordering nil)

;; TODO explore!!
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

;; TODO explore!!
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

;; TODO explore!!
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

;; TODO explore!!
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

;; Benchmarking
(use-package esup
  :ensure t
  :defer t)

;; TODO make flycheck only in go, js, elisp project!
(use-package flycheck
  :init (global-flycheck-mode)
  :ensure t
  :defer t)

;; Turns out i can survive without tabbar!!!
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

;; Crazy fast grep alternative
(use-package rg
 :ensure t
 :defer t
 :init (require 'rg)
 :bind (("C-S-f" . rg-project)))

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
         ("M-P" . recompile)
         ("<C-right>" . move-end-of-line)
         ("<C-left>" . move-beginning-of-line)
         )
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

;; prettier-js-command is the prettier command
;; prettier-js-args are the args passed to the prettier command
;; prettier-js-show-errors customizes where to display the error output (buffer, echo or nil)
;; prettier-js-width-mode customizes the width when formatting buffer contents (window, fill or nil)
(use-package prettier-js
  :ensure t
  :defer t
  :config
  (progn
    (setq prettier-js-args '("--tab-width" "2"
                             "--use-tabs" "false"
                             ;; "--trailing-comma" "all"
                             "--single-quote" "true"
                             "--print-width" "100"
                             "--semi" "true"))
    (add-hook 'js2-mode-hook 'prettier-js-mode)))

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json$")


(use-package add-node-modules-path
  :ensure t
  :defer t
  )

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :bind (("<C-right>" . move-end-of-line)
         ("<C-left>" . move-beginning-of-line))
  :config
  (progn
    (subword-mode)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq js2-strict-missing-semi-warning nil)

    
    ;; (define-key rjsx-mode-map "<" nil)
    ;; (define-key rjsx-mode-map (kbd "C-d") nil)
    ;; (define-key rjsx-mode-map ">" nil)
  ))

(use-package json-reformat
  :ensure t
  :defer t)

(defun insert-current-date () 
  (interactive)
  (insert (shell-command-to-string "echo -n $(date '+%d-%m-%Y %X')")))

