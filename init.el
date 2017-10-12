(setq gc-cons-threshold 80000000)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(defvar my-packages
  '(paredit
    beacon
    clojure-mode
    expand-region
    clojure-mode-extra-font-locking
    cider
    ido-ubiquitous
    smex
    rainbow-delimiters
    tagedit
    magit
    projectile
    company
    company-quickhelp
    company-go
    web-mode
    undo-tree
    material-theme
    tabbar
    tabbar-ruler
    flycheck
    go-mode
    go-guru
    golint
    hl-todo
    vimish-fold
    keyfreq
    smooth-scrolling
    multiple-cursors
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
  "dev>(reset). convenient function to reset my clojure development system"
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(dev/reset)")
  (cider-repl-return)
  ;;(cider-switch-to-last-clojure-buffer)
  )

(defun cider-dev>c.t.n.repl/refresh ()
  "dev>(reset). convenient function to reset my clojure development system"
  (interactive)
  (cider-switch-to-repl-buffer)
  (insert "(clojure.tools.namespace.repl/refresh)")
  (cider-repl-return)
  ;;(cider-switch-to-last-clojure-buffer)
  )

(defun cider-dev>eval-last-repl-input ()
  (interactive)
  )

(defun better-transpose-sexps-up (arg)
  "mimic move form up cursive"
  (interactive "*p")
  (transpose-sexps arg)
  (paredit-backward)
  (paredit-backward)
  ;;(previous-line)
  )

(defun better-transpose-sexps-down (arg)
  "mimic move form up cursive"
  (interactive "*p")
  (paredit-forward)
  (transpose-sexps arg)
  (paredit-backward)
  ;;(previous-line)
  )

(defun experiment-repair-all-unused-space ()
  "experiment stuff"
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward "[ ]+" nil t)
    (replace-match " "))
  (intellij-reformat-code))
 
;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
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
(require 'recentf)
(require 'saveplace)
(require 'expand-region)
(require 'keyfreq)
(require 'powerline)
(powerline-center-theme)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 10)
(electric-pair-mode 1)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(when (window-system)
  (set-default-font "Fira Code Retina"))
(load-theme 'material t)
;; (load-theme 'dracula t)
;; (set-face-background 'mode-line "#510370")
;; (set-face-background 'mode-line-inactive "black")
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(blink-cursor-mode 0)
(global-linum-mode)
(show-paren-mode 1)
(global-hl-line-mode 1)
(hl-todo-mode)
;; (toggle-frame-maximized)
(toggle-indicate-empty-lines)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(global-undo-tree-mode 1)
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)

(setq-default 
 frame-title-format "%b (%f)"
 indent-tabs-mode nil
 indicate-empty-lines t
 sh-basic-offset 2
 sh-indentation 2
 save-place t)

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 auto-save-default nil
 ring-bell-function 'ignore
 electric-indent-mode nil
 column-number-mode t
 create-lockfiles nil
 inhibit-startup-message t

 recentf-save-file (concat user-emacs-directory ".recentf")
 recentf-max-menu-items 100
 recentf-auto-cleanup 'never
 ido-enable-flex-matching t
 ido-use-filename-at-point nil
 ido-auto-merge-work-directories-length -1
 ido-use-virtual-buffers t
 smex-save-file (concat user-emacs-directory ".smex-items")
 save-place-file (concat user-emacs-directory "places")
 uniquify-buffer-name-style 'forward
 scroll-conservatively 10000
 scroll-preserve-screen-position t)

(smex-initialize)
(recentf-mode 1)
(ido-ubiquitous-mode 1)
(ido-mode t)

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;;;;;;;;;;
;;Global mode
;;;;;;;;;;

;;Some mode must maybe better to be set up locally
;;Like this company mode, I don't want to run company mode in org or scratch

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

(use-package flycheck
  :init (global-flycheck-mode)
  :ensure t
  :defer t)

(use-package tabbar
  :init (tabbar-mode t)
  :bind (("C-{" . tabbar-backward)
         ("C-}" . tabbar-forward)
         ("M-{" . tabbar-backward)
         ("M-}" . tabbar-forward))
  :config 
  (require 'tabbar-ruler)
  (setq tabbar-ruler-global-tabbar t)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (tabbar-ruler-group-by-projectile-project)
  )

;;Too lazy to change kbd for projectile
(global-set-key (kbd "C-p") (simulate-key-press "C-c p"))
;; projectile everywhere!
(projectile-global-mode)

(vimish-fold-global-mode 1)
(global-set-key (kbd "C-,") #'vimish-fold)
(global-set-key (kbd "") #'vimish-fold-delete)
;;;;;;;;;;
;; Settings that i still don't know
;;;;;;;;;;

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;;;;;;;;;;
;; Global Kbds
;;;;;;;;;;

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "<C-up>") 'better-transpose-sexps-up)
;; (global-set-key (kbd "<C-down>") 'better-transpose-sexps-down)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-1") 'smex)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-o") 'ido-find-file)
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'copy-region-as-kill)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "C-w") 'intellij-kill-current-buffer)
;; (global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-F") 'isearch-forward-regexp)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-M-q") 'save-buffers-kill-terminal)
;; (global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<M-up>") 'er/expand-region)
(global-set-key (kbd "<M-down>") 'er/contract-region)

;; TODO move to clojure mode 
(global-set-key (kbd "C-S-l") 'cider-load-buffer)
(global-set-key (kbd "C-S-n") 'cider-repl-set-ns)
(global-set-key (kbd "C-S-p") 'intellij-send-top-form-to-repl)
(global-set-key (kbd "<f5>") 'cider-dev>reset)
(global-set-key (kbd "<f6>") 'cider-dev>c.t.n.repl/refresh)

;;;;;;;;;;
;; Elisp
;;;;;;;;;;

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)

;;;;;;;;;;
;; Web Mode
;;;;;;;;;;

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4) 
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-comment-style 4)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'html-mode-hook 'subword-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; javascript
;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
;; (add-hook 'js-mode-hook 'subword-mode)
;; (setq js-indent-level 2)

;;;;;;;;;;
;; Clojure Mode
;;;;;;;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(require 'clojure-mode-extra-font-locking)


;;;;;;;;;;
;; Go Mode
;;;;;;;;;;

;;Don't forget to install some of go lang library

;;TODO Take GOPATH from bash env
(setenv "GOPATH" "/Users/skadinyo/Projects/go")
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/nsf/gocode/emacs-company"))
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'company-go)
(require 'golint)

(defun my-go-mode-hook ()
  (setq-default) 
  (setq tab-width 2) 
  (setq standard-indent 2) 
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'subword-mode)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'godef-jump-other-window)
  (local-set-key (kbd "M-p") 'compile) 
  (local-set-key (kbd "M-P") 'recompile)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;;;;;;;;;
;; Hook that i still don't know
;;;;;;;;;;

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;
;; Paredit
;;;;;;;;;;

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<backspace>") 'paredit-kill-region-or-backward-delete)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward)
     (define-key paredit-mode-map (kbd "<M-left>")  'paredit-backward)
     (define-key paredit-mode-map (kbd "<M-up>") nil)
     ;; (define-key paredit-mode-map (kbd "C-{") nil)
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

     ;; (define-key paredit-mode-map (kbd "M-d") nil)
     ;; (define-key paredit-mode-map (kbd "C-d") nil)
     (define-key paredit-mode-map (kbd "C-{") nil)
     (define-key paredit-mode-map (kbd "C-}") nil)
     
     ;;(define-key paredit-mode-map (kbd "<C-down>") (transpose-sexps -1))
     ;;(define-key paredit-mode-map (kbd "<C-up>") 'transpose-sexps)
     ))


;;;;;;;;;;
;; Org-mode
;;;;;;;;;;

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-support-shift-select t)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; (find-file "~/Dropbox/documents/worknote.org")

;;;;;;;;;;
;; js-mode
;;;;;;;;;;


(add-to-list 'company-backends 'company-tern)
(add-to-list 'auto-mode-alist '(".*\.js\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("containers\/.*\.js\'" . rjsx-mode))
;; (setq js-indent-level 2)
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;Use space instead of tab
            (setq js-indent-level 2) ;;space width is 2 (default is 4)
            (setq js2-strict-missing-semi-warning nil))) ;;disable the semicolon warning

;; (with-eval-after-load 'rjsx-mode
;;   (define-key rjsx-mode-map "<" nil)
;;   (define-key rjsx-mode-map (kbd "C-d") nil)
;;   (define-key rjsx-mode-map ">" nil))

;; (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
;;   "Workaround sgml-mode and follow airbnb component style."
;;   (save-excursion
;;     (beginning-of-line)
;;     (if (looking-at-p "^ +\/?> *$")
;;         (delete-char sgml-basic-offset))))

(when (window-system)
  (set-default-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package company-tern indium ox-twbs web-mode vimish-fold undo-tree tagedit tabbar-ruler smooth-scrolling smex rjsx-mode rainbow-delimiters projectile paredit multiple-cursors material-theme magit keyfreq ido-ubiquitous hl-todo golint go-guru flycheck expand-region exec-path-from-shell company-quickhelp company-go clojure-mode-extra-font-locking cider beacon))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
