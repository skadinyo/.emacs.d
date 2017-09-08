(setq make-backup-files nil)
(setq auto-save-default nil)
(setq message-log-max nil)
;;PACKAGE

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
    web-mode
    undo-tree
    material-theme
    tabbar
    tabbar-ruler
    ;;go lang
    go-mode
    go-guru
    ))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;PACKAGE

;;UI

;;theme
(load-theme 'material t)

;;font
;;(add-to-list 'default-frame-alist '(font . "PT Mono-14:weight=normal"))
;;(set-face-attribute 'default t :font "PT Mono-14:weight=normal")

;; Don't show native OS bars scroll for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;misc
(blink-cursor-mode 0)
(setq-default frame-title-format "%b (%f)")
(global-linum-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ring-bell-function 'ignore)
(global-set-key (kbd "s-t") '(lambda () (interactive)))

(setq
 ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 ;; apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 ;; mouse-yank-at-point t
 )

;;THEME AND UI

;;;;
;; Customization
;;;;
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

(load "navigation.el")
;;(load "ui.el")
(load "editing.el")
(load "misc.el")
;;(load "firacode.el")
(load "elisp-editing.el")
;; Language-specific
(load "setup-clojure.el")
(load "setup-js.el")

(load "kbd.el")
(load "intellij.el")


;; (auto-complete-mode 1)

;;company
(global-company-mode)
(setq company-idle-delay nil) ; never start completions automatically
(global-set-key (kbd "M-TAB") #'company-complete)
 ; use M-TAB, a.k.a. C-M-i, as manual trigger
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(company-quickhelp-mode 1)
;;company-go
(add-to-list 'load-path "/home/nakama/workspace/go/src/github.com/nsf/gocode/emacs-company")
(require 'company-go)                                ; load company mode go backend

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4) 
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-comment-style 4)
  )

(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;;Todo add hook to flycheck specifically
(global-flycheck-mode)

(setenv "GOPATH" "/home/nakama/workspace/go")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)


(defun my-go-mode-hook ()
  (setq-default) 
  (setq tab-width 2) 
  (setq standard-indent 2) 
  (setq indent-tabs-mode nil)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'godef-jump-other-window)
  (local-set-key (kbd "M-p") 'compile) 
  (local-set-key (kbd "M-P") 'recompile)
  ;; (local-set-key (kbd "<M-up>") 'go-guru-expand-region)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq column-number-mode t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun simulate-key-press (key)
  "Pretend that KEY was pressed.
KEY must be given in `kbd' notation."
  `(lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

;;Too lazy to change kbd for projectile
;;It's still not really used though :(
(global-set-key (kbd "C-p") (simulate-key-press "C-c p"))

(beacon-mode 1)
(ido-vertical-mode t)
(put 'upcase-region 'disabled nil)


;; load tabbar
(add-to-list 'load-path "~/dotfiles/tabbar")
;; Tabbar settings
;; Tabbar
(require 'tabbar)
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 1 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 1 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 1 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode t)
(global-set-key (kbd "M-[") 'tabbar-backward)
(global-set-key (kbd "M-]") 'tabbar-forward)

;; (setq tabbar-ruler-global-tabbar t)    ; get tabbar
;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
;; (setq tabbar-ruler-popup-menu t)       ; get popup menu.
;; (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;; (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
(require 'tabbar-ruler)
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
(tabbar-ruler-group-by-projectile-project)

(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max))
      )))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))
