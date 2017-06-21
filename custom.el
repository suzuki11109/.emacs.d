;;; Code:
(setq-default use-package-always-ensure t)

;; PATH from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; DEFAULTS OPTIONS
(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq help-window-select 't)
(setq require-final-newline t)
(setq echo-keystrokes 0.1)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default whitespace-line-column 250)
(setq-default show-paren-delay 0)
(show-paren-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Disable start-up screen
(setq inhibit-startup-screen t
  inhibit-splash-screen t
  inhibit-startup-message t
  initial-scratch-message "")

;; utf-8
(setq locale-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; spaces
(setq-default line-move-visual t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq indent-line-function 'insert-tab)

;; scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; font and initial window
(setq default-frame-alist
      '((font . "Fira Code-13")
        (vertical-scroll-bars . nil)
        (width . 157) ; character
        (height . 52))) ; lines

;; show absolute path of file on frame title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; OSX
(let ((is-mac (string-equal system-type "darwin")))
  (when is-mac
    (setq mac-allow-anti-aliasing t)
    (setq delete-by-moving-to-trash t)
    (setq trash-directory "~/.Trash")
    ;; Don't make new frames when opening a new file with Emacs
    (setq ns-pop-up-frames nil)
    (setq ns-use-native-fullscreen nil)
    (setq mac-option-modifier 'meta)))

(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; DEFAULT HOOKS
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq explicit-shell-file-name "/bin/bash")
(add-hook 'shell-mode-hook 'no-trailing-whitespace)
(add-hook 'shell-mode-hook '(lambda () (setq-local ml-interactive? t)))


;; FUNCTIONS
(defun goto-init.el ()
    "Open init.el file."
    (interactive)
    (find-file "~/.emacs.d/init.el"))

(defun goto-custom.el ()
    "Open custom.el file."
    (interactive)
    (find-file "~/.emacs.d/custom.el"))

(defun xah-user-buffer-q ()
  "List buffer that create by user (buffer that doesn't have *)."
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

(defun switch-projectile-buffer (direction)
  "Go to next or previous buffer that open in current project by DIRECTION."
  (interactive)
  (let ((i 0)
        (current-project-buffers (projectile-project-buffers)))
    (funcall direction)
    (while (< i 20)
      (if (or (not (member (current-buffer) current-project-buffers)) (not (xah-user-buffer-q)))
          (progn
                 (funcall direction)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun switch-to-previous-buffer ()
  "Switch to most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; DEFAULT KEYS
(global-set-key (kbd "M-`") 'switch-to-previous-buffer)
(global-set-key (kbd "C-`") 'ace-window)
(global-set-key [f1] 'help-command)
(global-set-key [f1 f1] 'help-for-help)
(global-set-key [kp-f1] 'help-command)
(global-set-key [kp-f1 kp-f1] 'help-for-help)
(global-set-key [ ?\C-h ] 'delete-backward-char)


;; PACKAGES

(use-package general
  :config
  (general-evil-setup t)

  (general-define-key
    :states '(normal)
    "M-." (general-simulate-keys "\\ M-."))

  (general-define-key
   :states '(normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  (general-define-key
   :states '(normal visual)
   "1" (lambda () (interactive) (switch-projectile-buffer 'previous-buffer))
   "2" (lambda () (interactive) (switch-projectile-buffer 'next-buffer))))

;; linum
(use-package nlinum
  :config
  (setq-default nlinum-format "%4d ")
  (setq line-number-display-limit-width 10000)
  (global-nlinum-mode))

;;theme
(use-package doom-themes
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'doom-one t)))
    (load-theme 'doom-one t)))

;; tab/spaces
(use-package whitespace
  :config
  (setq whitespace-style '(tabs newline space-mark
                           tab-mark newline-mark
                           face lines-tail))
  (setq whitespace-display-mappings
            '((space-mark nil)
              (newline-mark nil)
              (tab-mark 9 [124 9] [92 9])))
  (global-whitespace-mode 1)
  :diminish global-whitespace-mode)

;; EVIL
(use-package evil
  :init
  (progn
    (setq evil-want-C-u-scroll t)
    (setq evil-search-module 'evil-search)
    (fset 'evil-visual-update-x-selection 'ignore)
    (run-with-idle-timer 20 t 'evil-normal-state)
    (advice-add 'evil-ex-search-next :after
                (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
    (advice-add 'evil-ex-search-previous :after
                (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
    (evil-mode t)))


(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode t))

(use-package evil-surround
  :config (evil-surround-mode t))

(defun my/evil-shift-left-visual ()
    "Move selected block to left."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

(defun my/evil-shift-right-visual ()
    "Move selected block to right."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))


;; ivy
(use-package ivy
  :diminish (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories nil)
  (ivy-mode 1))

(use-package counsel
  :init
  (counsel-projectile-on)
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)))

;; Projectile
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode t))

;; Company
(use-package company
  :diminish company-mode
  :defer 2
  :bind (:map company-active-map
         ("<backtab>" . company-select-previous)
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (progn
    (setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))
    (setq company-idle-delay 0.3
          company-echo-delay 0
          company-tooltip-limit 20
          company-dabbrev-ignore-case t
          company-minimum-prefix-length 3)
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
    (global-company-mode)))


;; flycheck
(use-package flycheck
  :config
  (setq flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'prog-mode-hook 'global-flycheck-mode))

;; rainbow paren
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Virtualenvwrapper
(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

;; show window number
(use-package window-numbering
  :config (window-numbering-mode))

;; Robot Framework
(add-to-list 'auto-mode-alist '("\\.txt\\'" . robot-mode))

;; Go
(use-package go-guru)
(use-package go-mode
  :init
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (setq gofmt-show-errors nil)
    (add-hook 'before-save-hook 'gofmt-before-save)
    (define-key go-mode-map (kbd "M-.") 'go-guru-definition)
    (set (make-local-variable 'company-backends) '(company-go)))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package tide
  :config
  (progn
    (setq tide-format-options '(:indentSize 2 :tabSize 2))
    (eldoc-mode +1)
    (set (make-local-variable 'company-backends) '(company-tide company-files))
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook 'tide-setup)))

;; web
(use-package web-mode
  :config
  (progn
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting t)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (defun web-mode-for-css ()
      (web-mode)
      (setq flycheck-disabled-checkers (append '(javascript-eslint) flycheck-disabled-checkers)))
    (defun web-mode-for-html ()
      (web-mode)
      (setq flycheck-disabled-checkers (append '(javascript-eslint) flycheck-disabled-checkers)))
    (defun web-mode-for-js ()
      (web-mode)
      (tide-setup)
      (flycheck-select-checker 'javascript-eslint))
    (defun web-mode-for-tsx ()
      (web-mode)
      (tide-setup))

    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode-for-css))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode-for-html))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode-for-html))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode-for-js))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode-for-js))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode-for-tsx))))

;; Clojure
(use-package cider
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package parinfer
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults
             evil
             paredit
             smart-tab
             smart-yank))
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))
