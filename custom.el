(setq-default use-package-always-ensure t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; DEFAULTS OPTIONS
(defalias 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq help-window-select t)
(setq require-final-newline t)
(setq echo-keystrokes 0.1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq load-prefer-newer t)
(setq initial-major-mode 'fundamental-mode)
(setq-default whitespace-line-column 250)
(setq-default show-paren-delay 0)
(show-paren-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'nil)

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message "")

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq line-move-visual t)
(add-hook 'robot-mode-hook (lambda () (setq indent-line-function 'insert-tab)))


(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)


(setq default-frame-alist
      '((font . "Fira Code-13")
        (vertical-scroll-bars . nil)
        (width . 157)
        (height . 52)))

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


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


;; FUNCTIONS
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

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
    (while (< i 50)
      (if (or (not (member (current-buffer) current-project-buffers)) (not (xah-user-buffer-q)))
          (progn
            (funcall direction)
            (setq i (1+ i)))
        (progn (setq i 100))))))

(defun switch-to-previous-buffer ()
  "Switch to most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; DEFAULT HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq explicit-shell-file-name "/bin/bash")
(add-hook 'shell-mode-hook 'no-trailing-whitespace)
(add-hook 'shell-mode-hook '(lambda () (setq-local ml-interactive? t)))

(add-hook 'prog-mode-hook (lambda ()
                            (when (not (derived-mode-p
                                        'lisp-mode
                                        'clojure-mode
                                        'emacs-lisp-mode
                                        'common-lisp-mode))
                              (setq-local electric-pair-pairs '((?\' . ?\'))))
                            (electric-pair-mode)))

;; DEFAULT KEYS
(global-set-key (kbd "M-`") 'switch-to-previous-buffer)
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key [f1] 'help-command)
(global-set-key [f1 f1] 'help-for-help)
(global-set-key [kp-f1] 'help-command)
(global-set-key [kp-f1 kp-f1] 'help-for-help)
(global-set-key [(control ?h)] 'delete-backward-char)
;; Font size
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#ebbd80") (:weight bold) (:underline t)) t))))

(add-hook 'prog-mode-hook #'my/add-watchwords)

;; _ to part of words
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

;; PACKAGES
(use-package restclient)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package magit)

;; EVIL
(use-package evil
  :init
  (progn
    (setq evil-want-C-u-scroll t)
    (setq evil-search-module 'evil-search)

    (advice-add 'evil-ex-search-next :after
                (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
    (advice-add 'evil-ex-search-previous :after
                (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
    (evil-mode t)))

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


(use-package evil-surround
  :init (global-evil-surround-mode 1))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode t))

(use-package evil-magit)

(use-package midnight
  :init
  (midnight-delay-set 'midnight-delay "4:30am"))

;; linum
(use-package nlinum
  :config
  (setq-default nlinum-format "%4d ")
  (setq line-number-display-limit-width 10000)
  (global-nlinum-mode)
  (add-hook 'term-mode-hook (lambda () (nlinum-mode -1)))
  (add-hook 'shell-mode-hook (lambda () (nlinum-mode -1)))
  (add-hook 'eshell-mode-hook (lambda () (nlinum-mode -1)))
  (add-hook 'helm-mode-hook (lambda () (nlinum-mode -1))))


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
  (setq whitespace-global-modes '(not org-mode
                                      eshell-mode
                                      shell-mode
                                      web-mode
                                      log4j-mode
                                      dired-mode
                                      common-lisp-mode
                                      emacs-lisp-mode
                                      clojure-mode
                                      lisp-mode))
  (setq whitespace-style '(tabs
                           newline
                           space-mark
                           tab-mark
                           newline-mark
                           face
                           lines-tail))
  (setq whitespace-display-mappings
        '((space-mark nil)
          (newline-mark nil)
          (tab-mark 9 [124 9] [92 9])))
  (global-whitespace-mode 1)
  :diminish global-whitespace-mode)


;; ivy
(use-package ivy
  :diminish (ivy-mode)
  :config
  (setq ivy-initial-inputs-alist nil)
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
   ("C-c g"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)))

;; Projectile
(use-package projectile
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (projectile-mode t))

(use-package nameframe)
(use-package nameframe-projectile
  :config (nameframe-projectile-mode))

;; Company
(use-package company
  :diminish company-mode
  :bind (:map company-active-map
              ("<backtab>" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init (add-hook 'after-init-hook 'global-company-mode)
  (progn
    (setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))
    (setq company-idle-delay 0.1
          company-selection-wrap-around t
          company-tooltip-limit 20)
    (setq company-dabbrev-code-everywhere t)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-code-modes t)
    (setq company-dabbrev-code-other-buffers 'all)
    (setq company-dabbrev-ignore-buffers "\\`\\'")
    (setq company-transformers '(company-sort-by-occurrence))
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
    ))

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

(use-package aggressive-indent
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (when (derived-mode-p
                                     'lisp-mode
                                     'clojure-mode
                                     'emacs-lisp-mode
                                     'common-lisp-mode)
                                (aggressive-indent-mode)))))

(use-package aggressive-indent
  :config
  (add-hook 'prog-mode-hook (aggressive-indent-mode)))

;; Robot Framework
(add-to-list 'auto-mode-alist '("\\.txt\\'" . robot-mode))

(use-package json-mode
  :config
  (setq-default js-indent-level 2)
  (add-hook 'before-save-hook 'json-mode-beautify))

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
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  (eldoc-mode +1)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'tide-setup))

;; web
(use-package web-mode
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (defun web-mode-without-eslint ()
      (web-mode)
      (setq flycheck-disabled-checkers (append '(javascript-eslint) flycheck-disabled-checkers)))
    (defun web-mode-for-js ()
      (web-mode)
      (tide-setup)
      (flycheck-select-checker 'javascript-eslint))
    (defun web-mode-for-tsx ()
      (web-mode)
      (tide-setup))

    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode-without-eslint))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode-without-eslint))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode-without-eslint))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode-for-js))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode-for-tsx))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode-for-js))
    (setq-default web-mode-code-indent-offset 2)
    (setq-default web-mode-css-indent-offset 2)
    (setq-default web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting nil)
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))
    ))

;; Clojure
(use-package cider
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package prodigy
  :config
  (progn
    (prodigy-define-service
      :name "Calljumper web"
      :cwd "~/Code/calljumper/"
      :command "yarn"
      :args '("start")
      :tags '(node)
      :port 3000
      :stop-signal 'sigkill)
    ))
