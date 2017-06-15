;; sets up your PATH.
(defvar cpm-local-bin (concat (getenv "HOME") "/bin") "Local execs.")
(defvar usr-local-bin "/usr/local/bin")
(setenv "PATH" (concat usr-local-bin ":" (getenv "PATH") ":" cpm-local-bin))
(setq exec-path (append exec-path (list cpm-local-bin usr-local-bin)))

;; No bells
(setq visible-bell nil) ;; The default (setq ring-bell-function 'ignore)

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; confirm to quit window
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; syntax hightlight over 80 column
(setq whitespace-line-column 250)

;; save last position
(save-place-mode 1)

;; show paren hightlight
(setq show-paren-delay 0)
(show-paren-mode 1)

;; auto pair
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'org-mode-hook 'electric-pair-mode)
(add-hook 'markdown-mode-hook 'electric-pair-mode)
(add-hook 'robot-mode-hook 'electric-pair-mode)

; utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; auto reload when files changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Give buffers unique names
(setq uniquify-buffer-name-style 'forward)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

; Merge system's and Emacs' clipboard
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; files end in a newline
(setq require-final-newline t)

; Disable start-up screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'fundamental-mode)

;; And bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; disable backup/auto save files
(setq make-backup-files nil)
(setq auto-save-default nil)


;; OSX
(let ((is-mac (string-equal system-type "darwin")))
  (when is-mac
    (setq mac-allow-anti-aliasing t)
    (setq delete-by-moving-to-trash t)
    (setq trash-directory "~/.Trash")
    ;; Don't make new frames when opening a new file with Emacs
    (setq ns-pop-up-frames nil)
    (setq ns-use-native-fullscreen nil)
    ;; Set modifier keys
    (setq mac-option-modifier 'meta) ;; Bind meta to ALT
))

;; Not going to use these commands
(put 'ns-print-buffer 'disabled t)
(put 'suspend-frame 'disabled t)

;; line wrap
(setq line-move-visual t)

;; spaces
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)
(setq-default indicate-empty-lines nil)

;; disable ui tools
(menu-bar-mode -1)
(tool-bar-mode -1)

;; font and initial window
(setq default-frame-alist
      '((font . "Fira Code-13")
        (width . 157) ; character
        (height . 52) ; lines
        ))

;; show line and column in modeline
(line-number-mode 1)
(column-number-mode 1)

;; scrolling
(setq scroll-conservatively 10000)

;; reuse window
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

;; show absolute path of file on frame title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun my/disable-scroll-bars (frame)
  "Disable scrollbar on emacsclient."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; Config files
(defun goto-init.el ()
    "Open init.el file."
    (interactive)
    (find-file "~/.emacs.d/init.el"))
(defun goto-custom.el ()
    "Open custom.el file."
    (interactive)
    (find-file "~/.emacs.d/custom.el"))

(defun xah-next-user-buffer ()
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-user-buffer-q ()
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun switch-to-previous-buffer ()
  "Switch to most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-`") 'switch-to-previous-buffer)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; hide undo tree
(diminish 'undo-tree-mode)

;; shell
(setq explicit-shell-file-name "/bin/bash")
(add-hook 'shell-mode-hook 'no-trailing-whitespace)
(add-hook 'shell-mode-hook '(lambda () (setq-local ml-interactive? t)))

;;theme
(use-package doom-themes
  :ensure t
  :init
  (if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (select-frame frame)
            (load-theme 'doom-one t)))
    (load-theme 'doom-one t))
  )

;; tab/spaces
(use-package whitespace
  :config (setq whitespace-style '(tabs newline space-mark
                                tab-mark newline-mark
                                face lines-tail))
  (setq whitespace-display-mappings
        '((space-mark nil)
          (newline-mark nil)
          (tab-mark 9 [124 9] [92 9])))
  :init (global-whitespace-mode 1)
  :diminish global-whitespace-mode
  )

;; EVIL
(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-want-C-u-scroll t)
    (setq evil-search-module 'evil-search)

    (fset 'evil-visual-update-x-selection 'ignore)
    (run-with-idle-timer 20 t 'evil-normal-state)
    (setq evil-echo-state nil)
    (evil-mode t)
    )
  )
;
(use-package evil-commentary
  :ensure t
  :init (evil-commentary-mode t)
  :diminish evil-commentary-mode
  )

(use-package evil-surround
  :ensure t
  :init (evil-surround-mode t)
  )

(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (defun my-jump-to-tag ()
    (interactive)
    (evil-emacs-state)
    (call-interactively (key-binding (kbd "M-.")))
    (evil-change-to-previous-state (other-buffer))
    (evil-change-to-previous-state (current-buffer)))

  (general-define-key
   :states '(normal)
   "M-." 'my-jump-to-tag)

  (general-define-key
   :states '(normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  (general-define-key
   :states '(normal visual)
   "1" 'xah-previous-user-buffer
   "2" 'xah-next-user-buffer
   )
)

;; linum
(use-package nlinum
  :ensure t
  :init
  (setq-default nlinum-format "%4d ")
  (setq line-number-display-limit-width 10000)
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'robot-mode-hook 'nlinum-mode)
  (add-hook 'org-mode-hook 'nlinum-mode)
  (add-hook 'markdown-mode-hook 'nlinum-mode)
  )

;; Persp
(use-package persp-mode
  :ensure t
  :bind (("C-x b" . persp-switch-to-buffer)
         ("C-x k" . persp-kill-buffer))

  :init
  (setq wg-morph-on nil) ;; switch off animation
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))
  )
;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode t)
  )

;; ivy
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )

(use-package counsel
  :ensure t
  :init (counsel-projectile-on)
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate))
  )

;; Company
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'robot-mode-hook 'company-mode)
  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'markdown-mode-hook 'company-mode)

  :config
  (progn
    (setq company-idle-delay 0
          company-echo-delay 0
          ;; company-require-match nil
          company-minimum-prefix-length 3)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil))))))
  )

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
  (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
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
  :init
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'prog-mode-hook 'global-flycheck-mode)
  )

;; Virtualenvwrapper
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  )

;; show window number
(use-package window-numbering
  :ensure t
  :init (window-numbering-mode)
  )

(use-package go-mode
  :ensure t
  :commands (my-go-mode-hook)
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (setq gofmt-show-errors nil)
    (add-hook 'before-save-hook 'gofmt-before-save)
    (go-eldoc-setup)
    (local-set-key (kbd "M-.") 'go-guru-definition)
    (set (make-local-variable 'company-backends) '(company-go)))
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package tide
  :commands (tide-setup)
  :ensure t
  :config
  (progn
    (setq tide-format-options '(:indentSize 2 :tabSize 2))
    (eldoc-mode +1)
    (add-hook 'before-save-hook 'tide-format-before-save)
    )
  )
(add-hook 'typescript-mode-hook 'tide-setup)

(use-package web-mode
  :ensure t
  :config
  (progn
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting t)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (defun web-mode-for-css ()
      (web-mode)
      (setq flycheck-disabled-checkers (append '(javascript-eslint) flycheck-disabled-checkers))
      )
    (defun web-mode-for-html ()
      (web-mode)
      (setq flycheck-disabled-checkers (append '(javascript-eslint) flycheck-disabled-checkers))
      )
    (defun web-mode-for-js ()
      (web-mode)
      (tide-setup)
      (flycheck-select-checker 'javascript-eslint)
      )
    (defun web-mode-for-tsx ()
      (web-mode)
      (tide-setup)
      )

    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode-for-css))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode-for-html))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode-for-html))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode-for-js))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode-for-js))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode-for-tsx))
    )
  )

;; Robot Framework
(add-to-list 'auto-mode-alist '("\\.txt\\'" . robot-mode))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (kill-emacs)
  )
