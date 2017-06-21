(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/robot-mode.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dumb-jump persp-mode-projectile-bridge-mode persp-mode-projectile-bridge eyebrowse workgroups2 persp-projectile perspeen perspective workgroups exec-path-from-shell lispy rainbow-delimiters-mode parinfer smartparens evil-paredit paredit cider yasnippet yaml-mode window-numbering web-mode web virtualenvwrapper use-package tide tern smex smart-mode-line sane-term rainbow-mode rainbow-delimiters persp-mode nlinum multiple-cursors multi-term material-theme markdown-mode magit json-mode ido-vertical-mode ido-ubiquitous ido-clever-match helm-projectile helm-ls-git helm-git-files helm-ag go-guru go-eldoc go general fzf flx-ido find-file-in-repository find-file-in-project evil-surround evil-commentary doom-themes desktop+ dash-functional cyberpunk-theme counsel-projectile company-web company-go)))
 '(safe-local-variable-values (quote ((default-tab-width . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
