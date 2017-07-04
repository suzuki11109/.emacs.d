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
 '(custom-safe-themes
   (quote
    ("5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (evil-escape evil-magit nameframe-projectile nameframe editorconfig color-identifiers-mode prodigy shell-pop popwin go-playground aggressive-indent company-restclient restclient evil-cleverparens electric-pair-mode electric-pair evil-smartparens dumb-jump persp-mode-projectile-bridge-mode persp-mode-projectile-bridge eyebrowse workgroups2 persp-projectile perspeen perspective workgroups exec-path-from-shell lispy rainbow-delimiters-mode parinfer smartparens evil-paredit paredit cider yasnippet yaml-mode window-numbering web-mode web virtualenvwrapper use-package tide tern smex smart-mode-line sane-term rainbow-mode rainbow-delimiters persp-mode nlinum multiple-cursors multi-term material-theme markdown-mode magit json-mode ido-vertical-mode ido-ubiquitous ido-clever-match helm-projectile helm-ls-git helm-git-files helm-ag go-guru go-eldoc go general fzf flx-ido find-file-in-repository find-file-in-project evil-surround evil-commentary doom-themes desktop+ dash-functional cyberpunk-theme counsel-projectile company-web company-go)))
 '(safe-local-variable-values (quote ((default-tab-width . 4))))
 '(vc-annotate-background "#1B2229")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
