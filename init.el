;;; package --- Rawley Fowler's init.el
;;; Commentary:
;;; Rawley Fowler's Emacs configuration
;;; Code:

;; Font
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-13"))

;; Boilerplate/personalizations
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
(setq package-enable-at-startup nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-ic")
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(set-cursor-color "orange red")
(global-linum-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(recentf-mode 1)

;; My customized packages
(add-to-list 'load-path "~/.emacs.d/rf")
(require 'funcs)
(require 'perltidy) ; Thanks to https://github.com/zakame/perltidy.el

;; Perl stuff
(require 'perl-mode)
(add-hook 'cperl-mode-hook
          #'(lambda ()
            (setq font-lock-defaults
                  '((perl-font-lock-keywords perl-font-lock-keywords-1 perl-font-lock-keywords-2)
                    nil nil ((?\_ . "w")) nil
                    (font-lock-syntactic-face-function . perl-font-lock-syntactic-face-function)))
            (font-lock-refresh-defaults)))
(defalias 'perl-mode 'cperl-mode)
(add-hook 'before-save-hook #'(lambda ()
                              (when (or (eq major-mode 'perl-mode) (eq major-mode 'cperl-mode))
                                  (perltidy-buffer))))


;; Packages
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defconst packages '(ctrlf
                     flycheck
                     mmm-mode
                     doom-modeline
                     web-mode
                     all-the-icons
				     better-defaults
				     projectile
				     json-mode
				     clojure-mode
                     dashboard
				     cider
				     gruvbox-theme
				     yaml-mode
				     dockerfile-mode
				     cmake-mode
				     docker
				     smex
				     go-mode
				     raku-mode
				     tuareg
				     lsp-mode
				     company
                     company-box
				     treemacs
				     lsp-treemacs
				     ivy
				     lsp-ivy
				     lsp-ui
				     magit))
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package packages)
  (unless (package-installed-p package)
	(package-install package)))

;; Package configuration
(when (display-graphic-p)
  (require 'all-the-icons))

(setq raku-indent-level 4)

(require 'ctrlf)
(ctrlf-mode +1)

(require 'company-box)

(require 'ido)
(ido-mode 1)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents . 7)
                        (bookmarks . 7)))
(setq dashboard-startup-banner 'logo)
(setq dashboard-banner-logo-title "emacs")
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 30)
(setq doom-modeline-hud nil)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-time-icon nil)
(setq doom-modeline-env-perl-executable "perl")

(add-to-list 'auto-mode-alist '("\\.raku\\?\\(test\\|mod\\)$" . raku-mode))

(require 'ivy)
(require 'projectile)
(projectile-mode)
(setq projectile-completion-system 'ivy)

(require 'lsp-mode)
(setq lsp-enable-symbol-highlighting t)
(setq lsp-keymap-prefix "C-c l")
(setq lsp-clangd-binary-path (executable-find "clangd"))
(setq lsp-clients-clangd-library-directories "/usr/include/c++/12")
(setq raku-indent-level 4)
(setq go-indent-level 4)

(require 'lsp-ui)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-show-with-mouse nil)

(require 'company)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)

(require 'smex)
(smex-initialize)

(require 'docker)

;; Hooks
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
            (local-set-key (kbd "C-x E") 'eval-buffer)))
(add-hook 'perl-mode-hook #'lsp-deferred)
(add-hook 'cperl-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'company-mode-hook 'company-box-mode)

;; Keys
(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
(global-set-key (kbd "C-c C-d") #'rf/goto-dashboard)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c d") 'docker)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c n") #'rf/indent-buffer)
(global-set-key (kbd "C-x w") #'rf/kill-inner-word)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-medium))
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(web-mode spacemacs-theme company-box json-mode cmake-mode cider clojure-mode projectile better-defaults magit raku-mode go-mode smex docker dockerfile-mode yaml-mode gruvbox-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init.el)
;;; init.el ends here
