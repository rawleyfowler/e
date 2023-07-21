
;;; init.el --- Perl 5 Emacs Configuration
;;; Commentary:
;;; A Perl 5 focused Emacs configuration, for the unix genie.

;;; Code:
(server-start)

;; -- Get Font Here --
;; https://github.com/slavfox/Cozette/releases
(add-to-list 'default-frame-alist '(font . "CozetteHiDpi-13"))
(add-to-list 'default-frame-alist (list '(width . 72) '(height . 72)))

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
(global-linum-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(recentf-mode 1)

(defun set-exec-path-from-shell ()
  "Set up Emacs' 'exec-path' and PATH environment."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell)

(add-to-list 'load-path "~/.emacs.d/extras")
(require 'perltidy) ; Thanks to https://github.com/zakame/perltidy.el
(require 'perl-mode)
(require 'cperl-mode)
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
(setq cperl-indent-parens-as-block t)

;; Packages
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defconst pkgs
  '(dracula-theme
    flycheck
    web-mode
    json-mode
    yaml-mode
    dockerfile-mode
    ctrlf
    magit
    smex))
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (pkg pkgs)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(global-flycheck-mode)
(setq flycheck-perlcritic-severity 3)

(require 'ctrlf)
(ctrlf-mode +1)

(require 'ido)
(ido-mode 1)

(require 'smex)
(smex-initialize)

(defun kill-inner-word ()
  "It's ciw from Vim."
  (interactive)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "C-c C-c") #'kill-inner-word)
(global-set-key (kbd "M-x") #'smex)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-light))
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(delete-selection-mode nil)
 '(package-selected-packages '(flycheck dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here.
