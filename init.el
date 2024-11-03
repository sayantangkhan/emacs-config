;; inhibit-startup-echo-area-message MUST be set to a hardcoded
;; string of your login name
(setq inhibit-startup-echo-area-message "sayantan")
(setq inhibit-startup-message t)

;; Straight.el bootstrap code
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;	(url-retrieve-synchronously
;;	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;	 'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))


;; Adding melpa to package repos for non Nix config
;; Setting up package management

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ;; You might already have this line

;; use-package config
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
;(setq use-package-always-ensure t)

;; General packages used for all modes
(use-package use-package)
(use-package monokai-theme)
(use-package bind-key)
(use-package undo-tree)
(use-package async)
(use-package separedit)
(use-package flycheck)
(use-package nix-mode)

;; Package config files

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load-library "emacs-general")
(load-library "misc")
(load-library "smartparens-conf")
(load-library "dashboard-conf")
(load-library "yasnippet-conf")
;; (load-library "neotree-conf")
(load-library "evil-mode-conf")
(load-library "ibuffer-conf")
(load-library "helm-conf")
(load-library "org-mode-conf")
(load-library "markdown-mode-conf")
(load-library "auctex-conf")
(load-library "magit-conf")
;; (load-library "python-mode-conf")
;; (load-library "simple-rust-mode-conf")
;; (load-library "c-mode-conf")
(load-library "haskell-mode-conf")

;; Loading mu4e only on specific machines
(if (string= system-name "t420s") (progn
				    (load-library "mu4e-conf")
				    ))

;; (load-library "scheme-mode-conf")
;; (load-library "rust-mode-conf")
;; (load-library "fzf-conf")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(blacken-executable "/home/sayantan/.emacs.d/elpy/rpc-venv/bin/black")
 '(column-number-mode t)
 '(company-c-headers-path-system
   '("/usr/include/" "/usr/local/include/" "/usr/lib/x86_64-linux-gnu/openmpi/include/"))
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("bae098b53639a4478089251b6a1c8fcb86e5f6ba41ac3d6b79c0ce54812b8a90" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(ein:polymode t)
 '(evil-undo-system 'undo-tree)
 '(fci-rule-color "#3C3D37")
 '(fill-column 110)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-idle-change-delay 2)
 '(helm-ff-keep-cached-candidates 'nil)
 '(help-window-select t)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(org-M-RET-may-split-line '((default)))
 '(org-default-notes-file (concat org-directory "/notes.org"))
 '(org-directory "~/OrgMode")
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-noter-auto-save-last-location t)
 '(org-noter-notes-search-path '("~/OrgMode/math/"))
 '(org-startup-folded 'overview)
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(f dashboard nix-mode all-the-icons elpy rust-mode magit yasnippet helm markdown-mode tex use-package neotree monokai-theme evil-nerd-commenter evil-leader auctex-latexmk))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   '((eval LaTeX-add-environments
	   '("problem" "Date" "Tags"))
     (reftex-default-bibliography "references.bib")))
 '(save-place-mode t)
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t)
 '(smooth-scrolling-mode t)
 '(undo-tree-auto-save-history nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp)))
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :height 120 :family "DejaVu Sans Mono")))))
;; (put 'narrow-to-region 'disabled nil)
