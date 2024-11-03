;; Configuration of evil and related packages

;; (straight-use-package 'evil)
;; (straight-use-package 'evil-nerd-commenter)
;; (straight-use-package 'evil-leader)

(use-package evil)
(use-package evil-nerd-commenter)
(use-package evil-leader)

(use-package evil
  :config
  (global-evil-leader-mode)
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes 'neotree-mode)
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
  (add-to-list 'evil-emacs-state-modes 'geiser-repl-mode)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode)
  (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
  )

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys)
  )

(use-package evil-leader
  :config
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "'" 'ibuffer
    "k" 'kill-buffer
    "c" 'evilnc-comment-or-uncomment-lines)
  )
