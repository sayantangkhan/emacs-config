;; (straight-use-package 'haskell-mode)
;; (straight-use-package 'reformatter)

(use-package haskell-mode)
(use-package reformatter)

(use-package haskell-snippets
  ;; :straight t
  :hook (haskell-mode . yas-minor-mode)
  )

(use-package ormolu
  ;; :straight t
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))
