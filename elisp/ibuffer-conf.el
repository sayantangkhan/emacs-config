;; ibuffer settings
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Org" (mode . org-mode))
	 ("LaTeX" (or (mode . latex-mode)
		      (mode . bibtex-mode)
		      (mode . tex-mode)))
	 ("Rust" (mode . rustic-mode))
	 ("C" (mode . c-mode))
	 ("C++" (mode . c++-mode))
	 ("Python" (mode . python-mode))
	 ("Nix" (mode . nix-mode))
	 ("Markdown" (mode . markdown-mode))
	 ("Haskell" (or (mode . haskell-mode)
			(mode . haskell-cabal-mode)))
	 ("emacs-config" (filename . ".emacs.d"))
	 ("Dired" (mode . dired-mode))
	 ;; ("code" (filename . "code"))
	 ;; ("Magit" (mode . magit-status-mode))
	 ;; ("Helm" (mode . hmm-mode))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")
	     (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
	     (add-to-list 'ibuffer-never-show-predicates "magit")
	     ))
