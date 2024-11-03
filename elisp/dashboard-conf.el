;; Dashboard configuration
(use-package dashboard
  ;; :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq show-week-agenda-p t)
  ;; (setq time-string-formatting "%e %b ")
  (setq dashboard-agenda-time-string-format "%e %b ")
  (setq dashboard-items '(
			  (agenda . 10)
			  (bookmarks . 10)
			  ;; (recents  . 10)
			  )
	)
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (add-hook 'dashboard-mode-hook
	    (lambda ()
	      (local-set-key (kbd "o") 'org-capture)
	      )
	    )
  (add-hook 'dashboard-mode-hook
	    (lambda ()
	      (local-set-key (kbd "p") 'org-agenda)
	      )
	    )
  (setq dashboard-set-footer nil)
  )




;; Temporary workaround. Remove when upstream is fixed.

(setq dashboard-agenda-prefix-format "%-1:c %s")

;; (defun dashboard-agenda--formatted-headline ()
;;   "Set agenda faces to `HEADLINE' when face text property is nil."
;;   (let* ((headline (org-get-heading t t t t))
;;	 (todo (or (org-get-todo-state) ""))
;;	 (org-level-face (nth (- (org-outline-level) 1) org-level-faces))
;;	 (todo-state (format org-agenda-todo-keyword-format todo))
;;	 )
;;     (when (null (get-text-property 0 'face headline))
;;       (add-face-text-property 0 (length headline) org-level-face t headline))
;;     (when (null (get-text-property 0 'face todo-state))
;;       (add-face-text-property 0 (length todo-state) (org-get-todo-face todo) t todo-state))
;;     (concat todo-state " " headline)))
