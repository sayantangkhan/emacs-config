;; AUCTeX config

;; (use-package auctex)
;; (straight-use-package
;;  '(el-patch :type git :host github :repo "wang1zhen/auctex-latexmk"
;;	    :fork (:host github
;;		   :repo "sayantangkhan/auctex-latexmk")))

(use-package tex)
(use-package auctex-latexmk)
(require 'smartparens)

;; Setup for multi file tex documents
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; (setq-default TeX-master "main")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
;;(add-hook 'LaTeX-mode-hook (lambda () (linum-mode 0)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook
	  (lambda () (local-set-key (kbd "<backtab>") 'dabbrev-expand)))
(setq reftex-plug-into-AUCTeX t)
;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)

(if (string= (system-name) "Sayantans-Mac-mini.local")
    (progn
      (eval-after-load "tex"
	'(add-to-list 'TeX-view-program-list '("macOS-preview-build" "open ./build/%o")))
      (setq TeX-view-program-selection '((output-pdf "macOS-preview-build")))
      )
  (progn
    (eval-after-load "tex"
      '(add-to-list 'TeX-view-program-list '("Evince-build" "evince ./build/%o")))
    (setq TeX-view-program-selection '((output-pdf "Evince-build")))
    )
  )


(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("xelatexmk" "latexmk -xelatex %s" TeX-run-command t t :help "Run latexmk with xelatex") t))

(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("latexmk-continuous" "latexmk -pvc -view=none %s" TeX-run-command t t :help "Run latexmk in continuous mode") t))


; Setting default bibliography file
(setq reftex-default-bibliography '("references.bib"))

;; ;(require 'auctex-latexmk)
    (auctex-latexmk-setup)
(add-hook 'TeX-mode-hook
      (lambda ()
	(setq TeX-command-default "LatexMk")))

(add-hook 'LaTeX-mode-hook
      (lambda ()
	(setq TeX-command-default "LatexMk")))

(defun proper-bracket (bracket)
  (cond
   ((string= "(" bracket)
    (insert "\\left(\\right)")
    (backward-char 8)
   )
   ((string= "{" bracket)
    (insert "\\left\\{\\right\\}")
    (backward-char 9)
    )
   ((string= "[" bracket)
    (insert "\\left[\\right]")
    (backward-char 8)
   )
  )
)

(defun proper-round-brace ()
  (proper-bracket "(")
  (interactive)
  ()
)

(defun proper-curly-brace ()
  (proper-bracket "{")
  (interactive)
  ()
)

(defun proper-square-brace ()
  (proper-bracket "[")
  (interactive)
  ()
)

(add-hook 'LaTeX-mode-hook
      (lambda ()
	(evil-leader/set-key
	  "s" 'LaTeX-section
	  "RET" 'TeX-insert-macro
	  "e" 'LaTeX-environment
	  "9" 'proper-round-brace
	  "[" 'proper-square-brace
	  "{" 'proper-curly-brace)))

(sp-pair "`" nil :actions :rem)
(sp-local-pair 'LaTeX-mode "``" nil :actions :rem)
