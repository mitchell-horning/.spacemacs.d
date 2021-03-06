(defconst lispy-packages
  '((lispy :location (recipe :fetcher local))
    lispyville))

(defun lispy/init-lispyville ()
  (use-package lispyville
    ;; :init
    ;; (add-hook 'lispy-mode-hook #'lispyville-mode)
    :hook ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode) .
           lispyville-mode)
    :config
    (progn
      (diminish 'lispyville-mode (lispyville-mode-line-string "🍰" "🍰"))
      (lispyville-set-key-theme '(operators
                                  c-w
                                  c-u
                                  prettify
                                  atom-movement
                                  additional-motions
                                  commentary
                                  slurp/barf-lispy
                                  escape)))))

(defun lispy/init-lispy ()
  (use-package lispy
    :defer t
    :diminish lispy-mode ""
    :hook ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode) .
           lispy-mode)
    :custom
    (lispy-compat '(edebug cider magit-blame-mode))
    (lispy-eval-display-style 'overlay)
    :config
    (progn
      (define-key lispy-mode-map-lispy (kbd "<M-return>") nil)
      (define-key lispy-mode-map-evilcp (kbd "<M-return>") nil)
      (define-key lispy-mode-map-lispy (kbd "M-RET") nil)
      (evil-define-key 'insert lispy-mode-map
        (kbd "/")   'special-lispy-splice
        (kbd ")")   'lispy-right-nostring
        (kbd "C-d") 'lispy-delete
        (kbd "C-y") 'lispy-yank
        (kbd "C-e") 'lispy-move-end-of-line
        (kbd "M-.") 'lispy-goto-symbol))))

;;; packages.el ends here
