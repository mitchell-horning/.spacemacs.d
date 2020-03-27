(defconst lispy-packages
  '(lispy
    lispyville))

(defun lispy/init-lispyville ()
  (use-package lispyville
    :init
    (add-hook 'lispy-mode-hook #'lispyville-mode)
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
    :hook ((clojure-mode cider-repl-mode emacs-lisp-mode lisp-mode) . lispy-mode)
    :custom
    (lispy-compat '(edebug cider magit-blame-mode))
    (lispy-eval-display-style 'overlay)
    :config
    (progn
      (define-key lispy-mode-map (kbd "M-RET") nil)
      (evil-define-key 'insert lispy-mode-map
        (kbd ")")   'lispy-right-nostring
        (kbd "C-d") 'lispy-delete
        (kbd "C-k") 'lispy-kill
        (kbd "C-y") 'lispy-yank
        (kbd "C-e") 'lispy-ove-end-of-line
        (kbd "C-n") 'lispy-mark-symbol
        (kbd "M-.") 'lispy-goto-symbol))))

;;; packages.el ends here
