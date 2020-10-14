(defconst local-dev-packages
  '((all-the-icons :location (recipe :fetcher local))))

(defun local-dev/init-all-the-icons ()
  (use-package all-the-icons))

;;; packages.el ends here
