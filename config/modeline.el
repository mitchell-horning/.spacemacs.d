(push "~/projects/personal/all-the-icons.el/" load-path)

(setq all-the-icons-default-adjust -0.1)
(setq all-the-icons-default-faicon-adjust -0.06)
(setq all-the-icons-default-faicon-scale-factor 0.98)

(setq all-the-icons-default-octicon-adjust -0.04)
(setq all-the-icons-default-octicon-scale-factor 0.9)

(setq all-the-icons-default-fileicon-adjust -0.1)
(setq all-the-icons-default-fileicon-scale-factor 0.80)
;; (setq all-the-icons-default-fileicon-scale-factor 1.0)


;; (insert (all-the-icons-icon-for-mode 'emacs-lisp-mode))
;; 


                                        ;(spaceline-define-segment all-the-icons-mode-icon
                                        ;  "An `all-the-icons' segment indicating the current buffer's mode with an icon"
                                        ;  (let ((icon (all-the-icons-icon-for-mode major-mode)))
                                        ;    (unless (symbolp icon)
                                        ;      (propertize icon
                                        ;                  'help-echo (format "Major-mode: `%s'" major-mode)
                                        ;                  ;; 'display '(raise -0.15)
                                        ;                  ;;'face `(:family ,(all-the-icons-icon-family-for-mode major-mode)
                                        ;                  ;;               :inherit)
                                        ;      ))))

(defun spaceline-custom-theme (&rest additional-segments)
  (setq spaceline-all-the-icons-highlight-file-name t)
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead) ;; Enable # of commits ahead of upstream in git
  (spaceline-all-the-icons--setup-anzu)

  (spaceline-define-segment my-mode-icon
    "An `all-the-icons' segment indicating the current buffer's mode with an icon"
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (unless (symbolp icon)
        (propertize icon
                    'display '(raise -0.15)))))

  (spaceline-compile
    ;; left side
    '(((persp-name
        workspace-number
        window-number)
       :fallback evil-state
       :face highlight-face
       :priority 100)
      (purpose :priority 94)
      ((my-mode-icon :priority 79)
       ((all-the-icons-buffer-id remote-host
                                 buffer-size)
        :priority 98)
       ;; all-the-icons-modified
       modified
       )
      auto-compile
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 89)
      (minor-modes :priority 9)
      ((all-the-icons-vc-icon
        all-the-icons-vc-status
        (all-the-icons-git-ahead
         (all-the-icons-git-status
          :tight-right t)))
       :face 'mode-line)
      ((org-pomodoro :when active)
       (org-clock :when active))
      (anzu :face 'mode-line))
    ;; right side
    '((python-pyvenv :fallback python-pyenv)
      (selection-info :priority 95)
      input-method
      (global :when active)
      (column :priority 99)))
  (spaceline-toggle-buffer-size-off)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
