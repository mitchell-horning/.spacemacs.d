(defun exwm-input-set-keys (l)
  (dolist (a l)
    (exwm-input-set-key (eval (car a)) (eval (cdr a)))))

(defun browser-dev ()
  (interactive)
  (exwm/run-program-in-home "chromium http://localhost:9500")
  (run-at-time
   "1.5 sec" nil
   (lambda ()
     (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
     (exwm-input--fake-key (aref (kbd "C-S-i") 0))
     (exwm/app-launcher
      "chromium http://localhost:9500/figwheel-extra-main/auto-testing")))
  (purpose-load-window-layout "e|(b-d)"))

(use-package exwm
  :config
  (progn (exwm-input-set-keys
          '(((kbd "s-f") . (lambda () (interactive) (exwm/app-launcher "qutebrowser")))
            ((kbd "s-F") . (lambda () (interactive) (exwm/app-launcher "chromium")))
            ((kbd "s-C-F") . (lambda () (interactive) (exwm/app-launcher "firefox")))
            ((kbd "s-s") . (lambda () (interactive) (exwm/app-launcher "slack")))
            ((kbd "s-p") . (lambda () (interactive) (exwm/app-launcher "pavucontrol")))
            ((kbd "<M-return>") . #'projectile-run-vterm)
            ((kbd "<s-return>") . (lambda ()
                                    (interactive)
                                    (projectile-run-vterm t))))))
  :custom
  (mouse-autoselect-window t)
  (focus-follows-mouse t)
  (exwm-randr-workspace-monitor-plist '(0 "DP1" 1 "DP3" 2 "eDP1")))

(use-package window-purpose
  :config
  (progn (purpose-compile-user-configuration)
         (spacemacs/transient-state-register-add-bindings 'layouts
           '(("f" purpose-load-window-layout :exit t))))
  :custom
  (purpose-use-built-in-layouts nil)
  (purpose-user-mode-purposes
   '((magit-status-mode . debug)
     (vterm-mode . debug)
     (cider-repl-mode . debug)
     (helpful-mode . debug)
     (help-mode . debug)
     (exwm-mode . gui)))
  (purpose-user-regexp-purposes
   '(("^X:Zoom.*" . zoom)
     ("^X:Thunderbird.*" . mail)
     ("^X:discord.*" . discord)
     ("^X:Slack.*" . slack)
     ("^X:qutebrowser.*$" . browser)
     ("^X:firefox.*$" . browser)
     ("^X:Chromium/[^rD].*$" . browser)
     ("^X:Chromium/\\(re-frame-10x\\|DevTools\\).*$" . debug))))
