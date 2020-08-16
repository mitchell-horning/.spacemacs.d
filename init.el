;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   dotspacemacs-configuration-layers
   '(ansible
     asciidoc
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (clojure :variables
              clojure-enable-sayid nil
              clojure-enable-clj-refactor t
              clojure-enable-fancify-symbols t
              clojure-enable-linters '(clj-kondo)
              clojure-align-forms-automatically t
              clojure-indent-style 'align-arguments)
     (colors :variables colors-colorize-identifiers 'variables)
     command-log
     csv
     (docker :variables docker-dockerfile-backend 'lsp)
     (elfeed :variables
             rmh-elfeed-org-files '("~/org/feeds/feeds.org"
                                    "~/org/feeds/personal-feeds.org"))
     emacs-lisp
     ;; (evil-snipe :variables
     ;;             evil-snipe-enable-alternate-f-and-t-behaviors t)
     (exwm :variables
           exwm-workspace-display-echo-area-timeout 10

           exwm-workspace-number
           (string-to-number
            (shell-command-to-string "xrandr|grep \" connected\"|wc -l"))

           exwm-enable-systray t
           exwm-autostart-xdg-applications nil
           exwm-locking-command "xss-lock -l -- ~/bin/transfer-sleep-lock-generic-delay.sh"
           exwm-install-logind-lock-handler t
           exwm-terminal-command "termite"
           ;; exwm-custom-init (lambda() (exwm/autostart-process "Dunst OSD" "dunst"))
           )
     git
     github
     haskell
     (helm :variables
           helm-enable-auto-resize t
           hybrid-style-enable-hjkl-bindings t
           helm-mini-default-sources '(helm-source-buffers-list
                                       helm-source-buffer-not-found)
           helm-buffer-max-length nil)
     helpful
     html
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (javascript :variables javascript-backend 'lsp)
     kubernetes
     latex
     ;; lispy
     (lsp :variables
          lsp-enable-indentation nil
          lsp-enable-symbol-highlighting nil
          lsp-ui-doc-delay 5
          lsp-ui-doc-use-childframe t
          ;; lsp-ui-doc-position 'top
          lsp-ui-sideline-show-code-actions nil
          lsp-keymap-prefix "C-c C-l")
     major-modes
     (multiple-cursors :variables multiple-cursors-backend 'mc)
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :variables
          org-startup-folded nil
          org-agenda-files '("~/org/" "~/org/journal" "~/org/personal/")
          org-agenda-file-regexp "\\`[^.].*\\.org$\\'"
          org-complete-tags-always-offer-all-agenda-tags t
          org-confirm-babel-evaluate nil
          org-enable-org-journal-support t
          org-journal-dir "~/org/journal/"
          org-journal-date-prefix "#+TITLE: "
          org-journal-time-prefix "* "
          org-journal-file-format "%Y-%m-%d.org"
          org-journal-time-format "[%F %R]"
          org-enable-github-support t)
     org-roam
     (python :variables python-backend 'lsp python-lsp-server 'pyls)
     ranger
     (restclient :variables restclient-use-org t)
     (rust :variables
           rust-format-on-save t
           lsp-rust-server 'rust-analyzer)
     selectric
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'vterm)
     (shell-scripts :variables shell-scripts-backend 'lsp)
     ;; slack
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove 'kill-weak)
     spacemacs-purpose
     spell-checking
     (sql :variables
          sql-capitalize-keywords t
          sqlfmt-executable "pg_format"
          sqlfmt-options '("--wrap-limit" "80"))
     syntax-checking
     systemd
     terraform
     themes-megapack
     (treemacs :variables
               treemacs-use-scope-type 'Perspectives
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-git-mode 'deffered)
     version-control
     xclipboard
     yaml)

   dotspacemacs-additional-packages
   '((org-fc
      :location (recipe :fetcher github
                        :repo "l3kn/org-fc"
                        :files (:defaults "awk" "demo.org")))
     (evil-adjust :location (recipe :fetcher github :repo "troyp/evil-adjust"))
     kaocha-runner
     inf-clojure
     edbi
     org-roam
     mixed-pitch
     ;; (clj-refactor :location
     ;;               "~/projects/clj-refactor.el/")
     eterm-256color)

   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(bookmarks
                                (projects . 7)
                                (recents . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes '(doom-fairy-floss
                         doom-solarized-dark
                         doom-solarized-light
                         doom-laserwave
                         doom-city-lights
                         doom-manegarm)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-default-font '("SauceCodePro Nerd Font"
                               :size 8.0
                               :weight normal
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-SPC"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "s-,"
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for sekparate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  (defun custom-save-faces () (message "attempt to save face"))
  (require 'dbus)
  (setq confirm-kill-processes nil)
  (setq kill-buffer-query-funcrtions nil)
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setq exwm-randr-workspace-monitor-plist '(0 "DP1" 1 "DP3" 2 "eDP1")))

(defun dotspacemacs/user-config ()

  (add-to-list 'purpose-user-mode-purposes '(exwm-mode . gui))
  (add-to-list 'purpose-user-mode-purposes '(help-mode . debug))
  (add-to-list 'purpose-user-mode-purposes '(helpful-mode . debug))
  (add-to-list 'purpose-user-mode-purposes '(cider-repl-mode . debug))
  (add-to-list 'purpose-user-regexp-purposes
               '("^X:Chromium/DevTools.*$" . debug))
  (add-to-list 'purpose-user-regexp-purposes
               '("^X:Chromium/re-frame-10x.*$" . debug))
  (purpose-compile-user-configuration)

  (require 'evil-adjust)
  (evil-adjust)

  (setq-default evil-cleverparens-use-s-and-S nil)
  (setq-default evil-cleverparens-use-regular-insert nil)
  (setq-default evil-cleverparens-swap-move-by-word-and-symbol t)
  (load "~/.spacemacs.d/secret-codes.el")

  (spacemacs/toggle-desktop-environment-on)
  (spaceline-toggle-hud-off)

  (spacemacs/toggle-vi-tilde-fringe-off)

  (setq-default which-key-idle-delay 2)
  (setq-default which-key-show-early-on-C-h t)

  (spacemacs/toggle-evil-visual-mark-mode-on)
  (setq-default evil-ex-search-vim-style-regexp t)
  (avy-setup-default)
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-char)
  (global-set-key (kbd "M-n") 'avy-goto-char-2)

  (defun switch-to-buffer--hack (orig-fun &rest args)
    (if-let ((win (get-buffer-window (car args))))
        (select-window win)
      (apply orig-fun args)))

  (advice-add 'switch-to-buffer :around #'switch-to-buffer--hack)

  (defun spacemacs/home () nil)
  (add-hook 'org-mode-hook 'org-indent-mode)
  ;; (setq org-journal-find-file 'find-file)
  ;; (add-hook 'persp-created-functions
  ;;           (lambda (persp hash)
  ;;             (spacemacs-buffer/goto-buffer)
  ;;             (org-journal-new-entry t)
  ;;             (persp-add-buffer
  ;;              (get-buffer (format-time-string "%Y-%m-%d.org"))
  ;;              persp)))

  ;; (add-hook 'org-journal-after-header-create-hook
  ;;           (lambda ()
  ;;             (insert "\n\n* "
  ;;                     "Previous journal entry: "
  ;;                     "[[" (car (reverse (org-journal-list-files))) "]"
  ;;                     "[" (mapconcat 'int-to-string
  ;;                                    (car (reverse (org-journal-list-dates)))
  ;;                                    "-")
  ;;                     "]]")
  ;;             (org-agenda-file-to-front)
  ;;             (save-buffer)))

  ;; (add-hook 'org-journal-after-entry-create-hook
  ;;           (lambda ()
  ;;             (save-excursion
  ;;               (beginning-of-line)
  ;;               (insert "\n"))
  ;;             (save-buffer)))
  (setq browse-url-mosaic-program nil)

  (with-eval-after-load 'org
    (load-file "~/.spacemacs.d/private/org-habit-plus/org-habit-plus.el")
    (add-to-list 'org-modules 'org-habit-plus t))

  (require 'org-fc-hydra)
  (setq org-fc-directories '("~/org/"))

  (server-start)

  ;; (fancy-battery-mode)

  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (string-prefix-p exwm-buffer-name-prefix
                                     (buffer-name (first (buffer-list))))
                (setq exwm-input-line-mode-passthrough nil))))

  (require 'exwm-randr)
  (exwm-randr-enable)

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "arandr" "--change")
              (let ((surplus (- (exwm-workspace--count)
                                (string-to-number
                                 (shell-command-to-string
                                  "xrandr|grep \" connected\"|wc -l")))))
                (cond ((< 0 surplus) (dotimes (n surplus)
                                       (exwm-workspace-delete)))
                      ((< surplus 0) (dotimes (n (* -1 surplus))
                                       (exwm-workspace-add)))
                      (t nil)))))

  (exwm-input-set-key
   (kbd "s-f")
   (lambda ()
     (interactive)
     (start-process-shell-command "qutebrowser" nil "qutebrowser")))
  (exwm-input-set-key
   (kbd "s-F")
   (lambda ()
     (interactive)
     (start-process-shell-command "chromium" nil "chromium")))
  (exwm-input-set-key
   (kbd "s-C-F")
   (lambda ()
     (interactive)
     (start-process-shell-command "firefox" nil "firefox")))
  (exwm-input-set-key
   (kbd "s-s")
   (lambda ()
     (interactive)
     (start-process-shell-command "slack" nil "slack")))
  (exwm-input-set-key
   (kbd "s-p")
   (lambda ()
     (interactive)
     (start-process-shell-command "mixer" nil "termite -e pamix")))
  ;; (exwm-input-set-key (kbd "<s-return>") #'vterm)
  ;; (add-hook 'term-mode-hook #'eterm-256color-mode)

  (defun add-hooks (hooks functions)
    (cond ((not functions) nil)
          ((consp functions)
           (dolist (fun functions)
             (add-hooks hooks fun)))
          (t
           (dolist (hook hooks)
             (add-hook hook functions)))))

  (add-hooks '(org-mode-hook
               markdown-mode-hook ;; slack-mode-hook
               treemacs-mode-hook)
             #'mixed-pitch-mode)

  (setq-default fill-column 80)
  (add-hooks '(prog-mode-hook text-mode-hook) #'auto-fill-mode)
  ;; (add-hook 'prog-mode-hook #'fci-mode)
  (add-hook 'prog-mode-hook #'column-enforce-mode)
  (setq projectile-indexing-method 'hybrid)

  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  (spacemacs/declare-prefix "o" "custom")

  (setq-default treemacs-show-hidden-files nil
                treemacs-width 25)

  ;; (set-face-attribute 'treemacs-root-face t :font "Source Sans Pro")

  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)

  (add-hook 'prog-mode-hook #'rainbow-mode)

  (setq spaceline-org-clock-p t)

  (setq-default js-indent-level 2)

  ;; clojure setup
  (use-package lsp-mode
    :ensure t
    :hook ((clojure-mode . lsp)
           (clojurec-mode . lsp)
           (clojurescript-mode . lsp))
    :custom (lsp-enable-indentation nil)
    :commands lsp
    :config
    (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
      (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

  (setq cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var)
        nrepl-sync-request-timeout 120
        cider-stacktrace-default-filters '(project)
        ;; cider-jdk-src-paths '("~/.java-src/java-11-openjdk"
        ;;                       "~/.java-src/java-8-openjdk")
        )

  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-allow-jack-in-without-project t)
  ;; (setq cider-clojure-cli-global-options "-A:scratch")

  (add-hook 'cider-inspector-mode-hook #'spacemacs/toggle-truncate-lines-on)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  (setq cljr-warn-on-eval nil
        cljr-hotload-dependencies t)

  (with-eval-after-load 'clojure-mode
    (define-clojure-indent
      (prop/for-all 1)
      (rf/reg-event-fx 'defun)
      (rf/reg-event-db 'defun)
      (rf/reg-sub 'defun)
      (fn-traced 1)
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (OPTIONS 2)
      (PATCH 2)
      (rfn 2)
      (let-routes 1)
      (context 2)))

  (defun cider--guess-evaluation-context ()
    "returns list of let-binding strings from the inside out, without closing parens
     \"(let [...]\""
    (save-excursion
      (let ((res ()))
        (condition-case er
            (while t
              (backward-up-list)
              (when (looking-at (rx "(" (or "when-let" "if-let" "let") (opt "*")
                                    symbol-end (* space)
                                    (group "["))) ;; binding vector
                (let ((beg (match-beginning 0))
                      (end (save-excursion
                             (goto-char (match-beginning 1))
                             (forward-sexp 1)
                             (point))))
                  (push (buffer-substring-no-properties beg end) res))))
          (scan-error res)))))

  (defun cider-eval-dwim ()
    (interactive)
    (let ((ctx (cider--guess-evaluation-context))
          (bounds (cider-sexp-at-point 'bounds)))
      (cider-interactive-eval (concat (apply #'concat ctx)
                                      (buffer-substring-no-properties (car bounds) (cadr bounds))
                                      (make-string (length ctx) ?\)))
                              nil bounds
                              (cider--nrepl-pr-request-map))))

  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "tkn" 'kaocha-runner-run-tests
    "tkt" 'kaocha-runner-run-test-at-point
    "tka" 'kaocha-runner-run-all-tests
    "tks" 'kaocha-runner-show-warnings
    "tkh" 'kaocha-runner-hide-windows
    "ta" 'cider-test-run-project-tests
    "tn" 'cider-test-run-ns-tests
    "tt" 'cider-test-run-test
    "oe;"
    (lambda ()
      (interactive)
      (evil-insert-state)
      (forward-char)
      (newline)
      (cider-pprint-eval-last-sexp-to-comment)
      (evil-normal-state))
    "oec" 'cider-eval-dwim
    "oei" 'eval-sexp-fu-cider-eval-sexp-inner-list
    "oi" 'cider-inspect-last-result
    "os" 'cider-selector)

  (with-eval-after-load "cider-inspector"
    (define-key cider-inspector-mode-map
      (kbd "f") 'ace-link-cider-inspector))

  ;; rust setup
  ;; see https://github.com/kwrooijen/cargo.el/issues/29 for more info
  (with-eval-after-load 'rust-mode
    (define-key rust-mode-map (kbd "C-q") 'my-cargo-run))

  (defun my-cargo-run ()
    "Build and run Rust code."
    (interactive)
    (cargo-process-run)
    (let ((orig-win (selected-window))
          (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible)))
      (select-window run-win)
      (comint-mode)
      (read-only-mode 0)
      (end-of-buffer)))

  ;; haskell setup
  (with-eval-after-load "haskell-mode"
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))

    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))

    (evil-define-key 'normal haskell-mode-map "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(evil-want-Y-yank-to-eol t)
   '(org-agenda-files
     '("~/org/journal/2020-08-10.org" "~/org/journal/2020-08-07.org" "~/org/journal/2020-08-06.org" "~/org/journal/2020-08-05.org" "~/org/journal/2020-08-04.org" "~/org/journal/2020-08-03.org" "~/org/journal/2020-08-02.org" "~/org/journal/2020-08-01.org" "~/org/journal/2020-07-31.org" "~/org/journal/2020-07-30.org" "~/org/journal/2020-07-29.org" "~/org/journal/2020-07-28.org" "~/org/journal/2020-07-27.org" "~/org/journal/2020-07-26.org" "~/org/journal/2020-07-24.org"))
   '(package-selected-packages
     '(evil-adjust kaocha-runner exwm xelb exotica-theme evil-org evil-magit magit git-commit with-editor eterm-256color xterm-color espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elfeed-org elfeed-goodies ace-jump-mode noflet elfeed edbi epc ctable concurrent deferred ebuild-mode dracula-theme doom-themes dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat django-theme desktop-environment darktooth-theme darkokai-theme darkmine-theme darkburn-theme dap-mode posframe lsp-treemacs bui lsp-mode dash-functional dante lcr dakrone-theme cython-mode cyberpunk-theme csv-mode company-web web-completion-data company-restclient restclient know-your-http-well ws-butler writeroom-mode winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-persp treemacs-icons-dired treemacs-evil toc-org symon symbol-overlay string-inflection spaceline-all-the-icons restart-emacs request rainbow-delimiters popwin pcre2el password-generator paradox overseer org-superstar org-bullets open-junk-file nameless move-text macrostep lorem-ipsum link-hint indent-guide hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-ls-git helm-flx helm-descbinds helm-ag google-translate golden-ratio font-lock+ flycheck-package flycheck-elsa flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu emr elisp-slime-nav editorconfig dumb-jump dotenv-mode diminish devdocs define-word company-terraform company-statistics company-shell company-reftex company-quickhelp company-ghci company-ghc company-emoji company-cabal company-auctex company-ansible company-anaconda command-log-mode column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode cmm-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chocolate-theme cherry-blossom-theme centered-cursor-mode cargo busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk attrap arduino-mode apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme ample-theme alert alect-themes aggressive-indent afternoon-theme adoc-mode ace-link ace-jump-helm-line ac-ispell))
   '(safe-local-variable-values
     '((lsp-file-watch-ignored "\\.git$" "resources" "target" "/dist$" "/log$")
       (projectile-project-root . "~/projects/professional/VA-Fix/ui")
       (projectile-project-root "~/projects/professional/VA-Fix/ui")
       (lsp-file-watch-ignored "\\.git$" "resources/public/js$" "target$" "dist$" "log$")
       (lsp-file-watch-ignored "[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]resources/public/js$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$")
       (cider-clojure-cli-global-options nil)
       (javascript-backend . tide)
       (javascript-backend . tern)
       (javascript-backend . lsp))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
