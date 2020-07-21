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
              clojure-enable-sayid t
              clojure-enable-clj-refactor t
              clojure-enable-fancify-symbols t
              clojure-enable-linters '(clj-kondo)
              clojure-align-forms-automatically t)
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
     lispy
     lsp
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
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         doom-laserwave)
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
   dotspacemacs-helm-no-header nil
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
  (setq exwm-randr-workspace-monitor-plist '(0 "DP1" 1 "DP3" 2 "eDP1")))

(defun dotspacemacs/user-config ()
  (load "~/.spacemacs.d/secret-codes.el")

  (spaceline-toggle-hud-off)
  (global-set-key (kbd "<XF86MonBrightnessUp>") 'ignore)
  (global-set-key (kbd "<XF86MonBrightnessDown>") 'ignore)

  (setq-default which-key-idle-delay 3)
  (setq-default which-key-show-early-on-C-h t)

  (spacemacs/toggle-evil-visual-mark-mode-on)
  (setq-default evil-ex-search-vim-style-regexp t)
  (avy-setup-default)
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
  (global-set-key (kbd "M-n") 'avy-goto-char-2)

  (defun switch-to-buffer--hack (orig-fun &rest args)
    (if-let ((win (get-buffer-window (car args))))
        (select-window win)
      (apply orig-fun args)))

  (advice-add 'switch-to-buffer :around #'switch-to-buffer--hack)

  (defun spacemacs/home () nil)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-journal-find-file 'find-file)
  (add-hook 'persp-created-functions
            (lambda (persp hash)
              (spacemacs-buffer/goto-buffer)
              (org-journal-new-entry t)
              (persp-add-buffer
               (get-buffer (format-time-string "%Y-%m-%d.org"))
               persp)))

  (add-hook 'org-journal-after-header-create-hook
            (lambda ()
              (insert "\n\n* "
                      "Previous journal entry: "
                      "[[" (car (reverse (org-journal-list-files))) "]"
                      "[" (mapconcat 'int-to-string
                                     (car (reverse (org-journal-list-dates)))
                                     "-")
                      "]]")
              (org-agenda-file-to-front)
              (save-buffer)))

  (add-hook 'org-journal-after-entry-create-hook
            (lambda ()
              (save-excursion
                (beginning-of-line)
                (insert "\n"))
              (save-buffer)))

  (with-eval-after-load 'org
    (load-file "~/.spacemacs.d/private/org-habit-plus/org-habit-plus.el")
    (add-to-list 'org-modules 'org-habit-plus t))

  (require 'org-fc-hydra)
  (setq org-fc-directories '("~/org/"))

  (server-start)

  (fancy-battery-mode)

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
                (cond ((< 0 surplus) (dotimes (n surplus v)
                                       (exwm-workspace-delete)))
                      ((< surplus 0) (dotimes (n (* -1 surplus) v)
                                       (exwm-workspace-add)))
                      (t nil)))))

  (exwm-input-set-key
   (kbd "s-f")
   (lambda ()
     (interactive)
     (start-process-shell-command "qutebrowser" nil "qutebrowser")))
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

  (add-hooks '(org-mode-hook markdown-mode-hook ;; slack-mode-hook
                             )
             #'mixed-pitch-mode)

  (setq-default fill-column 80)
  (add-hooks '(prog-mode-hook text-mode-hook) #'auto-fill-mode)
  ;;(add-hooks '(prog-mode-hook text-mode-hook) #'fci-mode)
  (add-hook 'prog-mode-hook #'column-enforce-mode)
  (setq projectile-indexing-method 'hybrid)

  (spacemacs/declare-prefix "o" "custom")

  (setq-default treemacs-show-hidden-files nil
                treemacs-width 25)

  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)

  (add-hook 'prog-mode-hook #'rainbow-mode)

  (setq spaceline-org-clock-p t)

  (setq-default js-indent-level 2)

  ;; clojure setup
  (setq cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var)
        nrepl-sync-request-timeout 120
        cider-stacktrace-default-filters '(project)
        ;; cider-jdk-src-paths '("~/.java-src/java-11-openjdk"
        ;;                       "~/.java-src/java-8-openjdk")
        )

  (add-hook 'cider-inspector-mode-hook #'spacemacs/toggle-truncate-lines-on)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  (setq cljr-warn-on-eval nil
        cljr-hotload-dependencies t)

  (with-eval-after-load 'clojure-mode
    (define-clojure-indent
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
     (quote
      ("~/org/journal/2020-07-20.org" "~/org/journal/2020-07-17.org" "~/org/journal/2020-07-16.org" "~/org/journal/2020-07-15.org" "~/org/journal/2020-07-14.org" "~/org/journal/2020-07-13.org" "~/org/journal/2020-07-11.org" "~/org/journal/2020-07-10.org" "~/org/journal/2020-07-09.org" "~/org/journal/2020-07-08.org" "~/org/journal/2020-07-07.org" "~/org/journal/2020-07-06.org" "~/org/journal/2020-07-02.org" "~/org/journal/2020-07-01.org" "~/org/journal/2020-06-30.org" "~/org/journal/2020-06-29.org" "~/org/journal/2020-06-26.org" "~/org/journal/2020-06-25.org" "~/org/journal/2020-06-24.org" "~/org/journal/2020-06-23.org" "~/org/journal/2020-06-22.org" "~/org/journal/2020-06-20.org" "~/org/journal/2020-06-19.org" "~/org/journal/2020-06-18.org" "~/org/journal/2020-06-17.org")))
   '(package-selected-packages
     (quote
      (yasnippet-snippets vterm terminal-here slack scad-mode org-superstar org-roam-bibtex org-roam emacsql-sqlite3 org-journal org-brain modus-vivendi-theme modus-operandi-theme magit-section lsp-ui lsp-python-ms counsel ivy kaolin-themes helm-lsp forge ghub closql flycheck-bashate doom-themes dap-mode posframe lsp-treemacs treemacs package-lint lsp-mode restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well org-fc ranger company-terraform terraform-mode hcl-mode company-shell company-reftex company-quickhelp company-emoji company-ansible command-log-mode color-identifiers-mode chocolate-theme cargo rust-mode browse-at-remote blacken auctex-latexmk attrap arduino-mode ansible-doc ansible vmd-mode systemd dockerfile-mode docker tablist docker-tramp adoc-mode markup-faces sql-indent company-auctex auctex web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern tern coffee-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic tidal yaml-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data mu4e-maildirs-extension mu4e-alert ht xterm-color shell-pop multi-term mmm-mode markdown-toc markdown-mode gh-md eshell-z eshell-prompt-extras esh-help zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download magit-gitflow magit-popup htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit transient git-commit with-editor diff-hl company-statistics company clojure-snippets auto-yasnippet auto-dictionary ac-ispell auto-complete lispy clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
   '(safe-local-variable-values
     (quote
      ((clojurescript-mode
        (cider-clojure-cli-global-options . "-A:fig"))
       (javascript-backend . tide)
       (javascript-backend . tern)
       (javascript-backend . lsp))))
   '(send-mail-function (quote mailclient-send-it))))
