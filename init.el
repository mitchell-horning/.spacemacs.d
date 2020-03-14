;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   dotspacemacs-configuration-layers
   '(git
     version-control
     (docker :variables docker-dockerfile-backend 'lsp)
     kubernetes

     (markdown :variables markdown-live-preview-engine 'vmd)

     yaml

     html
     asciidoc
     (org :variables
          org-enable-github-support t
          org-projectile-file "TODOs.org"
          org-enable-org-journal-support t
          org-enable-sticky-header t)
     latex

     helpful
     (helm :variables
           helm-enable-auto-resize t
           hybrid-style-enable-hjkl-bindings t)
     (treemacs :variables
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-git-mode 'deffered)
     (shell :variables
            ;; shell-default-term 'term
            shell-default-height 30
            shell-default-position 'bottom)
     ranger
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)

     auto-completion
     spell-checking
     syntax-checking
     lsp

     xclipboard

     lispy
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)

     (clojure :variables
              clojure-enable-sayid t
              clojure-enable-clj-refactor t
              clojure-enable-fancify-symbols t
              clojure-enable-linters '(clj-kondo joker))
     emacs-lisp
     haskell
     javascript
     (python :variables python-backend 'lsp python-lsp-server 'pyls)
     major-modes

     (sql :variables sql-capitalize-keywords t)

     systemd

     slack

     command-log

     themes-megapack
     selectric
     (colors :variables colors-colorize-identifiers 'variables))
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '(bookmarks
                                (projects . 7)
                                (recents . 5))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   dotspacemacs-themes '(solarized-light
                         solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-default-font '("SauceCodePro Nerd Font"
                               :size 8.0
                               :weight normal
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
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

(defun dotspacemacs/user-init ())

(defun dotspacemacs/user-config ()
  ;; misc setup
  (evil-leader/set-key "q q" 'spacemacs/frame-killer)

  (spacemacs/declare-prefix "o" "custom")

  (setq powerline-default-separator 'arrow)

  (setq-default treemacs-show-hidden-files nil)

  (global-aggressive-indent-mode 1)

  (add-hook 'prog-mode-hook #'rainbow-mode)

  ;; org mode
  (with-eval-after-load 'org-agenda
    (require 'org-projectile)
    (with-eval-after-load 'org-agenda
      (require 'org-projectile)
      (mapcar '(lambda (file)
                 (when (file-exists-p file)
                   (push file org-agenda-files)))
              (org-projectile-todo-files))))

  (setq org-journal-dir "~/org/journal/"
        org-journal-file-format "%Y-%m-%d")

  (setq spaceline-org-clock-p t)

  ;; clojure setup
  (setq cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var)
        nrepl-sync-request-timeout 120
        cider-stacktrace-default-filters '(project)
        cider-jdk-src-paths '("~/.java-src/java-11-openjdk"
                              "~/.java-src/java-8-openjdk"))

  (setq cljr-warn-on-eval nil
        cljr-hotload-dependencies t)

  (eval-after-load 'clojure-mode
    '(define-clojure-indent
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

  (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
    "oec"
    (lambda ()
      (interactive)
      (evil-insert-state)
      (forward-char)
      (newline)
      (cider-pprint-eval-last-sexp-to-comment)
      (evil-normal-state))
    "oei"
    'eval-sexp-fu-cider-eval-sexp-inner-list)

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
      "O" 'haskell-evil-open-above))

  ;; slack
  (setq slack-buffer-emojify t
        slack-prefer-current-team t)
  (slack-register-team
   :name "ATA emacs-slack"
   :default t
   :client-id "mhorning@ata-llc.com"
   :client-secret (auth-source-pick-first-password
                   :host '("slack-emacs")
                   :user "secret"
                   :type 'netrc :max 1)
   :token (auth-source-pick-first-password
           :host '("slack-emacs")
           :user "token"
           :type 'netrc :max 1)
   :subscribed-channels '(articles
                          clojure
                          databases
                          dhs-badging
                          dhs-sosoa
                          elixer
                          emacs
                          full-stack-data-science
                          general
                          happyhourtrain
                          mandalorians
                          mda-watchtower
                          mob_programming
                          pa-access
                          pa-analytics
                          pa-collect_ingest
                          pa-data_architecture
                          pa-infrastructure
                          pa-measurement
                          pa-reqs
                          pa-strategy
                          pa-vis
                          plusone
                          random
                          wayfinders)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (vmd-mode systemd dockerfile-mode docker tablist docker-tramp adoc-mode markup-faces sql-indent company-auctex auctex web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern tern coffee-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic tidal yaml-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data mu4e-maildirs-extension mu4e-alert ht xterm-color shell-pop multi-term mmm-mode markdown-toc markdown-mode gh-md eshell-z eshell-prompt-extras esh-help zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download magit-gitflow magit-popup htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit transient git-commit with-editor diff-hl company-statistics company clojure-snippets auto-yasnippet auto-dictionary ac-ispell auto-complete lispy clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
   '(lispy-compat (quote (edebug cider magit-blame-mode)))
   '(lispy-eval-display-style (quote overlay))
   '(package-selected-packages
     (quote
      (sqlup-mode vmd-mode systemd dockerfile-mode docker tablist docker-tramp adoc-mode markup-faces sql-indent company-auctex auctex web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern tern coffee-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic tidal yaml-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data mu4e-maildirs-extension mu4e-alert ht xterm-color shell-pop multi-term mmm-mode markdown-toc markdown-mode gh-md eshell-z eshell-prompt-extras esh-help zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme smeargle orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download magit-gitflow magit-popup htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit transient git-commit with-editor diff-hl company-statistics company clojure-snippets auto-yasnippet auto-dictionary ac-ispell auto-complete lispy clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider sesman queue parseedn clojure-mode parseclj a ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
   '(send-mail-function (quote mailclient-send-it)))
  )
