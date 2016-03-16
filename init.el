(require 'package)

(add-to-list 'package-archives
  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Initialize all the ELPA packages (what is installed using the packages commands)
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    clj-refactor

    ;; minor mode with midje
    midje-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; company-mode for autocompletion
    company

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; syntax checker
    flycheck

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    haskell-mode
    elm-mode
    rust-mode
    elixir-mode

    solarized-theme

    ;; https://github.com/rejeep/f.el
    f

    ;; https://github.com/Malabarba/let-alist
    let-alist

    ;; https://github.com/magnars/s.el
    s))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; exec path setup
(require 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; font
(set-face-attribute 'default nil :height 140)
(set-frame-font "Hack")

;; theme
(load-theme 'solarized-dark t)

;; general
(menu-bar-mode -1) ;; hide menu bar
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(global-linum-mode) ;; line numbers
(show-paren-mode 1) ;; highlights matching parens

(setq-default truncate-lines t) ;; disable truncate line

;; switch window with S-<arrow>
(windmove-default-keybindings)

;; paredit
(require 'paredit)

;; ido-mode
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; syntax checker
(add-hook 'after-init-hook #'global-flycheck-mode)

;; clojure-mode
(require 'clojure-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'subword-mode)

(require 'midje-mode)
(add-hook 'clojure-mode-hook 'midje-mode)

;; cider
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)

;;; cider autocompletion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(setq company-idle-delay nil) ; never start completions automatically
(global-set-key (kbd "M-TAB") #'company-complete) ; use meta+tab, aka C-M-i, as manual trigger

; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;;; clj-refactor
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; haskell-mode

; cabal install happy ghc-mod
(require 'haskell-mode)

; cabal install hasktags
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))

; cabal install stylish-haskell
; (custom-set-variables
;   '(haskell-process-suggest-remove-import-lines t)
;   '(haskell-process-auto-import-loaded-modules t)
;   '(haskell-process-log t))
; (eval-after-load 'haskell-mode '(progn
;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
;   (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
; (eval-after-load 'haskell-cabal '(progn
;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
; 
(custom-set-variables '(haskell-process-type 'cabal-repl))

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

; (eval-after-load 'haskell-mode
;  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

; ghc-mod
; (autoload 'ghc-init "ghc" nil t)
; (autoload 'ghc-debug "ghc" nil t)
; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(require 'elm-mode)
(require 'elixir-mode)

(provide 'init)
