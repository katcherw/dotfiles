;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/
; Bill's emacs initialization file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

; package support. install new package with list-packages, then mark
; with i and press x
(require 'use-package)
(package-initialize)
(add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/")
    t)

(setq use-package-always-ensure t)

; Some packages like consult require lexical bindings
(setq lexical-binding t)

 ; evil mode 
(use-package evil
    :defer t
    :init
    (setq evil-want-keybinding nil)
    (setq evil-toggle-key "C-x C-z")
    (setq evil-insert-state-cursor '(bar "yellow"))
    (evil-mode t)

    ; makes evil treat underscore as part of word
    (add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

    :config
    (evil-set-leader 'normal (kbd "SPC"))
    (evil-set-initial-state 'speedbar-mode 'emacs)
    (evil-set-initial-state 'rustic-popup-mode 'emacs))

(use-package evil-collection
    :after evil
    :config
    (setq evil-collection-key-blacklist '("SPC"))
    (setq evil-collection-mode-list (remove 'calc evil-collection-mode-list))
    (evil-collection-init))

; use jj to leave insert mode - much easier than esc
(use-package key-chord
    :defer t
    :init
    (setq key-chord-two-keys-delay 1.0)
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

;; column indicator draws line at fill-column-mode
(use-package fill-column-indicator
    :defer t
    :init
    (add-hook 'c-mode-hook 'fci-mode)
    (add-hook 'c++-mode-hook 'fci-mode)
    (setq fci-rule-color "#444444"))

; Completion user interface
(use-package vertico
    :defer t
    :init
    (vertico-mode))

; Makes vertico use fuzzy matching
(use-package orderless
    :defer t
    :custom
    (orderless-matching-styles '(orderless-flex))
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))

; Gives enhanced output to verico
(use-package consult
    :defer t
    :bind 
    ("M-y" . consult-yank-pop))

; ui for completion-at-point (M-tab)
(use-package corfu
    :defer t
    :init
    (global-corfu-mode))

; compiler warnings on screen in real time
(use-package flycheck
    :defer t)

;; (setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs 
;;     projectile hydra flycheck company avy which-key dap-mode))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))

(setq read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.2
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

(use-package which-key
    :defer t
    :init
    (which-key-mode))

(use-package doom-themes
    :config
    (load-theme 'doom-tokyo-night t))

;; Much nicer mode line
(use-package doom-modeline
    :config
    ; don't always want to use nerd fonts in terminal mode
    (if (not (display-graphic-p))
        (setq doom-modeline-icon nil))
    (doom-modeline-mode))
    ;:hook (after-init . doom-modeline-mode))

(use-package xcscope
    :defer t
    :init
    (cscope-setup)
    (setq cscope-dateabase-regexps '~))

;; rust
(use-package rustic
    :defer t)

(use-package magit
    :defer t)

(use-package olivetti
    :defer t)

(use-package org-modern
    :config
    (with-eval-after-load 'org (global-org-modern-mode)))

;; eglot
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rustic-mode-hook 'eglot-ensure)

;; don't keep resizing echo area
(setq eldoc-echo-area-use-multiline-p nil)

;; use ibuffer instead of list-buffers
(if (>= emacs-major-version 22) 
    (defalias 'list-buffers 'ibuffer))

;; Allow copy and paste to work properly in kde
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;; Turn on font-lock mode
(global-font-lock-mode t)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Disable the menu bar to increase usable space
(menu-bar-mode 0)

;; put gdb in a graphical mode
(setq gdb-many-windows t)

;; no tool bar
(tool-bar-mode 0)

;; Sometimes we need this to get backspace to work properly
;(if (eq window-system nil)
;(normal-erase-is-backspace-mode nil)
;)

;; I find a blinking cursor annoying
(blink-cursor-mode 0)

;; Change the grep defaults
;(setq grep-command "grep -nr "
(setq grep-command "rg --no-heading "
      grep-use-null-device nil
      grep-window-height 20)

;; gets ANSI colors to work right in shell
;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Compiling defaults
(setq compile-command "cd ~/dev/libsdwan/scripts/build; ./build-apple.sh macos"
      compilation-window-height 20)

;; Wrap lines in compilation window
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook) 

;; alias y to yes and n to no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Pgup/dn will return exactly to the starting point.
(setq scroll-preserve-screen-position 1)

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; resize the mini-buffer when necessary
(setq resize-minibuffer-mode t)

(setq-default fill-column 80)

;; turn on line numbers
(global-display-line-numbers-mode)

;; disable scroll bar
;(toggle-scroll-bar -1)

;; turn off the bell
(setq ring-bell-function 'ignore)

; bookmarks should always be saved
(setq bookmark-save-flag 1)

;; recent files
(recentf-mode 1)

;; undo window changes with C-c left and C-c right
(setq winner-mode 1)

; stop making so many dired buffers
(setf dired-kill-when-opening-new-dired-buffer t)

;; put directories first in dired
;(setq dired-listing-switches "-lh --group-directories-first")

;; Avoid garbage characters in compilation window due to g++ outputting color
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
        (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;(setq shell-file-name "c:/apps/cygwin64/bin/bash.exe")
;(setq explicit-shell-file-name "c:/apps/cygwin64/bin/bash.exe")

;; set font
;(set-frame-font "Inconsolata Nerd Font 16" nil t)
;(set-frame-font "Inconsolata Regular 14" nil t)
;(set-frame-font ":family Inconsolata-18 :weight Regular" nil t)
(set-frame-font "Inconsolata:pixelsize=16:weight=regular:antialias=subpixel" nil t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asm-comment-char 59)
 '(backup-directory-alist '(("." . "~/backups")))
 '(c-basic-offset 4)
 '(c-tab-always-indent nil)
 '(c-ts-mode-indent-offset 4)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(custom-safe-themes
      '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "2f8eadc12bf60b581674a41ddc319a40ed373dd4a7c577933acaff15d2bf7cc6" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(delete-old-versions 'other)
 '(ediff-keep-variants nil)
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold 100000000)
 '(package-selected-packages
      '(lsp-mode org-modern olivetti magit corfu rustic treesit mood-line consult vertico key-chord spacemacs-theme spaceline fill-column-indicator evil doom-themes doom-modeline))
 '(perl-tab-always-indent nil)
 '(scroll-conservatively 999)
 '(speedbar-tag-hierarchy-method nil)
 '(tab-always-indent nil)
 '(tab-width 4)
 '(tags-case-fold-search nil)
 '(truncate-lines t)
 '(version-control t)
 '(warning-suppress-types '((use-package)))
 '(which-function-mode t))


;; Disable nag screen
(setq inhibit-startup-message t)

;; file types
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; timestamps on done entries
(setq org-log-done t)

;; python indentation
(add-hook 'python-mode-hook
    (lambda ()
        (setq tab-width 4)
        (setq indent-tabs-mode t)
        (setq python-indent-offset 4)))

  (setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
      (c . ("https://github.com/tree-sitter/tree-sitter-c"))
      (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
      (html . ("https://github.com/tree-sitter/tree-sitter-html"))
      (make . ("https://github.com/alemuller/tree-sitter-make"))
      (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
      (python . ("https://github.com/tree-sitter/tree-sitter-python"))
      (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
      (go . ("https://github.com/tree-sitter/tree-sitter-go"))))

;; Tree-sitter
;; (use-package treesit
;;  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
;;  :init
;;   (setq treesit-language-source-alist
;;       '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
;;       (c . ("https://github.com/tree-sitter/tree-sitter-c"))
;;       (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;       (html . ("https://github.com/tree-sitter/tree-sitter-html"))
;;       (make . ("https://github.com/alemuller/tree-sitter-make"))
;;       (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
;;       (python . ("https://github.com/tree-sitter/tree-sitter-python"))
;;       (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
;;       (go . ("https://github.com/smacker/go-tree-sitter"))))

;;     ; use tree sitter modes instead of normal major modes
;;     (setq major-mode-remap-alist
;;         '((c++-mode . c++-ts-mode)
;;         (c-mode . c-ts-mode)
;;         (c-or-c++-mode . c-or-c++-ts-mode)
;;         (rust-mode . rust-ts-mode)
;;         (python-mode . python-ts-mode)))
;;  :config
;;     (defun nf/treesit-install-all-languages ()
;;         "Install all languages specified by `treesit-language-source-alist'."
;;         (interactive)
;;         (let ((languages (mapcar 'car treesit-language-source-alist)))
;;         (dolist (lang languages)
;;             (treesit-install-language-grammar lang)
;;             (message "`%s' parser was installed." lang)
;;             (sit-for 0.75))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert functions
(global-set-key "\C-xg" 'g-at-point)
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "M-o") 'next-multiframe-window)
(global-set-key (kbd "C-M-O") 'evil-window-rotate-downwards)
;(global-set-key (kbd "\C-xo") 'next-multiframe-window)

(evil-define-key 'normal 'global (kbd "<leader>g") '("grep symbol" . g-at-point))
(evil-define-key 'normal 'global (kbd "<leader>b") '("buffer next" . switch-to-buffer))
(evil-define-key 'normal 'global (kbd "<leader>o") '("other window" . next-multiframe-window))
(evil-define-key 'normal 'global (kbd "<leader>w") '("other window" . next-multiframe-window))
(evil-define-key 'normal 'global (kbd "<leader>k") '("kill buffer" . kill-buffer))
(evil-define-key 'normal 'global (kbd "<leader>t") '("toggle line wrap" . toggle-truncate-lines))
(evil-define-key 'normal 'global (kbd "<leader>r") '("find references" . xref-find-references))
(evil-define-key 'normal 'global (kbd "<leader>q") '("quick fix" . eglog-code-action-quickfix))
(evil-define-key 'normal 'global (kbd "<leader>d") '("documentation" . eldoc-print-current-symbol-info))
(evil-define-key 'normal 'global (kbd "<leader>m") '("magit status" . magit-status))
;(evil-define-key 'normal 'global (kbd "<leader>ff") '("find files" . helm-find))
(evil-define-key 'normal 'global (kbd "<leader>0") '("delete window" . delete-window))
(evil-define-key 'normal 'global (kbd "<leader>1") '("delete others" . delete-other-windows))

;; (eval-after-load "dired" '(progn
;;     (define-key dired-mode-map "j" 'dired-next-line)
;;     (define-key dired-mode-map "k" 'dired-previous-line)))
;; (eval-after-load "XREF" '(progn
;;     (evil-define-key 'normal 'XREF-mode-map (kbd "RET") 'xref-goto-xref))

(evil-define-key 'normal 'global (kbd "<leader>ss") '("find symbol" . cscope-find-this-symbol))
(evil-define-key 'normal 'global (kbd "<leader>s=") '("find assignments to this symbol" . cscope-find-assignments-to-this-symbol))
(evil-define-key 'normal 'global (kbd "<leader>sd") '("find global definition" . cscope-find-global-definition))
(evil-define-key 'normal 'global (kbd "<leader>s.") '("find global definition no prompt" . cscope-find-global-definition-no-prompting))
(evil-define-key 'normal 'global (kbd "<leader>sc") '("find functions calling this symbol" . cscope-find-functions-calling-this-function))
(evil-define-key 'normal 'global (kbd "<leader>sC") '("find called functions" . cscope-find-called-functions))
(evil-define-key 'normal 'global (kbd "<leader>st") '("find text string" . cscope-find-this-text-string))
(evil-define-key 'normal 'global (kbd "<leader>se") '("find egrep pattern" . cscope-find-egrep-pattern))
(evil-define-key 'normal 'global (kbd "<leader>sf") '("find find file" . cscope-find-this-file))
(evil-define-key 'normal 'global (kbd "<leader>si") '("find files including file" . cscope-find-files-including-file))
(evil-define-key 'normal 'global (kbd "<leader>sb") '("display buffer" . cscope-display-buffer))
(evil-define-key 'normal 'global (kbd "<leader>sn") '("next result" . cscope-history-forward-line-current-result))
(evil-define-key 'normal 'global (kbd "<leader>sN") '("previous result" . cscope-history-backward-line-current-result))

(global-set-key (kbd "M-'") 'avy-goto-word-or-subword-1)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun g (expr)
  "greps all c files"
  (interactive "sEnter search expression: ")
  (grep-find (concat "find . '(' -name \"*.c\" -o -name \"*.h\" -o -name \"*.hpp\" -o -iname \"*.rs\" -o -name \"*.cc\" -o -name \"*.cpp\" -o -iname \"makefile\" -o -name \"*.module\" ')' -exec grep -nH " expr " \{\} /dev/null ';'")))

(defun gh (expr)
  "greps all header files"
  (interactive "sEnter search expression: ")
  (grep-find (concat "find . '(' -name \"*.h\" -o -name \"*.hpp\" ')' -exec grep -nH " expr " \{\} /dev/null ';'")))

(defun ga (expr)
  "greps all files"
  (interactive "sEnter search expression: ")
  (grep-find (concat "find . -name \"*\" -exec grep -nH " expr " \{\} /dev/null ';'")))

(defun g-at-point ()
  "greps all c files in current directory for the word at the current point"
  (interactive)
  (grep-find (concat "find . '(' -name \"*.c\" -o -name \"*.h\" -o -name \"*.hpp\" -o -iname \"*.rs\" -o -name \"*.cc\" -o -name \"*.cpp\" -o -iname \"makefile\" ')' -exec grep -nH " (current-word) " \{\} /dev/null ';'")))

(defun dp ()
  "dired at project path"
  (interactive)
  (dired "~/dev/libsdwan/src"))

(defun convert-buffer-to-unix ()
  "Converts a buffer to UNIX encoding"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix))

(defun convert-buffer-to-dos ()
  "Converts a buffer to DOS encoding"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos))

(put 'downcase-region 'disabled nil)

(defun insert-function-header () (interactive)
  (insert "/*************************************************************************\n")
  (insert "**        Name: \n")
  (insert "**   Arguments: \n")
  (insert "**     Returns: \n")
  (insert "** Description: \n")
  (insert "*************************************************************************/\n"))

(defun ih () (interactive)
  (insert-function-header))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
		(revert-buffer t t t) )))
  (message "Refreshed open files.") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-c-mode-hook ()
      (define-key c-mode-map "\r" 'newline-and-indent))
(add-hook 'c-mode-hook 'my-c-mode-hook)
;(add-hook 'java/l-mode-hook 'my-c-mode-hook)

(setq c-default-style "stroustrup"
	  c-basic-offset 4)
(setq lisp-indent-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart inference of indentation style from emacswiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.                    
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  "if our source file uses tabs, we use tabs, if spaces spaces, and if        
neither, we use the current indent-tabs-mode"
  (interactive)
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(add-hook 'c-ts-mode-hook 'infer-indentation-style)
(add-hook 'c-mode-hook 'infer-indentation-style)
(add-hook 'c++-ts-mode-hook 'infer-indentation-style)
(add-hook 'c++-mode-hook 'infer-indentation-style)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

