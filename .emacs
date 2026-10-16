;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Bill's emacs initialization file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Keep Custom's machine-local state (package-selected-packages etc.)
;; out of this file so it doesn't cause git conflicts.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

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

; fix macos path
(when (memq window-system '(mac ns x))
    (use-package exec-path-from-shell)
    :config
    (exec-path-from-shell-initialize))

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
    (with-eval-after-load 'evil-collection
      (remove-hook 'evil-collection-mode-list 'org-mode))
    (evil-collection-init))

; make sure cargo is in path
(setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
(add-to-list 'exec-path "~/.cargo/bin")

(add-to-list `exec-path (expand-file-name "~/go/bin"))

; use jj to leave insert mode - much easier than esc
(use-package key-chord
    :defer t
    :init
    (setq key-chord-two-keys-delay 1.0)
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

;; column indicator draws line at fill-column-mode
;; This doesn't work right with copilot so disabled for now
;; (use-package fill-column-indicator
;;     :defer t
;;     :init
;;     (add-hook 'c-mode-hook 'fci-mode)
;;     (add-hook 'c++-mode-hook 'fci-mode)
;;     (setq fci-rule-color "#444444"))

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
    (global-corfu-mode)
    (setq tab-always-indent 'complete)
    ;; show completions automatically as you type, instead of only
    ;; on demand via M-TAB / TAB
    (setq corfu-auto t
          corfu-auto-delay 0.2
          corfu-auto-prefix 2))

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
    (setq doom-tokyo-night-brighter-comments t)
    (setq doom-tokyo-night-comment-bg t)
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

(use-package avy
    :config
    (global-set-key (kbd "M-'") 'avy-goto-word-or-subword-1))

; clipboard pasting into org
;; (use-package org-download
;;     :config
;;     (setq org-download-method 'directory)
;;     (setq org-download-image-dir "./images")
;;     (setq org-download-screenshot-method "wl-paste > %s"))
    
;; (when (display-graphic-p)
;;     (use-package copilot
;;     :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;                 :rev :newest
;;                 :branch "main"))
;;     (use-package copilot-chat)
;;     (add-hook 'prog-mode-hook 'copilot-mode)
;;     (with-eval-after-load 'copilot
;;     (define-key copilot-mode-map (kbd "TAB") #'copilot-accept-completion))
;; )

(use-package fzf)

(use-package go-mode
  :ensure t
  :config
  ;; Set up proper Go indentation (4 spaces, no tabs)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4
                    indent-tabs-mode nil)
              ;; Enable automatic formatting and import organization on save
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)
              (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
  )

;; Add a simple keybinding for running the Go program
(defun my/go-run ()
  (interactive)
  (call-process-shell-command "go run ." nil "*Go Run*" t))

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'my/go-run)))

;; Ensure font-lock is enabled for Python
(add-hook 'python-mode-hook 'turn-on-font-lock)

;; eglot
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'rustic-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)

;; clangd (and possibly other servers) reformat via LSP "on-type
;; formatting" every time RET is pressed, since eglot treats
;; newline-and-indent's "\n" insertion as a formatting trigger just
;; like typing it directly. That's what moves an opening brace onto
;; the previous line regardless of our own cc-mode indentation style.
;; Disable just that capability; leave everything else eglot does
;; (completion, diagnostics, xref, etc.) untouched.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider))

;; eglot over tramp needs the remote language server on real pipes, not a tty.
;;
;; 1. By default tramp runs async processes through the shared ssh session's
;;    pty.  The `stty raw' eglot prepends to the remote command then races with
;;    the initialize request, which gets discarded by the line discipline about
;;    half the time; eglot times out after 30s, no server is registered, and
;;    every eglot/eldoc/flymake call afterwards fails with
;;    "No current JSON-RPC connection".  Direct async processes avoid the
;;    shared connection and give the local end a real pipe.
;;
;; 2. Direct async still passes "-t -t" to ssh, which allocates a pty on the
;;    *remote* side.  A pty input buffer is a few KB with no flow control, so
;;    the textDocument/didOpen carrying the file contents is silently
;;    truncated: clangd sits waiting for the rest of the message and never
;;    answers another request.  Dropping "-t -t" makes it pipes end to end.
(with-eval-after-load 'tramp
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh") 'remote-direct-async-process)
  (dolist (method '("ssh" "scp" "sshx" "sshfs"))
    (let ((entry (assq 'tramp-direct-async (assoc method tramp-methods))))
      (when entry (setcdr entry (list t))))))

;; remote clangd startup is slower than the 30s default is comfortable with
(setq eglot-connect-timeout 60)

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
(setq grep-command "rg --no-heading -n -H . -e "
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

;; dired will guess a default target directory
(setq dired-dwim-target t)

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

; undo warning keeps popping up
(add-to-list 'warning-suppress-types '(undo discard-info))

;; set font
;(set-frame-font "Inconsolata Nerd Font 16" nil t)
;(set-frame-font "Inconsolata Regular 14" nil t)
;(set-frame-font ":family Inconsolata-18 :weight Regular" nil t)
(set-frame-font "Inconsolata:pixelsize=16:weight=regular:antialias=subpixel" nil t)

; org-mode-modern uses some characters in this font
(set-face-attribute 'org-modern-symbol nil :family "Iosevka")

(setopt asm-comment-char 59
        backup-directory-alist '(("." . "~/backups"))
        c-basic-offset 4
        c-tab-always-indent nil
        c-ts-mode-indent-offset 4
        compilation-scroll-output t
        compilation-skip-threshold 2
        delete-old-versions 'other
        ediff-keep-variants nil
        indent-tabs-mode nil
        large-file-warning-threshold 100000000
        perl-tab-always-indent nil
        scroll-conservatively 999
        speedbar-tag-hierarchy-method nil
        tab-width 4
        tags-case-fold-search nil
        truncate-lines t
        version-control t
        warning-suppress-types '((use-package)))
(column-number-mode 1)
(which-function-mode 1)


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
        (setq indent-tabs-mode nil)
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

;; Force Windows 11 Taskbar to group WSLg Wayland windows properly
(when (eq window-system 'pgtk)
  (setq frame-title-format "Emacs")
  (modify-all-frames-parameters '((name . "Emacs"))))

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
(evil-define-key 'normal 'global (kbd "<leader>a") '("toggle anotations" . eglot-inlay-hints-mode))
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

(evil-define-key 'normal 'c++-mode-map (kbd "<leader>ss") '("find symbol" . cscope-find-this-symbol))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>s=") '("find assignments to this symbol" . cscope-find-assignments-to-this-symbol))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sd") '("find global definition" . cscope-find-global-definition))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>s.") '("find global definition no prompt" . cscope-find-global-definition-no-prompting))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sc") '("find functions calling this symbol" . cscope-find-functions-calling-this-function))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sC") '("find called functions" . cscope-find-called-functions))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>st") '("find text string" . cscope-find-this-text-string))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>se") '("find egrep pattern" . cscope-find-egrep-pattern))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sf") '("find find file" . cscope-find-this-file))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>si") '("find files including file" . cscope-find-files-including-file))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sb") '("display buffer" . cscope-display-buffer))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sn") '("next result" . cscope-history-forward-line-current-result))
(evil-define-key 'normal 'c++-mode-map (kbd "<leader>sN") '("previous result" . cscope-history-backward-line-current-result))

(evil-define-key 'normal 'global (kbd "<leader>cc") 'copilot-chat-toggle)
(evil-define-key 'normal 'global (kbd "<leader>cp") 'copilot-chat-yank)
(evil-define-key 'normal 'global (kbd "<leader>cy") 'copilot-chat-copy-code-at-point)
(evil-define-key 'normal 'global (kbd "<leader>cb") 'copilot-chat-add-current-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cl") 'copilot-chat-list)
(evil-define-key 'normal 'global (kbd "<leader>cd") 'copilot-chat-add-files-under-dir)

(global-set-key (kbd "M-'") 'avy-goto-word-or-subword-1)
(evil-define-key 'normal 'global (kbd "s-u") 'revert-buffer)

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

;; Format tables in md files
(defun my/markdown-align-all-tables ()
  "Align all Org/Markdown tables in the buffer upon opening."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^[ \t]*|" nil t)
      (when (org-at-table-p)
        (org-table-align)
        (goto-char (org-table-end))))))

(add-hook 'markdown-mode-hook #'my/markdown-align-all-tables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun g (expr)
  "greps all c files"
  (interactive "sEnter search expression: ")
    (grep-find (concat "rg --no-heading -n -H " expr " .")))
  ;(grep-find (concat "find . '(' -name \"*.c\" -o -name \"*.h\" -o -name \"*.hpp\" -o -iname \"*.rs\" -o -name \"*.cc\" -o -name \"*.cpp\" -o -iname \"makefile\" -o -name \"*.module\" ')' -exec grep -nH " expr " \{\} /dev/null ';'")))

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
    (grep-find (concat "rg --no-heading -n -H " (current-word) " .")))
   ; (grep rg (current-word)))
  ;(grep-find (concat "find . '(' -name \"*.c\" -o -name \"*.h\" -o -name \"*.hpp\" -o -iname \"*.rs\" -o -name \"*.cc\" -o -name \"*.cpp\" -o -iname \"makefile\" ')' -exec grep -nH " (current-word) " \{\} /dev/null ';'")))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copilot Chat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copilot-chat-toggle ()
  "Start a Copilot Chat instance if not running, or switch to it if already running."
  (interactive)
  (if-let ((buffer (get-buffer "*copilot-chat*")))
      (switch-to-buffer buffer)
    (copilot-chat)))

