;;; init.el --- Emacs configuration

;;; Commentary:

;; This is a basic Emacs config file, built around emacs-lsp.
;; If Emacs complains about missing packages, M-x install-my-packages.

;;; Code:

;; Customized settings (pervents emacs from messing with our custom-set-variables calls further down)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (go-eldoc go-mode lsp-go toml-mode slime paredit lsp-rust love-minor-mode highlight-symbol highlight-parentheses futhark-mode flymake-lua flycheck eziam-theme elpy cquery company-lua company-lsp cmake-mode cargo))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ctrl-Z by default backgrounds emacs, which is pretty pointless and annoying
(global-unset-key (kbd "C-z"))

;;;;;;;;;;;;;;;;;;
;; Proxy config ;;
;;;;;;;;;;;;;;;;;;

(if (file-exists-p "~/.emacs.d/proxyconfig.el")
    (load "~/.emacs.d/proxyconfig.el"))

;; Use MingW64 if available

(if (file-exists-p "C:\\msys64")
    (setenv "PATH"
            (concat
             "C:\\msys64\\usr\\bin" ";"
             "C:\\msys64\\mingw32\\bin" ";"
             "C:\\msys64\\mingw64\\bin" ";"
             (getenv "PATH"))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-dir-containing (current-dir fname)
  "Search for a directory containing a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
      current-dir
      (when parent
        (find-dir-containing parent fname)))))

(defun find-file-in-hierarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((found-dir (find-dir-containing current-dir fname)))
    (if found-dir
        (concat current-dir fname)
      nil)))

(defun cd-to-current-buffer ()
  (interactive)
  (cd (parent-directory (expand-file-name (buffer-file-name)))))

(defun run-for-current-buffer (func)
  (interactive)
  (let ((current-file-dir (parent-directory (expand-file-name (buffer-file-name))))
        (current-dir      (getenv "PWD")))
    (cd current-file-dir)
    (funcall func)
    (cd current-dir)))

(defun filter-existing-files (file-list)
  "Filter a list of files, returning only those that actually exist."
  (let ((file      (car file-list))
        (remainder (cdr file-list)))
    (if (file-exists-p file)
        (cons file (filter-existing-files remainder))
      (filter-existing-files remainder))))

(defun directories-files (dirlist &optional full match nosort)
  "Run directory-files on a list of directories, ignoring errors."
  (condition-case e
      (append (directory-files (car dirlist) full match nosort)) (directories-files (cdr dirlist))
      (directories-files (cdr dirlist))))

(defun install-my-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (pkg)
          (unless (package-installed-p pkg) (package-install pkg)))
        '(
          ;; Non-default language support
          cmake-mode rust-mode toml-mode lua-mode love-minor-mode
                     futhark-mode
                     ;; General emacs frameworks
                     company dash flycheck
                     ;; Language server protocol support
                     lsp-mode company-lsp
                     ;; Python
                     elpy
                     ;; Go
                     lsp-go go-mode go-eldoc
                     ;; LISP
                     paredit pkg-info slime
                     ;; Lua
                     flymake-lua company-lua
                     ;; Rugst
                     cargo lsp-rust
                     ;; C/C++
                     cquery ;; lsp-clangd
		     ;; Various quality of life plugins
		     highlight-symbol highlight-parentheses eziam-theme)))

(defun define-key-multimap (maps key command)
  (mapc (lambda (map) (define-key map key command)) maps))

;; Find index of an entry in a list
(defun index (element list &optional base)
  (when list
    (let ((curpos (or base 0)))
      (if (equal element (car list))
          curpos
        (index element (cdr list) (+ curpos 1))))))

;; Filter entries of a list matching a predicate
(defun filter-list (list predicate)
  (when list 
    (if (funcall predicate (car list))
        (cons (car list) (filter-list (cdr list) predicate))
      (filter-list (cdr list) predicate))))

;; Get the active window
(defun active-window ()
  (car (window-list)))

;; Get the active buffer
(defun active-buffer ()
  (window-buffer (active-window)))

;; Strip newline off the end of a string
(defun chomp (str)
    (if (and (> (length str) 0)
             (string= (substring str -1 nil) "\n"))
        (chomp (substring str 0 -1))
      str))

;; Add a path to exec-path if the directory exists & is not already in exec-path
(defun extend-path (dir)
  (if (and (file-exists-p dir) (not (member dir exec-path)))
      (progn
        (add-to-list 'exec-path dir)
        (message "Added %s to exec-path" dir))
      (message "Not adding %s to exec-path: exists = %s and member = %s"
               dir (file-exists-p dir)(member dir exec-path))))

;;;;;;;;;;;;;;
;; Speedbar ;;
;;;;;;;;;;;;;;

 ;; Show unknown file types in speedbar as well

;; no startup msg  
(setq inhibit-startup-message t)   ; Disable startup message

;; embed in main window
(add-to-list 'load-path "~/.emacs.d/scripts")
(require 'sr-speedbar)
(global-set-key (kbd "M-RET") 'sr-speedbar-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
                          '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move autosave files (#*#) and backups (*~) out of the code tree ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-auto-save-folder "~/.emacs.d/auto-save/"); folder for auto-saves
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save/.saves-"); set prefix for auto-saves 
(setq auto-save-file-name-transforms `((".*" ,my-auto-save-folder t))); location for all auto-save files
(setq tramp-auto-save-directory my-auto-save-folder)

(defvar my-backup-folder "~/.emacs.d/backups/")
(setq backup-directory-alist
      `((".*" . ,my-backup-folder)))
(setq auto-save-file-name-transforms
      `((".*" ,my-backup-folder t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code folding with C-tab ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code Folding for Ruby
(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(global-set-key [C-tab] 'hs-toggle-hiding)

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments t)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'code)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'ruby-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight symbols ;;
;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f12] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(shift meta f3)] 'highlight-symbol-query-replace)

;;;;;;;;;;;;;;;;;;;;;
;; Building config ;;
;;;;;;;;;;;;;;;;;;;;;

(setq make-project-args "")
(defun make-project ()
  "Run make. Automatically searches for a directory containing a Makefile first"
  (interactive)
  (let ((orig-dir (parent-directory (expand-file-name (buffer-file-name)))))
    (let ((make-dir (find-dir-containing orig-dir "Makefile")))
      (if make-dir
          (progn
            (print (concat "Running make in " make-dir))
            (cd make-dir)
            (compile (concat "make " make-project-args))
            (cd orig-dir))
        (error (concat "No Makefile found from " orig-dir))))))

(defun make-project-prompt ()
  "Make and ask for command line arguments"
  (interactive)
  (setq make-project-args (read-from-minibuffer "Make arguments: "
                                                make-project-args))
  (make-project))

(global-set-key [f5] 'make-project)
(global-set-key [(shift f5)] 'make-project-prompt)

;;;;;;;;;;;;;;
;; Git grep ;;
;;;;;;;;;;;;;;

(defun git-grep (search)
  "git grep through the repository."
  (interactive (list (read-from-minibuffer "Search for: " nil nil nil (current-word))))
  (let ((current-file-dir (parent-directory (expand-file-name (buffer-file-name))))
        (current-dir      (getenv "PWD")))
    (let ((git-top-dir      (find-dir-containing current-file-dir ".git")))
      (if git-top-dir
          (progn
            (cd git-top-dir)
            (grep-find (concat "git --no-pager grep -2n --color "
                               (shell-quote-argument search)))
            (cd current-dir))
        (progn
          (warn (concat "No git repository found for '" current-dir "', using normal grep."))
          (grep-find (concat "grep -R2n --color " search " " current-file-dir)))))))

(global-set-key [C-f3] 'git-grep)

;;;;;;;;;;;;;;;;;;;;;
;; Paredit & SLIME ;;
;;;;;;;;;;;;;;;;;;;;;

(when (boundp 'paredit)
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python environment settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elpy-enable)
(condition-case nil
    (if (file-exists-p "~/.elpy-virtualenv")
        (pyvenv-activate "~/.elpy-virtualenv")
      (warn "No virtualenv found at ~/.elpy-virtualenv"))
  (warn "Failed to init elpy virtualenv"))

;;;;;;;;;;;;;;;
;; Lua stuff ;;
;;;;;;;;;;;;;;;

(require 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)

;;;;;;;;;;;;
;; Golang ;;
;;;;;;;;;;;;

(extend-path (concat (getenv "HOME") "/go/bin"))

(with-eval-after-load 'lsp-mode
  (require 'lsp-go))
(add-hook 'go-mode-hook #'lsp-go-enable)
(add-hook 'before-save-hook #'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ setup using cquery ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq cquery-executable "~/bin/cquery")

;; (defun cquery//enable ()
;;   (condition-case nil
;;       (lsp-cquery-enable)
;;     (user-error nil)))

;; (add-hook 'c-mode-common-hook #'cquery//enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ setup using clangd ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'lsp-mode
  (require 'lsp-clangd))
(add-hook 'c-mode--hook #'lsp-clangd-c-enable)
(add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
(add-hook 'objc-mode-hook #'lsp-clangd-objc-enable)

;; C languages should use sane indentation (not GNU)
(setq-default c-default-style "linux"
              c-basic-offset 2
              tab-width 8
              indent-tabs-mode nil)

(defun my-c-mode-hook ()
    (c-set-offset 'innamespace [0])
;;  (c-set-offset 'statement-case-open '+)
    (c-set-offset 'case-label '+))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; Automatically guess C-style indentation
;;(require 'guess-offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust setup using RLS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Detect msys64 rustup install
(let ((cargo-bin-dir (concat "C:\\Users\\" (user-login-name) "\\.cargo\\bin")))
  (if (file-exists-p cargo-bin-dir)
      (progn
        (extend-path cargo-bin-dir)
        (setenv "PATH"
                (concat
                 cargo-bin-dir
                 (getenv "PATH"))))))

(extend-path (concat (getenv "HOME") "/.cargo/bin"))

(let ((rustup-default-channel (chomp (shell-command-to-string "rustup show | grep '(default)$' | cut -d'-' -f1")))
      (rustup-full-path (chomp (shell-command-to-string "which rustup"))))
  (message (concat "Setting up lsp-rust with " rustup-full-path " and using channel " rustup-default-channel))
  (setq lsp-rust-rls-command (list rustup-full-path "run" rustup-default-channel "rls"))
  (with-eval-after-load 'lsp-mode
    (require 'lsp-rust))
  (add-hook 'rust-mode-hook #'lsp-rust-enable))
  
(setq my-cargo-args "")
(defun cargo-args-prompt ()
  "Run cargo build after modifying the arguments to the command."
  (interactive)
  (setq my-cargo-args (read-from-minibuffer "cargo args: "
                                                my-cargo-args))
  (setq cargo-process--command-build (concat "build " my-cargo-args))
  (setq cargo-process--command-run (concat "run " my-cargo-args))
  (setq cargo-process--command-check (concat "check " my-cargo-args))
  (setq cargo-process--command-clippy (concat "clippy " my-cargo-args)))

(require 'rust-mode)
;(define-key toml-mode-map [f5] #'cargo-process-build)
;(define-key toml-mode-map [(shift f5)] #'cargo-process-run)
(define-key rust-mode-map [f5] #'cargo-process-build)
(define-key rust-mode-map [(shift f5)] #'cargo-args-prompt)
(define-key rust-mode-map [f6] #'cargo-process-run)
(define-key rust-mode-map [f7] #'cargo-process-clippy)
(define-key rust-mode-map [f8] #'cargo-process-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch between buffers in alphabetical order ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a list of buffers, sorted by filenames.
;; By default, this list does not contain non-file buffers.
;; To include non-file buffers, pass 't as argument.
(defun my-buffer-list (&optional nofile)
  (let ((buflist (sort (buffer-list)
                       (lambda (x y) (string-lessp (buffer-file-name x) (buffer-file-name y))))))
    (if (not nofile)
        (filter-list buflist 'buffer-file-name)
      buflist)))

(defun my-next-buffer (&optional nofile)
  (interactive)
  (let ((buflist (my-buffer-list nofile)))
    (when buflist
      (let ((curridx (index (active-buffer) buflist)))
        (if (and curridx (< (+ 1 curridx) (length buflist)))
            (set-window-buffer (active-window) (nth (+ curridx 1) buflist))
          (set-window-buffer (active-window) (car buflist)))))))

(defun my-prev-buffer (&optional nofile)
  (interactive)
  (let ((buflist (my-buffer-list nofile)))
    (when buflist
      (let ((curridx (index (active-buffer) buflist)))
        (if (and curridx (> curridx 0))
            (set-window-buffer (active-window) (nth (- curridx 1) buflist))
          (set-window-buffer (active-window) (nth (- (length buflist) 1) buflist)))))))

(defun my-prev-buffer-all ()
  (interactive)
  (my-prev-buffer 't))

(defun my-next-buffer-all ()
  (interactive)
  (my-next-buffer 't))

(global-set-key (kbd "<C-right>") 'my-next-buffer-all)
(global-set-key (kbd "<C-left>") 'my-prev-buffer-all)
(global-set-key (kbd "<C-next>") 'my-next-buffer-all)
(global-set-key (kbd "<C-prior>") 'my-prev-buffer-all)
(global-set-key (kbd "<M-right>") 'my-next-buffer)
(global-set-key (kbd "<M-left>") 'my-prev-buffer)
(global-set-key (kbd "<M-next>") 'my-next-buffer)
(global-set-key (kbd "<M-prior>") 'my-prev-buffer)



;;;;;;;;;;;;;;;;;;;;
;; General config ;;
;;;;;;;;;;;;;;;;;;;;

;; Stop nagging about changes on VBox shares (timestamps issue)
(defadvice ask-user-about-supersession-threat (around ask-user-about-supersession-threat-if-necessary)
  "Call ask-user-about-supersession-threat only if the buffer is actually obsolete."
  (if (or (buffer-modified-p)
          (verify-visited-file-modtime)
          (< (* 8 1024 1024) (buffer-size))
          (/= 0 (call-process-region 1 (+ 1 (buffer-size)) "diff" nil nil nil "-q" (buffer-file-name) "-")))
      ad-do-it
    (clear-visited-file-modtime)
    (not-modified)))
(ad-activate 'ask-user-about-supersession-threat)

;; On CentOS, the buffer doesn't always draw correctly, and text becomes invisible.
;; This defines a keyboard shortcut to force a redraw
;; (global-set-key [f4] 'redraw-display)

;; Terminal keycodes (because GNU screen and GNU emacs don't agree on keycodes & terminal capabilities)
(add-hook 'term-setup-hook
  '(lambda ()
     ; \e[1;5[ABCD] is C-arrow
     (define-key function-key-map "\e[1;5A" [C-up])
     (define-key function-key-map "\e[1;5B" [C-down])
     (define-key function-key-map "\e[1;5C" [C-right])
     (define-key function-key-map "\e[1;5D" [C-left])
     ; \e[1;2[ABCD] is shift-arrow
     (define-key function-key-map "\e[1;2A" [(shift up)])
     (define-key function-key-map "\e[1;2B" [(shift down)])
     (define-key function-key-map "\e[1;2C" [(shift right)])
     (define-key function-key-map "\e[1;2D" [(shift left)])
     ; \e[1;3[ABCD] is M-arrow
     (define-key function-key-map "\e[1;3A" [M-up])
     (define-key function-key-map "\e[1;3B" [M-down])
     (define-key function-key-map "\e[1;3C" [M-right])
     (define-key function-key-map "\e[1;3D" [M-left])))

;; Start emacs server to allow opening files in this session from the command line
(server-start)

(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'dracula t)
;; (load-theme 'eziam-dusk t)
(setq-default frame-title-format "%b (%f)")
(setq rust-format-on-save t)
(global-company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 9000000)
(setq company-minimum-prefix-length 2)
(global-linum-mode)
(global-eldoc-mode)
(global-flycheck-mode)
(global-highlight-parentheses-mode)
(lsp-mode)

;; Add all system path directories to emacs' exec-path
(dolist (dir (split-string (getenv "PATH") path-separator) nil)
  (extend-path dir))


;; Scaler specific config
(load "~/.emacs.d/scripts/scaler.el")
