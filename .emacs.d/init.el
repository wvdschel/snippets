;;;;;;;;;;;;;;;;;;; emacs config tested with emacs 23.1.3 and 24.5.1 ;;;;;;;;;;;;;;;;;
;; If emacs complains about missing packages, M-x install-my-packages.
;; Assumes rtags binaries are available in $PATH, if not, comment out rtags section.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This makes emacs find all the required binaries and libraries I compiled myself. ;;
;; (Cisco Aurora specific)                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-exists-p "/home/wvanders/pkgs/bin")
    (progn
      (setenv "PATH"
              (concat
               "/home/wvanders/pkgs/bin" ":"
               (getenv "PATH"))))
  
  (setenv "LD_LIBRARY_PATH"
          (concat
           "/home/wvanders/pkgs/lib" ":"
           "/home/wvanders/pkgs/lib64" ":"
           (getenv "LD_LIBRARY_PATH"))))

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
        '(cmake-ide cmake-mode company dash elixir-mode elpy epl erlang flycheck
                    love-minor-mode lua-mode lush-theme paredit pkg-info rtags slime
                    flymake-lua company-lua rust-mode racer)))

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

;;;;;;;;;;;;;;
;; Speedbar ;;
;;;;;;;;;;;;;;

(custom-set-variables
 '(speedbar-show-unknown-files t)) ; Show unknown file types in speedbar as well

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

(require 'highlight-symbol)
(global-set-key [f12] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(shift meta f3)] 'highlight-symbol-query-replace)
(highlight-symbol-mode)

;;;;;;;;;;;;;;;;;;;
;; CScope search ;;
;;;;;;;;;;;;;;;;;;;

;; Deprecated by rtags - see below!
;; (require 'xcscope)
;; ; Search for all callers of a symbol
;; (global-set-key (kbd "M-?") 'cscope-find-functions-calling-this-function)
;; ; Search for symbol definition
;; (global-set-key (kbd "M-.") 'cscope-find-global-definition)
;; ; Search for all symbol mentions (references and definitions), in case C-. and M-. jump to the wrong place
;; (global-set-key (kbd "C-?") 'cscope-find-this-symbol)
;; (global-set-key (kbd "C-,") 'cscope-find-this-text-string)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; General user config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight parens etc
(show-paren-mode 1)
(setq show-paren-delay 0)

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

;;;;;;;;;;;;;;;;;;;;;
;; Building config ;;
;;;;;;;;;;;;;;;;;;;;;

(setq make-project-args "")
(defun make-project ()
  "Run make. Automatically searches for a dixrectory containing a Makefile first"
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
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Godot GDScript mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'gdscript-mode)
;; (setq gdscript-tabs-mode t)
;; (setq gdscript-tab-width 4)

;;;;;;;;;;;;;;;;;
;; Dylan stuff ;;
;;;;;;;;;;;;;;;;;

(condition-case nil
    (progn
      (add-to-list 'load-path "~/.emacs.d/dylan-mode")
      (require 'dime))
  (error "Failed to load DIME"))

;;;;;;;;;;;;;;;;;;
;; Rust support ;;
;;;;;;;;;;;;;;;;;;

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

;;(let ((virtual-env "/ws/wvanders-gpk/pyenv"))
;;  (setenv "VIRTUAL_ENV" virtual-env)
;;  (setenv "PATH" (concat virtual-env "/bin:" (getenv "PATH")))
;;  (setenv "PYTHONHOME")) ;; Unset PYTHONHOME

;;(setenv "PYTHONHOME" "~/.elpy-libraries/easy_install")

(elpy-enable)
(condition-case nil
    (if (file-exists-p "~/.elpy-virtualenv")
        (pyvenv-activate "~/.elpy-virtualenv")
      (warn "No virtualenv found at ~/.elpy-virtualenv"))
  (error "Failed to init elpy virtualenv"))

;;;;;;;;;;;;;;;
;; Lua stuff ;;
;;;;;;;;;;;;;;;

(require 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)

;;;;;;;;;;;;;;;;;
;; rtags setup ;;
;;;;;;;;;;;;;;;;;

(if (executable-find "rc")
  (condition-case nil ; The following commands will fail if rtags is not installed.
    (progn
      (require 'rtags)
      (require 'company)
      (require 'flycheck-rtags)

      (setq rtags-autostart-diagnostics t)
      (rtags-diagnostics)
      (setq rtags-completions-enabled t)
      (push 'company-rtags company-backends)
      ;; (global-set-key (kbd "M-/") 'company-complete)
      (defun my-flycheck-rtags-setup ()
        (flycheck-select-checker 'rtags))
      ;; c-mode-common-hook is also called by c++-mode
      (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
      (rtags-restart-process)
      (rtags-start-process-unless-running)
      
      (let ((maps (list c-mode-base-map c++-mode-map)))
        (define-key-multimap maps (kbd "M-?") 'rtags-find-references-at-point) ; Search for references to current symbol
        (define-key-multimap maps (kbd "M-.") 'rtags-find-symbol-at-point)     ; Search for symbol definition
        (define-key-multimap maps (kbd "C-?") 'rtags-find-all-references-at-point) ; Search for all symbol mentions (references and definitions)
        (define-key-multimap maps (kbd "M-n") 'rtags-next-match)               ; Up/down/next/previous in search results
        (define-key-multimap maps (kbd "M-p") 'rtags-previous-match)
        (define-key-multimap maps (kbd "M-f") 'rtags-location-stack-forward)
        (define-key-multimap maps (kbd "M-b") 'rtags-location-stack-back)
        (define-key-multimap maps (kbd "M-SPC") (function company-complete)))) ; Force company completion with M-space
  (error (message "rtags could not be initialized."))))

(global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust setup using racer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

;; Detect msys64 rustup install
(if (file-exists-p (concat "C:\\Users\\" (user-login-name) "\\.cargo\\bin"))
    (progn
      (setenv "PATH"
              (concat
               "C:\\Users\\" (user-login-name) "\\.cargo\\bin" ";"
               (getenv "PATH")))
      (setq racer-cmd (concat "C:\\Users\\" (user-login-name) "\\.cargo\\bin\\racer"))
      (setq racer-rust-src-path (concat 
               "C:\\Users\\" (user-login-name) "\\.rustup\\toolchains\\stable-x86_64-pc-windows-gnu\\lib\\rustlib\\src\\rust\\src"))))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(setq company-idle-delay .3)
(setq company-minimum-prefix-length 2)

;;;;;;;;;;;;;
;; Futhark ;;
;;;;;;;;;;;;;

(require 'futhark-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; MELPA over HTTPS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-variables
;;  '(delete-selection-mode nil)
;;  '(package-archives (quote (("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/"))))
;;  '(tls-checktrust t))

;; If linked against GNU tls, these are used.
;; (setq gnutls-verify-error t)
;; (setq gnutls-trustfiles (directories-files
;;                          '("/var/lib/ca-certificates/pem/" "/usr/ssl/") t "\\.pem$"))

;; Using gnutls-cli if not linked against gnutls.
;; (setq tls-program
;;       (loop for cert-file in gnutls-trustfiles
;;             collect (format "my-gnutls-cli%s --x509cafile %s -p %%p %%h"
;;                             (if (eq window-system 'w32) ".exe" "") cert-file)))

;; Validate if this config works
;; (let ((bad-hosts
;;        (loop for bad
;;              in `("https://wrong.host.badssl.com/"
;;                   "https://self-signed.badssl.com/")
;;              if (condition-case e
;;                     (url-retrieve
;;                      bad (lambda (retrieved) t))
;;                   (error nil))
;;              collect bad)))
;;   (if bad-hosts
;;       (error (format "tls misconfigured; retrieved %s ok"
;;                      bad-hosts))
;;     (url-retrieve "https://badssl.com"
;;                   (lambda (retrieved) t))))

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
(global-set-key [f4] 'redraw-display)

;; (lush-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-start-syntax-check-on-newline t))

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

;; Start emacs server to allow opening files in this session from the command line
(server-start)

(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'wombat)
