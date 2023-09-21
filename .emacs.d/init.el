(setq inhibit-startup-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mac-option-modifier 'meta)
 '(package-selected-packages
   '(eglot yaml-mode flycheck-golangci-lint zig-mode rust-mode flycheck company-fuzzy go-dlv go-mode go company treemacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun extend-path (dir)
  "Add DIR to 'exec-path' if the directory exists & is not already in 'exec-path'."
  (if (and (file-exists-p dir) (not (member dir exec-path)))
      (progn
        (add-to-list 'exec-path dir)
        (message "Added %s to exec-path" dir))))

(defun parent-directory (dir)
  "Return the parent directory of DIR."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-dir-containing (current-dir fname)
  "Search for a parent directory of CURRENT-DIR containing a file FNAME."
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
      current-dir
      (when parent
        (find-dir-containing parent fname)))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Add tools to PATH ;;
;;;;;;;;;;;;;;;;;;;;;;;

(extend-path (concat (getenv "HOME") "/.cargo/bin"))
(extend-path (concat (getenv "HOME") "/go/bin"))

;;;;;;;;;;;;;;
;; Git grep ;;
;;;;;;;;;;;;;;

(defun git-grep (search)
  "Search through the git repository using git grep for occurences of SEARCH."
  (interactive (list (read-from-minibuffer "Search for: " (current-word))))
  (let ((current-file-dir (parent-directory (expand-file-name (buffer-file-name))))
        (current-dir      (getenv "PWD")))
    (let ((git-top-dir      (find-dir-containing current-file-dir ".git")))
      (if git-top-dir
          (progn
            (cd git-top-dir)
            (grep-find (concat "git --no-pager grep -n --color "
                               (shell-quote-argument search)))
            (cd current-dir))
        (progn
          (warn (concat "No git repository found for '" current-dir "', using normal grep."))
          (grep-find (concat "grep -Rn --color " search " " current-file-dir)))))))

(global-set-key [C-f3] 'git-grep)

;;;;;;;;;;;;;;;;;
;; LSP (eglot) ;;
;;;;;;;;;;;;;;;;;

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)

(defun format-on-save ()
  "Format on save using eglot."
  (when (member 'eglot--managed-mode minor-mode-list)
    (eglot-code-action-organize-imports)
    (eglot-format-buffer)))

(add-hook 'before-save-hook #'format-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use golangci for flycheck ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;;;;;;;;;;;;;;;;;;;
;; Global config ;;
;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 130)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-company-mode)
(global-eldoc-mode)
(global-flycheck-mode)

(server-start)

(provide 'init)
;;; init.el ends here
