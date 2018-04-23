(require 'cc-mode)

(defun set-scaler-mode-line ()
  (interactive)
  (setq mode-line-format
        (f-relative (f-this-file)
                    scaler-work-dir)))

(defconst scaler-cpp-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset 0 . 0)
    (c-tab-always-indent . t)
    (c-cleanup-list . (empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-hanging-braces-alist . ((substatement-open before after)))
    (c-offsets-alist . ((inline-open . 0)
                        (topmost-intro-cont . 0)
                        (statement-block-intro . +)
                        (knr-argdecl-intro . 5)
                        (template-args-cont . +)
                        (substatement-open . 0)
                        (innamespace . 0)
                        (namespace-open . 0)
                        (namespace-close . 0)
                        (substatement-label . +)
                        (label . +)
                        (member-init-intro . +)
                        (statement-case-open . 0)
                        (case-label . +)
                        (statement-cont . +)
                        (arglist-intro . +)
                        (arglist-close . c-lineup-arglist)
                        (access-label . /)
                        (inclass  . +)
                        (inher-cont . c-lineup-java-inher)
                        (func-decl-cont . c-lineup-java-throws)))))

(c-add-style "SCALER" scaler-cpp-style)
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(defvar scaler-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map c++-mode-map)
;     (define-key map (kbd "C-z") 'rtags-speeddial-menu-open)
;;    (define-key map [f5] #'amplibuild)
;;    (define-key map [(shift f5)] #'amplibuild-prompt)
    map)
  "Keymap for Scaler C++ projects.")

(define-minor-mode scaler-mode
  "mode for doing development on scaler"
  :init-value nil
  :lighter " SCAL"
  (c-set-style "SCALER")
  ;;(set-scaler-mode-line))
)

(defun il-maybe-scaler-mode ()
  (if buffer-file-name
    (let ((buffer-dir (parent-directory (expand-file-name (buffer-file-name)))))
      (when (find-dir-containing buffer-dir "amplibuild.sh")
        (scaler-mode)))))

(add-hook 'c++-mode-hook
          'il-maybe-scaler-mode)

(defcustom amplibuild-cmd "sandbox-run ./amplibuild.sh"
  "What command to use in order to run amplibuild"
  :type 'string
  :safe 'stringp)

(setq amplibuild-args "debug-static")
(defun amplibuild ()
  (interactive)
  (let ((project-dir      (find-dir-containing (parent-directory (expand-file-name (buffer-file-name))) "amplibuild.sh"))
        (current-dir      (getenv "PWD")))
    (if project-dir
        (progn
          (cd project-dir)
          (compile (concat amplibuild-cmd " " amplibuild-args))
          (cd current-dir))
      (error (concat "No amplibuild.sh found for " (buffer-file-name))))))

(defun amplibuild-prompt ()
  "Amplibuild and ask for command line arguments"
  (interactive)
  (setq amplibuild-args (read-from-minibuffer "Amplibuild arguments: "
                                                amplibuild-args))
  (amplibuild))
