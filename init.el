(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             SLDR Emacs config                              ;;
;;                 I literally have no idea what I'm doing here               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup package repository ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure Marmalade for package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Set load path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((default-directory
  (concat user-emacs-directory
    (convert-standard-filename "manual/"))))
  (setq load-path
    (append
      (let ((load-path (copy-sequence load-path))) ;; Shadow
        (append 
          (copy-sequence (normal-top-level-add-to-load-path '(".")))
          (normal-top-level-add-subdirs-to-load-path)))
      load-path)))

;;;;;;;;;;;;;;;;;;;;;; Check for auto-installed packages ;;;;;;;;;;;;;;;;;;;;;;;

;; Prelude list
(defvar prelude-packages
  '(auctex solarized-theme company ycmd company-ycmd)
  "A list of packages that should be installed; tested on launch.")

;; Tester function
(defun prelude-packages-installed-p (plist)
  (let (p exit-flag)
    (setq exit-flag nil)
    (while (and plist (not exit-flag))
      (setq p (car plist))
      (if (not (package-installed-p p))
          (setq exit-flag t))
      (setq plist (cdr plist)))
    (if exit-flag nil t)))

;; Test-and-install loop
(unless (prelude-packages-installed-p prelude-packages)
  ;; Look for newer versions of installed packages
  (message "%s" "Emacs Prelude is now looking for missing packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install any missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;; Load manually installed packages ;;;;;;;;;;;;;;;;;;;;;;;

;; Better defaults
(unless 
    (condition-case nil
        (load-library "better-defaults")
      (error nil))
  (message "[SLDR] Better Defaults not found. Re-clone repo."))

;; ;; Auto completion
;; (unless
;;     (condition-case nil
;;         (require 'auto-complete-config)
;;       (error nil))
;;   (message "[SLDR] Auto Complete not found. Re-clone repo."))

;; ;; Auto completion math support
;; (unless
;;     (condition-case nil
;;         (require 'ac-math)
;;       (error nil))
;;   (message "[SLDR] Auto Complete math library not found. Re-clone repo."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Markdown mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure markdown mode is where it should be
(unless
    (condition-case nil
        (file-exists-p "~/.emacs.d/manual/markdown-mode.el")
      (error nil))
  (message "[SLDR] Markdown mode not found. Re-clone repo."))

;; Set mode autoload
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Set some autoclose funstuff
(add-hook 'markdown-mode-hook 
          (lambda () 
            (global-set-key (kbd "*") 'skeleton-pair-insert-maybe)))
(add-hook 'markdown-mode-hook 
          (lambda () 
            (global-set-key (kbd "_") 'skeleton-pair-insert-maybe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Configure auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load in emacs-ycmd
(require 'ycmd)
(set-variable 'ycmd-server-command '("ycmd-server"))
(add-hook 'after-init-hook #'global-ycmd-mode)

;; (add-to-list 'ac-dictionary-directories 
;;              "~/.emacs.d/manual/auto-complete/ac-dict")
;; (ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configure ac-math ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'ac-modes 'latex-mode)
(defun ac-LaTeX-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex
                                         ac-source-latex-commands)
                ac-sources))
)
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(global-auto-complete-mode t)
(setq ac-math-unicode-in-math-p t)

;;;;;;;;;;;;;;;;;;;;;;; Set Solarized as the color theme ;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;; Set line wrapping globally ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto-fill-turn-on ()
  (auto-fill-mode 1)
  (setq-default fill-column 80))
(add-hook 'find-file-hooks 'auto-fill-turn-on)

;;;;;;;;;;;;;;;;;;;;;;; Set electric autoclose globally ;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'find-file-hooks 'electric-pair-mode)

;;;;;;;;;;;;;;;;;;;; Set current line highlighting globally ;;;;;;;;;;;;;;;;;;;;

(defun hl-line-mode-turn-on ()
  (hl-line-mode 1))
(add-hook 'find-file-hooks 'hl-line-mode-turn-on)

;;;;;;;;;;;;;;;;;;;;;;; Enable line and column numbering ;;;;;;;;;;;;;;;;;;;;;;;

(line-number-mode 1)
(column-number-mode 1)
(add-hook 'find-file-hooks 'linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Alias yes-or-no-p ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fewer button presses, happier programmers
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Redefine tab ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2-indent spaces. Because that's how I roll
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)
(add-hook 'before-save-hook
  '(lambda () (if (not indent-tabs-mode)
     (untabify (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;; Set automatic reloading of PDFs ;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;; Show offscreen matching parenthesis ;;;;;;;;;;;;;;;;;;;;;;

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
  echo area. Has no effect if the character before point is not of
  the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
    (matching-text (and cb
      (char-equal (char-syntax cb) ?\) )
      (blink-matching-open))))
    (when matching-text (message matching-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-mode stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-cycle-emulate-tab 'white)
(global-set-key [33554447] (quote org-mode))

(defun org-mode-step-up-and-sort ()
  "Steps up and sorts the current level of entities"
  (interactive)
  (outline-up-heading 1)
  (org-sort-entries nil ?a))
(global-set-key (kbd "C-c s") 'org-mode-step-up-and-sort)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mutt stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Starts the emacs client
(server-start)
(add-to-list 'auto-mode-alist '("mutt.*" . mail-mode))

;; Defines mail mode hook function and sets it
(defun my-mail-mode-hook ()
  'auto-fill-turn-on)
(add-hook 'mail-mode-hook 'my-mail-mode-hook)

;; Set the CTRL-x k combo to kill server buffer globally
(global-set-key "\C-Xk" 'server-edit)

;; Resets kill buffer to somewhere more-or-less sensible
(global-set-key "\C-x\M-k" 'kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Expand frame ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((frame (selected-frame)))
  (delete-other-windows)
  (set-frame-size frame 100 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
