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
;;                            DrSLDR Emacs config                             ;;
;;                 I literally have no idea what I'm doing here               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup package repository ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure Marmalade for package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
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
  '(auctex color-theme-solarized)
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
  (message "[DrSLDR] Better Defaults not found. Re-clone repo."))

;; Auto completion
(unless
    (condition-case nil
        (require 'auto-complete-config)
      (error nil))
  (message "[DrSLDR] Auto Complete not found. Re-clone repo."))

;; Auto completion math support
(unless
    (condition-case nil
        (require 'ac-math)
      (error nil))
  (message "[DrSLDR] Auto Complete math library not found. Re-clone repo."))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Configure auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'ac-dictionary-directories 
             "~/.emacs.d/manual/auto-complete/ac-dict")
(ac-config-default)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
