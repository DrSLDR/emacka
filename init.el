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

;; DrSLDR Emacs config
;; I literally have no idea what I'm doing here

;; Load path setting to allow packages to be loaded
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

;; Load the custom libraries
;; Now complete with error handling
(unless 
    (condition-case nil
        (load-library "better-defaults")
      (error nil))
  (message "[DrSLDR] Better Defaults not found. Re-clone repo."))

(unless
    (condition-case nil
        (require 'auto-complete-config)
      (error nil))
  (message "[DrSLDR] Auto Complete not found. Re-clone repo."))

(unless
    (condition-case nil
        (require 'ac-math)
      (error nil))
  (message "[DrSLDR] Auto Complete math library not found. Re-clone repo."))

;; Configure Auto-complete
(add-to-list 'ac-dictionary-directories 
             "~/.emacs.d/manual/auto-complete/ac-dict")
(ac-config-default)

;;Configure ac-math
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

;; Configure Marmalade for package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Set Solarized as the color theme and shouts if it's missing
(unless
    (condition-case nil
        (load-theme 'solarized-dark t)
      (error nil))
  (message "[DrSLDR] Solarized not found. Install by 'package-install
  color-theme-solarized'."))

;; Enable line wrapping at column 80 globally
(defun auto-fill-turn-on ()
  (auto-fill-mode 1)
  (setq-default fill-column 80))
(add-hook 'find-file-hooks 'auto-fill-turn-on)

;; Enable line and column numbering
(line-number-mode 1)
(column-number-mode 1)

;; Fewer button presses, happier programmers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Redefine tab function to 2-indent spaces
;; Because that's how I roll
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default indent-line-function 'insert-tab)
(add-hook 'before-save-hook
  '(lambda () (if (not indent-tabs-mode)
     (untabify (point-min) (point-max)))))

;; Enable and configure desktops
(desktop-save-mode 1)
(setq history-length 512)
(setq desktop-buffers-not-to-save
  (concat "\\("
          "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
          "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
          "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
    '(lambda ()
       ;; desktop-remove clears desktop-dirname
       (setq desktop-dirname-tmp desktop-dirname)
       (desktop-remove)
       (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
    (desktop-save-in-desktop-dir)
  (message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
    '(lambda ()
       (if (saved-session)
     (if (y-or-n-p "Restore desktop? ")
         (session-restore)))))
