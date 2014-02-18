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

;; Test the custom libraries
;; Whine loudly if they aren't there
(unless (is-file-manually-installed "better-defaults.el") (error "Better Defaults
  missing. Entire thing (likely) bork'd. Re-clone repo."))
(unless (require 'color-theme-solarized nil 'noerror) (error "Solarized
  missing. Install by 'package-install color-theme-solarized'."))

;; Tester function to find if manually installed files exist
(defun is-file-manually-installed (name)
  (file-exists-p 
   (concat user-emacs-directory 
	   (convert-standard-filename "manual/") 
	   name))

;; Load the custom libraries
(load-library "better-defaults")

;; Configure Marmalade for package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Set Solarized as the color theme
(load-theme 'solarized-dark t)

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
