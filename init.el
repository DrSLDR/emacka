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
    (convert-standard-filename "packs/"))))
  (setq load-path
    (append
      (let ((load-path (copy-sequence load-path))) ;; Shadow
        (append 
          (copy-sequence (normal-top-level-add-to-load-path '(".")))
          (normal-top-level-add-subdirs-to-load-path)))
      load-path)))

;; Load the custom libraries
(load-library "better-defaults")

;; Enable line wrapping at column 80 globally
(setq-default fill-column 80)
(defun auto-fill-turn-on ()
  (auto-fill-mode 1))
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

;; Enable color-themes
(load-library "color-theme")

;; Set color scheme to solarized dark
(require 'color-theme-solarized)
(load-theme 'solarized-dark t)
