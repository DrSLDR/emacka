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

;; Enable line wrapping at column 80 globally
(setq-default fill-column 80)
(setq auto-fill-mode 1)

;; Enable line and column numbering
(line-number-mode 1)
(column-number-mode 1)