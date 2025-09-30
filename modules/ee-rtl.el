;;; ee-rtl.el --- RTL language support (Farsi, Arabic) -*- lexical-binding: t; -*-

;; Set RTL languages font
(when (display-graphic-p)
  ;; Vazirmatn for Farsi/Arabic text (both use Arabic script)
  (set-fontset-font t 'arabic "Vazirmatn"))

(provide 'ee-rtl)
;;; ee-rtl.el ends here