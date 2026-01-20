;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Frame settings with asymmetric fringes and internal border
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars)
        (left-fringe . 8)
        (right-fringe . 13)
        (internal-border-width . 5)
        (fullscreen . maximized)))
