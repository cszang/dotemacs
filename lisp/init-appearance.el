(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(add-to-list 'default-frame-alist '(height . 34))
(add-to-list 'default-frame-alist '(width . 84))

(setq mouse-avoidance-mode 'banish)
(setq mouse-yank-at-point t)
(mouse-wheel-mode t)

(tool-bar-mode -1)
(if (string= "darwin" system-type)
    (menu-bar-mode 1)
  (menu-bar-mode -1))
(tooltip-mode -1)
(scroll-bar-mode -1)

(if (string= "buck" (system-name))
    (progn (set-face-attribute 'default nil :height 131 :font "Inconsolata")
           (setq-default line-spacing 3))
  (progn (set-face-attribute 'default nil :height 151 :font "Input")
         (setq-default line-spacing 5)))

(load-theme 'nord)

;; modeline enhancements
(require 'powerline)
(setq powerline-image-apple-rgb t)
(require 'spaceline-config)
(require 'spaceline-all-the-icons)
(spaceline-all-the-icons-theme)
(setq spaceline-all-the-icons-separator-type "arrow")
(spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
(spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
(spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
(spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line
(require 'yahoo-weather)
(yahoo-weather-mode t)
(setq yahoo-weather-location "Freising")
(setq spaceline-all-the-icons-clock-always-visible nil)
(spaceline-toggle-all-the-icons-weather-on)

(setq shift-select-mode nil)
(setq uniquify-buffer-name-style 'forward)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)
(set-default 'indicate-empty-lines t)

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key [f11] 'switch-full-screen)

(setq global-visual-line-mode t)

(setq smex-prompt-string "I love you. ")

(beacon-mode 1)

(provide 'init-appearance)
