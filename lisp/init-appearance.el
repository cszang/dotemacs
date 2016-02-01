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

(if (string= "buck" system-name)
    (progn (set-face-attribute 'default nil :height 131 :font "Inconsolata")
           (setq-default line-spacing 3))
  (progn (set-face-attribute 'default nil :height 131 :font "Monoid")
         (setq-default line-spacing 5)))

(load-theme 'writer)

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
(nyan-mode 1)

(provide 'init-appearance)
