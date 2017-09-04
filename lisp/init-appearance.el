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

(load-theme 'zenburn)

;; ;; modeline enhancements
(require 'powerline)
(setq powerline-image-apple-rgb t)
(setq ns-use-srgb-colorspace nil)
(require 'spaceline-config)
(display-time-mode 1)
(setq display-time-format "%H:%M")
(setq spaceline-window-numbers-unicode t)
(setq spaceline-workspace-numbers-unicode t)
(setq-default
   powerline-default-separator 'utf-8
   spaceline-flycheck-bullet "❖ %s"
   spaceline-separator-dir-left '(right . right)
   spaceline-separator-dir-right '(left . left))
(spaceline-install
    'main
    '((workspace-number :face highlight-face)
      (buffer-modified)
      ((remote-host buffer-id) :face region)
      (major-mode)
      (projectile-root)
      (process :when active))
    '((selection-info :face region :when mark-active)
      ((flycheck-error flycheck-warning flycheck-info) :when active)
      (which-function)
      (version-control :when active)
      (line-column)
      (global :when active)
      (mu4e-alert-segment :when active)
      (buffer-position)))
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

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
