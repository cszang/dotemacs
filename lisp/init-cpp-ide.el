(setq speedbar-show-unknown-files t)

;; (add-hook 'after-init-hook 'global-company-mode)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
;; (add-to-list 'company-backends 'company-c-headers)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")

(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)
(setq cmake-ide-flags-c++
      '("/Library/Developer/CommandLineTools/usr/bin/../include/c++/v1"
        "/usr/local/include"
        "/Library/Developer/CommandLineTools/usr/bin/../lib/clang/8.0.0/include"
        "/Library/Developer/CommandLineTools/usr/include"
        "/usr/include"))

(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(add-hook 'c++-mode-hook 'company-mode)

(provide 'init-cpp-ide)
