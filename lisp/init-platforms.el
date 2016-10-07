(cond
 ((string-match "darwin" system-configuration)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq magit-git-executable "/usr/bin/git")
  (setq mac-option-modifier nil)))

(cond
 ((string-match "mingw" system-configuration)
  ;; to be _absolutely_ shure, we use unix encoding in all files
  (set-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-default default-buffer-file-coding-system 'utf-8-unix)
  ;; some thing concerning the PATH to mark magit and spell-checking work correctly
  (setq exec-path (append exec-path '("C:/Program Files (x86)/Git/bin")))
  (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe")
  (setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
  ;; make sure firefox is found
  (setq browse-url-firefox-program "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
  (setq browse-url-generic-program "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
  ;; make org-babel find R on Windows
  (setq org-babel-R-command "C:/Progra~1/R/R-3.2.1/bin/x64/R.exe --slave --no-save")
  ;; add Python installation and scripts to the PATH
  (setq exec-path (append exec-path '("C:/Python27")))
  (setq exec-path (append exec-path '("C:/Python27/Scripts")))
  ))

(provide 'init-platforms)
