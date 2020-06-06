;; windows specific things
(if (string-equal (getenv "OS") "Windows_NT")
    (begin (require 'server)
           (unless (server-running-p)
             (server-start))
           (setq visible-bell 1)))

;; *scratch*
(setq initial-major-mode 'org-mode)

;; pretty
(set-default-font "Fira Code")

;; ignore this infuriating at-point file search
;; https://emacs.stackexchange.com/a/5331
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)

;(use-package doom-themes
;  :ensure t
;  :init
;  (load-theme 'doom-nova t)
;  :config
;  (doom-themes-org-config))

(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-attractive)
  (global-linum-mode -1)
  (sublimity-mode 1))

(global-prettify-symbols-mode 1)
(defun pretty-lambda-fun ()
  "make mostly for lisps"
  (setq prettify-symbols-alist
      '(("lambda" . 955)  ;; λ
        ;; ("->"     . 8594) ;; →
        ;; ("=>"     . 8658) ;; ⇒
        )))
(add-hook 'racket-mode-hook 'pretty-lambda-fun)
(add-hook 'elisp-mode-hook 'pretty-lambda-fun)
(add-hook 'scheme-mode-hook 'pretty-lambda-fun)


(defun buffer-go ()
  (with-current-buffer (get-buffer "*scratch*")
    (let* ((wh (window-body-height))
           (h (if wh (/ wh 2) 1)))
      (end-of-buffer)
      (delete-region 1 (point))
      (insert (make-string h ?\n))))
  t)
(when window-system
  (setq initial-buffer-choice 'buffer-go))

;; C
(defun my-flycheck-c-setup ()
  (setq flycheck-gcc-language-standard "c11"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

(setq c-default-style "linux" c-basic-offset 2)

(use-package clang-format
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  :config
  (add-hook 'c++-mode-hook
            (function (lambda ()
                        (add-hook 'before-save-hook
                                  'clang-format-buffer)))))

;; haskell
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  :config
  (use-package hlint-refactor
    :ensure t
    :init
    (add-hook 'dante-mode-hook
              '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                '(warning . haskell-hlint))))))
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;; CLOS
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "~/.nix-profile/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; quicklisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; keybinds
(use-package evil
  :ensure t
  :config
  (evil-mode 1))
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(define-key dired-mode-map (kbd "C-c o") 'dired-open-file)

(global-set-key (kbd " ") (lookup-key global-map (kbd " ")))

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 2)
            (setq python-indent 8)))
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

; (load-file (let ((coding-system-for-read 'utf-8))
;                (shell-command-to-string "agda-mode locate")))

(global-set-key "\C-c\C-k" 'describe-char)

; saving sessions
(defun load-desktop-default ()
  (interactive)
  (desktop-change-dir "~/.emacs.d/.#desktop#"))
(defun save-desktop-default ()
  (interactive)
  (desktop-save "~/.emacs.d/.#desktop#"))
(global-set-key (kbd "C-c r l") 'load-desktop-default)
(global-set-key (kbd "C-c r s") 'save-desktop-default)
