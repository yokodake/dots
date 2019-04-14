;; pretty
(set-default-font "Fira Code")

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-vibrant t)
  :config
  (doom-themes-org-config))

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
    (end-of-buffer)
    (delete-region 1 (point))
    (insert "\n\n\n\n\n"))
  t)
(when window-system
  (setq initial-buffer-choice 'buffer-go))

;; C
(defun my-flycheck-c-setup ()
  (setq flycheck-gcc-language-standard "c11"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

(setq c-default-style "linux" c-basic-offset 4)

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

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
