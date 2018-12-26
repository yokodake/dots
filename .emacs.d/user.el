;; pretty
(set-default-font "Fira Code")

(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-attractive)
(sublimity-mode 1)
(global-linum-mode -1)

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

;; C
(defun my-flycheck-c-setup ()
  (setq flycheck-gcc-language-standard "c11"))
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

(setq c-default-style "linux" c-basic-offset 4)

;; keybinds
(require 'evil)
(evil-mode 1)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
