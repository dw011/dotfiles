;; Buffer-local defaults; C-h v for more info
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level 2)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq-default sh-indentation 2)

;; Global settings
(load-theme 'wheatgrass)
(setq auto-save-default nil)
(setq default-major-mode 'text-mode)
(setq frame-title-format (concat  "%b - " (system-name)))
(setq inhibit-startup-screen t)
(setq ispell-program-name "aspell")
(setq make-backup-files nil)
(setq mouse-drag-copy-region t)
(setq next-line-add-newlines nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq visible-bell t)
(menu-bar-mode -1)
(auto-fill-mode 1)
(global-font-lock-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)

;; Global mode line settings; C-h v mode-line-format for more info
(setq mode-line-modes nil)
(column-number-mode 1)
(line-number-mode 1)
(size-indication-mode 1)

;; Global hooks
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Packages and MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package `(elpy projectile))
  (unless (package-installed-p package)
    (package-install package)))

;; Python
;; Note after initial installation of elpy, run `M-x elpy-config` to
;; setup required python packages.
;; See https://elpy.readthedocs.io/en/latest/introduction.html
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-timeout 10)
(add-hook 'elpy-mode-hook
          (lambda () (add-hook 'before-save-hook 'elpy-format-code nil t)))
(projectile-mode 1)

;; Key bindings
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Play nicely with window managers, X11, etc.
(when window-system
  (setq x-alt-keysym 'meta)
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary nil)
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 100))
  (set-scroll-bar-mode 'right)
  (tool-bar-mode -1)
  (add-hook 'prog-mode-hook 'linum-mode))

;; Play nicely with terminal
(unless window-system
  (xterm-mouse-mode 1))

;; Play nicely with macOS and MacPorts
;; Note macOS doesn't source shell startup scripts on login
(when (string-equal system-type "darwin")
  (setq exec-path (append '("/opt/local/bin") exec-path))
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(projectile elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
