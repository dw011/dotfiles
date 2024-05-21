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
(setq make-backup-files nil)
(setq mouse-drag-copy-region t)
(setq next-line-add-newlines nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq visible-bell t)
(menu-bar-mode -1)
(auto-fill-mode 1)
(fido-vertical-mode 1)
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
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (setq-local completion-styles '(basic substring))))

;; Packages and MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package `(csv-mode elpy projectile))
  (unless (package-installed-p package)
    (package-install package)))

;; Python
;; Note after initial installation of elpy, run `M-x elpy-config` to
;; setup required python packages.
;; See https://elpy.readthedocs.io/en/latest/introduction.html
(elpy-enable)
(setq elpy-project-ignored-directories (cons ".*" nil))
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-timeout 10)
(setq elpy-rpc-ignored-buffer-size 512000)
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
(global-set-key (kbd "C-<up>") 'scroll-down-command)
(global-set-key (kbd "C-<down>") 'scroll-up-command)
(global-set-key (kbd "C-<left>") 'move-beginning-of-line)
(global-set-key (kbd "C-<right>") 'move-end-of-line)
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "ESC <up>") 'backward-paragraph)
(global-set-key (kbd "ESC <down>") 'forward-paragraph)
(global-set-key (kbd "ESC <left>") 'backward-word)
(global-set-key (kbd "ESC <right>") 'forward-word)
(global-set-key (kbd "C-c C-f") 'elpy-find-file)
(global-set-key (kbd "C-c C-s") 'elpy-rgrep-symbol)

(define-key elpy-mode-map (kbd "C-<up>") 'scroll-down-command)
(define-key elpy-mode-map (kbd "C-<down>") 'scroll-up-command)
(define-key elpy-mode-map (kbd "C-<left>") 'move-beginning-of-line)
(define-key elpy-mode-map (kbd "C-<right>") 'move-end-of-line)
(define-key elpy-mode-map (kbd "M-<up>") 'backward-paragraph)
(define-key elpy-mode-map (kbd "M-<down>") 'forward-paragraph)
(define-key elpy-mode-map (kbd "M-<left>") 'backward-word)
(define-key elpy-mode-map (kbd "M-<right>") 'forward-word)
(define-key elpy-mode-map (kbd "ESC <up>") 'backward-paragraph)
(define-key elpy-mode-map (kbd "ESC <down>") 'forward-paragraph)
(define-key elpy-mode-map (kbd "ESC left>") 'backward-word)
(define-key elpy-mode-map (kbd "ESC <right>") 'forward-word)
(define-key elpy-mode-map (kbd "ESC p") 'elpy-nav-move-line-or-region-up)
(define-key elpy-mode-map (kbd "ESC n") 'elpy-nav-move-line-or-region-down)
(define-key elpy-mode-map (kbd "ESC b") 'elpy-nav-indent-shift-left)
(define-key elpy-mode-map (kbd "ESC f") 'elpy-nav-indent-shift-right)

;; Mode hooks
(add-hook 'csv-mode-hook
          (lambda ()
            (csv-align-mode 1)
            (csv-header-line)
            (remove-hook 'before-save-hook 'whitespace-cleanup)))
;; (add-hook 'elpy-mode-hook
;;           (lambda () (add-hook 'before-save-hook 'elpy-format-code nil t)))
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local linum-format "%d ")
            (linum-mode 1)))

;; Play nicely with window managers, X11, etc.
(when window-system
  (setq x-alt-keysym 'meta)
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary nil)
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 100))
  (set-scroll-bar-mode 'right)
  (tool-bar-mode -1))

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
