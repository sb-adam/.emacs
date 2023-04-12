;; Package lists should be added here
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This will automatically load packages which use use-package and aren't installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure t
  :init (global-company-mode)
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

;; Optional, if you want prettier icons in the completion menu
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Makes sure emacs is using shell $PATH
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x)) ;; Use NVM node
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "NVM_DIR"))

(use-package helm
  :ensure t
  :config (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :after (projectile)
  :bind (("C-c p" . helm-projectile))
  :config
  (helm-projectile-on))

;; LSP and autocompletion stuff
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred))
  :config
  (setq lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil) ; Disable sideline if you don't want it
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point))

(use-package multiple-cursors
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (projectile-mode +1))

(use-package treemacs
  :ensure t
  :bind (("C-x t t" . treemacs)
         ("C-x t p" . treemacs-projectile)
         ("C-x t a" . treemacs-add-and-display-current-project))
  :config
  (setq treemacs-is-never-other-window t))
(setq treemacs-icon-set 'default)


;; Treemacs config
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t p") 'treemacs-projectile)
(global-set-key (kbd "C-x t a") 'treemacs-add-and-display-current-project)
(setq treemacs-is-never-other-window t)

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom))

;; Set to maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Don't touch this stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(which-key treemacs-projectile treemacs multiple-cursors company-box exec-path-from-shell company helm-projectile helm projectile magit)))


;;Reload this file
(defun my-reload-init-file ()
  "Reload the Emacs init file."
  (interactive)
  (load-file "~/.emacs"))

(global-set-key (kbd "C-c e") 'my-reload-init-file)

;; Custom theme
(load-theme 'monokai t)

(global-display-line-numbers-mode 1) ; For displaying line numbers

;; Change goto-line to be C-:
(global-set-key (kbd "C-:") 'goto-line)

;; Opens a shell
(defun my/open-shell-in-bottom ()
  "Open a shell in a window at the bottom of the frame."
  (interactive)
  (let ((buffer (shell)))
    (switch-to-buffer (other-buffer buffer))
    (split-window-below)
    (windmove-down)
    (switch-to-buffer buffer)))


(global-set-key (kbd "C-c s") #'my/open-shell-in-bottom)

;; Closes the current active window
(defun my/close-active-window ()
  "Close the active window and kill its buffer without prompting."
  (interactive)
  (let ((current-buffer (current-buffer))
        (kill-buffer-query-functions ; Bypass process exit confirmation
         (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (delete-window (selected-window))
    (kill-buffer current-buffer)))

(global-set-key (kbd "C-c w") #'my/close-active-window);; Set default frame setting


;; Define a function to create a four-window layout
(defun my-startup-layout ()
  "Set up the startup layout."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 2)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window 1)
  (switch-to-buffer "*Messages*")
  (other-window 1)
  (switch-to-buffer (other-buffer))
  (balance-windows))

;; Going to try removing this to see if it helps with desktop restoration
(add-hook 'emacs-startup-hook 'my-startup-layout)

; (desktop-save-mode 1)
; (setq desktop-restore-frames t) ; Restore frame configuration
; (setq desktop-load-locked-desktop t) ; Load the desktop even if it is locked

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(setq initial-buffer-choice t)



;; Set node version
(setq my-nvm-node-path "~/.nvm/versions/node/v18.15.0/bin/node")
(if (file-exists-p my-nvm-node-path)
    (progn
      (setq my-nvm-bin-path (file-name-directory my-nvm-node-path))
      (setenv "PATH" (concat my-nvm-bin-path ":" (getenv "PATH")))
      (add-to-list 'exec-path my-nvm-bin-path))
  (message "Warning: nvm node not found at %s" my-nvm-node-path))




