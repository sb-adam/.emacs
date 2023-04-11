; Required packages should go on top
(require 'company)
(require 'eglot)
(require 'helm)
(require 'helm-projectile)
(require 'multiple-cursors)
(require 'package)
(require 'projectile)
(require 'treemacs)
(require 'treemacs-projectile)
(require 'use-package)
(require 'which-key)

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
   '(which-key treemacs-projectile treemacs multiple-cursors burly all-the-icons-gnus centaur-tabs yasnippet all-the-icons company-box exec-path-from-shell eglot company tide helm-projectile helm projectile magit neotree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-banner-face ((t (:foreground "#FF0000"))))
 '(neo-dir-link-face ((t (:foreground "#0000FF"))))
 '(neo-file-link-face ((t (:foreground "#BA36A5"))))
 '(neo-header-face ((t (:foreground "#FF0000"))))
 '(neo-root-dir-face ((t (:foreground "#8D8D84"))))
 '(neo-tree-entry-face ((t (:background "#d6d6d6"))))
 '(neo-tree-header-face ((t (:background "#d6d6d6")))))

;; Package lists should be added here
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;Reload this file
(defun my-reload-init-file ()
  "Reload the Emacs init file."
  (interactive)
  (load-file "~/.emacs"))

(global-set-key (kbd "C-c e") 'my-reload-init-file)


;; Custom theme
(load-theme 'monokai t)

;; Treemacs config
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t p") 'treemacs-projectile)
(global-set-key (kbd "C-x t a") 'treemacs-add-and-display-current-project)

(setq treemacs-is-never-other-window t)

;; Set treemacs background color
; (setf treemacs-window-background-color (cons "#E6E6E6" "#E6E6E6"))
(defun my-treemacs-customizations ()
  "Customize Treemacs faces."
  (with-current-buffer (treemacs-get-local-buffer)
    (face-remap-add-relative 'default :background "#d6d6d6" :foreground "#262721")
    (face-remap-add-relative 'fringe :background "#d6d6d6")
    (face-remap-add-relative 'treemacs-directory-face :foreground "#262721" :background "#d6d6d6")
    (face-remap-add-relative 'treemacs-root-face :foreground "#262721")
    (face-remap-add-relative 'treemacs-git-untracked-face :foreground "#262721")
    (face-remap-add-relative 'treemacs-custom-file-highlight :background "#d6d6d6" :foreground "#262721")
    (face-remap-add-relative 'hl-line :background "#d6d6d6")
    (face-remap-add-relative 'treemacs-python-tag-face :foreground "#262721")
    (dolist (face '(treemacs-file-face treemacs-tags-face))
      (face-remap-add-relative face :foreground "#262721"))
    (display-line-numbers-mode -1)))

; (add-hook 'treemacs-mode-hook 'my-treemacs-customizations)

;; Open up treemacs when we open a project using projectile
(defun my/treemacs-projectile-setup ()
  (when (projectile-project-p) ; Check if we are in a Projectile project
    (unless (treemacs-get-local-window) ; Check if Treemacs window is not already open
      (treemacs)))) ; Open Treemacs

(add-hook 'projectile-after-switch-project-hook #'my/treemacs-projectile-setup)


;; Set background color neotree ;; Doesn't seem possible
;; (custom-set-faces
;; '(neo-banner-face ((t (:foreground "#FF0000"))))
;; '(neo-root-dir-face ((t (:foreground "#FF0000"))))
;; '(neo-dir-link-face ((t (:foreground "#0000FF"))))
;; '(neo-file-link-face ((t (:foreground "#BA36A5")))))

;; Tab line mode
;; Enable tab-line-mode
(tab-line-mode 1)
(add-hook 'after-init-hook #'tab-line-mode)

;; Set the background color of active tabs
(set-face-attribute 'tab-line-tab-current nil :background "#282c34")

;; Set the foreground color of active tabs
(set-face-attribute 'tab-line-tab-current nil :foreground "#ffffff")

;; Set the background color of inactive tabs
(set-face-attribute 'tab-line-tab-inactive nil :background "#282c34")

;; Set the foreground color of inactive tabs
(set-face-attribute 'tab-line-tab-inactive nil :foreground "#a9a9a9")

;; Set the separator character between tabs
(setq tab-line-separator " ")


;; Multiple cursors config
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-k") 'mc/skip-to-next-like-this)


;; Set the maximum width of tabs before they start to shrink
; (setq tab-line-close-button-show nil)
; (setq tab-line-tab-name-function 'tab-line-tab-label)
; (setq tab-line-tab-label-function 'tab-line-tab-label)


;; Set default frame setting
;; Define a function to create a four-window layout
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 40)))
;; (setq default-frame-alist '((width . 80) (height . 40)))
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

(desktop-save-mode 1)
(setq desktop-restore-frames t) ; Restore frame configuration
(setq desktop-load-locked-desktop t) ; Load the desktop even if it is locked

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(setq initial-buffer-choice t)



;; This will automatically load packages which use use-package and aren't installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Makes sure emacs is using shell $PATH
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x)) ;; Use NVM node
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "NVM_DIR"))



;; Set node version
(setq my-nvm-node-path "/home/avoliva/.nvm/versions/node/v16.20.0/bin/node")
(if (file-exists-p my-nvm-node-path)
    (progn
      (setq my-nvm-bin-path (file-name-directory my-nvm-node-path))
      (setenv "PATH" (concat my-nvm-bin-path ":" (getenv "PATH")))
      (add-to-list 'exec-path my-nvm-bin-path))
  (message "Warning: nvm node not found at %s" my-nvm-node-path))

;; Set default modes
(helm-mode 1)
(global-company-mode 1) ; Add this line
(global-display-line-numbers-mode 1) ; For displaying line numbers

;; Change goto-line to be C-:
(global-set-key (kbd "C-:") 'goto-line)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-keymap-prefix (kbd "C-c C-p"))
(setq helm-projectile-prefix (kbd "C-c p"))


;; Default keybind for projectile is C-x p. I changed it to M-p because it's easier and was not binded
(use-package projectile
	     :ensure t
	     :config
	     (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
	     (projectile-mode +1))


;; This was for opening up neotree automatically when projectile opened a project
;(defun neotree-project-dir ()
;  "Open NeoTree using the project root, using projexctile or the current buffer directory."
;  (interactive)
;  (let ((project-dir (ignore-errors (projectile-project-root)))
;        (file-name (buffer-file-name))
;        (neo-smart-open t))
;    (if project-dir
;        (progn
;          (neotree-dir project-dir))
;      (neotree-find file-name))))
;
;(defadvice projectile-switch-project-by-name (after neotree-projectile-open activate)
;  "Open NeoTree at projectile project root when switching projects."
;  (neotree-project-dir))

;; This is for asking what project on boot. I found it somewhat annoying
;; Ideally I would want something that allows me to select an existing project
;; with my cursor on startup.
;(defun my-relevant-projectile-projects ()
;  (interactive)
;  (let* ((projects (projectile-load-known-projects))
;         (selected-project (completing-read "Select a project: " projects)))
;    (when selected-project
;      (projectile-switch-project-by-name selected-project)
;      (current-buffer))))

;(setq initial-buffer-choice #'my-relevant-projectile-projects)

;; eglot will be the future of autocompletion but right now LSP-ui makes lsp superior
;; Add these lines to modify the company keymap
;; (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
;; (define-key company-active-map (kbd "<return>") nil)
;; (define-key company-active-map (kbd "RET") nil)


;; (setq company-idle-delay 0.2) ; Add this line (optional)
;; (use-package eglot
;;   :ensure t
;;  :config
;;  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) "typescript-language-server" "--stdio")))

;; Add this line to enable eldoc-mode in eglot-managed buffers
;; (add-hook 'eglot--managed-mode-hook 'eldoc-mode)



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

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

;; Optional, if you want prettier icons in the completion menu
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

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

(global-set-key (kbd "C-c w") #'my/close-active-window)


;; Configures which-key keybind
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'bottom)
