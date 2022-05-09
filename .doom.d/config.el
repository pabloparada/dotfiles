;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Whenever Emacs loads some elisp that is not compiled yet,
;; compile it and load it
(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Pablo Parada"
      user-mail-address "pablo.paradabol@gmail.com")

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16)
      doom-big-font (font-spec :family "FiraCode Nerd Font" :size 22))

;; Load theme with `doom' machinery
(setq doom-theme 'gruvbox-dark-medium)
(setq doom-themes-treemacs-theme "doom-colors")

;; Show space and newline characters
(setq global-whitespace-mode nil)

;; Cursor should be blinking
(blink-cursor-mode t)

;; Exclude some styles from whitespacing
(setq whitespace-style
      '(face
        spaces
        tabs
        newline
        space-mark
        tab-mark
        newline-mark))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Delete files to trash
(setq-default delete-by-moving-to-trash t)

;; Take new window space from all other windows (not just current)
(setq-default window-combination-resize t)

;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

;; Raise undo-limit to 80mb
(setq undo-limit 80000000)

;; Auto-save by default
(setq auto-save-default t)

;; Apply `rainbow-mode' to every buffer
(add-hook! (text-mode prog-mode)
           ;; Ignore `x-colors' (e.g red, blue, green)
           (setq rainbow-x-colors nil)

           ;; Enable `rainbow-mode' for hex/rgb (e.g: #ffffff)
           (rainbow-mode t))

;; Configure `hydras' utilities
(after! hydra
  (defhydra hydra-zoom (:color red)
    "Zooming"
    ("+" (text-scale-increase 1) "Increase")
    ("-" (text-scale-decrease 1) "Decrease")
    ("/" (text-scale-adjust 0) "Reset")
    ("q" nil "Quit"))

  (defhydra hydra-window-scale (:color yellow)
    "Zooming"
    (">" (evil-window-increase-width 1) "Increase Width")
    ("+" (evil-window-increase-height 1) "Increase Height")
    ("<" (evil-window-decrease-width 1) "Decrease Width")
    ("-" (evil-window-decrease-height 1) "Decrease Height")
    ("=" (balance-windows) "Balance")
    ("q" nil "Quit"))

  (defun hydra-zoom()
    "Run `hydra-zoom/body'."
    (interactive)
    (hydra-zoom/body))

  (defun hydra-window-scale()
    "Run `hydra-window-scale/body'."
    (interactive)
    (hydra-window-scale/body)))

(after! evil
  ;; By default while in insert all changes are one big blob, be more granular
  (setq evil-want-fine-undo t)

  ;; s/../.. global by default
  (setq evil-ex-substitute-global t)

  ;; Don't move the block cursor when toggling insert mode
  (setq evil-move-cursor-back nil)

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil)

  ;; Don't let cursor move beyond end of line
  (setq evil-move-beyond-eol nil)

  ;; Move to window after splitting
  (setq evil-vsplit-window-right t
        evil-split-window-below t)

  ;; Yay, comment and uncomment lines easily
  (map! :map evil-normal-state-map
        :desc "(Un)comment lines" "M-/" #'evilnc-comment-or-uncomment-lines)

  ;; Map `zoom' `hydra' to `<F2>'
  (map! :map (evil-normal-state-map evil-insert-state-map)
        :desc "Zoom In Out" "<f2>" 'hydra-zoom)

  ;; Map `window-scale' `hydra' to `<F5>'
  (map! :map (evil-normal-state-map evil-insert-state-map)
        :desc "Zoom In Out" "<f5>" 'hydra-window-scale)

  ;; Replace mapping for `macro-recording' (q) with `+workspace/close-window-or-workspace'
  (map! :map evil-normal-state-map
        :desc "Delete Window" "q" '+workspace/close-window-or-workspace)

  ;; Make `ctrl-d' exit insert-mode
  (map! :map evil-insert-state-map
        :desc "Switch to normal state" "C-d" 'evil-force-normal-state)

  ;; Do not yanking for `backward' and `forward' deletion
  (map! :map evil-normal-state-map
        :desc "Forward's deletion without yanking to killring" "x" 'delete-forward-char
        :desc "Backward's deletion without yanking to killring" "X" 'delete-backward-char)

  ;; Ask which buffer user's want to see after splitting windows
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (consult-buffer)))

;; Make `ctrl-d' exit insert-mode in `markdown-mode'
(map! :after evil-markdown
      :map evil-markdown-mode-map
      :desc "Switch to normal state" :i "C-d" 'evil-force-normal-state)

;; Tweak `embark'
(after! embark
  (defun +custom/filter-devdoc-by-name(name devdoc)
    "Return `devdoc' when name matches, otherwise empty list."
    (if (rassoc name devdoc)
        devdoc
      '()))

  (defun +custom/find-devdoc-entry-by-name(name devdocs)
    "Filter `devdocs' by it's name returning the first match."
    (let* ((devdoc-entries (mapcar (apply-partially '+custom/filter-devdoc-by-name name) devdocs))
           (devdoc-non-nill-entries (remove nil devdoc-entries)))
      (car devdoc-non-nill-entries)))

  (defun +custom/embark-devdoc-peruse (_arg)
    "Invoke `devdocs-peruse' with `devdocs--installed-docs' filtered by `thing-at-point'."
    (let* ((action (thing-at-point 'word 'no-properties))
           (devdoc_entry (+custom/find-devdoc-entry-by-name action (devdocs--installed-docs))))
      (devdocs-peruse devdoc_entry)))

  ;; Customize embark maps to easily invoke devdocs
  ;; with `embark-act'
  (map! :map (embark-symbol-map
              embark-variable-map
              embark-function-map
              embark-identifier-map)
        :desc "Devdoc peruse embark symbol" "1" '+custom/embark-devdoc-peruse))

;; Tweak `devdocs'
(after! devdocs
  ;; Better `devdocs' key bindings
  (map! :map devdocs-mode-map
        :desc "Next devdocs page" "C-j" 'devdocs-next-page
        :desc "Previous devdocs page" "C-k" 'devdocs-previous-page
        :desc "Goto page" "C-s" 'devdocs-goto-page))

;; Tweak `vertico'
(after! vertico
  ;; Don't be annoying, lets complete searching with `ctrl-d'
  (map! :map vertico-map
        :desc "Abort search" "C-d" 'abort-minibuffers))

;; Configure `real-auto-save' for `prog-mode' with 5 seconds interval
(use-package! real-auto-save
  :config
  (add-hook! 'prog-mode-hook 'real-auto-save-mode)
  (setq real-auto-save-interval 5))

;; Remaps word-based editing commands to subword-based commands that handle
;; symbols with mixed uppercase and lowercase
;; letters, e.g. "GtkWidget", "EmacsFrameClass", "NSGraphicsContext"
(global-subword-mode 1)

;; Better delay for `which-key'
(setq which-key-idle-delay 0.3)

;; Recursively search for projects in home dir
(setq projectile-project-search-path '(("~/projects" . 2)))

;; Show `perspective' name in modeline
(setq doom-modeline-persp-name t)

;; Disable `perspective' modeline icon
(setq doom-modeline-persp-icon nil)

;; Customize `dired' defaults
(after! dired
  (defun +custom/dired-default-config ()
    "Customize `dired' defaults"
    (dired-hide-details-mode)
    (dired-sort-toggle-or-edit))

  ;; Enable subtree toggling for `dired'
  (map! :map dired-mode-map
        :desc "Toggle dired subtree" "TAB" 'dired-subtree-toggle)

  (add-hook! 'dired-mode-hook '+custom/dired-default-config)
  (add-hook! 'dired-mode 'dired-async-mode))

;; Better defaults for company completions
(after! company
  ;; Configure completion delay
  (setq company-idle-delay 0.5)

  ;; Wait for the same amount than `company' to show docs
  (setq company-posframe-quickhelp-delay 0.2)

  ;; Try to list at least 30 completions
  (setq company-tooltip-limit 10)

  ;; Generate text icons background if none
  (setq company-text-icons-add-background t)

  ;; Disable `vs-code-icons' and use text icons instead
  (setq company-format-margin-function 'company-text-icons-margin)

  ;; Better separators for `posframe'
  (setq company-posframe-backend-separator " | ")

  ;; Don't show metadata
  (setq company-posframe-show-metadata nil)

  ;; Launch company completion with at least two keys
  (setq company-minimum-prefix-length 2)

  ;; Wrap around when reaching completion list end
  (setq company-selection-wrap-around t)

  ;; Use others buffers for `company-dabbrev' completion
  (setq company-dabbrev-other-buffers t)

  ;; Ignore case sensitive when looking for `company-abbrev' completions
  (setq company-dabbrev-ignore-case t)

  ;; Ignore some files in `company' suggestions
  (setq company-files-exclusions '(".git/" ".DS_Store"))

  ;; Better coloring for completion previews
  (custom-set-faces '(company-preview ((t (:background "#282828" :foreground "#d787af")))))

  ;; Activate `company-quickhelp'
  (company-posframe-mode t)

  ;; Configure `company-frontends'
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend))

  ;; All `company-backends' are globally set and shouldn't be modified
  (setq +company-backend-alist nil)

  ;; Configure `company-backends'
  (setq company-backends '((company-keywords
                            company-capf
                            company-dabbrev-code
                            company-files
                            company-ispell
                            company-yasnippet
                            company-abbrev)))

  ;; Configure `+lsp-company-backends'
  ;; Do not override previously configured `company-backends'
  (setq +lsp-company-backends '(company-keywords
                                company-capf
                                company-dabbrev-code
                                company-files
                                company-ispell
                                company-yasnippet
                                company-abbrev))

  ;; Map `+company/complete' to `C-SPC' when in insert-mode
  (map! :map evil-insert-state-map
        :desc "Invoke Company completion when in insert mode" "C-SPC" '+company/complete)

  ;; Map `company-complete-selection' to `<tab>' when in company-active-mode
  (map! :map company-active-map
        :desc "Insert the selected Company candidate" "<tab>" 'company-complete-selection
        :desc "Close active Company" "C-d" 'company-abort))

;; Make manual pages nicer to look at
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; Tweak `yasnippets'
(after! yasnippet
  ;; Enable nested snippet triggers
  (setq yas-triggers-in-field t))

;; Tweak `flyspell'
(after! flyspell
  ;; Configure `company' word-list dictionary
  (setq ispell-alternate-dictionary (expand-file-name ".ispell_word_list_en-US" doom-private-dir))

  ;; Configure custom `ispell' dictionary
  (setq ispell-dictionary "en-custom")

  ;; Keep personal dictionary under `doom' private dir
  (setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  ;; No mouse, please, use vertico instead
  (use-package! flyspell-correct
    :config
    (map! :map flyspell-mode-map
          :desc "Correct word with vertico" "M-RET" 'flyspell-correct-wrapper)))

;; Tweak `vterm'
(after! vterm
  ;; Do not wait, refresh quickly
  (setq vterm-timer-delay 0.001))

;; Tweak `lsp-java'
(after! lsp-java
  ;; Enable code lens for references
  (setq lsp-java-references-code-lens-enabled t)

  ;; Enable code lens for implementations
  (setq lsp-java-implementations-code-lens-enabled t)

  ;; Set third-party decompiler
  (setq lsp-java-content-provider-preferred "fernflower"))

;; Tweak `lsp-mode'
(after! lsp-mode
  ;; Integrate lsp with treemacs
  (setq lsp-signature-auto-activate nil)

  ;; Show code actions
  (setq lsp-ui-sideline-show-code-actions t)

  ;; Launch docs faster
  (setq lsp-ui-doc-delay 0.2)

  ;; Map `dap-hydra' in lsp category
  (map! :map lsp-command-map
        :desc "DAP Hydra" "d" 'dap-hydra)

  ;; Map `lsp-avy-lens' to `SPC l'
  (map! :leader
        :desc "Invoke Avy for LSP Code Lens" "l" 'lsp-avy-lens))

;; Tweak `treemacs'
(after! (lsp-mode treemacs)
  ;; Reduce `file-follow'
  (setq treemacs-file-follow-delay 0.01)

  ;; Make `treemacs' follow buffer movement
  (treemacs-follow-mode t)

  ;; Keep `treemacs' in sync with lsp
  (lsp-treemacs-sync-mode t))

;;Ensure `left-only' `fring-mode'
(after! lsp-ui
  (set-fringe-mode '(nil . 0)))

(after! dap-mode
  ;; Configure `dap' output window height
  (setq dap-output-window-max-height 20)
  (setq dap-output-window-min-height 20)

  ;; Disable debug controls
  (dap-ui-controls-mode nil)

  ;; Show only `locals' during `dap' debugging
  (setq dap-auto-configure-features '(locals))

  (defun +custom/interactively-dap-hydra (_session)
    "Call `dap-hydra' interactively."
    (call-interactively #'dap-hydra))

  ;; Launch `dap-hydra' when stopping at breakpoint
  (add-hook! 'dap-stopped-hook '+custom/interactively-dap-hydra)

  ;; Use `debugpy' instead of deprecated `ptsv'
  (setq dap-python-debugger 'debugpy)

  (defun +custom/dap-pyenv-executable-find (_command)
    "Infer `.venv' python path to be used during `dap-python'
invocation. This function will try to locate a `.venv' dir within current
directory, otherwise fallback to `projectile-project-root'."
    (let* ((buffer_dir (file-name-directory (buffer-file-name)))
           (buffer_venv_dir (concat buffer_dir ".venv"))
           (venv_dir nil))
      (if (file-directory-p buffer_venv_dir)
          (setq venv_dir buffer_venv_dir)
        (setq venv_dir (projectile-project-root)))
      (concat venv_dir "/bin/python")))

  ;; Replace default `dap-python--pyenv-executable-find' by
  ;; our function making `dap' successfully recognize virtual envs
  (advice-add 'dap-python--pyenv-executable-find :override #'+custom/dap-pyenv-executable-find))
