;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company               ; the ultimate code completion backend
       (vertico +icons)      ; the search engine of the future

       :ui
       doom                  ; what makes DOOM look the way it does
       doom-dashboard        ; a nifty splash screen for Emacs
       doom-quit             ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)      ; 🙂
       hl-todo               ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       (ligatures            ; ligatures and symbols to make your code pretty again
        +extras
        +fira)
       modeline              ; snazzy, Atom-inspired modeline, plus API
       nav-flash             ; blink cursor line after big motions
       ophints               ; highlight the region an operation acts on
       (popup                ; tame sudden yet inevitable temporary windows
        +all
        +defaults)
       (treemacs +lsp)       ; a project drawer, like neotree but cooler
       vc-gutter             ; vcs diff in the fringe
       vi-tilde-fringe       ; fringe tildes to mark beyond EOB
       workspaces            ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)    ; come to the dark side, we have cookies
       file-templates        ; auto-snippets for empty files
       fold                  ; (nigh) universal code folding
       format                ; automated prettiness
       multiple-cursors      ; editing in many places at once
       snippets              ; my elves. They type so I don't have to

       :emacs
       (dired +icons)        ; making dired pretty [functional]
       electric              ; smarter, keyword-based electric-indent
       (ibuffer +icons)      ; interactive buffer management
       (undo +tree)          ; persistent, smarter undo for your inevitable mistakes
       vc                    ; version-control and Emacs, sitting in a tree

       :term
       vterm                 ; the best terminal emulation in Emacs

       :checkers
       syntax                ; tasing you for every semicolon you forget
       (spell                ; tasing you for misspelling mispelling
        +flyspell
        +aspell
        +everywhere)
       (grammar              ; tasing grammar mistake every you make
        +writegood-mode)

       :tools
       (debugger +lsp)       ; stepping through code, to help you add bugs
       (docker +lsp)         ; enables integration for the Dockerfile Language Server
       (eval +overlay)       ; run code, run (also, repls)
       (lsp +peak)           ; language servers
       magit                 ; a git porcelain for Emacs
       pdf                   ; pdf enhancements
       rgb                   ; creating color strings
       lookup                ; navigate your code and its documentation
       (direnv +envrc)       ; integrates direnv into Emacs

       :os
       (:if IS-MAC macos)    ; improve compatibility with macOS
       tty                   ; improve the terminal Emacs experience

       :lang
       (cc +lsp)             ; C > C++ == 1
       (elixir +lsp)         ; erlang done right
       emacs-lisp            ; drown in parentheses
       erlang                ; an elegant language for a more civilized age
       (go +lsp)             ; the hipster dialect
       (graphql +lsp)        ; Give queries a REST
       (haskell +lsp)        ; a language that's lazier than I am
       json                  ; At least it ain't XML
       (java +lsp)           ; the poster child for carpal tunnel syndrome
       (javascript +lsp)     ; all(hope(abandon(ye(who(enter(here))))))
       (kotlin +lsp)         ; a better, slicker Java(Script)
       markdown              ; writing docs for people to ignore
       (org                  ; organize your plain life in plain text
        +pretty              ; yessss my pretties! (nice unicode symbols)
        +dragndrop           ; drag & drop files/images into org buffers
        +pandoc              ; export-with-pandoc support
        +gnuplot             ; who doesn't like pretty pictures
        +present             ; using org-mode for presentations
        +roam2)              ; wander around notes
       (python               ; beautiful is better than ugly
        +lsp
        +pyright)
       (rust +lsp)           ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scala +lsp)          ; java, but good
       sh                    ; she sells {ba,z,fi}sh shells on the C xor
       (yaml +lsp)           ; JSON, but readable

       :config
       (default +bindings +smartparens))
