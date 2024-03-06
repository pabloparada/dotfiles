# Create virtual env inheriting system packages
function venv_create
    python -m venv .venv --system-site-packages
end

# Activate venv within default venv path
function venv_activate
    source "$(pwd)/.venv/bin/activate.fish"
end

# Function for taking the first 'n' lines
# ex: seq 10 | take 5
# results: prints only the first 5 lines
function take --argument number
    head -$number
end

function merge_k8s_config --argument new_config_path
    cp ~/.kube/config ~/.kube/config.bak
    KUBECONFIG="~/.kube/config:$new_config_path" kubectl config view --flatten > /tmp/config
    mv /tmp/config ~/.kube/config
    rm -rf /tmp/config
end

# Function for ignoring the first 'n' lines
# ex: seq 10 | skip 5
# results: prints everything but the first 5 lines
function skip --argument n
    tail +(math 1 + $n)
end

# Function for copying files and directories, even recursively.
# ex: copy DIRNAME LOCATIONS
# result: copies the directory and all of its contents.
function copy
    set count (count $argv | tr -d \n)
    if test "$count" = 2; and test -d "$argv[1]"
        set from (echo $argv[1] | trim-right /)
        set to (echo $argv[2])
        command cp -r $from $to
    else
        command cp $argv
    end
end

# Function for creating a backup file
# ex: backup file.txt
# result: copies file as file.txt.bak
function backup --argument filename
    cp $filename $filename.bak
end

source ~/.asdf/asdf.fish

set -gx _JAVA_AWT_WM_NONREPARENTING 1
set -gx PATH "$PATH:$HOME/.local/bin:$HOME/.emacs.d/bin:$HOME/.cargo/bin"
set -gx PIP_REQUIRE_VIRTUALENV true
set -gx SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.socket"
set -gx EDITOR "nvim"
set -gx VISUAL "nvim"

starship init fish | source
direnv hook fish | source

alias v='nvim'
alias vim='nvim'
alias ll='exa -lha --color=always --group-directories-first'
alias cp='cp -i'
alias ys='yay -s'
alias df='df -h'
alias free='free -m'
alias rick='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'
alias c='bat'
alias gs='git switch'
alias gt='git status'
alias ga='git add -p'
alias gc='git commit -v'
alias gl='git log'
alias gb='git switch -c'
alias gp='git push'
alias gf='git fetch'
alias xclip='xclip -selection c'
alias screenshot='maim -s | xclip -selection clipboard -t image/png'

set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border="none" --height=50% --preview-window="wrap,border-sharp" --prompt=" " --marker=" " --pointer="*" --bind "j:down,k:up,ctrl-j:preview-down,ctrl-k:preview-up"'

fzf_configure_bindings --directory=\ct --variables=

set fzf_preview_file_cmd cat
set fzf_preview_dir_cmd exa --all --color=always
set fzf_fd_opts --hidden --exclude=.git --exclude=.cache --exclude=.local

set FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS" \
" --color=bg+:#3c3836,bg:#32302f,spinner:#8ec07c,hl:#83a598" \
" --color=fg:#bdae93,header:#83a598,info:#fabd2f,pointer:#8ec07c" \
" --color=marker:#8ec07c,fg+:#ebdbb2,prompt:#fabd2f,hl+:#83a598"
