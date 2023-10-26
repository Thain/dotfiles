# this file is last of my conf files. for interactive shells,
# meaning it should have behavior and appearance for when
# I've opened an interactive shell window.
# Avoid putting anything time-intensive here.

# variables controlling behaviour
export HISTFILE=~/.config/zsh/histfile
export HISTSIZE=1000
export SAVEHIST=1000

# variables controlling appearance
# export PS1=$'\033[36m——————————————————————
# \033[37m[\033[34m%n %~\033[37m]
# \033[36m=> λ\033[37m '
PROMPT="%F{white}[%F{blue}%~%F{white}]
%F{cyan}λ %f"

# show directories in bold light blue
LS_COLORS="di=01;94"

# show symbolic links in cyan
LS_COLORS="${LS_COLORS}:ln=00;36"

# show broken symbolic links in light red
LS_COLORS="${LS_COLORS}:or=00;91"

# show executable files in light green
LS_COLORS="${LS_COLORS}:ex=00;92"

export LS_COLORS

setopt autocd extendedglob nomatch

# dont beep on error
unsetopt beep
bindkey -e
# for vim keybinds. was buggy and probably needs more tweaking somehow
# bindkey -e

# my aliases
alias ls='ls --color=auto'
alias la='ls -la'
alias rmd='rm -rf'
alias cls='clear && ls'
alias yay-clean='yay -Sc'
alias panels='ifuse --documents es.produkt.app.panels /mnt/iphone; thunar /mnt/iphone & disown; thunar ~/media/comics & disown; exit'
alias unpanels='fusermount -u /mnt/iphone'
alias sleepy='systemctl suspend'
alias pi='ssh pi@192.168.1.28'
alias oldie='ssh 192.168.1.167'

# command autocompletion
zstyle :compinstall filename '/home/thain/.config/zsh/.zshrc'

autoload -Uz compinit
compinit
