#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias cls='clear && ls'
alias yay-clean='yay -Sc'
alias xmod='xmodmap ~/.Xmodmap'
alias slippi='~/media/games/slippi/slippi-launcher.AppImage & disown & exit'
alias panels='ifuse --documents es.produkt.app.panels /mnt/iphone; thunar /mnt/iphone & disown; thunar ~/media/comics & disown; exit'
alias unpanels='fusermount -u /mnt/iphone'
alias sleepy='systemctl suspend'
alias pi='ssh pi@192.168.1.28'

# PS1='[\u@\h \W]\$ '
export PS1="\e[0;36m\]——————————————————————\[\e[m\]\n[\e[0;34m\]\u \w\[\e[m\]] \n\[\e[0;36m\]=> λ\[\e[m\] "
PATH=$PATH:"/home/thain/.config/chemacs/doom/bin":"/home/thain/.local/bin":"/home/thain/.config/shscripts"
export EDITOR=vim

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export TERM=ansi

#if [ -z "${DISPLAY}"  ] && [ "${XDG_VTNR}" -eq 1 ]; then
#    exec startx
#fi
