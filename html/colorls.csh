# color-ls initialization
set COLORS=/etc/DIR_COLORS
eval `dircolors -c /etc/DIR_COLORS`

# DNM 11/14/00: modified this from a single test statement with
# && on the eval and set, because it gave error and exited when
# csh is invoked with -e
if (-f ~/.dircolors) then
    eval `dircolors -c ~/.dircolors`
    set COLORS=~/.dircolors
endif

# DNM: This statement had no effect EXCEPT to exit when csh is
# invoked with -e, so leave it out
#egrep -i "^COLOR.*none" $COLORS

if ( $? != 0 ) then
alias ll 'ls -l --color=tty'
alias l. 'ls -d .[a-zA-Z]* --color=tty'
alias ls 'ls --color=tty'
else
alias ll 'ls -l'
alias l. 'ls -d .[a-zA-Z]*'
endif
