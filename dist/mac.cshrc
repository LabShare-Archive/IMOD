# To set the environment for using IMOD, insert this command in the user's
# .cshrc or a system cshrc file (if # /etc/csh.login has a command to set path
# absolutely, it has to go after that command in /etc/csh.login
#
# It assumes that IMOD is located in /usr/local - if not, modify

if (-e /usr/local/IMOD/IMOD-mac.csh) source /usr/local/IMOD/IMOD-mac.csh
