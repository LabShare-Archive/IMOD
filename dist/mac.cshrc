# To set the environment for using IMOD, insert this command in the user's
# .cshrc or a system cshrc file (if # /etc/csh.login has a command to set path
# absolutely, it has to go after that command in /etc/csh.login)
#
# It assumes that IMOD is located in /Applications - if not, modify

if (-e /Applications/IMOD/IMOD-mac.csh) source /Applications/IMOD/IMOD-mac.csh
