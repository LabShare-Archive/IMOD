# Insert this command in the user's .cshrc or system cshrc file (probably
# /etc/csh.cshrc) to set the environment for using IMOD
#
# It assumes that IMOD is located in /usr/local - if not, modify

if (-e /usr/local/IMOD/IMOD-mac.csh) source /usr/local/IMOD/IMOD-mac.csh
