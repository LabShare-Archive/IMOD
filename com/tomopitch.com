# THIS IS A COMMAND FILE TO RUN TOMOPITCH ON 3 MODELS
# YOU MAY WANT TO ADJUST THE THICKNESS TO ADD OUTSIDE YOUR MODEL LINES
#
$tomopitch
2	Pixels to add to thickness on each side, above and below model lines
0	spacing between sample files
3	number of files
top.mod
mid.mod
bot.mod
