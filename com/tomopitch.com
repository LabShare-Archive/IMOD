# THIS IS A COMMAND FILE TO RUN TOMOPITCH ON ONE MODEL
# YOU SHOULD CREATE THE MODEL WITH:
#     imod top.rec mid.rec bot.rec tomopitch.mod
#
# If you already have 3 model files instead, change the entry for
# "number of files" to 3 and replace the "tomopitch.mod" entry with 3 lines:
# top.mod
# mid.mod
# bot.mod
#
# YOU MAY WANT TO ADJUST THE THICKNESS TO ADD OUTSIDE YOUR MODEL LINES
#
$tomopitch
2	Pixels to add to thickness on each side, above and below model lines
0	spacing between sample files
1	number of files
tomopitch.mod
