# THIS IS A COMMAND FILE TO RUN TOMOPITCH ON ONE MODEL
#
####CreatedVersion#### 3.8.7
#
# YOU SHOULD CREATE THE MODEL WITH:
#     3dmod top.rec mid.rec bot.rec tomopitch.mod
#
# If you already have 3 model files instead, make 3 ModelFile entries:
#
# ModelFile top.mod
# ModelFile mid.mod
# ModelFile bot.mod
#
# YOU MAY WANT TO ADJUST THE THICKNESS TO ADD OUTSIDE YOUR MODEL LINES
#
$tomopitch -StandardInput
ModelFile	tomopitch.mod
#
# Pixels to add to thickness on each side, above and below model lines
ExtraThickness	5
SpacingInY	0
