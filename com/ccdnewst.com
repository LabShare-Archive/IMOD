# THIS IS A COMMAND FILE TO MAKE AN ALIGNED STACK FROM THE ORIGINAL STACK
#
# It assumes that the views are in order in the image stack
#  
# The -size argument should be ,, for the full area or specify the desired 
# size (e.g.: ,10)
#
# The -offset argument should be 0,0 for no offset, 0,300 to take an area
# 300 pixels above the center, etc.
#
$newst -size ,, -offset 0,0 -xf g5a.xf g5a.st g5a.ali
$mrctaper g5a.ali
