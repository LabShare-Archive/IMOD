$newstack
1
g5a.st
39-0,40-78	!list of sections from input file
1
g5a.ali
/	size
/	mode
-1	offset: -1 to impose same offset to all, then offset on next line
0,0	single offset in X and Y
1	transform
g5a.xf
0-78	!list of transforms
0	0 as is, 2 to float to means
$mrctaper g5a.ali
