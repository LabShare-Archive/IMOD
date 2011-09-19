$fixboundaries BBa_full.rec tilt-bound.info
$collectmmm pixels= tilt 11 BBa_full.rec 1
$\rm -f tilt-[0-9]*.* BBa_full-[0-9]*.rbound tilt-bound.info
