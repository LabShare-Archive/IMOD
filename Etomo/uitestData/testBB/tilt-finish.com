$fixboundaries testBBa_full.rec tilt-bound.info
$collectmmm pixels= tilt 11 testBBa_full.rec 1
$\rm -f tilt-[0-9]*.* testBBa_full-[0-9]*.rbound tilt-bound.info
