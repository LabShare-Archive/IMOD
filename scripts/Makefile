include ../configure

#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.13  2004/10/07 16:18:30  mast
#  Added tomosnapshot
#
#  Revision 3.12  2004/07/21 04:58:04  mast
#  Added splittilt and splitcombine
#
#  Revision 3.11  2004/07/08 22:52:59  mast
#  Added processchunks
#
#  Revision 3.10  2004/06/17 15:58:53  mast
#  Added chunksetup
#
#  Revision 3.9  2004/03/11 18:46:16  mast
#  Added matchrotpairs
#
#  Revision 3.8  2003/10/24 02:03:50  mast
#  Add the sourceable file to set tmpdir
#
#  Revision 3.7  2003/09/11 19:54:31  mast
#  Need to take explicit generated list from configure
#
#  Revision 3.6  2003/08/29 17:36:05  mast
#  Add imodpsview
#
#  Revision 3.5  2003/08/25 18:48:43  mast
#  Add runimodqtapp and setlibpath, created by setup
#
#  Revision 3.4  2003/04/18 22:55:59  mast
#  Add submfg
#
#  Revision 3.3  2002/05/20 19:48:41  mast
#  Add transferfid
#
#  Revision 3.2  2002/01/13 17:40:11  mast
#  *** empty log message ***
#
#  Revision 3.1  2001/12/31 19:17:52  mast
#  Add dm2mrc
#
# 3/12/01 take bgimod out, no longer needed

SCRIPTS = copytomocoms finishjoin makejoincom matchorwarp matchshifts\
 newst onepatchcorr patchcorr3d patchcrawl3d recombine reduceby2 selfalign\
 setupcombine setupcoms setuplong splitmrc squeezevol trimvol xf2xg xfalign\
 dm2mrc alignlog transferfid submfg imodpsview settmpdir matchrotpairs\
 chunksetup processchunks splittilt splitcombine tomosnapshot clipflipyz\
 $(GENERATED_SCRIPTS)

all:

install:
	$(CP) $(SCRIPTS) $(INSTBINDIR)
#	(cd $(INSTBINDIR) ; if (! -e bgimodv) ln -s bgimod bgimodv)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

