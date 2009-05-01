include ../configure

#  $Id$
#  Log at end

SCRIPTS = copytomocoms finishjoin makejoincom matchorwarp matchshifts\
 newst onepatchcorr patchcorr3d patchcrawl3d recombine reduceby2 selfalign\
 setupcombine setupcoms setuplong splitmrc squeezevol trimvol xf2xg xfalign\
 dm2mrc alignlog transferfid submfg imodpsview settmpdir matchrotpairs\
 chunksetup processchunks splittilt splitcombine tomosnapshot clipflipyz\
 archiveorig imodkillgroup collectmmm sampletilt runpyscript imodhelp\
 sirtsetup managesirt queuechunk splitcorrection runraptor $(GENERATED_SCRIPTS)

all:

install:  $(CMDWRAPPERS)
	$(CP) $(SCRIPTS) $(INSTBINDIR)

cmdwrappers:
	./copywrappers $(INSTBINDIR) . $(SCRIPTS)

clean:
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;

#  $Log$
#  Revision 3.27  2009/01/02 06:53:32  mast
#  Add imodhelp
#
#  Revision 3.26  2007/12/17 15:26:42  mast
#  Removed splitblend
#
#  Revision 3.25  2007/11/06 23:58:23  mast
#  Added splitcorrection
#
#  Revision 3.24  2007/09/14 21:51:22  mast
#  *** empty log message ***
#
#  Revision 3.23  2007/09/11 22:30:56  mast
#  *** empty log message ***
#
#  Revision 3.22  2007/07/19 14:52:50  mast
#  Added sirtsetup
#
#  Revision 3.21  2007/04/10 19:41:49  mast
#  Change for new copywrapper script
#
#  Revision 3.20  2007/04/08 16:47:05  mast
#  Added splitblend
#
#  Revision 3.19  2006/09/26 22:19:06  mast
#  Python stuff
#
#  Revision 3.18  2006/09/18 23:50:50  mast
#  Added sampletilt
#
#  Revision 3.17  2006/03/24 23:09:56  mast
#  *** empty log message ***
#
#  Revision 3.16  2005/08/30 17:41:31  mast
#  Added imodkillgroup
#
#  Revision 3.15  2005/05/10 03:14:32  mast
#  Added archiveorig
#
#  Revision 3.14  2004/10/08 03:33:55  mast
#  Added clipflipyz
#
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

