#
# Makefile for mantohtml program
#
#

HERE = $(PWD)

always : mantohtml ALWAYS
	(cd ../manpages; ./convert $(HERE))
	(cd ../flib/man; ./makedist $(HERE))


clean :
	-\rm -f *.o mantohtml *~


ALWAYS:
