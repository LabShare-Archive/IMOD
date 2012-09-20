include ../../configure

O        = $(OBJEXT)
LIBRARY  = ../liblasik.$(STATICEXT)
BASE = ./svl/lib/base
PGM = ./svl/lib/pgm
LBFGS = ./external/lbfgs
XML = ./external/xmlParser
EWIN = ./external/win32

all: $(LIBRARY)

OBJS = ap.$(O) lbfgs.$(O) xmlParser.$(O) svlBitArray.$(O) \
 svlCodeProfiler.$(O) svlCommandLine.$(O) svlConfigManager.$(O) \
 svlLogger.$(O) svlLoopTimer.$(O) svlOptimizer.$(O) \
 svlOptions.$(O) svlStatsUtils.$(O) svlStrUtils.$(O) svlClusterGraph.$(O) \
 svlFactor.$(O) svlFactorOperations.$(O) svlFactorTemplate.$(O) \
 svlGraphUtils.$(O) svlMessagePassing.$(O)


LASFLAGS = $(CXXFLAGS) -I. -I./external -I./include $(EIGENFLAGS)


ap.$(O):  $(LBFGS)/ap.cpp
	$(CXX) $(LASFLAGS) -c -o ap.$(O) $(LBFGS)/ap.cpp
lbfgs.$(O):  $(LBFGS)/lbfgs.cpp
	$(CXX) $(LASFLAGS) -c -o lbfgs.$(O) $(LBFGS)/lbfgs.cpp
xmlParser.$(O):  $(XML)/xmlParser.cpp $(XML)/xmlParser.h
	$(CXX) $(LASFLAGS) -c -o xmlParser.$(O) $(XML)/xmlParser.cpp
dirent.$(O): $(EWIN)/dirent.cpp $(EWIN)/dirent.h
	$(CXX) $(LASFLAGS) -c -o dirent.$(O) $(EWIN)/dirent.cpp
svlBitArray.$(O):  $(BASE)/svlBitArray.cpp
	$(CXX) $(LASFLAGS) -c -o svlBitArray.$(O) $(BASE)/svlBitArray.cpp
svlCodeProfiler.$(O):  $(BASE)/svlCodeProfiler.cpp
	$(CXX) $(LASFLAGS) -c -o svlCodeProfiler.$(O) $(BASE)/svlCodeProfiler.cpp
svlCommandLine.$(O):  $(BASE)/svlCommandLine.cpp
	$(CXX) $(LASFLAGS) -c -o svlCommandLine.$(O) $(BASE)/svlCommandLine.cpp
svlConfigManager.$(O):  $(BASE)/svlConfigManager.cpp
	$(CXX) $(LASFLAGS) -c -o svlConfigManager.$(O) $(BASE)/svlConfigManager.cpp
svlLogger.$(O):  $(BASE)/svlLogger.cpp
	$(CXX) $(LASFLAGS) -c -o svlLogger.$(O) $(BASE)/svlLogger.cpp
svlLoopTimer.$(O):  $(BASE)/svlLoopTimer.cpp
	$(CXX) $(LASFLAGS) -c -o svlLoopTimer.$(O) $(BASE)/svlLoopTimer.cpp
svlOptimizer.$(O):  $(BASE)/svlOptimizer.cpp
	$(CXX) $(LASFLAGS) -c -o svlOptimizer.$(O) $(BASE)/svlOptimizer.cpp
svlOptions.$(O):  $(BASE)/svlOptions.cpp
	$(CXX) $(LASFLAGS) -c -o svlOptions.$(O) $(BASE)/svlOptions.cpp
svlStatsUtils.$(O):  $(BASE)/svlStatsUtils.cpp
	$(CXX) $(LASFLAGS) -c -o svlStatsUtils.$(O) $(BASE)/svlStatsUtils.cpp
svlStrUtils.$(O):  $(BASE)/svlStrUtils.cpp
	$(CXX) $(LASFLAGS) -c -o svlStrUtils.$(O) $(BASE)/svlStrUtils.cpp
svlClusterGraph.$(O):  $(PGM)/svlClusterGraph.cpp
	$(CXX) $(LASFLAGS) -c -o svlClusterGraph.$(O) $(PGM)/svlClusterGraph.cpp
svlFactor.$(O):  $(PGM)/svlFactor.cpp
	$(CXX) $(LASFLAGS) -c -o svlFactor.$(O) $(PGM)/svlFactor.cpp
svlFactorOperations.$(O):  $(PGM)/svlFactorOperations.cpp
	$(CXX) $(LASFLAGS) -c -o svlFactorOperations.$(O) $(PGM)/svlFactorOperations.cpp
svlFactorTemplate.$(O):  $(PGM)/svlFactorTemplate.cpp
	$(CXX) $(LASFLAGS) -c -o svlFactorTemplate.$(O) $(PGM)/svlFactorTemplate.cpp
svlGraphUtils.$(O):  $(PGM)/svlGraphUtils.cpp
	$(CXX) $(LASFLAGS) -c -o svlGraphUtils.$(O) $(PGM)/svlGraphUtils.cpp
svlMessagePassing.$(O):  $(PGM)/svlMessagePassing.cpp
	$(CXX) $(LASFLAGS) -c -o svlMessagePassing.$(O) $(PGM)/svlMessagePassing.cpp


$(LIBRARY): $(OBJS)
	$(MAKESTATICLIB)$(LIBRARY) $(OBJS)
	$(STATICRANLIB) $(LIBRARY)

clean:
	-\rm -f *.$(OBJEXT) *.a *.lib *~

