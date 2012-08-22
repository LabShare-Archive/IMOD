include ../configure

O        = $(OBJEXT)
RAPTOR   = RAPTOR$(BINEXT)
MARCOR   = MarkersCorrespond$(BINEXT) 
PROGS    = $(RAPTOR) $(MARCOR) 
OPENCV   = libopencv.$(STATICEXT)
LASIK    = liblasik.$(STATICEXT)
CSPARSE  = libcsparse.$(STATICEXT)
CORR     = ./correspondence
MAIN     = ./mainClasses
TRAJ     = ./trajectory
OPTI     = ./optimization
FILL     = ./fillContours
TMPL     = ./template

MARFLAGS = $(CXXFLAGS) -I./lasik -I./lasik/external -I./lasik/include -c -o

RAPOBJS = main.$(O) constants.$(O) ioMRCVol.$(O) template.$(O) stat.$(O) \
 correspondence.$(O) frame.$(O) trajectory.$(O) SFMestimationWithBA.$(O) \
 estimation3d.$(O) std_qp.$(O) contour.$(O) estimation3ddata.$(O) \
 SFMdata.$(O) probData.$(O) STDQPdata.$(O) fillContours.$(O)

MAROBJS = markersCorrespondMainTest.$(O) svlMarkerCorrespondenceLBModel.$(O)

all: $(PROGS)

constants.$(O): $(MAIN)/constants.cpp
	$(CXX) $(CXXFLAGS) -c -o constants.$(O) $(MAIN)/constants.cpp
ioMRCVol.$(O): $(MAIN)/ioMRCVol.cpp
	$(CXX) $(CXXFLAGS) -c -o ioMRCVol.$(O) $(MAIN)/ioMRCVol.cpp
template.$(O): $(TMPL)/template.cpp
	$(CXX) $(CXXFLAGS) -c -o template.$(O) $(TMPL)/template.cpp
stat.$(O): $(MAIN)/stat.cpp
	$(CXX) $(CXXFLAGS) -c -o stat.$(O) $(MAIN)/stat.cpp
correspondence.$(O): $(CORR)/correspondence.cpp
	$(CXX) $(CXXFLAGS) -c -o correspondence.$(O) $(CORR)/correspondence.cpp
frame.$(O): $(MAIN)/frame.cpp
	$(CXX) $(CXXFLAGS) -c -o frame.$(O) $(MAIN)/frame.cpp
trajectory.$(O): $(TRAJ)/trajectory.cpp
	$(CXX) $(CXXFLAGS) -c -o trajectory.$(O) $(TRAJ)/trajectory.cpp
SFMestimationWithBA.$(O): $(OPTI)/SFMestimationWithBA.cpp
	$(CXX) $(CXXFLAGS) -c -o SFMestimationWithBA.$(O) $(OPTI)/SFMestimationWithBA.cpp
estimation3d.$(O): $(OPTI)/estimation3d.cpp
	$(CXX) $(CXXFLAGS) -c -o estimation3d.$(O) $(OPTI)/estimation3d.cpp
std_qp.$(O): $(OPTI)/std_qp.cpp
	$(CXX) $(CXXFLAGS) -c -o std_qp.$(O) $(OPTI)/std_qp.cpp
contour.$(O): $(OPTI)/contour.cpp
	$(CXX) $(CXXFLAGS) -c -o contour.$(O) $(OPTI)/contour.cpp
estimation3ddata.$(O): $(OPTI)/estimation3ddata.cpp
	$(CXX) $(CXXFLAGS) -c -o estimation3ddata.$(O) $(OPTI)/estimation3ddata.cpp
SFMdata.$(O): $(OPTI)/SFMdata.cpp
	$(CXX) $(CXXFLAGS) -c -o SFMdata.$(O) $(OPTI)/SFMdata.cpp
probData.$(O): $(OPTI)/probData.cpp
	$(CXX) $(CXXFLAGS) -c -o probData.$(O) $(OPTI)/probData.cpp
STDQPdata.$(O): $(OPTI)/STDQPdata.cpp
	$(CXX) $(CXXFLAGS) -c -o STDQPdata.$(O) $(OPTI)/STDQPdata.cpp
fillContours.$(O): $(FILL)/fillContours.cpp
	$(CXX) $(CXXFLAGS) -c -o fillContours.$(O) $(FILL)/fillContours.cpp

$(RAPTOR): $(CSPARSE) $(OPENCV) $(RAPOBJS)
	$(CXX) -o $(RAPTOR) $(RAPOBJS) $(LDFLAGS) $(OPENCV) $(CSPARSE) \
	$(LCFSHR) $(CLIBS) $(RAPTORLIBS)

markersCorrespondMainTest.$(O): $(CORR)/markersCorrespondMainTest.cpp
	$(CXX) $(MARFLAGS) markersCorrespondMainTest.$(O) $(CORR)/markersCorrespondMainTest.cpp
svlMarkerCorrespondenceLBModel.$(O): $(CORR)/svlMarkerCorrespondenceLBModel.cpp
	$(CXX) $(MARFLAGS) svlMarkerCorrespondenceLBModel.$(O) $(CORR)/svlMarkerCorrespondenceLBModel.cpp

$(MARCOR): $(LASIK) $(MAROBJS) 
	$(CXX) -o $(MARCOR) $(MAROBJS) $(LDFLAGS) $(LASIK) $(CLIBS) $(MARCORLIBS)


$(OPENCV): opencv/*.cpp opencv/*.h opencv/*.hpp
	(cd opencv ; $(MAKE))

$(CSPARSE): suitesparse/*.c suitesparse/*.h
	(cd suitesparse ; $(MAKE))

$(LASIK): lasik/*/*/*.cpp lasik/*/*/*/*.cpp
	(cd lasik ; $(MAKE))


install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	(cd suitesparse ; $(MAKE) clean)
	(cd opencv ; $(MAKE) clean)
	(cd lasik ; $(MAKE) clean)
	-\rm -f *.$(OBJEXT) *.manifest $(PROGS) *.$(STATICEXT) *~

