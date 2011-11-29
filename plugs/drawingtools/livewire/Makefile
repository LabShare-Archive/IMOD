# Makefile for livewire modules

include ../../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)  $(QTFLAGS)

all : $(OBJS)

$(OBJS) : LivewireCalculator.$(O) PointPriorityQueue.$(O) Threaded.$(O) \
	 moc_Threaded.$(O) WeightCalculator.$(O) general.$(O)

moc_Threaded.cpp : Threaded.h
	$(MOC) Threaded.h -o moc_Threaded.cpp
