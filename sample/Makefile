CABAL_DIR = ../.cabal-sandbox
CFRP = $(CABAL_DIR)/bin/cfrp
DATA_DIR = $(shell $(CABAL_DIR)/bin/cfrp_datadir)

CXX = g++
CXXFLAGS = -I $(DATA_DIR) -Wall -W
LDFLAGS =

SAMPLE1_OBJ = timer.o last_press.o misc.o sample_on.o memory_manager.o sample1.o

.SUFFIXES: .cfrp

.PHONY: clean

sample1: $(SAMPLE1_OBJ)
	$(CXX) $(LDFLAGS) $(SAMPLE1_OBJ) -o $@

memory_manager.o: $(DATA_DIR)/memory_manager.cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<

.cc.o:
	$(CXX) -c $(CXXFLAGS) -o $@ $<

.cfrp.cc:
	$(CFRP) $< > $@

clean:
	$(RM) sample1
	$(RM) *.o

