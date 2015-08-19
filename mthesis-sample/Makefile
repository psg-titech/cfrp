CXX = g++
CABAL_DIR = ../.cabal-sandbox
DATA_DIR = $(CABAL_DIR)/share/x86_64-linux-ghc-7.6.3/cfrp-0.0.1
CXXFLAGS = -I $(DATA_DIR) -Wall -W
LDFLAGS =
CFRP = $(CABAL_DIR)/bin/cfrp
SAMPLE1_OBJ = timer.o last_press.o misc.o sample_on.o memory_manager.o sample1.o

.SUFFIXES: .cfrp

sample1: $(SAMPLE1_OBJ)
	$(CXX) $(LDFLAGS) $(SAMPLE1_OBJ) -o $@

memory_manager.o: $(DATA_DIR)/memory_manager.cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<

.cc.o:
	$(CXX) -c $(CXXFLAGS) -o $@ $<

.cfrp.cc:
	$(CFRP) $< > $@
