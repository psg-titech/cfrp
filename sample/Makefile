CFRP_VERSION = 0.0.1
CXX = g++
CABAL_DIR = ../.cabal-sandbox
HASKELL_ABI = x86_64-osx-ghc-7.10.2
DATA_DIR = $(CABAL_DIR)/share/$(HASKELL_ABI)/cfrp-$(CFRP_VERSION)
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

clean:
	$(RM) sample1
	$(RM) *.o

