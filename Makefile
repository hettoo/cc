SOURCE = source
BUILD = build
EXE = $(BUILD)/compiler

all:
	mkdir -p build
	ghc -O2 -outputdir $(BUILD) -i$(SOURCE) -o $(EXE) $(SOURCE)/Main.hs #-prof -fprof-auto -rtsopts -fforce-recomp

run: all
	$(BUILD)/compiler < test.spl

.PHONY: all run
