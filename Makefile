SOURCE = source
BUILD = build
EXE = $(BUILD)/compiler

all:
	ghc -outputdir $(BUILD) -i$(SOURCE) -o $(EXE) $(SOURCE)/Main.hs #-prof -fprof-auto -rtsopts -fforce-recomp

run: all
	$(BUILD)/compiler < test

.PHONY: all run
