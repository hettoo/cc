SOURCE = source
BUILD = build
EXE = $(BUILD)/compiler

all:
	mkdir -p build
	ghc -O2 -outputdir $(BUILD) -i$(SOURCE) -o $(EXE) $(SOURCE)/Main.hs

run: all
	$(EXE) < test.spl

.PHONY: all run
