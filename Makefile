SOURCE = source
BUILD = build
EXE = $(BUILD)/compiler

all:
	mkdir -p build
	ghc -O2 -outputdir $(BUILD) -i$(SOURCE) -o $(EXE) $(SOURCE)/Main.hs

debug:
	mkdir -p build
	ghc -O2 -prof -fprof-auto -outputdir $(BUILD) -i$(SOURCE) -o $(EXE) $(SOURCE)/Main.hs

run: all
	$(EXE) < tests/main.spl

testCorrect = if $(EXE) < tests/$(1).spl 1>/dev/null 2>/dev/null; then echo "test $(1) succeeded"; else echo "test $(1) failed"; exit 1; fi
testError = if $(EXE) < tests/$(1).spl 1>/dev/null 2>/dev/null; then echo "error test $(1) failed"; exit 1; else echo "error test $(1) succeeded"; fi

tests: all
	@$(call testCorrect,main)
	@$(call testCorrect,general/ambiguity)
	@$(call testCorrect,general/dfa)
	@$(call testCorrect,general/exp)
	@$(call testCorrect,general/gcd)
	@$(call testCorrect,general/hanoi)
	@$(call testCorrect,general/lists)
	@$(call testCorrect,general/matrix)
	@$(call testCorrect,general/rand)
	@$(call testCorrect,general/re)
	@$(call testCorrect,general/sort)
	@$(call testCorrect,typing/scope1)
	@$(call testCorrect,typing/scope2)
	@$(call testCorrect,typing/overloading1)
	@$(call testCorrect,typing/overloading2)
	@$(call testCorrect,typing/emptyMain)
	@$(call testError,typing/error1)
	@$(call testError,typing/error2)
	@$(call testError,typing/error3)
	@$(call testError,typing/error4)
	@$(call testError,typing/error5)
	@$(call testError,typing/error6)
	@$(call testError,typing/error7)
	@$(call testError,typing/error8)
	@$(call testError,typing/error9)
	@$(call testError,typing/error10)
	@$(call testError,typing/error11)
	@$(call testError,typing/error12)
	@$(call testError,typing/error13)
	@$(call testError,typing/error14)
	@$(call testError,typing/error16)
	@$(call testError,typing/error17)
	@$(call testError,typing/error18)
	@$(call testError,data/error1)
	@$(call testError,data/error2)
	@$(call testError,data/error3)
	@$(call testError,data/error4)
	@$(call testError,data/error5)
	@$(call testError,data/error6)
	@$(call testError,data/error7)
	@$(call testError,data/error8)
	@$(call testError,data/error9)
	@$(call testError,data/error10)
	@$(call testError,data/error11)

.PHONY: all debug run tests
