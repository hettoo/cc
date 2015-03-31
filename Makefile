SOURCE = source
BUILD = build
EXE = $(BUILD)/compiler

all:
	mkdir -p build
	ghc -O2 -outputdir $(BUILD) -i$(SOURCE) -o $(EXE) $(SOURCE)/Main.hs

run: all
	$(EXE) < tests/main.spl

testCorrect = if $(EXE) < tests/$(1).spl 1>/dev/null 2>/dev/null; then echo "test $(1) succeeded"; else echo "test $(1) failed"; exit 1; fi
testError = if $(EXE) < tests/$(1).spl 1>/dev/null 2>/dev/null; then echo "error test $(1) failed"; exit 1; else echo "error test $(1) succeeded"; fi

tests: all
	@$(call testCorrect,main)
	@$(call testCorrect,typing/scope1)
	@$(call testCorrect,typing/scope2)
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

.PHONY: all run tests
