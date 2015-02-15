all:
	ghc -outputdir build -o compiler Main.hs #-prof -auto-all -caf-all -fforce-recomp

.PHONY: all
