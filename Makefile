FLAGS   = -Wall -O2 -threaded -feager-blackholing
TARGETS = ninetyninesolutions



all: $(TARGETS)

ninetyninesolutions: Main.hs NinetyNineSolutions.hs
	ghc $(FLAGS) $< -o $@

clean:
	rm -fv $(TARGETS) *.hi *.o
