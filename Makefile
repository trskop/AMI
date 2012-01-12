GHC=ghc --make

all: test

test: test.hs Network/AMI.hs
	$(GHC) $<

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
	rm test
