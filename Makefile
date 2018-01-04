TrainPlan: TrainPlan.hs src/*/*hs
	ghc -O2 -isrc -isatplus TrainPlan.hs

testinputs  := $(wildcard examples/*/*inp)
testoutputs := $(testinputs:.inp=.outp)

.PHONY: test
test: $(testoutputs)

%.outp : %.inp TrainPlan
	./TrainPlan < $< > $@
