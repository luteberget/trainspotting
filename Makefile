TestDrive: TestDrive.hs src/TrainSim/builder.o
	ghc -O2 -isrc -isatplus TestDrive.hs src/TrainSim/builder.o -lstdc++

RoutePlan: RoutePlan.hs src/TrainSim/builder.o
	ghc -O2 -isrc -isatplus RoutePlan.hs src/TrainSim/builder.o -lstdc++

src/TrainSim/builder.o: src/TrainSim/builder.cpp
	g++ -std=c++11 -O2 -c src/TrainSim/builder.cpp -o src/TrainSim/builder.o

TrainPlan: TrainPlan.hs src/*/*hs
	ghc -O2 -isrc -isatplus TrainPlan.hs

testinputs  := $(wildcard examples/*/*inp)
testoutputs := $(testinputs:.inp=.outp)

.PHONY: test
test: $(testoutputs)

%.outp : %.inp TrainPlan
	./TrainPlan < $< > $@
