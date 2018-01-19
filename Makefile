TestDrive: TestDrive.hs src/TrainSim/builder.o src/TrainSim/il.o src/TrainSim/traindynamics.o
	ghc -O2 -isrc -isatplus TestDrive.hs src/TrainSim/builder.o src/TrainSim/il.o src/TrainSim/traindynamics.o -lstdc++

RoutePlan: RoutePlan.hs src/TrainSim/builder.o
	ghc -O2 -isrc -isatplus RoutePlan.hs src/TrainSim/builder.o -lstdc++

src/TrainSim/builder.o: src/TrainSim/builder.cpp
	g++ -std=c++11 -Wall -O2 -c src/TrainSim/builder.cpp -o src/TrainSim/builder.o

src/TrainSim/il.o: src/TrainSim/il.cpp
	g++ -std=c++11 -Wall -O2 -c src/TrainSim/il.cpp -o src/TrainSim/il.o

src/TrainSim/traindynamics.o: src/TrainSim/traindynamics.cpp
	g++ -std=c++11 -Wall -O2 -c src/TrainSim/traindynamics.cpp -o src/TrainSim/traindynamics.o

TrainPlan: TrainPlan.hs src/*/*hs
	ghc -O2 -isrc -isatplus TrainPlan.hs

testinputs  := $(wildcard examples/*/*inp)
testoutputs := $(testinputs:.inp=.outp)

.PHONY: test
test: $(testoutputs)

%.outp : %.inp TrainPlan
	./TrainPlan < $< > $@
