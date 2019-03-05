#!/bin/bash

mkdir -p convert/dgraph
mkdir -p convert/in
mkdir -p convert/levelsout
mkdir -p convert/linout
mkdir -p convert/gridout
mkdir -p convert/gridvisformat

for i in models/*railml generated/*railml; do
	#echo dgraph $i

	filename=$(basename -- "$i")
	extension="${filename##*.}"
	filename="${filename%.*}"

	# 1. convert railml to dgraph
	#echo ../target/release/railml2dgraph -o convert/dgraph/$filename.dgraph $i
	../target/release/railml2dgraph -o convert/dgraph/$filename.dgraph $i >/dev/null

	# 2. convert 
	../target/release/vis_rs -g convert/in/$filename.in -i convert/dgraph/$filename.dgraph -z convert/levelsout/$filename.tikz -s convert/levelsout/$filename.svg > /dev/null

	# measure levelout

	t1=$(date +%s.%N)
	./timeout.sh -t 300 ../target/release/vis_rs convert/in/$filename.in -z convert/levelsout/$filename.tikz > convert/levelsout/$filename.log
	t2=$(date +%s.%N)
	tlevelout=$(echo "$t2 - $t1" | bc -l)

	# measure linprog
	t1=$(date +%s.%N)
	#./timeout.sh -t 300 ../levelbasedsatplus/linprog/linprog convert/in/$filename.in > convert/linout/$filename.tikz
	t2=$(date +%s.%N)
	tlinprog=$(echo "$t2 - $t1" | bc -l)


	# measure Front, using criteria hwb, hbw, and bhw

	##for c in hwb hbw bhw; do
	t1=$(date +%s.%N)
	./timeout.sh -t 300 ../rail-layout-gridbased/frontperf hwb $(cat maxheight/$filename) convert/in/$filename.in > convert/gridout/$filename.hwb.tikz
	t2=$(date +%s.%N)
	thwb=$(echo "$t2 - $t1" | bc -l)

	t1=$(date +%s.%N)
	./timeout.sh -t 300 ../rail-layout-gridbased/frontperf hbw $(cat maxheight/$filename) convert/in/$filename.in > convert/gridout/$filename.hbw.tikz
	t2=$(date +%s.%N)
	thbw=$(echo "$t2 - $t1" | bc -l)

	t1=$(date +%s.%N)
	./timeout.sh -t 300 ../rail-layout-gridbased/frontperf bhw $(cat maxheight/$filename) convert/in/$filename.in > convert/gridout/$filename.bhw.tikz
	t2=$(date +%s.%N)
	tbhw=$(echo "$t2 - $t1" | bc -l)


	# convert to old gridvis format
	../target/release/rolling convert/dgraph/$filename.dgraph empty empty -g convert/gridvisformat/$filename.g > /dev/null

	# run the direct sat (gridvis) method

	t1=$(date +%s.%N)
	./timeout.sh -t 300 ../gridvis/gridjson convert/gridvisformat/$filename.g > convert/gridvisformat/$filename.log 2>&1
	t2=$(date +%s.%N)
	tdirect=$(echo "$t2 - $t1" | bc -l)



	size=$(wc -l convert/in/$filename.in)
	echo "$filename $size $tlinprog $tlevelout $thwb $thbw $tbhw $tdirect"



done
