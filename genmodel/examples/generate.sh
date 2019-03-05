for i in $(seq 1 100)
do 
    ../target/debug/genmodel -n$i -t5 > gen_${i}x5.railml
    ../target/debug/genmodel -n5 -t$i > gen_5x${i}.railml
done

for i in $(seq 5 20)
do 
    ../target/debug/genmodel -n$i -t$i > gen_${i}x${i}.railml
done
