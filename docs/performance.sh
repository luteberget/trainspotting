#!/bin/bash

rm performance
for i in examples/case_* examples/gen_* examples/{runningtime,frequency,overtaking,bad_overtaking,crossing,bad_crossing} ;do 
	echo "" >> performance
	echo "" >> performance
	echo "# $i" >> performance
	tail -n2 $i/time | sed 's/^/  /'>> performance
	tail -n8 $i/railperfchecklog | sed 's/^/  /'>> performance
done
