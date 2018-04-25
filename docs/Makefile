examples = $(addsuffix .md, $(addprefix src/,\
	   $(patsubst %/,%,$(dir $(wildcard examples/*/.)))))

example_history = $(patsubst %.md,%-history.js, $(examples))
example_vis     = $(patsubst %.md,%-vis.js, $(examples))

ROLLING = ../rolling/target/debug/rolling
GRIDVIS = ../gridvis/gridjson
RAILPERFCHECK = ../railperfcheck/railperfcheck
ROLLINGRAILML = ../rollingrailml/target/debug/rollingrailml

.PHONY: book
book: $(examples) $(example_history) $(example_vis)
	mdbook build

src/%.md: src/%-history.js src/%-vis.js %/template %/infrastructure %/routes %/plan
	sed \
	    -e "/RAILWAYINFRASTRUCTURE/ {r $*/infrastructure" -e 'd }' \
	    -e "/RAILWAYROUTES/ {r $*/routes" -e 'd }' \
	    -e "/RAILWAYDISPATCH/ {r $*/plan" -e 'd }' \
	    -e "/RAILWAYUSAGE/ {r $*/usage" -e 'd }' \
	    -e "/RAILWAYRAILMLINFRASTRUCTURE/ {r $*/railmlinfrastructure" -e 'd }' \
	    -e '/RAILWAYVIEW/ { i <div id="railview"></div>'  \
	    -e 'i <script src="d3/d3.js"></script>' \
	    -e 'i <script src="$*-history.js"></script>' \
	    -e 'i <script src="$*-vis.js"></script>' \
	    -e 'i <script src="railview.js"></script>' \
	    -e 'd }' \
	    $*/template > src/$*.md

src/%-history.js: $(ROLLING) %/infrastructure %/routes %/plan
	$(ROLLING) $*/infrastructure $*/routes $*/plan -vv \
	    -J src/$*-history.js

%/graph: $(ROLLING) %/infrastructure
	$(ROLLING) $*/infrastructure $*/routes $*/plan -vv -g $*/graph

src/%-vis.js: $(GRIDVIS) %/graph
	[ -f $*/nooptgrid ] && $(GRIDVIS) -n $*/graph > src/$*-vis.js || $(GRIDVIS) $*/graph > src/$*-vis.js

%/plan: $(RAILPERFCHECK) %/infrastructure %/routes %/usage
	$(RAILPERFCHECK) $*/infrastructure $*/routes $*/usage -p $*/plan

%/infrastructure %/routes: $(ROLLINGRAILML) %/railmlinfrastructure
	$(ROLLINGRAILML) $*/railmlinfrastructure -o $*/infrastructure -r $*/routes

