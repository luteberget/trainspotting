examples = $(addsuffix .md, $(addprefix src/,\
	   $(patsubst %/,%,$(dir $(wildcard examples/*/.)))))

example_history = $(patsubst %.md,%-history.js, $(examples))
example_vis     = $(patsubst %.md,%-vis.js, $(examples))

ROLLING = ../target/debug/rolling
GRIDVIS = ../../linearschematic/gridjson

.PHONY: book
book: $(examples) $(example_history) $(example_vis)
	mdbook build

.PRECIOUS: src/*/*-history.js src/*/*-vis.js

src/%.md: src/%-history.js src/%-vis.js src/%.md.template %/infrastructure %/routes %/plan
	sed \
	    -e "/RAILWAYINFRASTRUCTURE/ {r $*/infrastructure" -e 'd }' \
	    -e "/RAILWAYROUTES/ {r $*/routes" -e 'd }' \
	    -e "/RAILWAYDISPATCH/ {r $*/plan" -e 'd }' \
	    -e '/RAILWAYVIEW/ { i <div id="railview"></div>'  \
	    -e 'i <script src="$*-history.js"></script>' \
	    -e 'i <script src="$*-vis.js"></script>' \
	    -e 'i <script src="../railview.js"></script>' \
	    -e 'd }' \
	    src/$*.md.template > src/$*.md

src/%-history.js %/graph: %/infrastructure %/routes %/plan
	$(ROLLING) $*/infrastructure $*/routes $*/plan -vv \
	    -J src/$*-history.js -g $*/graph

src/%-vis.js: %/graph
	$(GRIDVIS) $*/graph > src/$*-vis.js

.PHONY: cleanexamples
cleanexamples:
	rm examples/*/history.js
	rm examples/*/graph
	rm examples/*/vis.js
