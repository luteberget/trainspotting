.PHONY: rolling railml2dgraph vis-rs docs railperfcheck

docs: rolling vis-rs railml2dgraph railperfcheck
	$(MAKE) -C docs


rolling:
	$(MAKE) -C rolling 

railperfcheck: rolling
	$(MAKE) -C railperfcheck

vis-rs:
	$(MAKE) -C vis-rs

railml2dgraph:
	$(MAKE) -C railml2dgraph 
