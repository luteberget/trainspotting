.PHONY: rolling rollingrailml gridvis docs railperfcheck

rolling:
	$(MAKE) -C rolling 

railperfcheck: rolling
	$(MAKE) -C railperfcheck

gridvis:
	$(MAKE) -C gridvis

docs: rolling rollingrailml gridvis railperfcheck
	$(MAKE) -C docs

rollingrailml:
	$(MAKE) -C rollingrailml
