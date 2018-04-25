.PHONY: rolling rollingrailml gridvis docs railperfcheck

docs: rolling rollingrailml gridvis railperfcheck
	$(MAKE) -C docs


rolling:
	$(MAKE) -C rolling 

railperfcheck: rolling
	$(MAKE) -C railperfcheck

gridvis:
	$(MAKE) -C gridvis

rollingrailml:
	$(MAKE) -C rollingrailml
