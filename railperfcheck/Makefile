.PHONY: railperfcheck
railperfcheck: RailPerfCheck.hs
	ghc -O2 -isrc -isatplus -o railperfcheck RailPerfCheck.hs ../target/release/librolling.a -lpthread
