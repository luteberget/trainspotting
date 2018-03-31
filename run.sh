ghc --make -O2 grid.hs -o grid -isatplus && ./grid > test.html && chromium-browser test.html 

