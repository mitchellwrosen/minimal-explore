all: test fay

.PHONY: test
test:
	runhaskell -Wall -Werror -itest:src test/Spec.hs
buildTest: clean
	ghc -fhpc -odir=obj -hidir=obj -itest:src test/Spec.hs
	test/Spec
clean:
	rm -rf obj/* Spec.tix
coverage: buildTest
	-./coverage.sh
fay:
	cd src; fay Main/Game.hs -o ../gen/game.js
serve:
	xdg-open "game.html"
