all: test fay
fay:
	fay Main/Game.hs
test:
	runhaskell Spec.hs
serve:
	python -m SimpleHTTPServer
