python_version_full := $(wordlist 2,4,$(subst ., ,$(shell python --version 2>&1)))
python_version_major := $(word 1,${python_version_full})

server.python.2 := python -m SimpleHTTPServer
server.python.3 := python -m http.server
server := ${server.python.${python_version_major}}

all: test fay

test:
	ghc -fhpc -odir=obj -hidir=obj -itest:src test/Spec.hs
	test/Spec
clean:
	rm obj/* Spec.tix
fay:
	cd src; fay Main/Game.hs -o ../gen/game.js
serve:
	${server}

.PHONY: all
.PHONY: test
