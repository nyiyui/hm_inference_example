.PHONY: run

run: hm_inference_example.js
	python3 -m http.server -b 127.0.0.1

_build/default/bin/main.bc.js:
	dune build ./bin/main.bc.js

hm_inference_example.js: _build/default/bin/main.bc.js
	cat $< > $@
