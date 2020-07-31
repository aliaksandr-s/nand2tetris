uberjar:
	rm -rf classes
	mkdir classes
	clj -e "(compile 'compiler.main)"	
	clojure -A:uberjar --main-class compiler.main

.ONESHELL:
zip:
	cd target
	rm project11.zip
	zip -r project11.zip ./
