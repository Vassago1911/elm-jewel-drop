build:
	mkdir -p gh-pages
	elm make --output=gh-pages/index.html --optimize src/Main.elm