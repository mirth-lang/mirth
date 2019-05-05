
build:
	python3 bootstrap/mirth.py src build

test:
	python3 test.py

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

.PHONY: build test

