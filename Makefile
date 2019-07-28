
typecheck:
	python3 bootstrap/mirth.py --typecheck src

test:
	python3 bootstrap/mirth.py --testonly src

build:
	python3 bootstrap/mirth.py src build

test-all:
	python3 test.py

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

.PHONY: build test test-all typecheck install-vim

