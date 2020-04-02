
mirth: mirth.c
	gcc -std=c99 -Wall -Werror -pedantic -o mirth mirth.c
	./mirth mirth.mth
	nasm -f macho64 mirth.asm
	ld -static mirth.o -o mirth2
	./mirth2 mirth.mth
	gcc -std=c99 -o mirth3 mirth2.c
	./mirth3

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

.PHONY: mirth install-vim install-code
