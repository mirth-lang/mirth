C99FLAGS =-std=c99 -Weverything -Wno-missing-noreturn -Wno-unused-function -Werror -pedantic

mirth: mirth0.c mirth.mth
	gcc -std=c99 -Wall -Werror -pedantic -o mirth0 mirth0.c
	./mirth0 mirth.mth
	nasm -f macho64 mirth.asm
	ld -static mirth.o -o mirth
	./mirth mirth.mth
	gcc $(C99FLAGS) -o mirth2 mirth2.c
	./mirth2
	echo
	diff mirth2.c mirth3.c || echo
	gcc $(C99FLAGS) -o mirth3 mirth3.c
	./mirth3

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

.PHONY: mirth install-vim install-code
