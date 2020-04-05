C99FLAGS =-std=c99 -Weverything -Wno-missing-noreturn -Wno-unused-function -Werror -pedantic

mirth: mirth0.c mirth.mth
	gcc $(C99FLAGS) -o mirth0 mirth0.c
	./mirth0
	mv mirth.c mirth1.c
	gcc $(C99FLAGS) -o mirth1 mirth1.c
	./mirth1
	mv mirth.c mirth2.c
	gcc $(C99FLAGS) -o mirth2 mirth2.c
	diff mirth1.c mirth2.c || true
	./mirth2
	mv mirth.c mirth3.c
	diff mirth2.c mirth3.c

update-mirth: mirth
	cp mirth3.c mirth0.c

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

.PHONY: mirth update-mirth install-vim install-code
