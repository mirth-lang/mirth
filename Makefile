C99FLAGS =-std=c99 -Weverything -Wno-missing-noreturn -Wno-unused-function -Werror -pedantic

.PHONY: mirth update-mirth install-vim install-code

mirth: mirth3.c
	diff mirth2.c mirth3.c

update-mirth: mirth3.c
	cp mirth3.c mirth0.c

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

#########

mirth1.c: mirth0.c mirth.mth
	clang $(C99FLAGS) -o mirth0 mirth0.c
	./mirth0
	mv mirth.c mirth1.c

mirth2.c: mirth1.c mirth.mth
	clang $(C99FLAGS) -o mirth1 mirth1.c
	./mirth1
	mv mirth.c mirth2.c

mirth3.c: mirth2.c mirth.mth
	clang $(C99FLAGS) -o mirth2 mirth2.c
	./mirth2
	mv mirth.c mirth3.c
