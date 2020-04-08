C99FLAGS=-std=c99 -Weverything -Wno-missing-noreturn -Wno-unused-function -Werror -pedantic
CC=clang $(C99FLAGS)

.PHONY: show build update check update-mirth install-vim install-code

show: mirth0.c mirth1.c mirth2.c mirth3.c
	diff mirth0.c mirth1.c | head -n 5
	diff mirth1.c mirth2.c | head -n 5
	diff mirth2.c mirth3.c

build: mirth0 mirth1 mirth2 mirth1.c mirth2.c mirth3.c

update: mirth0.c mirth3.c
	cp mirth3.c mirth0.c

check: mirth0.c mirth1.c mirth2.c mirth3.c
	diff mirth2.c mirth3.c
	diff mirth1.c mirth3.c
	diff mirth0.c mirth3.c

clean:
	rm -f mirth1.c mirth2.c mirth3.c mirth0 mirth1 mirth2

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

#########

mirth0: mirth0.c
	$(CC) -o mirth0 mirth0.c

mirth1: mirth1.c
	$(CC) -o mirth1 mirth1.c

mirth2: mirth2.c
	$(CC) -o mirth2 mirth2.c

mirth1.c: mirth0 mirth.mth
	./mirth0
	mv mirth.c mirth1.c

mirth2.c: mirth1 mirth.mth
	./mirth1
	mv mirth.c mirth2.c

mirth3.c: mirth2 mirth.mth
	./mirth2
	mv mirth.c mirth3.c
