C99FLAGS=-std=c99 -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Werror -pedantic
CC=gcc $(C99FLAGS)

.PHONY: default show build update check update-mirth install-vim install-code profile play-snake

default: show

show: mirth0.c mirth1.c mirth2.c mirth3.c
	diff --strip-trailing-cr mirth0.c mirth1.c | head -n 5
	diff --strip-trailing-cr mirth1.c mirth2.c | head -n 10
	diff --strip-trailing-cr mirth2.c mirth3.c

build: mirth0 mirth1 mirth2 mirth1.c mirth2.c mirth3.c

update: mirth0.c mirth3.c
	cp mirth3.c mirth0.c

check:
	diff --strip-trailing-cr mirth2.c mirth3.c
	diff --strip-trailing-cr mirth1.c mirth3.c
	diff --strip-trailing-cr mirth0.c mirth3.c

clean:
	rm -f mirth1.c mirth2.c mirth3.c mirth0 mirth1 mirth2

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

profile: mirth_prof
	time ./mirth_prof
	rm -f mirth.c

play-snake: snake
	./snake

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

mirth_prof.c: mirth3.c

mirth_prof: mirth_prof.c
	$(CC) -g -fprofile-instr-generate -o mirth_prof mirth_prof.c

snake.c: mirth2 mirth.mth
	./mirth2
	rm -f mirth.c

snake: snake.c
	$(CC) -o snake snake.c `pkg-config --libs sdl2`
