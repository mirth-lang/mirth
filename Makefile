C99FLAGS=-std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-function -Wno-unused-parameter \
 -Wno-unused-value -Wno-missing-braces -Wno-overlength-strings -Wno-infinite-recursion \
 -Werror -pedantic -O0 -fmax-errors=9 -Wno-unused-command-line-argument

CC=gcc $(C99FLAGS)
CCSAN=$(CC) -fsanitize=undefined -fsanitize=address

SRCS=lib/std/* lib/arg-parser/* src/*

.PHONY: default show showsan build buildsan debug update check checksan clean \
 install-vim install-code install-atom install profile play-snake test-verify test-update \
 examples

default: show bin/snake.c

show: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth1.c | head -n 5
	diff --strip-trailing-cr bin/mirth1.c bin/mirth2.c | head -n 10
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3.c
showsan: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth1.c | head -n 5
	diff --strip-trailing-cr bin/mirth1.c bin/mirth2.c | head -n 10
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3san.c

build: bin/mirth0 bin/mirth1 bin/mirth2 bin/mirth1.c bin/mirth2.c bin/mirth3.c
buildsan: bin/mirth0 bin/mirth1 bin/mirth2san bin/mirth1.c bin/mirth2.c bin/mirth3san.c
debug: bin/mirth3debug

update: bin/mirth0.c bin/mirth3.c
	cp bin/mirth3.c bin/mirth0.c

check: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth1.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth3.c

checksan: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth1.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3san.c

clean:
	cp bin/mirth0.c mirth0.c
	rm -rf bin/*.c bin/*.exe bin/eval bin/test
	mv mirth0.c bin/

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

install-atom:
	apm link tools/mirth-atom

install: bin/mirth0
	tools/install.sh bin/mirth0

profile: bin/mirth_prof
	time bin/mirth_prof

play-snake: bin/snake
	bin/snake

examples: bin/mirth2
	bash tools/build-examples.sh

test-verify:
	bash tools/mirth-test.sh -v

test-update:
	bash tools/mirth-test.sh -u

#########

bin/%: bin/%.c
	$(CC) -o $@ $^

bin/mirth0san: bin/mirth0.c
	$(CCSAN) -o bin/mirth0san bin/mirth0.c

bin/mirth1san: bin/mirth1.c
	$(CCSAN) -o bin/mirth1san bin/mirth1.c

bin/mirth2san: bin/mirth2.c
	$(CCSAN) -o bin/mirth2san bin/mirth2.c

bin/mirth1debug: bin/mirth1debug.c
	$(CC) -g -o bin/mirth1debug bin/mirth1debug.c

bin/mirth2debug: bin/mirth2debug.c
	$(CC) -g -o bin/mirth2debug bin/mirth2debug.c

bin/mirth3debug: bin/mirth3debug.c
	$(CC) -g -o bin/mirth3debug bin/mirth3debug.c

bin/mirth1.c: bin/mirth0 $(SRCS)
	bin/mirth0 src/main.mth -o bin/mirth1.c

bin/mirth2.c: bin/mirth1 $(SRCS)
	bin/mirth1 src/main.mth -o bin/mirth2.c

bin/mirth3.c: bin/mirth2 $(SRCS)
	bin/mirth2 src/main.mth -o bin/mirth3.c

bin/mirth1debug.c: bin/mirth0 $(SRCS)
	bin/mirth0 --debug src/main.mth -o bin/mirth1debug.c

bin/mirth2debug.c: bin/mirth1debug $(SRCS)
	bin/mirth1debug --debug src/main.mth -o bin/mirth2debug.c

bin/mirth3debug.c: bin/mirth2debug $(SRCS)
	bin/mirth2debug --debug src/main.mth -o bin/mirth3debug.c

bin/mirth3san.c: bin/mirth2san $(SRCS)
	bin/mirth2san src/main.mth -o bin/mirth3san.c

bin/mirth_prof.c: bin/mirth3.c

bin/mirth_prof: bin/mirth_prof.c
	$(CC) -g -fprofile-instr-generate -o bin/mirth_prof bin/mirth_prof.c

bin/snake.c: bin/mirth2 lib/std/* examples/snake.mth examples/sdl2.mth
	bin/mirth2 --debug examples/snake.mth -o bin/snake.c

bin/snake: bin/snake.c
	$(CC) -o bin/snake bin/snake.c `pkg-config --cflags --libs sdl2`

bin/snake-infer-types: bin/snake-infer-types.c
	$(CC) -o bin/snake-infer-types bin/snake-infer-types.c `pkg-config --cflags --libs sdl2`

bin/hello: bin/hello.c
	$(CC) -o bin/hello bin/hello.c
