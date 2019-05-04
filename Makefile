
build:
	python3 bootstrap/mirth.py src build

test:
	python3 test.py

.PHONY: build test

