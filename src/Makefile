UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
	format=macho64
else ifeq ($(UNAME), Linux)
	format=elf64
else
	format=win64
endif

%.run: %.o main.o char.o
	gcc main.o char.o $< -o $@

main.o: main.c types.h
	gcc -c main.c -o main.o

char.o: char.c types.h
	gcc -c char.c -o char.o

#if you want to distribute youll want to replace elf64 with $(format)
%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.rkt
	ghc -dynamic --make Compile_file.hs 
	./Compile_file $< $@

clean:
	rm *.o *.s *.run *.hi

test: 42.run
	@test "$(shell ./42.run)" = "42"
