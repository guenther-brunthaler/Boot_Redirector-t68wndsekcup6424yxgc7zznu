ASMS = $(shell find -type f -name "*.asm" | sed -e 's, ,\\ ,g')
BINS = $(patsubst %.asm,%.bin,$(ASMS))
TARGETS = $(BINS)

all: $(TARGETS)

.PHONY: all clean
.SUFFIXES: .bin .asm

.asm.bin:
	nasm -l "$*".lst -o "$@" "$<"

clean:
	-rm $(TARGETS)
