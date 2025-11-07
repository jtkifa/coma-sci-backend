# Makefile for nrwavelets shared library
# Daubechies wavelet transforms from Numerical Recipes

CC = gcc
CFLAGS = -O2 -fPIC -Wall
TARGET = nrwavelets.so

all: $(TARGET)

$(TARGET): wavelets.o
	$(CC) -shared -o $@ $^

wavelets.o: wavelets.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o $(TARGET)

install: $(TARGET)
	cp $(TARGET) /usr/local/lib/
	ldconfig

.PHONY: all clean install
