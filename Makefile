CFLAGS = $(shell sdl2-config --cflags) -Wall -O0
LIBS = -lm -lGL $(shell sdl2-config --libs) -lSDL2_image
SRC = command.c empic.c render.c

all: empic

empic: $(SRC) render.h
	gcc -g -Wall -o $@ $(SRC) $(CFLAGS) $(LIBS)
