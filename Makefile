ALL: gtetris ttetris

gtetris: tetris.ml gtetris.ml gtetris.mk
	make -f gtetris.mk

ttetris: tetris.ml ttetris.ml gtetris.mk
	make -f ttetris.mk

clean:
	make -f ttetris.mk clean
	make -f gtetris.mk clean

