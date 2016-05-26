install: snake
	./bless-the-snake

snake: snake.c
	$(CC) -o $@ -lchibi-scheme -lm $<

clean:
	rm snake
