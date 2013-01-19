LIBS=-pa ../wrangler/ebin
SOURCES=$(wildcard src/*.erl)

default: all

ebin/%.beam: src/%.erl
	erlc -o ebin $(LIBS) $<

clean:
	rm ebin/*

all: $(patsubst src/%.erl,ebin/%.beam,$(SOURCES))


