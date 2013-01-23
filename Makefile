WRANGLER_ROOT=../wrangler
LIBS=-pa $(WRANGLER_ROOT)/ebin
SOURCES=$(wildcard src/*.erl)
SED=sed

default: all
	for x in bin/*; do $(SED) -i.tmp 's|MU2_ROOT=.*|MU2_ROOT='$(CURDIR)'|' $$x; done
	rm bin/*.tmp
	$(SED) -i.tmp 's|-include(".*wrangler/include/wrangler.hrl")\.|-include("'$(WRANGLER_ROOT)'/include/wrangler.hrl")\.|' include/install.hrl
	rm include/*.tmp

ebin/%.beam: src/%.erl
	erlc -o ebin $(LIBS) $<

clean:
	rm ebin/*

all: $(patsubst src/%.erl,ebin/%.beam,$(SOURCES))


