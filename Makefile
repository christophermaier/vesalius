DEPS = deps/getopt

all: escript

clean:
	@./rebar clean

$(DEPS):
	@./rebar get-deps

distclean:
	@rm -rf deps
	@./rebar clean

compile: $(DEPS)
	@./rebar compile

escript: compile
	@./rebar escriptize
