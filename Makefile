.PHONY: all get-deps clean compile run eunit check

REBAR=$(shell which rebar || echo ./rebar)

# eventually this should be just ebin/*.beam, but there are a number
# of warnings in other files. Just check the clean files for now.
CHECK_FILES=\
	ebin/task_manager.beam \
	ebin/task_worker.beam \
	ebin/utils.beam

all: get-deps compile

get-deps:
	@$(REBAR) get-deps
	
clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

run:
	erl -pa deps/*/ebin -pa ./ebin

eunit: compile
	@$(REBAR) eunit skip_deps=true

check: compile
	dialyzer --verbose --no_check_plt --no_native --fullpath \
		$(CHECK_FILES) \
		-Wunmatched_returns \
		-Werror_handling

