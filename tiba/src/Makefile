ERLC = erlc
EBIN = ../ebin
ERLINC = ../include
ERL_FLAGS = -I$(ERLINC) +warn_unused_vars +debug_info

MODULES = \
	tibia_files\
	tibia_iomap\
	tibia_items\
	tibia_login\
	tibia_message\
	tibia_parse\
	tibia_proxy\
	xtea

INCLUDES =

TARGETS = $(MODULES:%=$(EBIN)/%.beam)

all:	$(TARGETS)

clean:
	rm -f  $(TARGETS) erl_crash.dump

$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERL_FLAGS) -o$(EBIN) $<
