BITPUNCH_BUILD_DIR ?= build
LBITPUNCH_DIR = libbitpunch
LBITPUNCH_SRCDIR = $(LBITPUNCH_DIR)/src
LBITPUNCH_OBJDIR = $(LBITPUNCH_DIR)/obj
LBITPUNCH_TMPDIR = $(LBITPUNCH_DIR)/tmp
LIB_DIR = $(BITPUNCH_BUILD_DIR)/lib
BIN_DIR = $(BITPUNCH_BUILD_DIR)/bin
SCRIPTS_DIR = scripts

TESTS_DIR = tests
UTESTS_DIR = $(TESTS_DIR)/unit
CHECK_DIR = $(UTESTS_DIR)/check
CHECK_SRCDIR = $(CHECK_DIR)
CHECK_OBJDIR = $(CHECK_DIR)/obj

INCS = -I$(LBITPUNCH_DIR)/include -I.
EXTRA_INCDIR = $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_DIR)/tmp

PATH_TO_PARSER_TAB_H="\"$(BITPUNCH_BUILD_DIR)/libbitpunch/tmp/core/parser.tab.h\""

CC = gcc
CFLAGS_COMMON = -g3 -O0 -Wall -DDEBUG -DPATH_TO_PARSER_TAB_H=$(PATH_TO_PARSER_TAB_H)
CFLAGS_YACC = $(CFLAGS_COMMON) -fPIC
CFLAGS_LBITPUNCH = $(CFLAGS_COMMON) -fPIC -Werror
CFLAGS_CHECK = $(CFLAGS_COMMON) -Werror

LEXSRC_LBITPUNCH = $(addprefix $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/,core/parser.l.c core/parser.tab.c)
LEXHDR_LBITPUNCH = $(addprefix $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/,core/parser.tab.h)
SRC_LBITPUNCH = $(addprefix $(LBITPUNCH_SRCDIR)/,api/bitpunch_api.c api/schema.c api/data_source.c core/ast.c core/expr.c core/browse.c core/scope.c core/filter.c core/print.c core/debug.c filters/file.c filters/item.c filters/container.c filters/byte.c filters/composite.c filters/array.c filters/byte_array.c filters/array_slice.c filters/byte_slice.c filters/array_index_cache.c filters/integer.c filters/varint.c filters/bytes.c filters/string.c filters/base64.c filters/snappy.c filters/formatted_integer.c utils/dep_resolver.c utils/bloom.c utils/port.c)
SRC_CHECK_BITPUNCH = $(addprefix $(CHECK_SRCDIR)/,check_bitpunch.c check_array.c check_struct.c check_slack.c check_tracker.c check_cond.c check_dynarray.c testcase_radio.c)
OBJ_LBITPUNCH = $(patsubst $(LBITPUNCH_SRCDIR)/%.c,$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o,$(SRC_LBITPUNCH)) $(patsubst $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.c,$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o,$(LEXSRC_LBITPUNCH))
OBJ_CHECK_BITPUNCH = $(patsubst $(CHECK_SRCDIR)/%.c,$(BITPUNCH_BUILD_DIR)/$(CHECK_OBJDIR)/%.o,$(SRC_CHECK_BITPUNCH))
OBJ_ALL = $(OBJ_LBITPUNCH) $(OBJ_CHECK_BITPUNCH)
DEPS_ALL = $(patsubst %.o,%.d,$(OBJ_ALL))
CHECK_LIBS = `pkg-config --libs check`
LIBS_LBITPUNCH = -lfl -L/usr/local/lib -lreadline -ltermcap $(CHECK_LIBS) -lsnappy
LIBS_CHECK_BITPUNCH = $(LIBS_LBITPUNCH) -Wl,-rpath=. -L$(LIB_DIR) -lbitpunch $(CHECK_LIBS) -lm

LBITPUNCH = $(LIB_DIR)/libbitpunch.so
CHECK_BITPUNCH = $(BIN_DIR)/check_bitpunch
BITPUNCH_CLI = bitpunch
BITPUNCH_CLI_DEBUG = bitpunch.debug

.PHONY: all pythonlib clean


all: $(LBITPUNCH) $(CHECK_BITPUNCH) pythonlib cli

%/.dir:
	mkdir -p $(dir $@)
	touch $@

.PRECIOUS: %/.dir

# prevent automatic removal of these files
.SECONDARY: $(LEXSRC_LBITPUNCH) $(LEXHDR_LBITPUNCH)

clean:
	rm -rf $(BITPUNCH_BUILD_DIR)

check: $(CHECK_BITPUNCH) pythonlib
	$(CHECK_BITPUNCH)
	BITPUNCH_BUILD_DIR=$(BITPUNCH_BUILD_DIR) ./tests/run_pytests.sh

pythonlib:
	BITPUNCH_BUILD_DIR=$(BITPUNCH_BUILD_DIR) python ./setup.py build --debug --build-base=$(BITPUNCH_BUILD_DIR)

cli: $(BIN_DIR)/$(BITPUNCH_CLI) $(BIN_DIR)/$(BITPUNCH_CLI_DEBUG)

$(BIN_DIR)/$(BITPUNCH_CLI): $(SCRIPTS_DIR)/$(BITPUNCH_CLI)
	cp -a $(SCRIPTS_DIR)/$(BITPUNCH_CLI) $(BIN_DIR)/
$(BIN_DIR)/$(BITPUNCH_CLI_DEBUG): $(SCRIPTS_DIR)/$(BITPUNCH_CLI_DEBUG)
	cp -a $(SCRIPTS_DIR)/$(BITPUNCH_CLI_DEBUG) $(BIN_DIR)/

dot: deps.svg

deps.svg: deps.dot
	dot -Tsvg deps.dot > deps.svg

# needed to expand $(dir ...) to pre-create directories
.SECONDEXPANSION:

$(LBITPUNCH): $$(OBJ_LBITPUNCH) | $$(@D)/.dir
	$(CC) $(LDFLAGS) -shared -o $@ $(OBJ_LBITPUNCH) $(LIBS_LBITPUNCH)

$(CHECK_BITPUNCH): $$(OBJ_CHECK_BITPUNCH) $$(LBITPUNCH) | $$(@D)/.dir
	$(CC) $(LDFLAGS) -o $@ $(OBJ_LBITPUNCH) $(OBJ_CHECK_BITPUNCH) $(INCS) $(LIBS_CHECK_BITPUNCH)

$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.d: CFLAGS = $(CFLAGS_LBITPUNCH)
$(BITPUNCH_BUILD_DIR)/$(UTESTS_DIR)/%.o $(BITPUNCH_BUILD_DIR)/$(UTESTS_DIR)/%.d: CFLAGS = $(CFLAGS_CHECK)


$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.d: $(LBITPUNCH_SRCDIR)/%.c | $$(@D)/.dir
	gcc -c $(CFLAGS) -MM -MG -MT$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.o $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.d $<
	gcc -c $(CFLAGS) $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.o $<

$(BITPUNCH_BUILD_DIR)/$(CHECK_OBJDIR)/%.o $(BITPUNCH_BUILD_DIR)/$(CHECK_OBJDIR)/%.d: $(CHECK_SRCDIR)/%.c | $$(@D)/.dir
	gcc -c $(CFLAGS) -MM -MG -MT$(BITPUNCH_BUILD_DIR)/$(CHECK_OBJDIR)/$*.o $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(CHECK_OBJDIR)/$*.d $<
	gcc -c $(CFLAGS) $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(CHECK_OBJDIR)/$*.o $<



$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.l.o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.l.d: $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.l.c | $$(@D)/.dir
	gcc -c $(CFLAGS_YACC) -MM -MG -MT$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.l.o $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.l.d $<
	gcc -c $(CFLAGS_YACC) $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.l.o $<

$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.tab.o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.tab.d: $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.c | $$(@D)/.dir
	gcc -c $(CFLAGS) -MM -MG -MT$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.tab.o $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.tab.d $<
	gcc -c $(CFLAGS) $(INCS) -I$(EXTRA_INCDIR) -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.tab.o $<

$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.l.c: $(LBITPUNCH_SRCDIR)/%.l $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.h | $$(@D)/.dir
	flex --yylineno -o $@ $<

$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.c $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.h: $(LBITPUNCH_SRCDIR)/%.y | $$(@D)/.dir
	bison --defines=$(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/$*.tab.h -o $(BITPUNCH_BUILD_DIR)/$(LBITPUNCH_TMPDIR)/$*.tab.c $<

ifneq ($(MAKECMDGOALS),clean)
-include $(DEPS_ALL)
endif
