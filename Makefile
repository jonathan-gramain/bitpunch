BUILD_DIR = build
LBITPUNCH_DIR = libbitpunch
LBITPUNCH_SRCDIR = $(LBITPUNCH_DIR)/src
LBITPUNCH_OBJDIR = $(LBITPUNCH_DIR)/obj
LBITPUNCH_TMPDIR = $(LBITPUNCH_DIR)/tmp
LIB_DIR = $(BUILD_DIR)/lib
BIN_DIR = $(BUILD_DIR)/bin

TESTS_DIR = tests
UTESTS_DIR = $(TESTS_DIR)/unit
CHECK_DIR = $(UTESTS_DIR)/check
CHECK_SRCDIR = $(CHECK_DIR)
CHECK_OBJDIR = $(CHECK_DIR)/obj

INCS = -I$(LBITPUNCH_DIR)/include -I.
EXTRA_INCDIR = $(BUILD_DIR)/$(LBITPUNCH_DIR)/tmp

CC = gcc
CFLAGS_COMMON = -g3 -O0 -Wall -DDEBUG
CFLAGS_YACC = $(CFLAGS_COMMON) -fPIC
CFLAGS_LBITPUNCH = $(CFLAGS_COMMON) -fPIC -Werror
CFLAGS_CHECK = $(CFLAGS_COMMON) -Werror

LEXSRC_LBITPUNCH = $(addprefix $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/,core/parser.l.c core/parser.tab.c)
LEXHDR_LBITPUNCH = $(addprefix $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/,core/parser.tab.h)
SRC_LBITPUNCH = $(addprefix $(LBITPUNCH_SRCDIR)/,api/bitpunch_api.c core/ast.c core/expr.c core/browse.c core/interpreter.c core/print.c core/debug.c interpreters/integer.c interpreters/varint.c interpreters/string.c interpreters/base64.c interpreters/snappy.c utils/bloom.c utils/port.c)
SRC_CHECK_BITPUNCH = $(addprefix $(CHECK_SRCDIR)/,check_bitpunch.c check_array.c check_struct.c check_slack.c check_tracker.c check_cond.c check_dynarray.c testcase_radio.c)
OBJ_LBITPUNCH = $(patsubst $(LBITPUNCH_SRCDIR)/%.c,$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o,$(SRC_LBITPUNCH)) $(patsubst $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.c,$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o,$(LEXSRC_LBITPUNCH))
OBJ_CHECK_BITPUNCH = $(patsubst $(CHECK_SRCDIR)/%.c,$(BUILD_DIR)/$(CHECK_OBJDIR)/%.o,$(SRC_CHECK_BITPUNCH))
OBJ_ALL = $(OBJ_LBITPUNCH) $(OBJ_CHECK_BITPUNCH)
DEPS_ALL = $(patsubst %.o,%.d,$(OBJ_ALL))
CHECK_LIBS = `pkg-config --libs check`
LIBS_LBITPUNCH = -lfl -L/usr/local/lib -lreadline -ltermcap $(CHECK_LIBS) -lsnappy
LIBS_CHECK_BITPUNCH = $(LIBS_LBITPUNCH) -Wl,-rpath=. -L$(LIB_DIR) -lbitpunch $(CHECK_LIBS) -lm

LBITPUNCH = $(LIB_DIR)/libbitpunch.so
CHECK_BITPUNCH = $(BIN_DIR)/check_bitpunch

.PHONY: all pythonlib clean


all: $(LBITPUNCH) $(CHECK_BITPUNCH) pythonlib

%/.dir:
	mkdir -p $(dir $@)
	touch $@

.PRECIOUS: %/.dir

# prevent automatic removal of these files
.SECONDARY: $(LEXSRC_LBITPUNCH) $(LEXHDR_LBITPUNCH)

clean:
	rm -rf $(BUILD_DIR)

check: $(CHECK_BITPUNCH)
	$(CHECK_BITPUNCH)
	./tests/run_pytests.sh

pythonlib:
	python ./setup.py build

# needed to expand $(dir ...) to pre-create directories
.SECONDEXPANSION:

$(LBITPUNCH): $$(OBJ_LBITPUNCH) | $$(@D)/.dir
	$(CC) $(LDFLAGS) -shared -o $@ $(OBJ_LBITPUNCH) $(LIBS_LBITPUNCH)

$(CHECK_BITPUNCH): $$(OBJ_CHECK_BITPUNCH) $$(LBITPUNCH) | $$(@D)/.dir
	$(CC) $(LDFLAGS) -o $@ $(OBJ_LBITPUNCH) $(OBJ_CHECK_BITPUNCH) $(INCS) $(LIBS_CHECK_BITPUNCH)

$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.d: CFLAGS = $(CFLAGS_LBITPUNCH)
$(BUILD_DIR)/$(UTESTS_DIR)/%.o $(BUILD_DIR)/$(UTESTS_DIR)/%.d: CFLAGS = $(CFLAGS_CHECK)


$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.d: $(LBITPUNCH_SRCDIR)/%.c | $$(@D)/.dir
	gcc -c $(CFLAGS) -MM -MG -MT$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.o $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.d $<
	gcc -c $(CFLAGS) $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.o $<

$(BUILD_DIR)/$(CHECK_OBJDIR)/%.o $(BUILD_DIR)/$(CHECK_OBJDIR)/%.d: $(CHECK_SRCDIR)/%.c | $$(@D)/.dir
	gcc -c $(CFLAGS) -MM -MG -MT$(BUILD_DIR)/$(CHECK_OBJDIR)/$*.o $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(CHECK_OBJDIR)/$*.d $<
	gcc -c $(CFLAGS) $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(CHECK_OBJDIR)/$*.o $<



$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.l.o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.l.d: $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.l.c | $$(@D)/.dir
	gcc -c $(CFLAGS_YACC) -MM -MG -MT$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.l.o $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.l.d $<
	gcc -c $(CFLAGS_YACC) $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.l.o $<

$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.tab.o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/%.tab.d: $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.c | $$(@D)/.dir
	gcc -c $(CFLAGS) -MM -MG -MT$(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.tab.o $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.tab.d $<
	gcc -c $(CFLAGS) $(INCS) -I$(EXTRA_INCDIR) -o $(BUILD_DIR)/$(LBITPUNCH_OBJDIR)/$*.tab.o $<

$(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.l.c: $(LBITPUNCH_SRCDIR)/%.l $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.h | $$(@D)/.dir
	flex --yylineno -o $@ $<

$(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.c $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/%.tab.h: $(LBITPUNCH_SRCDIR)/%.y | $$(@D)/.dir
	bison --defines=$(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/$*.tab.h -o $(BUILD_DIR)/$(LBITPUNCH_TMPDIR)/$*.tab.c $<

-include $(DEPS_ALL)
