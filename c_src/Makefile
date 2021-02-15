ERLANG_PATH = $(shell erl -args_file get_erl_path.args)
BIN_DIR=../priv
C_SRC=$(wildcard *.c)
OBJECTS=$(C_SRC:.c=.o)

CFLAGS=-g -Wall -fpic -O2
CPPFLAGS=-g -Wall -fpic -I$(ERLANG_PATH) -O2
LDFLAGS=-shared

$(BIN_DIR)/rb_sets.so: $(OBJECTS)
	mkdir -p $(BIN_DIR)
	$(CC) -o $@ $^ $(CPPFLAGS) $(LDFLAGS)

%.o: %.c
	$(CC) $(CPPFLAGS) -c $< -o $@

clean:
	rm -rf ../priv/rb_sets.so ./*.o
