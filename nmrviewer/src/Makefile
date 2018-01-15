#CC      = gcc -w -g -O3
CC      = gcc -w -g -O
SCFLAGS  = -fno-builtin-exit -fno-builtin-strcat -fno-builtin-strncat -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-calloc
CFLAGS  = $(SCFLAGS) -I/usr/include -c
LD      = $(CC)
LDFLAGS = -L/usr/lib -lm 
bindir = /usr/local/bin

all: 
	make pack
	make bin4gp

bin4gp: bin4gp_1r.o
	$(CC) $(LDFLAGS) -o $(bindir)/$(@)_1r $(@)_1r.c
	chmod 755  $(bindir)/$(@)_1r

pack: pack_1r.o
	$(CC) $(LDFLAGS) -o $(bindir)/$(@)_1r $(@)_1r.c
	chmod 755  $(bindir)/$(@)_1r

.c.o:
	$(CC) $(CFLAGS) $<
