 

ERL_CFLAGS  = -I/opt/erl/lib/erlang/lib/erl_interface-3.7.7/include -I/opt/erl/lib/erlang/usr/include
ERL_LIB     = -L/opt/erl/lib/erlang/lib/erl_interface-3.7.7/lib -lerl_interface -lei
CFLAGS      = -g -O2 -Wall -m64
LDFLAGS     =  -m64

#CFLAGS=-Wall -I/usr/local/include ${ERL_CFLAGS}
ERL_LDFLAGS= ${LDFLAGS} ${ERL_LIB}


gsuffixtree:	main.o gsuffix_tree.o
	gcc  ${ERL_LDFLAGS} main.o gsuffix_tree.o -lerl_interface -lei -lpthread -o gsuffixtree 

gsuffix_tree.o:	gsuffix_tree.c gsuffix_tree.h
	gcc ${CFLAGS} ${ERL_CFLAGS} -o gsuffix_tree.o -c gsuffix_tree.c

main.o:	gsuffix_tree.h 
	gcc ${CFLAGS} ${ERL_CFLAGS} -o main.o -c main.c 

clean: 
	-rm -f *.o 
	-rm -f gsuffixtree* 

