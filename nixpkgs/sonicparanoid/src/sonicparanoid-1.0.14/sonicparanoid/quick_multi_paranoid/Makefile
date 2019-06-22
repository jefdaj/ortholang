include ./Makefile.in

CC = gcc
CPP = g++

# must match INTERMEDIATE_HEADER_FILE defined in qp.h.
INTERMEDIATE_HEADER_FILE=__ortholog.h

all: $(EXEC_FILE) $(EXEC_FILEs) qa

dump: $(CONFIG_FILE) dump.cpp qp.h
	$(CPP) -O3 -o dump dump.cpp
	./dump $(CONFIG_FILE)

gen_header: dump gen_header.cpp qp.h
	$(CPP) -O3 -o gen_header gen_header.cpp

$(EXEC_FILE): ortholog.o hashtable.o hashtable_itr.o qp.c qp.h
	$(CC) -O3 -o $(EXEC_FILE) hashtable.o hashtable_itr.o ortholog.o qp.c  -lm

$(EXEC_FILEs): $(CONFIG_FILE) qp.h qps.c
	$(CC) -O3 -o $(EXEC_FILEs) qps.c

qa: qa1 qa2

qa1: qa.h qa1.cpp
	$(CPP) -O3 -o qa1 qa1.cpp

qa2: qa.h hashtable.o qa2.cpp
	$(CPP) -O3 -o qa2 hashtable.o qa2.cpp

$(INTERMEDIATE_HEADER_FILE): $(CONFIG_FILE) gen_header
	./gen_header $(CONFIG_FILE) $(INTERMEDIATE_HEADER_FILE)

ortholog.o: $(INTERMEDIATE_HEADER_FILE) ortholog.c qa.h
	$(CC) -O3 -c ortholog.c -o ortholog.o

hashtable.o: hashtable_private.h hashtable.h hashtable.c
	$(CC) -O3 -c hashtable.c -o hashtable.o

hashtable_itr.o: hashtable_private.h hashtable_itr.h hashtable_itr.c
	$(CC) -O3 -c hashtable_itr.c -o hashtable_itr.o

clean :
	rm -fr *.o $(INTERMEDIATE_HEADER_FILE) dump gen_header $(EXEC_FILE) $(EXEC_FILEs) qa1 qa2 *.exe qp.h QuickParanoid.tar.* QuickParanoid.zip

copy: clean
	tar cvf QuickParanoid.tar *
	gzip QuickParanoid.tar
	cp -f QuickParanoid.tar.gz /var/www/html/QuickParanoid/
	rm -f QuickParanoid.tar.gz
	zip QuickParanoid.zip *
	cp -f QuickParanoid.zip /var/www/html/QuickParanoid/
	rm -f QuickParanoid.zip
