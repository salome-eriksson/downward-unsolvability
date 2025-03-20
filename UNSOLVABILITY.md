To run a Fast Downward with certificate generation, you need to have CUDD
installed. If you set up the environment variable CUDD_DIR, cmake should find
it.

Step by step guide:

(In what follows <path-to-cudd> is the path where you want CUDD to be
installed to)
1. Download CUDD 3.0.0  as a zip from here: https://github.com/ivmai/cudd/archive/refs/heads/release.zip
 (unofficial
mirror https://github.com/ivmai/cudd)
2. Unpack the archive.
3. In the folder cudd-release call the following steps to get the 64-bit
library with dddmp and c++-wrapper:

        ./configure --prefix=\<path-to-cudd\> --enable-shared --enable-dddmp --enable-obj --enable-static "CFLAGS=-D_FILE_OFFSET_BITS=64" "CXXFLAGS=-D_FILE_OFFSET_BITS=64"
        && make
        && make install

4. Move the following two header files config.h and util/util.h to \<path-to-cudd\>/include:

        cp config.h \<path-to-cudd\>/include
        && cp util/util.h \<path-to-cudd\>/include

  (I don't know why this is necessary, but else the dddmp library complains...)

5. Set the environment variable CUDD_DIR to \<path-to-cudd\> (or change the
Makefile, adding the path in place of the variable).
