all: lib
	gnatmake src/my_main.adb -gnat2012 -o test.exe -Wl,rpath=./ -largs -ltest --LINK=c++
lib:
	g++-8 -c src/test.cpp  -Wall -fPIC -O3 -std=c++17
	gnatmake src/my_lib.adb -a -c -fPIC -gnat2012 -largs test.o --LINK=c++
	gcc --shared -fPIC -o libtest.so *.o
clean:
	rm -f *.o
	rm -f *.so
	rm -f *.ali
	rm -f *.exe
