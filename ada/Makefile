all: lib
	gnatmake src/my_main.adb -gnat95 -o test.exe -Wl,rpath=./ -largs -ltest --LINK=c++
lib:
	g++-8 -c src/test.cpp  -Wall -fPIC -O3 -std=c++17
	gnatmake src/my_lib.adb -a -c -fPIC -gnat95 -largs test.o --LINK=c++
	gcc --shared -fPIC -o libtest.so *.o
clean:
	rm *.o
	rm *.so
	rm *.ali
	rm ada_test.exe
