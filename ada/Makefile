all:
	g++-8 -c src/test.cpp  -Wall -fPIC -O3 -std=c++17
	gnatmake src/my_main.adb -a -gnat95 -o ada_test.exe -largs test.o --LINK=c++
	#gnatmake my_main.adb -gnat95 --RTS=/home/stefano/gnat_runtime/ -o ada_test.exe -largs test.o --LINK=c++
clean:
	rm *.o
	rm *.ali
	rm ada_test.exe
