all:
	g++ -c test.cpp  -Wall -fPIC -O3 -std=c++11
	gnatmake my_main.adb -o ada_test -largs test.o --LINK=c++
	#gnatmake my_main.adb --RTS=/home/stefano/gnat_runtime/ -o ada_test -largs test.o --LINK=c++
clean:
	rm *.o
	rm *.ali
	rm ada_test
