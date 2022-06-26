#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>

#include "word.h"
#include "timer.h"

int main(int argc, char **argv)
{
  Word<16> word;
  Timer timer;
  bool reverse = true;

  // profile put_data exmaple
  timer.set_name("put_data example");
  timer.start();
  {
    word.put_data<int>(1, 0,  0,  reverse);
    word.put_data<int>(7, 1,  3,  reverse);
    word.put_data<int>(7, 4,  7,  reverse);
    word.put_data<int>(7, 8,  11, reverse);
    word.put_data<int>(7, 12, 15, reverse);
  }
  timer.stop();

  std::cout << timer << "\n"; 
  std::cout << word << "\n \n";

  // profile get_data
  unsigned long val;

  timer.set_name("get_data");
  timer.reset();
  timer.start();
  {
    val = word.get_data(4, 7, reverse);
  }
  timer.stop();

  std::cout << timer << "\n"; 
  std::cout << "Extracted value = " << val << "\n\n";
  word.reset();

  // profiling swap_bytes
  word.put_data<int>(5, 0, word.get_size() - 1, reverse);
  
  timer.set_name("swap_bytes");
  timer.reset();
  timer.start();
  {
    word.swap_bytes();
  }
  timer.stop();

  std::cout << timer << "\n";
  std::cout << word << "\n \n";
  word.reset();

  // profiling put_data
  timer.set_name("put_data");
  timer.reset();
  timer.start();
  {
    word.put_data<int>(1, 0, word.get_size() - 1, reverse);
  }
  timer.stop();

  std::cout << timer << "\n";
  std::cout << word << "\n \n";
  word.reset();

  return 0;
}
