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
    word.put_data<int>(1, 0,  1, reverse);
    word.put_data<int>(7, 1,  3, reverse);
    word.put_data<int>(7, 4,  4, reverse);
    word.put_data<int>(7, 8,  4, reverse);
    word.put_data<int>(7, 12, 4, reverse);
  }
  timer.stop();

  std::cout << timer << "\n"; 
  std::cout << word << "\n \n";
  word.reset();

  // profiling swap_bytes
  word.put_data<int>(5, 0, word.get_size(), reverse);
  
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
  int val = 1;
  size_t lsb = 0;
  size_t size = word.get_size();

  timer.set_name("put_data");
  timer.reset();
  timer.start();
  {
    word.put_data<int>(val, lsb, size, reverse);
  }
  timer.stop();

  std::cout << timer << "\n";
  std::cout << word << "\n \n";
  word.reset();

  return 0;
}
