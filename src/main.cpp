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

  // profile put_data example
  timer.start();
  {
    word.put_data<int>(1, 0,  0,  reverse);
    word.put_data<int>(7, 1,  3,  reverse);
    word.put_data<int>(7, 4,  7,  reverse);
    word.put_data<int>(7, 8,  11, reverse);
    word.put_data<int>(7, 12, 15, reverse);
  }
  timer.stop();
  std::cout << "Elapsed time(put_data example): " <<  timer.get_elapsed_time_us() << "us \n"; 
  std::cout << word << "\n \n";

  // profile to_ulong
  unsigned long val;

  timer.reset();
  timer.start();
  {
    val = word.to_ulong(4, 7, reverse);
  }
  timer.stop();
  word.reset();
  std::cout << "Elapsed time(to_ulong): " << timer.get_elapsed_time_us() << "us \n"; 
  std::cout << "Extracted value: " << val << "\n\n";

  // profiling swap_bytes
  word.reset();
  word.put_data<int>(5, 0, word.get_size() - 1);
  
  timer.reset();
  timer.start();
  {
    word.swap_bytes();
  }
  timer.stop();
  std::cout << "Elapsed time(swap_byte): " <<  timer.get_elapsed_time_us() << "us \n"; 
  std::cout << word << "\n \n";

  // profiling put_data
  word.reset();
  timer.reset();
  timer.start();
  {
    word.put_data<int>(1, 0, word.get_size() - 1, reverse);
  }
  timer.stop();
  std::cout << "Elapsed time(put_data): " <<  timer.get_elapsed_time_us() << "us \n"; 
  std::cout << word << "\n \n";
  word.reset();

  return 0;
}
