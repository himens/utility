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
  bool reverse = true;

  word.put_data<int>(1, 0,  1, reverse);
  word.put_data<int>(7, 1,  3, reverse);
  word.put_data<int>(7, 4,  4, reverse);
  word.put_data<int>(7, 8,  4, reverse);
  word.put_data<int>(7, 12, 4, reverse);

  std::cout << word << "\n";
  word.reset();

  // profiling put_data
  int val = 1;
  size_t lsb = 0;
  size_t size = word.get_size();

  Timer timer("put_data");
  timer.start();
  {
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
    word.put_data<int>(val, lsb, size, reverse);
  }
  timer.stop();

  std::cout << timer << "\n";
  std::cout << word << "\n";

  return 0;
}
