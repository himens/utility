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
  Timer timer;
  Word word;

  //size_t lsb = 0;
  //size_t size = 16;
  //float msb_value = -180.0;
  //float value = 180.0;

  // profile example
  size_t lsb = 2;
  size_t size = 12;
  float msb_value = 10.0;
  float value = 0;
  float out_value = 0;

  timer.set_name("example");
  timer.start();
  {
    word.put_data<float>(value, lsb, size, msb_value);
    out_value = word.get_data<float>(lsb, size, msb_value);
  }
  timer.stop();

  std::cout << "msb_value = " << msb_value << "\n";
  std::cout << "in value = " << value << "\n";
  std::cout << word << "\n";
  std::cout << "mil = " << word.to_mil() << "\n";
  std::cout << "out value = " << out_value << "\n";
  std::cout << timer << "\n";

  word.reset();
  word.put_data<char>('d', 0, 8, 0);
  word.put_data<char>('i', 8, 8, 0);
  char d = word.get_data<char>(0, 8, 0);
  char i = word.get_data<char>(8, 8, 0);
  std::cout << d << " " << i << "\n";

  return 0;
}
