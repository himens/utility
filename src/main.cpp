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
  Word word;

  //size_t lsb = 0;
  //size_t size = 16;
  //float msb_value = -180.0;
  //float value = 180.0;

  size_t lsb = 2;
  size_t size = 12;
  float msb_value = -10.0;
  float value = -3.1;

  // profile put_data example
  word.put_data<float>(value, lsb, size, msb_value);
  float out_value = word.get_data<float>(lsb, size, msb_value);

  std::cout << "in value = " << value << "\n";
  std::cout << word << "\n";
  std::cout << "mil = " << word.to_mil() << "\n";
  std::cout << "out value = " << out_value << "\n";

  word.reset();
  word.put_data<char>('d', 0, 8, 0);
  word.put_data<char>('i', 8, 8, 0);
  char d = word.get_data<char>(0, 8, 0);
  char i = word.get_data<char>(8, 8, 0);
  std::cout << d << " " << i << "\n";

  return 0;
}
