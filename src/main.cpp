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
  size_t size = 8;
  float msb_value = -10.0;
  float value = -1.0;

  // profile put_data example
  word.put_data<float>(value, lsb, size, msb_value);

  std::cout << "in value = " << value << "\n";
  std::cout << word << "\n";
  std::cout << "mil = " << word.to_mil() << "\n";
  std::cout << "out value = " << word.get_data<float>(lsb, size, msb_value) << "\n";

  return 0;
}
