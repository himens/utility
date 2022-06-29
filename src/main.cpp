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

  size_t lsb = 0;
  size_t msb = 15;
  float msb_value = -180.0;
  float value = 180.0;

  // profile put_data example
  word.put_data<float>(value, lsb, msb, msb_value);

  std::cout << "in value = " << value << "\n";
  std::cout << word << "\n";
  std::cout << word.to_mil() << " " << word.to_umil() << "\n";
  std::cout << "out value = " << word.get_data<float>(lsb, msb, msb_value) << "\n";

  return 0;
}
