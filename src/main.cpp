#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>

#include "data.h"
#include "timer.h"

int main(int argc, char **argv)
{
  uint16_t *dest = new uint16_t[10];

  Timer timer;
  Data data = {dest};

  // put float example
  std::cout << "put float: \n";

  size_t lsb = 8;
  size_t size = 19;
  float msb_value = 32;
  float value = 31.4578;
  float out_value = 0;

  // profile put_data example
  timer.set_name("example");
  timer.start();
  {
    data.put<float>(value, lsb, size, msb_value);
    out_value = data.get<float>(lsb, size, msb_value);
  }
  timer.stop();

  std::cout << "in value = " << value << "\n";
  std::cout << "out value = " << out_value << "\n";
  std::cout << "msb_value = " << msb_value << "\n";
  std::cout << "dest = " << dest[0] << " " << dest[1] << "\n";
  std::cout << timer << "\n \n";

  data.put<char>('b', 0, 8);
  std::cout << data.get<char>(0, 8) << "\n";

  // put char example
  std::cout << "put char: \n";

  std::string str = "dio";
  std::string out_str = "";

  for (size_t i = 0; i < str.size(); i++)
  {
    data = {dest + 5 + i};
    data.put<char>(str[i], 0, 8, 0);
    out_str += data.get<char>(0, 8, 0);
  }

  std::cout << "in string = " << str <<  "\n";
  std::cout << "out string = " << out_str <<  "\n";

  return 0;
}
