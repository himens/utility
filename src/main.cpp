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

const size_t WORD_SIZE = 16;

typedef Data<WORD_SIZE> Word;
typedef Data<2 * WORD_SIZE> BusData;

///////////////////
// BusData class //
///////////////////
//class BusData : public Data<2 * Word::SIZE>
//{
//  public:
//    using Data::Data;
//
//    static const size_t NUMBER_OF_WORDS = 2;
//
//    // get data word 
//    Word get_word(const size_t i) const 
//    {
//      if (i >= NUMBER_OF_WORDS) throw std::out_of_range("BusData::get_word: index out-of-range!");
//
//      bitset_t bits = _data & get_mask(i * Word::SIZE, Word::SIZE);
//      bits >>= i * Word::SIZE;
//      Word word = bits.to_ulong();
//
//      return word;
//    }
//};

int main(int argc, char **argv)
{
  uint16_t *dest = new uint16_t[10];

  Timer timer;
  BusData data;

  // profile example
  size_t lsb = 0;
  size_t size = 19;
  float msb_value = 32.0;
  float value = 72.1271;

  float out_value = 0;
  timer.set_name("example");
  timer.start();
  {
    data.put<float>(value, lsb, size, msb_value);
    out_value = data.get<float>(lsb, size, msb_value);

    dest[0] |= data.to_mil();
    dest[1] |= (data.to_mil() >> WORD_SIZE);
  }
  timer.stop();

  std::cout << "in value = " << value << "\n";
  std::cout << "out value = " << out_value << "\n";
  std::cout << data << "\n";
  std::cout << "msb_value = " << msb_value << "\n";
  std::cout << "mil = " << data.to_mil() << "\n";
  std::cout << "dest = " << dest[0] << " " << dest[1] << "\n";
  std::cout << timer << "\n";

  data.reset();
  data.put<char>('d', 0, 8, 0);
  data.put<char>('i', 8, 8, 0);
  char d = data.get<char>(0, 8, 0);
  char i = data.get<char>(8, 8, 0);
  std::cout << d << " " << i << "\n";

  return 0;
}

