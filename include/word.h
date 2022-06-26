#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <memory>
#include <algorithm>
#include <math.h>
#include <byteswap.h>
#include <bitset>

#ifndef WORD_H
#define WORD_H

////////////////
// Word class //
////////////////
template <size_t SIZE>
class Word 
{
  public:
    // Bits type definition
    typedef std::bitset<SIZE> Bits;

    // constructor
    Word() {}

    // print word
    friend std::ostream& operator<<(std::ostream& os, const Word w) 
    {
      os << "[Word " << SIZE << "-bit] Data: " << w.get_data();

      return os;
    }

    // get word size
    size_t get_size() const { return SIZE; }

    // get word data 
    Bits get_data() const { return _word; }

    // convert to unsigned types
    uint16_t to_uint8()  const { return static_cast<uint8_t>(_word.to_ulong()); }
    uint16_t to_uint16() const { return static_cast<uint16_t>(_word.to_ulong()); }
    uint16_t to_uint32() const { return static_cast<uint32_t>(_word.to_ulong()); }
    uint16_t to_uint64() const { return static_cast<uint64_t>(_word.to_ulong()); }
    uint16_t to_ulong()  const { return _word.to_ulong(); }

    // reset word bits to zero
    void reset() { _word.reset(); }

    // reverse word bits
    void reverse_bits() { reverse_bits(_word); }

    // swap word bytes
    void swap_bytes() 
    { 
      if (SIZE < 16) return;
      else if (SIZE == 16) _word = Bits(__bswap_16(_word.to_ulong())); 
      else if (SIZE == 32) _word = Bits(__bswap_32(_word.to_ulong())); 
      else if (SIZE == 64) _word = Bits(__bswap_64(_word.to_ulong())); 
      else 
      {
	std::cout << "[WARNING] Word::swap_bytes: bytes swap for word size " << SIZE << " not supported! \n";
      }
    }

    // put data to word
    template <typename T>
    void put_data(const T &data, const size_t lsb, const size_t size, const bool reverse = false)
    {
      if (size < 0 || size > SIZE) throw std::out_of_range("put_data: size out-of-range!");
      if (lsb < 0 || lsb > (SIZE - 1)) throw std::out_of_range("put_data: lsb out-of-range!");
      if ((lsb + size) > SIZE) throw std::range_error("put_data: lsb + size greater than word size!");

      size_t msb = lsb + size - 1;
      Bits bits(data);
      Bits mask = ~Bits(0);

      mask = (mask >> (SIZE - 1 - msb)) & (mask << lsb);
      bits = (bits << lsb) & mask;
      if (reverse) reverse_bits(bits, lsb, msb);

      _word |= bits;
    }

  private:
    // reverse bits from lsb to msb
    void reverse_bits(Bits &bits, const size_t lsb = 0, const size_t msb = SIZE - 1)
    {
      if (lsb > msb) throw std::range_error("reverse_bits: lsb greater than msb!");
      if (lsb < 0 || msb > (SIZE - 1)) throw std::out_of_range("reverse_bits: lsb or msb out-of-range!");

      size_t n_bits = msb - lsb + 1;

      for (size_t i = 0; i < (n_bits / 2); i++)
      {
        bool bit = bits[lsb + i];

        bits[lsb + i] = bits[msb - i];
        bits[msb - i] = bit;
      }
    }

    // data members
    Bits _word;  
};
#endif
