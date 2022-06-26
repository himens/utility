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
      os << "Word data: " << w.get_data();

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

    // get data stored between bits [lsb, msb]
    unsigned long get_data(const size_t lsb, const size_t msb, const bool reverse = false)
    {
      check_bit_range(lsb, msb);

      size_t n_bits = msb - lsb + 1;
      Bits bits = _word & get_mask(lsb, msb); // extract bits in [lsb, msb]
      bits >>= lsb; // shift back to bit 0
      if (reverse) reverse_bits(bits, 0, n_bits - 1);

      return bits.to_ulong();
    }

    // put data to word between bits [lsb, msb]
    template <typename T>
    void put_data(const T &data, const size_t lsb, const size_t msb, const bool reverse = false)
    {
      check_bit_range(lsb, msb);

      Bits bits(data);
      bits = (bits << lsb) & get_mask(lsb, msb); // shift bits by lsb then mask the rest above msb
      if (reverse) reverse_bits(bits, lsb, msb);

      _word |= bits;
    }

  private:
    // check bit range [lsb, msb]
    void check_bit_range(const size_t lsb, const size_t msb)
    {
      if (lsb > msb) throw std::range_error("Word::check_bit_range: lsb greater than msb!");
      if (lsb < 0 || msb > (SIZE - 1)) throw std::out_of_range("Word::check_bit_range: lsb or msb out-of-range!");
    }

    // get mask for bits [lsb, msb]
    Bits get_mask(const size_t lsb, const size_t msb)
    {
      check_bit_range(lsb, msb);

      Bits mask = ~Bits(0); // all bits to 1
      mask = (mask >> (SIZE - 1 - msb)) & (mask << lsb); // set all bits above msb to 0 and all bits below lsb to 0

      return mask;
    }

    // reverse bits between bits [lsb, msb]
    void reverse_bits(Bits &bits, const size_t lsb = 0, const size_t msb = SIZE - 1)
    {
      check_bit_range(lsb, msb);

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
