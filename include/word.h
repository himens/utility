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

    // constructors
    Word() {}

    template <typename T>
    Word(const T &data) { _word = Bits(data); }  

    // print word
    friend std::ostream& operator<<(std::ostream& os, const Word w) 
    {
      os << "Word bitset: " << w.get_bits();

      return os;
    }

    // get word size
    size_t get_size() const { return SIZE; }

    // get word bits
    Bits get_bits() const { return _word; }

    // get bits stored in [lsb, msb] 
    Bits get_bits(const size_t lsb, const size_t size) const 
    {
      check_bit_range(lsb, size);

      Bits bits = _word & get_mask(lsb, size); // extract bits in [lsb, msb]
      bits >>= lsb; // shift back to bit 0

      return bits;
    }

    // reset word bits to zero
    void reset() { _word.reset(); }

    // reverse word bits between [lsb, msb]
    void reverse_bits(const size_t lsb = 0, const size_t size = SIZE) { reverse_bits(_word, lsb, size); }

    // get data stored in [lsb, msb] in MIL format
    int get_mil_data(const size_t lsb, const size_t size, const float msb_value) const
    {
      Bits bits = get_bits(lsb, size);
      if (msb_value < 0 && bits[size - 1] == 1) bits &= get_mask(lsb, size - 2);

      return static_cast<int>(bits.to_ulong());
    }

    // get data stored in [lsb, msb] in type T
    template <typename T>
    T get_data(const size_t lsb, const size_t size, const float msb_value) const 
    {
      int mil_data = get_mil_data(lsb, size, msb_value);
      float lsb_value = get_lsb_value(size, msb_value);
      float data = mil_data * lsb_value;

      return static_cast<T>(data);
    }

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

    // put data to word location [lsb, msb] in MIL format
    template <typename T>
    void put_data_to_mil(const T &data, const size_t lsb, const size_t size, const float msb_value, const bool reverse = false)
    {
      check_bit_range(lsb, size);

      float lsb_value = get_lsb_value(msb_value, size);
      Bits bits(std::abs(data / lsb_value));
      bits = (bits << lsb) & get_mask(lsb, size); // shift bits by lsb and mask the rest above msb

      if (data < 0) bits.set(size - 1, 1); // set sign bit if data is negative
      if (reverse) reverse_bits(bits, lsb, size);

      _word |= bits;
    }

  private:
    // check bit range [lsb, msb]
    void check_bit_range(const size_t lsb, const size_t size) const
    {
      if (lsb > msb) throw std::range_error("Word::check_bit_range: lsb greater than msb!");
      if (lsb < 0 || msb > (SIZE - 1)) throw std::out_of_range("Word::check_bit_range: lsb or msb out-of-range!");
    }

    // get mask for bits [lsb, msb]
    Bits get_mask(const size_t lsb, const size_t size) const
    {
      check_bit_range(lsb, size);

      Bits mask = ~Bits(0); // all bits to 1
      mask = (mask >> (SIZE - size)) & (mask << lsb); // set bits above msb to 0 and bits below lsb to 0

      return mask;
    }

    // get lsb value 
    float get_lsb_value(const float msb_value, const size_t size) const
    {
      if (size == 0) return 0.0;
      
      int w = 1 << (size - 1);
      float lsb_value = std::abs(msb_value) / w;

      return lsb_value;
    }

    // reverse bits between [lsb, msb]
    void reverse_bits(Bits &bits, const size_t lsb, const size_t size) const
    {
      check_bit_range(lsb, size);

      for (size_t i = 0; i < (size / 2); i++)
      {
        bool bit = bits[lsb + i];

        bits[lsb + i] = bits[size - 1 - i];
        bits[size - 1 - i] = bit;
      }
    }

    // data members
    Bits _word;  
};
#endif
