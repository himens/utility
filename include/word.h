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
class Word 
{
  public:
    static const size_t SIZE = 16;

    // bitset_t type definition
    typedef std::bitset<SIZE> bitset_t;
    typedef uint16_t mil_t;

    // constructors
    Word() {}

    template <typename T>
    Word(const T &data) { _word = bitset_t(data); }  

    // print word
    friend std::ostream& operator<<(std::ostream& os, const Word w) 
    {
      os << "Word bitset: " << w.get_bitset();

      return os;
    }

    // get word size
    size_t get_size() const { return SIZE; }

    // get word bits
    bitset_t get_bitset() const { return _word; }

    // reset word bits to zero
    void reset() { _word.reset(); }

    // reverse word bits 
    void reverse_bits(const size_t lsb = 0, const size_t size = SIZE) { reverse_bits(_word, lsb, size); }

    // get word to MIL type (signed/unsigned)
    mil_t to_mil() const { return static_cast<mil_t>(_word.to_ulong()); }
    
    // get data to MIL type
    mil_t to_mil(const size_t lsb, const size_t size, const float msb_value) const
    {
      check_bit_range(lsb, size);

      bitset_t bits = _word & get_mask(lsb, size); // extract bits 
      bits >>= lsb; // shift back to bit 0
      mil_t mil = static_cast<mil_t>(bits.to_ulong());

      return mil;
    }

    // get data to type T
    template <typename T>
    T get_data(const size_t lsb, const size_t size, const float msb_value) const 
    {
      check_bit_range(lsb, size);

      const float lsb_value = get_lsb_value(size, msb_value);
      int mil = to_mil(lsb, size, msb_value);

      if (msb_value < 0) mil -= (1 << size); // it's a negative number

      T data = static_cast<T>(mil * lsb_value);

      return data;
    }

    // swap word bytes
    void swap_bytes() { _word = bitset_t(__bswap_16(_word.to_ulong())); }

    // put data to word in MIL format
    template <typename T>
    void put_data(const T &data, const size_t lsb, const size_t size, const float msb_value, const bool reverse = false)
    {
      check_bit_range(lsb, size);

      const float lsb_value = get_lsb_value(size, msb_value);
      int mil = static_cast<int>(data / lsb_value) + (1 << size);
      bitset_t bits(mil);
      bits = (bits << lsb) & get_mask(lsb, size); // shift bits by lsb and mask the rest above msb

      if (reverse) reverse_bits(bits, lsb, size);

      _word |= bits;
    }

  private:
    // check bit range 
    void check_bit_range(const size_t lsb, const size_t size) const
    {
      size_t msb = lsb + size - 1;
      if (lsb > msb) throw std::range_error("Word::check_bit_range: lsb greater than msb!");
      if (lsb < 0 || msb > (SIZE - 1)) throw std::out_of_range("Word::check_bit_range: lsb or msb out-of-range!");
    }

    // get mask for bit interval
    bitset_t get_mask(const size_t lsb, const size_t size) const
    {
      check_bit_range(lsb, size);

      bitset_t mask = ~bitset_t(0); // all bits to 1
      mask = (mask >> (SIZE - lsb - size)) & (mask << lsb); // set bits above msb to 0 and bits below lsb to 0

      return mask;
    }

    // get lsb value
    float get_lsb_value(const size_t size, const float msb_value) const
    {
      float lsb_value = msb_value != 0.0 ? std::abs(msb_value) / (1 << (size - 1)) : 1.0;

      return lsb_value;
    }

    // reverse bits in a bit interval
    void reverse_bits(bitset_t &bits, const size_t lsb, const size_t size) const
    {
      check_bit_range(lsb, size);

      for (size_t i = 0; i < (size / 2); i++)
      {
	size_t msb = lsb + size - 1;
        bool bit = bits[lsb + i];

        bits[lsb + i] = bits[msb - i];
        bits[msb - i] = bit;
      }
    }

    // data members
    bitset_t _word;  
};
#endif
