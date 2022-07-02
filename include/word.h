#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
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
    typedef unsigned int mil_t;

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

    // swap word bytes
    void swap_bytes() { _word = bitset_t(__bswap_16(_word.to_ulong())); }

    // get word to MIL type 
    mil_t to_mil() const { return _word.to_ulong(); }
    
    // get data to type T
    template <typename T>
    T get_data(const size_t lsb, const size_t size, const float msb_value) const 
    {
      check_bit_range(lsb, size);

      bitset_t bits = _word & get_mask(lsb, size); // extract bits 
      bits >>= lsb; // shift back to bit 0

      const bool is_signed = msb_value < 0;
      const float lsb_value = get_lsb_value(size, msb_value);
      const mil_t mil = bits.to_ulong();
      T data = to_int(mil, size, is_signed) * lsb_value;

      return data;
    }

    // put data to word in MIL format
    template <typename T>
    void put_data(const T &data, const size_t lsb, const size_t size, const float msb_value, const bool reverse = false)
    {
      check_bit_range(lsb, size);

      const bool is_signed = msb_value < 0;
      const float lsb_value = get_lsb_value(size, msb_value);
      const int scaled_data = data / lsb_value;
      mil_t mil = to_mil(scaled_data, size, is_signed);

      bitset_t bits(mil);
      bits = (bits << lsb) & get_mask(lsb, size); // shift bits by lsb and mask the rest
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

    // get mask 
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
      if (size == 0) throw std::invalid_argument("Word::get_lsb_value: size is zero!");

      float lsb_value = msb_value != 0 ? std::abs(msb_value) / pow2(size - 1) : 1;

      return lsb_value;
    }

    // reverse bits 
    void reverse_bits(bitset_t &bits, const size_t lsb, const size_t size) const
    {
      check_bit_range(lsb, size);

      for (size_t i = 0; i < (size / 2); i++)
      {
	const size_t msb = lsb + size - 1;
        const bool bit = bits[lsb + i];

        bits[lsb + i] = bits[msb - i];
        bits[msb - i] = bit;
      }
    }

    // convert data from int to mil_t 
    mil_t to_mil(int value, const size_t size, const bool is_signed) const
    {
      if (size == 0) throw std::invalid_argument("Word::to_mil: size is zero!");

      const int min = is_signed ? -pow2(size - 1) : 0; 
      const int max = is_signed ? pow2(size - 1) - 1 : pow2(size) - 1; 

      if (value < min) value = min;
      if (value > max) value = max;

      mil_t mil = value < 0 ? value + pow2(size) : value;

      return mil;
    }

    // convert data from mil_t to int 
    int to_int(mil_t mil, const size_t size, const bool is_signed) const
    {
      if (size == 0) throw std::invalid_argument("Word::to_int: size is zero!");

      const mil_t positive_int_max = pow2(size - 1) - 1;
      const bool is_negative = is_signed && mil > positive_int_max;
      const mil_t min = is_negative ? positive_int_max + 1 : 0; 
      const mil_t max = !is_signed || is_negative ? pow2(size) - 1 : positive_int_max; 

      if (mil > max) mil = max;
      if (mil < min) mil = min;

      int value = is_negative ? mil - pow2(size) : mil;

      return value;
    }

    // fast power of 2 
    unsigned int pow2(const size_t size) const { return (1 << size); }

    // data members
    bitset_t _word;  
};
#endif
