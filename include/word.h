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
    typedef int16_t mil_t;
    typedef uint16_t umil_t;

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

    // reverse word bits between [lsb, msb]
    void reverse_bits(const size_t lsb = 0, const size_t msb = SIZE - 1) { reverse_bits(_word, lsb, msb); }

    // get word to MIL type (signed/unsigned)
    mil_t to_mil() const { return static_cast<mil_t>(_word.to_ulong()); }
    umil_t to_umil() const { return static_cast<umil_t>(_word.to_ulong()); }
    
    // get data in [lsb, msb] to MIL type
    mil_t to_mil(const size_t lsb, const size_t msb, const float msb_value) const
    {
      check_bit_range(lsb, msb);

      const size_t n_bits = msb - lsb + 1;

      bitset_t bits = _word & get_mask(lsb, msb); // extract bits in [lsb, msb]
      bits >>= lsb; // shift back to bit 0
      mil_t mil_data = static_cast<mil_t>(bits.to_ulong());

      if (bits[n_bits - 1] == 1) mil_data -= (1 << n_bits); // if sign bit is set, mil is negative

      return mil_data;
    }

    // get data stored in [lsb, msb] to type T
    template <typename T>
    T get_data(const size_t lsb, const size_t msb, const float msb_value) const 
    {
      check_bit_range(lsb, msb);

      const size_t n_bits = msb - lsb + 1;
      const float lsb_value = std::abs(msb_value) / (1 << (n_bits - 1));

      mil_t mil_data = to_mil(lsb, msb, msb_value);

      return static_cast<T>(mil_data * lsb_value);
    }

    // swap word bytes
    void swap_bytes() { _word = bitset_t(__bswap_16(_word.to_ulong())); }

    // put data to word location [lsb, msb] in MIL format
    template <typename T>
    void put_data(const T &data, const size_t lsb, const size_t msb, const float msb_value, const bool reverse = false)
    {
      check_bit_range(lsb, msb);

      const size_t n_bits = msb - lsb + 1;
      const float lsb_value = std::abs(msb_value) / (1 << (n_bits - 1));

      bitset_t bits(static_cast<mil_t>(data / lsb_value));
      bits = (bits << lsb) & get_mask(lsb, msb); // shift bits by lsb and mask the rest above msb

      if (reverse) reverse_bits(bits, lsb, msb);

      _word |= bits;
    }

  private:
    // check bit range [lsb, msb]
    void check_bit_range(const size_t lsb, const size_t msb) const
    {
      if (lsb > msb) throw std::range_error("Word::check_bit_range: lsb greater than msb!");
      if (lsb < 0 || msb > (SIZE - 1)) throw std::out_of_range("Word::check_bit_range: lsb or msb out-of-range!");
    }

    // get mask for bits [lsb, msb]
    bitset_t get_mask(const size_t lsb, const size_t msb) const
    {
      check_bit_range(lsb, msb);

      bitset_t mask = ~bitset_t(0); // all bits to 1
      mask = (mask >> (SIZE - 1 - msb)) & (mask << lsb); // set bits above msb to 0 and bits below lsb to 0

      return mask;
    }

    // reverse bits between [lsb, msb]
    void reverse_bits(bitset_t &bits, const size_t lsb, const size_t msb) const
    {
      check_bit_range(lsb, msb);

      const size_t n_bits = msb - lsb + 1;

      for (size_t i = 0; i < (n_bits / 2); i++)
      {
        bool bit = bits[lsb + i];

        bits[lsb + i] = bits[msb - i];
        bits[msb - i] = bit;
      }
    }

    // data members
    bitset_t _word;  
};
#endif
