#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <math.h>
#include <cstring>
#include <byteswap.h>
#include <bitset>

#ifndef _DATA_H_
#define _DATA_H_


////////////////
// Data class //
////////////////
class Data 
{
  public:
    // constants
    static const size_t WORD_SIZE = 16;

    // typedefs
    typedef uint16_t data_t;
    typedef uint64_t mil_t;

    // constructors
    Data() {}

    Data(data_t *data) { _data = data; }

    // get data 
    data_t* get_data() const { return _data; }

    // get data word
    data_t get_word(const size_t i) const { return _data[i]; }

    // swap data bytes
    void swap_bytes(const size_t i) { __bswap_16(_data[i]); } 

    // reverse bits 
    void reverse_bits(const size_t lsb, const size_t size) 
    {
      check_bit_range(lsb, size);

      //for (size_t i = 0; i < (size / 2); i++)
      //{
      //  const size_t msb = lsb + size - 1;
      //  const bool bit = _data[lsb + i];

      //  _data[lsb + i] = _data[msb - i];
      //  _data[msb - i] = bit;
      //}
    }

    // get data to type T
    template <class T>
      T get(const size_t lsb, const size_t size, const float msb_value) const 
      {
	check_bit_range(lsb, size);

	const bool is_signed = msb_value < 0;
	const float lsb_value = get_lsb_value(size, msb_value);

        mil_t mil = 0;

        for (size_t i = 0; i < size; i++) 
        {
          const size_t i_word = (lsb + i) / WORD_SIZE;
          const size_t offset = (lsb + i) < WORD_SIZE ? lsb : 0;
          const size_t i_bit_in_word = (lsb + i) % WORD_SIZE;

          auto bit = _data[i_word] & (1 << i_bit_in_word); 
          bit >>= offset; 
          bit <<= WORD_SIZE * i_word;

          mil |= bit;
        }

	T data = to_int(mil, size, is_signed) * lsb_value;

	return data;
      }

    // put data to word in MIL format
    template <class T>
      void put(const T &data, const size_t lsb, const size_t size, const float msb_value)
      {
	check_bit_range(lsb, size);

	const bool is_signed = msb_value < 0;
	const float lsb_value = get_lsb_value(size, msb_value);
	const int scaled_data = data / lsb_value;
	const mil_t mil = to_mil(scaled_data, size, is_signed);

        for (size_t i = 0; i < size; i++)
        {
          const size_t i_word = (lsb + i) / WORD_SIZE;
          const size_t offset = (lsb + i) < WORD_SIZE ? lsb : 0;

          auto bit = mil & (1 << i); 
          bit <<= offset; 
          bit >>= WORD_SIZE * i_word;

          _data[i_word] |= bit;
        }
      }

  protected:
    // check bit range 
    void check_bit_range(const size_t lsb, const size_t size) const
    {
      if (size == 0) throw std::invalid_argument("Data::check_bit_range: size is zero!");
      else if (lsb < 0 || lsb > (WORD_SIZE - 1)) throw std::out_of_range("Data::check_bit_range: lsb out-of_range!");
    }

    // get lsb value
    float get_lsb_value(const size_t size, const float msb_value) const
    {
      if (size == 0) throw std::invalid_argument("Data::get_lsb_value: size is zero!");

      float lsb_value = msb_value != 0 ? std::abs(msb_value) / pow2(size - 1) : 1;

      return lsb_value;
    }

    // convert data from int to mil_t 
    mil_t to_mil(int value, const size_t size, const bool is_signed) const
    {
      if (size == 0) throw std::invalid_argument("Data::to_mil: size is zero!");

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
      if (size == 0) throw std::invalid_argument("Data::to_int: size is zero!");

      const mil_t positive_int_max = pow2(size - 1) - 1;
      const bool is_negative = is_signed && mil > positive_int_max;
      const mil_t min = is_negative ? positive_int_max + 1 : 0; 
      const mil_t max = !is_signed || is_negative ? pow2(size) - 1 : positive_int_max; 

      if (mil < min) mil = min;
      if (mil > max) mil = max;

      int value = is_negative ? mil - pow2(size) : mil;

      return value;
    }

    // fast power of 2 
    unsigned int pow2(const size_t size) const { return (1 << size); }

    // data members
    data_t *_data = nullptr;  
};
#endif
