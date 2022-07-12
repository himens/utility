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
    // typedefs
    typedef uint16_t data_t;
    typedef uint64_t mil_t;

    // constants
    static const size_t WORD_SIZE = 16;
    static const size_t MIL_SIZE = 8 * sizeof(mil_t);

    // constructors
    Data() {}

    Data(data_t *data) 
    {
      if (data == nullptr) throw std::invalid_argument("Data::Data: data is nullptr!");

      _data = data; 
    }

    // get data 
    data_t* get_data() const { return _data; }

    // swap data bytes
    void swap_bytes(const size_t i) { __bswap_16(_data[i]); } 

    // reverse bits 
    void reverse_bits(const size_t lsb, const size_t size) 
    {
      check_range(lsb, size);

      auto mil = get<mil_t>(lsb, size);
      size_t nb_of_bits = size; 
      mil_t mask = ~0; 

      while (nb_of_bits >>= 1) 
      {
        mask ^= mask << nb_of_bits; 
        mil = (mil & ~mask) >> nb_of_bits | (mil & mask) << nb_of_bits; 
      }
      
      put<mil_t>(mil, lsb, size);
    }

    // get data to type T
    template <class T>
      T get(const size_t lsb, const size_t size, const double msb_value = 0) const 
      {
	check_range(lsb, size);

        const size_t nb_of_words = std::ceil(static_cast<float>(lsb + size) / WORD_SIZE);
	const bool is_signed = msb_value < 0;
	const auto lsb_value = get_lsb_value(size, msb_value);

        mil_t mil = 0;
        auto mask = get_mask(lsb, size);

        for (size_t i = 0; i < nb_of_words; i++)
        {
          mil |= (static_cast<mil_t>(_data[i]) << WORD_SIZE * i);
        }

        mil = (mil & mask) >> lsb; // mask and remove offset

	T data = to_int(mil, size, is_signed) * lsb_value;

	return data;
      }

    // put data to word in MIL format
    template <class T>
      void put(const T &data, const size_t lsb, const size_t size, const double msb_value = 0)
      {
	check_range(lsb, size);

        const size_t nb_of_words = std::ceil(static_cast<float>(lsb + size) / WORD_SIZE);
	const bool is_signed = msb_value < 0;
	const auto lsb_value = get_lsb_value(size, msb_value);
	const int scaled_data = data / lsb_value;

	auto mil = to_mil(scaled_data, size, is_signed);
        auto mask = get_mask(lsb, size);

        mil = (mil << lsb) & mask; // add offset

        for (size_t i = 0; i < nb_of_words; i++)
        {
          _data[i] &= ~(mask >> WORD_SIZE * i); // delete old data in range, keep the rest
          _data[i] |= mil >> WORD_SIZE * i;
        }
      }

  protected:
    // check bit range 
    void check_range(const size_t lsb, const size_t size) const
    {
      const size_t msb = lsb + size - 1;

      if (size == 0) throw std::invalid_argument("Data::check_range: size is zero!");
      else if (lsb < 0 || lsb > WORD_SIZE - 1) throw std::out_of_range("Data::check_range: lsb out-of_range!");
      else if (msb > MIL_SIZE - 1) throw std::out_of_range("Data::check_range: msb out-of_range!");
    }

    // get lsb value
    double get_lsb_value(const size_t size, const double msb_value) const
    {
      if (size == 0) throw std::invalid_argument("Data::get_lsb_value: size is zero!");

      double lsb_value = msb_value != 0 ? std::abs(msb_value) / pow2(size - 1) : 1;

      return lsb_value;
    }

    // get mask in range
    mil_t get_mask(const size_t lsb, const size_t size) const
    {
	check_range(lsb, size);
        
        mil_t mask = ~0; 
        mask = (mask >> (MIL_SIZE - lsb - size)) & (mask << lsb); 

        return mask;
    }

    // convert data from int to mil_t 
    mil_t to_mil(int value, const size_t size, const bool is_signed) const
    {
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
