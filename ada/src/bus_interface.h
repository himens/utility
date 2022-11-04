#include <iostream>
#include <stdexcept>
#include <cmath>
#include <cassert>
#include <bitset>

#ifndef _BUS_INTERFACE_H_
#define _BUS_INTERFACE_H_

////////////////////////
// BusInterface class //
////////////////////////
class BusInterface {
  public:
    // typedefs
    typedef uint16_t word_t;
    typedef uint64_t mil_t;

    // constants
    static const size_t byte_size = 8;
    static const size_t word_size = sizeof(word_t) * byte_size;
    static const size_t mil_size = sizeof(mil_t) * byte_size;

    // constructors
    BusInterface() {}
    BusInterface(word_t *data) { set_data(data); }

    // set data 
    void set_data(word_t *data) 
    { 
      if (data == nullptr)
        throw std::invalid_argument("Data::set_data: data is nullptr!");

      _data = data; 
    }

    // get data 
    word_t* get_data() const { return _data; }

    // get data to type T
    template <class T>
      T get(const size_t word, const size_t msb, const size_t size, const double msb_value = 0) const
      {
        check_type<T>();
	check_range(msb, size);

	const auto lsb_value = get_lsb_value(size, msb_value);
        const auto nb_of_words = get_number_of_words(msb, size);
        const auto mask = get_mask(msb, size);
        const auto shift = get_shift(msb, size);

        mil_t mil = 0;
        for (size_t i = 0; i < nb_of_words; i++) { // reconstruct MIL word by word
          mil |= static_cast<mil_t>(_data[word + i]) << word_size * i;
        }
        swap_bytes(mil); // to little-endian
        mil = (mil & mask) >> shift; // extract data and move it to the right-most part of the MIL
	T data = to_int(mil, size, msb_value) * lsb_value; // convert MIL to its original data value

	return data;
      }

    // put data to word in MIL format
    template <class T>
      void put(const T &data, const size_t word, const size_t msb, const size_t size, const double msb_value = 0)
      {
        check_type<T>();
	check_range(msb, size);

	const auto lsb_value = get_lsb_value(size, msb_value);
        const auto nb_of_words = get_number_of_words(msb, size);
        auto mask = get_mask(msb, size);
        const auto shift = get_shift(msb, size);

	auto mil = to_mil(data / lsb_value, size, msb_value); // convert scaled data to MIL format
        mil = (mil << shift) & mask; // move data to the correct position along the MIL

        swap_bytes(mil); // to big-endian
        swap_bytes(mask); // to big-endian

        for (size_t i = 0; i < nb_of_words; i++) { // extract MIL words 
          _data[word + i] &= ~(mask >> word_size * i); // delete old data in range, keep the rest
          _data[word + i] |= mil >> word_size * i;
        }
      }

  protected:
    // check data type
    template <class T>
      void check_type() const
      {
        static_assert(std::is_arithmetic<T>::value, "Data::check_type: input data type is not arithmetic!");
        static_assert(sizeof(T) <= sizeof(mil_t), "Data::check_type: size of input data greater than mil_t size!");
      }

    // check bit range 
    void check_range(const size_t msb, const size_t size) const
    {
      const size_t lsb = msb + size - 1;

      if (size == 0 || size > mil_size)
        throw std::length_error("Data::check_range: invalid data size!");
      else if (msb > word_size - 1)
        throw std::out_of_range("Data::check_range: msb out-of_range!");
      else if (lsb > mil_size - 1)
        throw std::out_of_range("Data::check_range: lsb out-of_range!");
    }

    // convert data from int to mil_t 
    mil_t to_mil(int value, const size_t size, const double msb_value) const
    {
      assert(size > 0 && size <= sizeof(int) * byte_size);

      const bool is_signed = msb_value < 0;
      const int min = is_signed ? -pow2(size - 1) : 0; 
      const int max = is_signed ? pow2(size - 1) - 1 : pow2(size) - 1; 

      if (value < min) value = min;
      if (value > max) value = max;

      mil_t mil = (value < 0) ? value + pow2(size) : value;

      return mil;
    }

    // convert data from mil_t to int 
    int to_int(mil_t mil, const size_t size, const double msb_value) const
    {
      assert(size > 0 && size <= sizeof(int) * byte_size);

      const bool is_signed = msb_value < 0;
      const mil_t uint_max = pow2(size) - 1;
      const mil_t int_max = pow2(size - 1) - 1; // all ones but last bit
      const bool is_negative = is_signed && (mil > int_max);
      const mil_t mil_min = is_negative ? int_max + 1 : 0; 
      const mil_t mil_max = (!is_signed || is_negative) ? uint_max : int_max; 

      if (mil < mil_min) mil = mil_min;
      if (mil > mil_max) mil = mil_max;

      int value = is_negative ? mil - pow2(size) : mil;

      return value;
    }

    // swap bytes of data of type T
    template <class T>
      void swap_bytes(T &data) const 
      { 
        check_type<T>();

        if (sizeof(data) < 2)
          return;
        else if (sizeof(data) == 2)
          data = __builtin_bswap16(data);
        else if (sizeof(data) == 4)
          data = __builtin_bswap32(data);
        else if (sizeof(data) == 8)
          data = __builtin_bswap64(data);
        else
          std::cout << "[WARNING] Data::swap_bytes: no built-in swap for " << sizeof(data) << "-bytes data! \n"; 
      }

    // get lsb value
    double get_lsb_value(const size_t size, const double msb_value) const
    {
      assert(size > 0);

      return (msb_value != 0) ? std::abs(msb_value) / pow2(size - 1) : 1;
    }

    // get mask in range
    mil_t get_mask(const size_t msb, const size_t size) const
    {
      const auto shift = get_shift(msb, size);

      return (~mil_t(0) >> (mil_size - size)) << shift; 
    }

    // get number of words spanned by data
    size_t get_number_of_words(const size_t msb, const size_t size) const 
    { 
      assert(word_size > 0);

      return std::ceil(static_cast<float>(msb + size) / word_size); 
    }

    // get offset by which data has to be moved along MIL in order to be placed/extracted
    size_t get_shift(const size_t msb, const size_t size) const 
    { 
      assert(mil_size - msb - size >= 0);

      return (mil_size - msb - size); 
    }

    // fast power of 2 
    unsigned long pow2(const size_t size) const { return (1UL << size); }

    // data members
    word_t *_data = nullptr;  
};
#endif