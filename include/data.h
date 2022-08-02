#include <iostream>
#include <math.h>

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
    Data(data_t *data) { set_data(data); }

    // get data 
    void set_data(data_t *data) 
    { 
      if (data == nullptr) throw std::invalid_argument("Data::set_data: data is nullptr!");

      _data = data; 
    }

    // get data 
    data_t* get_data() const { return _data; }

    // get data to type T
    template <class T>
      T get(const size_t msb, const size_t size, const double msb_value = 0) const 
      {
        check_type<T>();
	check_range(msb, size);

	const bool is_signed = msb_value < 0;
	const auto lsb_value = get_lsb_value(size, msb_value);
        const auto nb_of_words = get_number_of_words(msb, size);
        const auto mask = get_mask(msb, size);
        const auto shift = get_shift(msb, size);

        mil_t mil = 0;

        for (size_t i = 0; i < nb_of_words; i++)
        {
          mil |= static_cast<mil_t>(_data[nb_of_words - 1 - i]) << WORD_SIZE * i;
        }

        mil = (mil & mask) >> shift; 

	T data = to_int(mil, size, is_signed) * lsb_value;

	return data;
      }

    // put data to word in MIL format
    template <class T>
      void put(const T &data, const size_t msb, const size_t size, const double msb_value = 0)
      {
        check_type<T>();
	check_range(msb, size);

	const bool is_signed = msb_value < 0;
	const auto lsb_value = get_lsb_value(size, msb_value);
        const auto nb_of_words = get_number_of_words(msb, size);
        const auto mask = get_mask(msb, size);
        const auto shift = get_shift(msb, size);

	auto mil = to_mil(data / lsb_value, size, is_signed);
        mil = (mil << shift) & mask; 
        
        for (size_t i = 0; i < nb_of_words; i++)
        {
          _data[nb_of_words - 1 - i] &= ~(mask >> WORD_SIZE * i); // delete old data in range, keep the rest
          _data[nb_of_words - 1 - i] |= mil >> WORD_SIZE * i;
        }
      }

  protected:
    // check data type
    template <class T>
      void check_type() const
      {
        static_assert(std::is_arithmetic<T>::value, "Data::check_type: input data type not arithmetic!");
        static_assert(sizeof(T) <= sizeof(mil_t), "Data::check_type: size of input data greater than mil_t!");
      }

    // check bit range 
    void check_range(const size_t msb, const size_t size) const
    {
      const size_t lsb = msb + size - 1;

      if (size == 0) throw std::invalid_argument("Data::check_range: size is zero!");
      else if (msb < 0 || msb > WORD_SIZE - 1) throw std::out_of_range("Data::check_range: msb out-of_range!");
      else if (lsb > MIL_SIZE - 1) throw std::out_of_range("Data::check_range: lsb out-of_range!");
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

    // get lsb value
    double get_lsb_value(const size_t size, const double msb_value) const
    {
      if (size == 0) throw std::invalid_argument("Data::get_lsb_value: size is zero!");

      double lsb_value = msb_value != 0 ? std::abs(msb_value) / pow2(size - 1) : 1;

      return lsb_value;
    }

    // get mask in range
    mil_t get_mask(const size_t msb, const size_t size) const
    {
      check_range(msb, size);

      const auto shift = get_shift(msb, size);
      mil_t mask = (~mil_t(0) >> (MIL_SIZE - size)) << shift; 

      return mask;
    }

    // get number of words spanned by data
    size_t get_number_of_words(const size_t msb, const size_t size) const 
    {
      check_range(msb, size);

      size_t nb_of_words = std::ceil(static_cast<float>(msb + size) / WORD_SIZE);

      return nb_of_words;
    }

    // get data offset
    size_t get_shift(const size_t msb, const size_t size) const
    {
      check_range(msb, size);

      const auto nb_of_words = get_number_of_words(msb, size);
      size_t shift = (nb_of_words * WORD_SIZE) - msb - size;

      return shift;
    }

    // fast power of 2 
    unsigned long pow2(const size_t size) const { return (1UL << size); }

    // data members
    data_t *_data = nullptr;  
};
#endif
