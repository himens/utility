#include "bus_interface.h"

extern "C" 
{
  void get_mil(BusInterface::word_t *point);
  long int my_clock();
  int my_clock_gettime (clockid_t __clock_id, struct timespec *__tp) __THROW;
  void print_bytes(BusInterface::word_t *point, size_t n_bytes);
}

struct data_t {
    uint16_t md_id;
    uint16_t msg_type;
    uint16_t msg_no;
} __attribute__((packed));

void get_mil(BusInterface::word_t *point)
{
  try {
    BusInterface bus{point};

    data_t my_data = {44, 1, 3};
    bus.put<int>(my_data.md_id, 0, 0, 7, 0);
    bus.put<int>(my_data.msg_type, 0, 7, 9, 0);
    bus.put<int>(my_data.msg_no, 1, 0, 8, 0);

    //std::cout << bus.get<int>(0, 0, 7, 0) << "\n";
    //std::cout << bus.get<int>(0, 7, 9, 0) << "\n";
  }
  catch (const std::exception &e) {
    std::cerr << e.what() << "\n";
    throw;
  }
}

void print_bytes(BusInterface::word_t *point, size_t n_bytes)
{
  char* c = (char*)point;
  for (size_t i = 0; i < n_bytes; i++) 
    //printf("%i: 0x%02x\n", i, c[i]);
    std::cout << "Byte " << i << ": " << std::hex << c[i] << "\n";;
}

long int my_clock() 
{ 
  std::cout << "Uff \n"; 
  return 1; 
}

int my_clock_gettime (clockid_t __clock_id, struct timespec *__tp) __THROW
{
  std::cout << "Uff2 \n"; 

  __tp->tv_sec = 1;
  __tp->tv_nsec = 0;

  return 0; 
}
