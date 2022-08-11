#include "data.h"

extern "C" 
{
  void get_mil(Data::word_t *point);
}

struct data_t {
    uint16_t md_id;
    uint16_t msg_type;
};

void get_mil(Data::word_t *point)
{
  Data data{point};

  data_t my_data = {1, 18};
  data.put<int>(my_data.md_id, 0, 0, 7, 0);
  data.put<int>(my_data.msg_type, 0, 7, 9, 0);

  std::cout << data.get<int>(0, 0, 7, 0) << "\n";
  std::cout << data.get<int>(0, 7, 9, 0) << "\n";

  char* c = (char*)point;
  for (int i = 0; i < 2; i++) printf("%i: 0x%02x\n", i, c[i]);
}


