
extern void print_data(void);
extern void adainit (void);
extern void adafinal (void);

int main (int argc, char *argv[])
{
   adainit();
   print_data();
   adafinal();

   return 1;
}
