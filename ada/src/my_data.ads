with System;
with Ada.Text_IO;
with Ada.Real_Time;
with System.OS_Interface;
with Ada.Execution_Time;

package My_Data is
  subtype RNG1_TYPE is Natural range 0 .. 2**1 - 1;
  subtype RNG2_TYPE is Natural range 0 .. 2**2 - 1;
  subtype RNG3_TYPE is Natural range 0 .. 2**3 - 1;
  subtype RNG4_TYPE is Natural range 0 .. 2**4 - 1;
  subtype RNG5_TYPE is Natural range 0 .. 2**5 - 1;
  subtype RNG6_TYPE is Natural range 0 .. 2**6 - 1;
  subtype RNG7_TYPE is Natural range 0 .. 2**7 - 1;
  subtype RNG8_TYPE is Natural range 0 .. 2**8 - 1;
  subtype RNG9_TYPE is Natural range 0 .. 2**9 - 1;
  subtype RNG10_TYPE is Natural range 0 .. 2**10 - 1;
  subtype RNG16_TYPE is Natural range 0 .. 2**16 - 1;

  type Block1 is record 
    MD_ID     : RNG7_TYPE := 0;
    MSG_TYPE  : RNG9_TYPE := 0;
    MSG_NO    : RNG8_TYPE := 0;
  end record;

  --for Block1'Scalar_Storage_Order use System.Low_Order_First;
  --for Block1'Bit_Order use System.Low_Order_First;
  for Block1 use record
    MD_ID     at 0 range 0  ..  6;
    MSG_TYPE  at 0 range 7  ..  15;
    MSG_NO    at 2 range 0  ..  7;
  end record;

  procedure Print_Data (data : Block1);
  pragma Export(C, Print_Data, "print_data");
  --
  procedure Show_Bytes (A : System.Address);
end My_Data;
