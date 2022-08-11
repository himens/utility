with System;

package My_Data is

  subtype RNG7_TYPE is Natural range 0 .. 2**7 - 1;
  subtype RNG8_TYPE is Natural range 0 .. 2**8 - 1;
  subtype RNG9_TYPE is Natural range 0 .. 2**9 - 1;
  subtype RNG16_TYPE is Natural range 0 .. 2**16 - 1;

  type Block1 is record 
    MD_ID     : RNG7_TYPE := 0;
    MESS_TYPE : RNG9_TYPE := 0;
  end record;

  --for Block1'Scalar_Storage_Order use System.High_Order_First;
  --for Block1'Bit_Order use System.High_Order_First;
  for Block1 use record
    MD_ID     at 0 range 0  ..  6;
    MESS_TYPE at 0 range 7  ..  15;
  end record;

end My_Data;
