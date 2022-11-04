with System; 
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Byte_Swapping;

with My_Data; use My_Data;

procedure My_Main is
  procedure Get_Mil(point : System.Address);
  pragma Import(CPP, Get_Mil, "get_mil");

  procedure Print_Bytes(point : System.Address; Num_Bytes : Integer);
  pragma Import(CPP, Print_Bytes, "print_bytes");

  data : Block1;
  big_arr : Big_Array := (16961, 0, 0, 0, 0);
  big_arr_swap : Big_Array := big_arr;
  little_arr  : Little_Array;
begin

  Get_Mil(data'Address);
  -- Print_Data(data);

  for I in Index loop
    little_arr(I) := big_arr(I);
    GNAT.Byte_Swapping.Swap2(big_arr_swap(I)'Address);
  end loop;  

  Ada.Text_IO.Put_Line("Little-Endian:");
  Print_Bytes(little_arr(1)'Address, little_arr'Length);

  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("Big-Endian:");
  Print_Bytes(big_arr(1)'Address, big_arr'Length);

  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("Big-Endian swapped:");
  Print_Bytes(big_arr_swap(1)'Address, big_arr_swap'Length);

exception 
  when Others => 
    Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "Error has occurred!");
    raise;

end My_Main;
