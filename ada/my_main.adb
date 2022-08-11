with System; 
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO; use Ada.Text_IO;

with My_Data; use My_Data;

procedure My_Main is
  procedure Show (A : System.Address) is
    Arr : Storage_Array (1 .. 2);
    for Arr'Address use A;
    pragma Import (Ada, Arr);
  begin
    for J in Arr'Range loop
      Put (Arr (J)'Img);
    end loop;
    New_Line;
  end Show;

  procedure Get_Mil(point : System.Address);
  pragma Import(CPP, Get_Mil, "get_mil");

  data : Block1;
begin

  --data.MD_ID := 1;
  --data.MESS_TYPE := 18;

  Get_Mil(data'Address);

  Ada.Text_IO.Put_Line(System.Default_Bit_Order'Img);
  Ada.Text_IO.Put_Line("MD_ID =" &data.MD_ID'Img);
  Ada.Text_IO.Put_Line("MESS_TYPE =" &data.MESS_TYPE'Img);
  --Show(data'Address);

end My_Main;
