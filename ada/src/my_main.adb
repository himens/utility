with System; 
with Ada.Exceptions;
with Ada.Text_IO;

with My_Data; use My_Data;

procedure My_Main is
  procedure Get_Mil(point : System.Address);
  pragma Import(CPP, Get_Mil, "get_mil");

  data : Block1;
begin

  Get_Mil(data'Address);
  Print_Data(data);
  Show_Bytes(data'Address);

exception 
  when Others => 
    Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "Error has occurred!");
    raise;

end My_Main;
