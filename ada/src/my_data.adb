with Ada.Text_IO;

package body My_Data is
  --
  procedure Print_Data (data : Block1) is
  begin
    Ada.Text_IO.Put_Line("MD_ID =" &data.MD_ID'Img);
    Ada.Text_IO.Put_Line("DUMMY1 =" &data.DUMMY1'Img);
    Ada.Text_IO.Put_Line("MESS_TYPE =" &data.MESS_TYPE'Img);
  end Print_Data;
  --
end My_Data;  
