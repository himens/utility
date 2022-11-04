with Ada.Text_IO;
with System; 
with System.Storage_Elements; use System.Storage_Elements;

package body My_Data is
  --
  procedure Print_Data (data : Block1) is
  begin
    Ada.Text_IO.Put_Line("MD_ID =" &data.MD_ID'Img);
    --Ada.Text_IO.Put_Line("DUMMY1 =" &data.DUMMY1'Img);
    Ada.Text_IO.Put_Line("MSG_TYPE =" &data.MSG_TYPE'Img);
    Ada.Text_IO.Put_Line("MSG_NO =" &data.MSG_NO'Img);
  end Print_Data;
  --
  --procedure Print_Bytes (A : System.Address) is
  --  Arr : Storage_Array (1 .. 10);
  --  for Arr'Address use A;
  --  pragma Import (Ada, Arr);
  --begin
  --  for J in Arr'Range loop
  --    Ada.Text_IO.Put ("Byte"&J'Img&":" &Arr (J)'Img);
  --    Ada.Text_IO.New_Line;
  --  end loop;
  --  Ada.Text_IO.New_Line;
  --end Print_Bytes;
  --
end My_Data;  
