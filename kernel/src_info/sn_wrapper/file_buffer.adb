with Ada.Text_IO; use Ada.Text_IO;
with SN; use SN;
package body File_Buffer is

   MAX_FILE_SIZE : constant Integer := 1000000;
   MAX_NUMBER_OF_LINES : constant Integer := 100000;
   MAX_LINE_LENGTH : constant Integer := Max_Src_Line_Length;

   F : File_Type;

   File_Buffer_Arr : String (1 .. MAX_FILE_SIZE);
   File_Buffer_Pos : Integer := 1;
   Line_Positions : array (1 .. MAX_NUMBER_OF_LINES) of Integer;
   Number_Of_Lines : Integer := 0;

   procedure Init (File_Name : String) is
      S : String (1 .. MAX_LINE_LENGTH);
      L : Integer;
   begin
      File_Buffer_Pos := 1;
      Number_Of_Lines := 0;
      Open (F, In_File, File_Name);
      loop
         Get_Line (F, S, L);
         Number_Of_Lines := Number_Of_Lines + 1;
         File_Buffer_Arr (File_Buffer_Pos .. (File_Buffer_Pos + L - 1))
               := S (1 .. L);
         Line_Positions (Number_Of_Lines) := File_Buffer_Pos;
         File_Buffer_Pos := File_Buffer_Pos + L;
         exit when End_Of_File (F);
      end loop;
      Line_Positions (Number_Of_Lines + 1) := File_Buffer_Pos;
      Close (F);
   end Init;

   function Get_Line (Line : Integer) return String is
   begin
      if (Line < 1) or (Line > Number_Of_Lines) then
         return "";
      else
         return File_Buffer_Arr
            (Line_Positions (Line) .. (Line_Positions (Line + 1) - 1));
      end if;
   end Get_Line;

   procedure Done is
   begin
      File_Buffer_Pos := 1;
   end Done;

end File_Buffer;
