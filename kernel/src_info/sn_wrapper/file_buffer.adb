with Ada.Text_IO; use Ada.Text_IO;
with SN; use SN;

package body File_Buffer is

   MAX_FILE_SIZE        : constant Integer := 1000000;
   MAX_NUMBER_OF_LINES  : constant Integer := 100000;
   MAX_LINE_LENGTH      : constant Integer := Max_Src_Line_Length;

   Tab_Size             : constant Integer := 8;
   --  Number of spaces to replace one ASCII.HT character

   F : File_Type;

   File_Buffer_Arr : String (1 .. MAX_FILE_SIZE);
   File_Buffer_Pos : Integer := 1;
   Line_Positions : array (1 .. MAX_NUMBER_OF_LINES) of Integer;
   Number_Of_Lines : Integer := 0;

   procedure Translate_Tabs
     (Src      : in  String;
      SrcLen   : in  Natural;
      Dst      : out String;
      DstLen   : out Natural);
   --  Translate Tabs (ASCII.HT) to spaces.

   --------------------
   -- Translate_Tabs --
   --------------------

   procedure Translate_Tabs
     (Src      : in  String;
      SrcLen   : in  Natural;
      Dst      : out String;
      DstLen   : out Natural)
   is
      DstPos   : Natural := Dst'First;
      C        : Natural;
   begin
      DstLen   := 0;

      for SrcPos in Src'First .. Src'First + SrcLen - 1 loop
         if Src (SrcPos) = ASCII.HT then
            C := Tab_Size - (DstPos - 1) mod Tab_Size;
            for I in 1 .. C loop
               Dst (DstPos) := Src (SrcPos);
               DstLen := Natural'Succ (DstLen);
               DstPos := Natural'Succ (DstPos);
            end loop;
         else
            Dst (DstPos) := Src (SrcPos);
            DstLen := Natural'Succ (DstLen);
            DstPos := Natural'Succ (DstPos);
         end if;
      end loop;

   end Translate_Tabs;

   ----------
   -- Init --
   ----------

   procedure Init (File_Name : String) is
      S  : String (1 .. MAX_LINE_LENGTH);
      TS : String (1 .. MAX_LINE_LENGTH);
      L  : Natural;
      TL : Natural;
   begin

      File_Buffer_Pos := 1;
      Number_Of_Lines := 0;

      Open (F, In_File, File_Name);

      loop
         exit when End_Of_File (F);
         Get_Line (F, S, L);
         Translate_Tabs (S, L, TS, TL);

         Number_Of_Lines := Number_Of_Lines + 1;
         File_Buffer_Arr (File_Buffer_Pos .. File_Buffer_Pos + TL - 1)
           := TS (1 .. TL);
         Line_Positions (Number_Of_Lines) := File_Buffer_Pos;
         File_Buffer_Pos := File_Buffer_Pos + TL;
      end loop;

      Line_Positions (Number_Of_Lines + 1) := File_Buffer_Pos;

      Close (F);

   end Init;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Line : Integer) return String is
   begin
      if (Line < 1) or (Line > Number_Of_Lines) then
         return "";
      else
         return File_Buffer_Arr
            (Line_Positions (Line) .. (Line_Positions (Line + 1) - 1));
      end if;
   end Get_Line;

   ----------
   -- Done --
   ----------

   procedure Done is
   begin
      File_Buffer_Pos := 1;
   end Done;

end File_Buffer;
