with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body File_Buffer is

   MAX_NUMBER_OF_LINES  : constant Integer := 100000;
   MAX_LINE_LENGTH      : constant Integer := Max_Src_Line_Length;

   Tab_Size             : constant Integer := 8;
   --  Number of spaces to replace one ASCII.HT character

   F : File_Type;

   File_Buffer        : SN.String_Access;
   File_Buffer_Length : Integer := 0;
   File_Buffer_Pos    : Integer := 1;
   Line_Positions     : array (1 .. MAX_NUMBER_OF_LINES) of Integer;
   Number_Of_Lines    : Integer := 0;

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
      declare
         FD : File_Descriptor := Invalid_FD;
      begin
         FD := Open_Read (File_Name, Fmode => Binary);
         if FD /= Invalid_FD then
            File_Buffer_Length := Integer (File_Length (FD));
            --  reserve extra (50%) space
            File_Buffer_Length :=
              File_Buffer_Length + File_Buffer_Length / 2;
            File_Buffer := new String (1 .. File_Buffer_Length);
         end if;
      end;

      File_Buffer_Pos := 1;
      Number_Of_Lines := 0;

      Open (F, In_File, File_Name);
      --  exception is raised here if any IO error

      loop
         exit when End_Of_File (F);
         Get_Line (F, S, L);
         Translate_Tabs (S, L, TS, TL);

         Number_Of_Lines := Number_Of_Lines + 1;

         if File_Buffer_Pos + TL - 1 > File_Buffer_Length then
            --  Buffer exeeded, allocate the new one
            declare
               New_Buf : SN.String_Access;
               Old_Len : Integer := File_Buffer_Length;
            begin
               File_Buffer_Length := File_Buffer_Length * 2;
               New_Buf := new String (1 .. File_Buffer_Length);
               New_Buf.all (1 .. Old_Len) :=
                  File_Buffer.all (1 .. Old_Len);
               Free (File_Buffer);
               File_Buffer := New_Buf;
            end;
         end if;

         File_Buffer.all (File_Buffer_Pos .. File_Buffer_Pos + TL - 1)
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

   procedure Get_Line
     (Line   : in Integer;
      Buffer : out SN.String_Access;
      Slice  : out Segment) is
   begin
      Buffer := File_Buffer;
      if (Line < 1) or (Line > Number_Of_Lines) then
         Slice.First := 2;
         Slice.Last  := 1;
      else
         Slice.First := Line_Positions (Line);
         Slice.Last  := Line_Positions (Line + 1) - 1;
      end if;
   end Get_Line;

   ----------
   -- Done --
   ----------

   procedure Done is
   begin
      Free (File_Buffer);
      File_Buffer_Pos := 1;
   end Done;

end File_Buffer;
