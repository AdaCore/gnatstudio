with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Regression is

   Max_Str_Size : Integer := 1024;

   function Compare (Output, Oracle : String) return Natural;

   function Compare (Output, Oracle : String) return Natural is
      File_Output, File_Oracle : File_Type;
      First_Difference         : Natural := 0;
      Current_Line             : Natural := 1;
      Str1, Str2               : String (1 .. Max_Str_Size) := (others => ' ');
      Len1, Len2               : Integer := 0;

   begin
      Open (File_Output, In_File, Output);
      Open (File_Oracle, In_File, Oracle);

      loop

         if End_Of_File (File_Output) xor End_Of_File (File_Oracle) then
            First_Difference := Current_Line;
            exit;
         elsif End_Of_File (File_Output)
           and then End_Of_File (File_Oracle)
         then
            exit;
         end if;

         Get_Line (File_Output, Str1, Len1);
         Get_Line (File_Oracle, Str2, Len2);

         if Str1 (1 .. Len1) /= Str2 (1 .. Len2) then
            First_Difference := Current_Line;
            exit;
         end if;

         Current_Line := Current_Line + 1;

      end loop;

      Close (File_Output);
      Close (File_Oracle);

      return First_Difference;
   end Compare;

   Difference   : Integer;
   Dir          : Dir_Type;
   Current_Name : String (1 .. 1024);
   Name_Length  : Natural;
   Success      : Boolean;
   Arguments    : Argument_List_Access;

begin

   Open (Dir, "./regressions");
   Read (Dir, Current_Name, Name_Length);

   while Name_Length /= 0 loop
      declare
         Path_Name : String := Current_Name (1 .. Name_Length);
      begin
         if Path_Name /= "."
           and then Path_Name /= ".."
           and then Path_Name /= ""
           and then Path_Name /= "CVS"
         then
            Arguments := Argument_String_To_List (Path_Name);
            Normalize_Arguments (Arguments.all);

            Put (Path_Name & " ");

            Spawn ("fixreg",
                   Arguments.all,
                   Success);

            Difference := Compare
              ("regressions/" & Path_Name & "/err.ora",
               "regressions/" & Path_Name & "/err.out");

            if Difference /= 0 then
               Put_Line ("failed at line " & Difference'Img);
            else
               Put_Line ("success");
            end if;
         end if;
      end;
      Read (Dir, Current_Name, Name_Length);
   end loop;

   Close (Dir);
end Regression;
