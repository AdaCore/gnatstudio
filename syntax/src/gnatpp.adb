with Source_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Gnatpp is
   F      : File_Descriptor;
   Name   : constant String := Argument (1) & ASCII.NUL;
   Buffer : String_Access;
   Length : Integer;

begin
   F := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);
   Source_Analyzer.Format_Ada (Buffer.all);
end Gnatpp;
