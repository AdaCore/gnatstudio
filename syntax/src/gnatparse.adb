with Source_Analyzer; use Source_Analyzer;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Ada.Text_IO;  use Ada.Text_IO;
with String_Utils; use String_Utils;

procedure Gnatparse is
   F           : File_Descriptor;
   Name        : constant String := Argument (1) & ASCII.NUL;
   Buffer      : String_Access;
   Length      : Integer;
   Constructs  : Construct_List;
   Info        : Construct_Access;
   Indent      : Natural;
   Next_Indent : Natural;

begin
   F      := Open_Read (Name, Binary);
   Buffer := new String (1 .. Integer (File_Length (F)));
   Length := Read (F, Buffer.all'Address, Buffer'Length);
   Close (F);
   Parse_Ada_Constructs
     (Buffer (1 .. Length), Constructs, Indent, Next_Indent);
   Free (Buffer);
   Put_Line ("Indent = " & Image (Indent));
   Put_Line ("Next Indent = " & Image (Next_Indent));
   Info := Constructs.First;

   loop
      exit when Info = null;

      Put (Info.Token'Img & " ");

      if Info.Name /= null then
         Put (Info.Name.all & " ");
      end if;

      if Info.Subprogram_Spec then
         Put ("(spec) ");
      end if;

      Put ("First => " &
           Image (Info.Sloc_Start.Line) & ":" &
           Image (Info.Sloc_Start.Column) & ", ");
      Put ("Last => " &
           Image (Info.Sloc_End.Line) & ":" &
           Image (Info.Sloc_End.Column));
      New_Line;
      Info := Info.Next;
   end loop;

   Free (Constructs);
end Gnatparse;
