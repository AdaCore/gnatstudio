with GNAT.Command_Line;  use GNAT.Command_Line;
with Input_Sources;      use Input_Sources;
with Input_Sources.File; use Input_Sources.File;
with Input_Sources.Http; use Input_Sources.Http;
with Sax.Readers;        use Sax.Readers;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;
with Instances;          use Instances;

procedure Testxmlgtk is
   use XML_Int, Gtk_Readers;

   Read : Input_Source_Access;
   My_Tree_Reader : Gtk_Reader;
   Name_Start : Natural;
   Tree : Node_Ptr;

begin
   declare
      S : constant String := Get_Argument;
   begin
      if S'Length > 0 then
         if S'Length > 6 and then S (S'First .. S'First + 6) = "http://" then
            Read := new Http_Input;
            Open (S, Http_Input (Read.all));
         else
            Read := new File_Input;
            Open (S, File_Input (Read.all));
         end if;

         --  Base file name should be used as the public Id
         Name_Start := S'Last;
         while Name_Start >= S'First  and then S (Name_Start) /= '/' loop
            Name_Start := Name_Start - 1;
         end loop;

         Set_Public_Id (Read.all, S (Name_Start + 1 .. S'Last));

         --  Full name is used as the system id
         Set_System_Id (Read.all, S);
      else
         return;
      end if;
   end;

   Set_Feature (My_Tree_Reader, Validation_Feature, False);
   Set_Feature (My_Tree_Reader, Test_Valid_Chars_Feature, False);

   Parse (My_Tree_Reader, Read.all);
   Close (Read.all);

   Tree := Get_Tree (My_Tree_Reader);
   XML_Int.Print (Tree);
   XML_Int.Free (Tree);

exception
   when E : XML_Fatal_Error =>
      Close (Read.all);
      Put_Line (Exception_Message (E));
end Testxmlgtk;
