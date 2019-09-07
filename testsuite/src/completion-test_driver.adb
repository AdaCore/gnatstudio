------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.Symbols;              use GNATCOLL.Symbols;

with Glib.Unicode;                    use Glib.Unicode;

with Completion.Ada;                  use Completion.Ada;
with Completion.History;              use Completion.History;

with Completion.Ada.Constructs_Extractor;
use Completion.Ada.Constructs_Extractor;

with Language;                use Language;
with String_Utils;            use String_Utils;
with Language.Ada;            use Language.Ada;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree;       use Ada_Semantic_Tree;
with Ada_Semantic_Tree.Assistants; use Ada_Semantic_Tree.Assistants;
with Projects;                use Projects;
with GNATCOLL.Utils;          use GNATCOLL.Utils;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Xref;                    use Xref;

procedure Completion.Test_Driver is
   use Standard.Ada;
   use Token_List;

   Symbols      : constant Symbol_Table_Access := Allocate;
   Db           : General_Xref_Database;
   Tree         : constant Project_Tree_Access := new Project_Tree;
   New_Registry : constant Project_Registry_Access := Create (Tree);

   procedure Next_Complete_Tag
     (Buffer      : String;
      Index       : in out Natural;
      Word_Start  : out Natural;
      Word_End    : out Natural);

   procedure Next_Complete_Number
     (Buffer : String;
      Index  : in out Natural;
      Number : out Integer);
   --  Retreived the next number given in parenthesis after the offset, null
   --  if none found until the next LF character.

   procedure Display (List : Completion_List; Name : String);

   function Get_New_Construct_Extractor
     (File : Virtual_File; Project : String) return Completion_Resolver_Access;

   procedure Parse_File (File : Virtual_File);
   procedure Extract_Constructs (File : Virtual_File);
   procedure Analyze_Proposal (File : Virtual_File);
   procedure Extract_Entities (File : Virtual_File; Project : String);
   procedure Full_Test
     (File : Virtual_File; Project : String; History : Boolean);

   -----------------------
   -- Next_Complete_Tag --
   -----------------------

   procedure Next_Complete_Tag
     (Buffer      : String;
      Index       : in out Natural;
      Word_Start  : out Natural;
      Word_End    : out Natural)
   is
      Pattern : constant String := "-- COMPLETE HERE";
   begin
      Skip_To_String (Buffer, Index, Pattern);

      if Index + Pattern'Length - 1 > Buffer'Last
        or else Buffer (Index .. Index + Pattern'Length - 1)
        /= Pattern
      then
         Index := 0;
         Word_Start := 0;
         Word_End := 0;

         return;
      end if;

      Word_End := Index - 1;
      Word_Start := Word_End;
      Skip_Word (Buffer, Word_Start, -1);
      Word_Start := Word_Start + 1;

      Index := Index + Pattern'Length;
   end Next_Complete_Tag;

   --------------------------
   -- Next_Complete_Number --
   --------------------------

   procedure Next_Complete_Number
     (Buffer : String;
      Index  : in out Natural;
      Number : out Integer)
   is
      Is_Start : Boolean := False;
      First : Integer := -1;
      Last : Integer := -1;
   begin
      Number := -1;

      while Index in Buffer'Range and then Buffer (Index) /= ASCII.LF loop
         if not Is_Blank (Buffer (Index)) then
            if not Is_Start then
               if Buffer (Index) /= '[' then
                  return;
               end if;

               Is_Start := True;
            else
               if Buffer (Index) in '0' .. '9' then
                  if First = -1 then
                     First := Index;
                  end if;

                  Last := Index;
               elsif Buffer (Index) = ']' then
                  Number := Integer'Value (Buffer (First .. Last));
                  return;
               else
                  return;
               end if;
            end if;
         end if;

         Index := Index + 1;
      end loop;
   end Next_Complete_Number;

   -------------
   -- Display --
   -------------

   procedure Display (List : Completion_List; Name : String) is
      Iter : Completion_Iterator;
   begin
      Put_Line (" *** " & Name & " *** ");

      Iter := First (List, Db);

      --  This loop displays the contents of the list.

      while not At_End (Iter) loop
         Text_IO.Put (Get_Completion (Get_Proposal (Iter), Db) & " (");
         Text_IO.Put
           (Language_Category'Image
             (Get_Category (Get_Proposal (Iter))) & ")");
         New_Line;

         Next (Iter, Db);
      end loop;
   end Display;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File (File : Virtual_File) is
      Result      : Parsed_Expression;

      Buffer : constant String_Access := Read_File (File);

      procedure Display (List  : Token_List.Vector);
      procedure Display (Token : Token_Record);

      procedure Display (List : Token_List.Vector) is
         Current : Token_List.Cursor := List.First;
      begin
         while Has_Element (Current) loop
            Display (Element (Current));
            Current := Next (Current);
         end loop;
      end Display;

      procedure Display (Token : Token_Record) is
      begin
         Put (Image (Token.Tok_Type));

         case Token.Tok_Type is
            when Tok_Identifier | Tok_Operator =>
               Put (" ");
               Put (Buffer
                 (Integer (Token.Token_First)
                    .. Integer (Token.Token_Last)));
            when Tok_Expression =>
               Put (" (");
               Put (String_Index_Type'Image (Token.Token_First));
               Put (",");
               Put (String_Index_Type'Image (Token.Token_Last));
               Put (")");
            when others =>
               null;
         end case;

         New_Line;
      end Display;

   begin
      Put_Line ("*** COMPLETE RESULT ***");
      Result := Parse_Expression_Backward
        (Buffer,
         String_Index_Type (UTF8_Find_Prev_Char (Buffer.all, Buffer'Last)),
         0);
      Display (Result.Tokens);
      Free (Result);
      Put_Line ("*** REFERENCE ***");
      Put_Line
        ("[" & Ada_Lang.Parse_Reference_Backwards
           (Buffer.all,
            String_Index_Type (UTF8_Find_Prev_Char (Buffer.all, Buffer'Last)),
            0) & "]");
      Free (Result);
   end Parse_File;

   ---------------------------------
   -- Get_New_Construct_Extractor --
   ---------------------------------

   function Get_New_Construct_Extractor
     (File : Virtual_File; Project : String) return Completion_Resolver_Access
   is
      procedure Project_Error (Msg : String);

      procedure Project_Error (Msg : String) is
      begin
         Text_IO.Put_Line ("Error loading project: " & Msg);
      end Project_Error;

      Current_File : Structured_File_Access;

   begin
      declare
         File : constant Virtual_File := GNATCOLL.VFS.Create
           ("../../../share/predefined_ada.xml");
      begin
         if File.Is_Regular_File then
            Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
              (Db.Constructs, File);
         else
            declare
               File : constant Virtual_File :=  GNATCOLL.VFS.Create
                 (+Executable_Location & "share/predefined_ada.xml");
            begin
               if File.Is_Regular_File then
                  Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
                    (Db.Constructs, File);
               else
                  Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
                    (Db.Constructs, No_File);
               end if;
            end;
         end if;
      end;

      if Project /= "" then
         Tree.Load
           (Root_Project_Path  => Create_From_Dir (Get_Current_Dir, +Project),
            Errors             => Project_Error'Unrestricted_Access);

         declare
            Files : constant GNATCOLL.VFS.File_Array_Access :=
               Tree.Root_Project.Source_Files (Recursive => True);
         begin
            for J in Files.all'Range loop
               declare
                  Dummy_File_Node : Structured_File_Access;
                  pragma Unreferenced (Dummy_File_Node);
               begin
                  Dummy_File_Node := Get_Or_Create
                    (Db.Constructs, Files.all (J));
               end;
            end loop;
         end;
      end if;

      Current_File := Get_Or_Create (Db.Constructs, File);

      return New_Construct_Completion_Resolver
        (Db.Constructs,
         File,
         Get_Buffer (Current_File));
   end Get_New_Construct_Extractor;

   ------------------------
   -- Extract_Constructs --
   ------------------------

   procedure Extract_Constructs (File : Virtual_File) is
      Result      : Completion_List;
      Tag_Index   : Natural;

      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : constant Completion_Resolver_Access :=
        Get_New_Construct_Extractor (File, "");

      Buffer : constant String_Access := Read_File (File);

   begin
      Tag_Index := 1;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Result := Get_Initial_Completion_List
           (Manager => Manager,
            Context =>
              Create_Context
                (Manager,
                 File,
                 Buffer,
                 Ada_Lang,
                 String_Index_Type (End_Word)));

         Display (Result, Buffer (Start_Word .. End_Word));

         Free (Result);
      end loop;
   end Extract_Constructs;

   ----------------------
   -- Analyze_Proposal --
   ----------------------

   procedure Analyze_Proposal (File : Virtual_File) is
      Result      : Completion_List;
      Tag_Index   : Natural;

      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : constant Completion_Resolver_Access :=
        Get_New_Construct_Extractor (File, "");

      Buffer      : constant String_Access := Read_File (File);

   begin
      Tag_Index := 1;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Result := Get_Initial_Completion_List
           (Manager => Manager,
            Context => Create_Context
              (Manager, File, Buffer, Ada_Lang, String_Index_Type (End_Word)));

         Display (Result, Buffer (Start_Word .. End_Word));
      end loop;
   end Analyze_Proposal;

   ----------------------
   -- Extract_Entities --
   ----------------------

   procedure Extract_Entities (File : Virtual_File; Project : String) is
      Result      : Completion_List;
      Tag_Index   : Natural;
      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : constant Completion_Resolver_Access :=
        Get_New_Construct_Extractor (File, Project);

      Buffer      : constant String_Access := Read_File (File);

   begin
      Tag_Index := 1;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Get_Completion_Root
           (Resolver => Resolver,
            Offset   => String_Index_Type (End_Word),
            Context  => Create_Context
              (Manager, File, Buffer, Ada_Lang, String_Index_Type (End_Word)),
            Result   => Result);

         Display (Result, Buffer (Start_Word .. End_Word));

         Free (Result);
      end loop;
   end Extract_Entities;

   ---------------
   -- Full_Test --
   ---------------

   procedure Full_Test
     (File : Virtual_File; Project : String; History : Boolean)
   is
      Result      : Completion_List;
      Tag_Index   : Natural;
      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : Completion_Resolver_Access;

      Buffer  : constant String_Access := Read_File (File);

      History_Resolver : Completion_History_Access;

      Apply_Number : Integer := -1;

   begin
      Resolver := Get_New_Construct_Extractor (File, Project);

      Tag_Index := 1;

      if History then
         History_Resolver := new Completion_History;
         Register_Resolver (Manager, History_Resolver);
      end if;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);
         Next_Complete_Number (Buffer.all, Tag_Index, Apply_Number);

         exit when Tag_Index = 0;

         Result := Get_Initial_Completion_List
           (Manager => Manager,
            Context => Create_Context
              (Manager, File, Buffer, Ada_Lang, String_Index_Type (End_Word)));

         Display (Result, Buffer (Start_Word .. End_Word));

         if Apply_Number /= -1 then
            declare
               Iter : Completion_Iterator;
            begin
               Iter := First (Result, Db);

               for J in 1 .. Apply_Number loop
                  Next (Iter, Db);
               end loop;

               if Get_Proposal (Iter) in Storable_Proposal'Class then
                  Prepend_Proposal (History_Resolver, Get_Proposal (Iter));
               end if;
            end;
         end if;
      end loop;
   end Full_Test;

   Given_File : Virtual_File;
   Handler : Abstract_Language_Handler;

begin
   if Argument_Count < 2 then
      Put_Line ("Usage : <command> <file_name> <mode>");
      return;
   end if;

   Handler := new Ada_Semantic_Tree.Lang.Ada_Language_Handler;
--   Create_Handler (Handler, Symbols);
--   Set_Registry (Handler, New_Registry);

   Db := new General_Xref_Database_Record;
   Db.Initialize
     (Lang_Handler => Handler,
      Symbols      => Symbols,
      Registry     => New_Registry);
   Set_Provider
     (Db.Constructs,
      new File_Buffer_Provider);

--   Handler.Register_Language (Ada_Lang, null);
   Set_Symbols (Ada_Lang, Symbols);

   Given_File := Create_From_Base (+Argument (1));

   Tree.Load_Empty_Project;

   if Argument (2) = "parse" then
      Parse_File (Given_File);
   elsif Argument (2) = "construct" then
      Extract_Constructs (Given_File);
   elsif Argument (2) = "analyze" then
      Analyze_Proposal (Given_File);
   elsif Argument (2) = "entity" then
      if Argument_Count < 3 then
         Put_Line ("Usage : <command> <file_name> entity <project_name>");
         return;
      end if;

      Extract_Entities (Given_File, Argument (3));

   elsif Argument (2) = "full" then
      if Argument_Count < 3 then
         Put_Line ("Usage : <command> <file_name> full <project_name>");
         return;
      end if;

      if Command_Line.Argument_Count >= 4 then
         if Argument (4) = "-history" then
            Full_Test (Given_File, Argument (3), True);
         else
            Full_Test (Given_File, Argument (3), False);
         end if;
      else
         Full_Test (Given_File, Argument (3), False);
      end if;
   end if;

   Flush;

   Destroy (Db);

exception
   when E : others =>
      Put_Line ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Completion.Test_Driver;
