-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Strings;                   use GNAT.Strings;

with Language;                       use Language;
with Language.Ada;                   use Language.Ada;
with Language.Tree;                  use Language.Tree;
with Language.Tree.Ada;              use Language.Tree.Ada;
with Language.Tree.Database;         use Language.Tree.Database;
with Ada_Semantic_Tree.Declarations;
use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Expression_Parser;
use Ada_Semantic_Tree.Expression_Parser;
with Ada_Semantic_Tree.Parts;      use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Assistants; use Ada_Semantic_Tree.Assistants;
with Ada_Semantic_Tree.Type_Tree;  use Ada_Semantic_Tree.Type_Tree;
with Namet;                          use Namet;
with Projects;                       use Projects;
with Projects.Registry;              use Projects.Registry;
with String_Utils;                   use String_Utils;
with Entities;                       use Entities;
with VFS;                            use VFS;

procedure Ada_Semantic_Tree.Test is

   procedure Next_Test_Command
     (Buffer : String;
      File   : Structured_File_Access;
      Index  : in out Natural);

   procedure Read_Next_Word
     (Buffer     : String;
      Index      : in out Natural;
      Word_Begin : out Natural;
      Word_End   : out Natural);

   procedure Analyze_File (File : VFS.Virtual_File);

   New_Registry : aliased Project_Registry;
   Construct_Db : constant Construct_Database_Access := new Construct_Database;

   -----------------------
   -- Next_Complete_Tag --
   -----------------------

   procedure Next_Test_Command
     (Buffer : String;
      File   : Structured_File_Access;
      Index  : in out Natural)
   is
      Pattern : constant String := "-- TEST";

      Word_Begin, Word_End : Natural;

      Looked_Offset : Natural;
   begin
      Skip_To_String (Buffer, Index, Pattern);

      if Index + Pattern'Length - 1 > Buffer'Last
        or else Buffer (Index .. Index + Pattern'Length - 1) /= Pattern
      then
         Index := 0;

         return;
      end if;

      Looked_Offset := Index - 1;

      Index := Index + Pattern'Length;

      Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

      Put ("COMMAND " & Base_Name (Get_File_Path (File)) & " l" & Integer'Image
        (Lines_Count (Buffer (Buffer'First .. Looked_Offset))) & ": ");

      if Buffer (Word_Begin .. Word_End) = "DECLARATION" then
         Put_Line ("GET DECLARATION");

         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         declare
            Visibility : Visibility_Confidence := Public_Library_Visible;
            pragma Unreferenced (Visibility);
            --  ??? This visibility parameter is currenty not used.
            List : Declaration_List;
            It : Declaration_Iterator;
            Construct : access Simple_Construct_Information;
            Decl      : Entity_Access;
         begin
            if Buffer (Word_Begin .. Word_End) = "LIBRARY_VISIBLE" then
               Visibility := Public_Library_Visible;
            elsif Buffer (Word_Begin .. Word_End) = "WITH_VISIBLE" then
               Visibility := With_Visible;
            elsif Buffer (Word_Begin .. Word_End) = "USE_VISIBLE" then
               Visibility := Use_Visible;
            end if;

            List := Find_Declarations
              (File              => File,
               Offset            => Looked_Offset,
               From_Visibility   => Null_Visibility_Context,
               Expression        => Null_Parsed_Expression,
               Categories        => Null_Category_Array,
               Is_Partial        => False);

            It := First (List);

            while not At_End (It) loop
               Decl := Get_Entity (It);

               Construct := Get_Construct
                 (To_Construct_Tree_Iterator (Decl));

               Put_Line
                 ("---> DECLARATION "
                  & Base_Name
                    (Get_File_Path (Get_File (Decl)))
                  & ":" & Construct.Sloc_Start.Line'Img
                  & ":" & Construct.Sloc_Start.Column'Img
                  & ": " & Construct.Name.all);

               Next (It);
            end loop;

            Free (It);
            Free (List);
         end;
      elsif Buffer (Word_Begin .. Word_End) = "RELATION" then
         Put_Line ("GET RELATION");

         declare
            It : constant Construct_Tree_Iterator := Get_Iterator_At
              (Tree              => Get_Tree (File),
               Location          => To_Location (Word_Begin),
               From_Type         => Start_Construct,
               Position          => Before,
               Categories_Seeked => Null_Category_Array);

            Entity : constant Entity_Access := To_Entity_Access (File, It);
            Relation_Entity : Entity_Access := Null_Entity_Access;
         begin

            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            if Buffer (Word_Begin .. Word_End) = "FIRST" then
               Relation_Entity := Get_First_Occurence (Entity);
            elsif Buffer (Word_Begin .. Word_End) = "SECOND" then
               Relation_Entity := Get_Second_Occurence (Entity);
            elsif Buffer (Word_Begin .. Word_End) = "THIRD" then
               Relation_Entity := Get_Third_Occurence (Entity);
            else
               Put_Line
                 (Buffer (Word_Begin .. Word_End) & " IS NOT A VALID PART");
            end if;

            if Relation_Entity = Null_Entity_Access then
               Put_Line ("---> RELATION: NULL");
            else
               Put_Line
                 ("---> RELATION: "
                  & Base_Name (Get_File_Path (Get_File (Relation_Entity)))
                  & ":" & Get_Construct (Relation_Entity).Sloc_Start.Line'Img
                  & ":" & Get_Construct
                    (Relation_Entity).Sloc_Start.Column'Img);
            end if;
         end;
      elsif Buffer (Word_Begin .. Word_End) = "TYPE" then
         declare
            It : constant Construct_Tree_Iterator := Get_Iterator_At
              (Tree              => Get_Tree (File),
               Location          => To_Location (Word_Begin),
               From_Type         => Start_Construct,
               Position          => Before,
               Categories_Seeked => Null_Category_Array);
         begin
            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            if Buffer (Word_Begin .. Word_End) = "PRIMITIVES" then
               Put_Line ("GET PRIMITIVES");

               declare
                  Primitives : constant Primitive_Array := Extract_Primitives
                    (Get_Ada_Type (To_Entity_Access (File, It)));
                  Entity     : Entity_Access;
               begin
                  for J in Primitives'Range loop
                     Entity := Get_Entity_Or_Overriden (Primitives (J));

                     Put_Line
                       ("---> PRIMITIVE: " & Get_Construct (Entity).Name.all
                        & " " & Base_Name (Get_File_Path (Get_File (Entity)))
                        & " l" & Get_Construct (Entity).Sloc_Start.Line'Img);
                  end loop;
               end;
            end if;
         end;
      elsif Buffer (Word_Begin .. Word_End) = "REPLACE" then
         declare
            File_Modified : Virtual_File;
            File_W   : Writable_File;
            Contents : String_Access;
         begin
            Put ("REPLACE ");

            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            Put (Buffer (Word_Begin .. Word_End));

            File_Modified :=
              Create (Buffer (Word_Begin .. Word_End), New_Registry);

            File_W := Write_File (File_Modified);

            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            Put_Line (" BY " & Buffer (Word_Begin .. Word_End));

            Contents := Read_File
              (Create_From_Dir
                 (Get_Current_Dir, Buffer (Word_Begin .. Word_End)));

            Write (File_W, Contents.all);

            Free (Contents);

            Close (File_W);

            Update_Contents (Construct_Db, File_Modified);
         end;
      elsif Buffer (Word_Begin .. Word_End) = "ANALYZE" then
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         Put_Line ("ANALYZE " & Buffer (Word_Begin .. Word_End));

         Analyze_File
           (Create (Buffer (Word_Begin .. Word_End), New_Registry));
      elsif Buffer (Word_Begin .. Word_End) = "DIFF" then
         declare
            Left_Tree, Right_Tree : Construct_Tree;
            Left_File, Right_File : Virtual_File;

            procedure Diff_Callback
              (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

            procedure Put
              (Obj : Construct_Tree_Iterator);

            procedure Put
              (Obj : Construct_Tree_Iterator)
            is
               C : access Simple_Construct_Information;
            begin
               C := Get_Construct (Obj);
               Put ("(l" & C.Sloc_Start.Line'Img & ")");

               if C.Name /= null then
                  Put (" "  & C.Name.all);
               end if;
            end Put;

            procedure Diff_Callback
              (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind) is
            begin
               case Kind is
                  when Removed =>
                     Put ("- ");
                     Put (Old_Obj);
                     New_Line;
                  when Added =>
                     Put ("+ ");
                     Put (New_Obj);
                     New_Line;
                  when others =>
                     null;
               end case;
            end Diff_Callback;

         begin
            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);
            Left_File := Create_From_Dir
              (Get_Current_Dir, Buffer (Word_Begin .. Word_End));
            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);
            Right_File := Create_From_Dir
              (Get_Current_Dir, Buffer (Word_Begin .. Word_End));

            Left_Tree :=
              Get_Tree
                (Get_Or_Create (Construct_Db, Left_File, Ada_Tree_Lang));
            Right_Tree :=
              Get_Tree
                (Get_Or_Create (Construct_Db, Right_File, Ada_Tree_Lang));

            New_Line;
            Diff
              (Lang     => Ada_Tree_Lang,
               Old_Tree => Left_Tree,
               New_Tree => Right_Tree,
               Callback => Diff_Callback'Unrestricted_Access);
         end;
      elsif Buffer (Word_Begin .. Word_End) = "ALL_N_DECLARATIONS"
        or else Buffer (Word_Begin .. Word_End) = "ALL_DECLARATIONS"
      then
         declare
            Verbose   : Boolean := False;
            Max_Files : Integer;

            Full_File : Structured_File_Access;

            function Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;

            function Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Entity, Sloc_Start, Partial_Entity);

               List     : Declaration_List;
               It       : Declaration_Iterator;
               --  Construct : Simple_Construct_Information;
            begin
               if Verbose then
                  Put ("[" & Sloc_End.Line'Img & ":" & Sloc_End.Column'Img);
                  Flush;
               end if;

               List := Find_Declarations
                 (File              => Full_File,
                  Offset            => Sloc_End.Index,
                  From_Visibility   => Null_Visibility_Context,
                  Expression        => Null_Parsed_Expression,
                  Categories        => Null_Category_Array,
                  Is_Partial        => False);

               It := First (List);

               while not At_End (It) loop
                  Next (It);
               end loop;

               Free (It);
               Free (List);

               if Verbose then
                  Put ("]");
                  Flush;
               end if;

               return False;
            exception
               when E : others =>
                  Put_Line
                    ("UNEXPECTED EXCEPTION: " & Exception_Information (E));

               return True;
            end Callback;

            Files : constant VFS.File_Array_Access :=
              Get_Source_Files (Get_Root_Project (New_Registry), True);

            Lang_Name : Namet.Name_Id;
         begin
            if Buffer (Word_Begin .. Word_End) = "ALL_N_DECLARATIONS" then
               Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

               Put_Line
                 ("ALL_N_DECLARATIONS " & Buffer (Word_Begin .. Word_End));

               Max_Files := Integer'Value (Buffer (Word_Begin .. Word_End));

               Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

               if Buffer (Word_Begin .. Word_End) = "VERBOSE" then
                  Verbose := True;
               end if;

               for J in Files.all'Range loop
                  exit when J > Max_Files;

                  Put
                    ("---> ANALYZE"
                     & J'Img & " => " & Base_Name (Files.all (J)));

                  Flush;

                  Lang_Name := Get_Language_From_File_From_Project
                    (New_Registry, Files.all (J));

                  if Lang_Name /= No_Name
                    and then To_Lower (Get_Name_String (Lang_Name)) = "ada"
                  then
                     Full_File := Get_Or_Create
                       (Construct_Db,
                        Files.all (J),
                        Ada_Tree_Lang);

--                       Set_Profiling (True);
                     Parse_Entities
                       (Lang     => Ada_Lang,
                        Buffer   => Get_Buffer (Full_File).all,
                        Callback => Callback'Unrestricted_Access);
--                       Set_Profiling (False);

                     Put_Line (" done.");
                  else
                     Put_Line (" skipped.");
                  end if;
               end loop;
            else
               Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

               Put_Line
                 ("ALL_DECLARATIONS " & Buffer (Word_Begin .. Word_End));

               Full_File := Get_Or_Create
                 (Get_Database (File),
                  Create (Buffer (Word_Begin .. Word_End), New_Registry),
                  Ada_Tree_Lang);

               Put_Line
                 ("---> ANALYZE " & Base_Name (Get_File_Path (Full_File)));

               Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

               if Buffer (Word_Begin .. Word_End) = "VERBOSE" then
                  Verbose := True;
               end if;

               Parse_Entities
                 (Lang     => Ada_Lang,
                  Buffer   => Get_Buffer (Full_File).all,
                  Callback => Callback'Unrestricted_Access);
            end if;
         end;
      else
         Put_Line ("UNKOWN COMMAND " & Buffer (Word_Begin .. Word_End));
      end if;

   exception
      when E : others =>
         Put_Line ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
   end Next_Test_Command;

   --------------------
   -- Read_Next_Word --
   --------------------

   procedure Read_Next_Word
     (Buffer     : String;
      Index      : in out Natural;
      Word_Begin : out Natural;
      Word_End   : out Natural) is
   begin
      Word_Begin := Index;
      Skip_Blanks (Buffer, Word_Begin);
      Word_End := Word_Begin;

      while Is_Alphanumeric (Buffer (Word_End))
        or else Buffer (Word_End) = '_'
        or else Buffer (Word_End) = '.'
        or else Buffer (Word_End) = '-'
      loop
         Word_End := Word_End + 1;
      end loop;

      Word_End := Word_End - 1;

      Index := Word_End + 1;
   end Read_Next_Word;

   procedure Analyze_File (File : VFS.Virtual_File) is
      Index : Natural;
      File_Node : Structured_File_Access;
   begin
      Index := 1;

      File_Node := Get_Or_Create
        (Construct_Db,
         File,
         Ada_Tree_Lang);

      while Index /= 0 loop
         Next_Test_Command
           (Buffer => Get_Buffer (File_Node).all,
            File   => File_Node,
            Index  => Index);
      end loop;
   end Analyze_File;

   procedure Project_Error (Msg : String);

   procedure Project_Error (Msg : String) is
   begin
      Put_Line ("Error loading project: " & Msg);
   end Project_Error;

   Db           : Entities_Database;
   pragma Unreferenced (Db);

   Loaded, Success : Boolean;
begin
--     Set_Profiling (False);
   Projects.Registry.Initialize;

   Db := Create (New_Registry'Unchecked_Access);

   Load
     (Registry           => New_Registry,
      Root_Project_Path  => Create_From_Dir (Get_Current_Dir, Argument (1)),
      Errors             => Project_Error'Unrestricted_Access,
      New_Project_Loaded => Loaded,
      Status             => Success);

   Recompute_View (New_Registry, Project_Error'Unrestricted_Access);

   Initialize (Construct_Db.all, new File_Buffer_Provider);

   Ada_Semantic_Tree.Assistants.Register_Ada_Assistants (Construct_Db);

   declare
      Files : constant VFS.File_Array_Access :=
        Get_Source_Files (Get_Root_Project (New_Registry), True);
      File_Node : Structured_File_Access;

      pragma Unreferenced (File_Node);
   begin
      --  First, generate the whole database

      for J in Files.all'Range loop
         File_Node := Get_Or_Create
           (Construct_Db,
            Files.all (J),
            Ada_Tree_Lang);
      end loop;

      --  Then, execute the tests

      if Argument_Count > 1 then
         Analyze_File
           (Create (Argument (2), New_Registry));
      else
         for J in Files.all'Range loop
            Analyze_File (Files.all (J));
         end loop;
      end if;
   end;

   Destroy (Construct_Db);
   Destroy (New_Registry);
   Projects.Registry.Finalize;

end Ada_Semantic_Tree.Test;
