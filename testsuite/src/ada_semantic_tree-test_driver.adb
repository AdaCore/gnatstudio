------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;

with GNAT.Strings;                   use GNAT.Strings;

with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.Symbols;               use GNATCOLL.Symbols;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.VFS;                   use GNATCOLL.VFS;

with Ada_Semantic_Tree.Assistants;   use Ada_Semantic_Tree.Assistants;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Interfaces;   use Ada_Semantic_Tree.Interfaces;
with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;
with Ada_Semantic_Tree.Parts;        use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Type_Tree;    use Ada_Semantic_Tree.Type_Tree;
with Ada_Semantic_Tree.Generics;     use Ada_Semantic_Tree.Generics;
with Language.Ada;                   use Language.Ada;
with Language.Tree.Database;         use Language.Tree.Database;
with Language.Tree;                  use Language.Tree;
with Language;                       use Language;
with String_Utils;                   use String_Utils;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

procedure Ada_Semantic_Tree.Test_Driver is

   procedure Next_Test_Command
     (Buffer : access String;
      File   : Structured_File_Access;
      Index  : in out Natural);

   procedure Read_Next_Word
     (Buffer     : access String;
      Index      : in out Natural;
      Word_Begin : out Natural;
      Word_End   : out Natural);

   procedure Analyze_File (File : GNATCOLL.VFS.Virtual_File);

   Symbols      : constant Symbol_Table_Access := Allocate;
   Tree         : constant Project_Tree_Access := new Project_Tree;
   Construct_Db : constant Construct_Database_Access := new Construct_Database;

   Test_Trace : constant Trace_Handle := Create
     ("GPS.INTERNAL.ADA_SEMANTIC_TREE.TEST");

   -----------------------
   -- Next_Test_Command --
   -----------------------

   procedure Next_Test_Command
     (Buffer : access String;
      File   : Structured_File_Access;
      Index  : in out Natural)
   is
      Pattern : constant String := "-- TEST";

      Word_Begin, Word_End : Natural;

      Looked_Offset : Natural;
   begin
      Skip_To_String (Buffer.all, Index, Pattern);

      if Index + Pattern'Length - 1 > Buffer'Last
        or else Buffer (Index .. Index + Pattern'Length - 1) /= Pattern
      then
         Index := 0;

         return;
      end if;

      Looked_Offset := Index - 1;

      Index := Index + Pattern'Length;

      Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

      if Buffer (Word_Begin .. Word_End) /= "ECHO" then
         Put ("COMMAND " &
              (+Base_Name (Get_File_Path (File))) & " l" & Integer'Image
              (Lines_Count (Buffer (Buffer'First .. Looked_Offset))) & ": ");
      end if;

      if Buffer (Word_Begin .. Word_End) = "DECLARATION" then
         Put_Line ("GET DECLARATION");

         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         declare
            Visibility : Visibility_Confidence := Public_Library_Visible;
            List : Entity_List;
            It : Entity_Iterator;
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
              ((From_File,
                Null_Instance_Info,
                File,
                String_Index_Type (Looked_Offset)),
               From_Visibility   =>
                 (File,
                  String_Index_Type (Looked_Offset),
                  Everything,
                  Visibility),
               Expression        => Null_Parsed_Expression,
               Filter            => Null_Filter,
               Is_Partial        => False);

            It := First (List);

            while not At_End (It) loop
               Decl := Get_Entity (It);

               Construct := Get_Construct
                 (To_Construct_Tree_Iterator (Decl));

               Put_Line
                 ("---> DECLARATION "
                  & (+Base_Name (Get_File_Path (Get_File (Decl))))
                  & ":" & Construct.Sloc_Start.Line'Img
                  & ":" & Construct.Sloc_Start.Column'Img
                  & ": " & Get (Construct.Name).all);

               Next (It);
            end loop;

            Free (It);
            Free (List);
         end;

      elsif Buffer (Word_Begin .. Word_End) = "TREE_DECLARATION" then
         Put_Line ("GET DECLARATION FROM LANGUAGE TREE");

         declare
            Line      : Integer;
            Column    : Visible_Column_Type;
            Decl      : Entity_Access;
            Construct : access Simple_Construct_Information;
         begin
            To_Line_Column
              (File                 => File,
               Absolute_Byte_Offset => String_Index_Type (Looked_Offset),
               Line                 => Line,
               Column               => Column);

            Decl := Ada_Tree_Lang.Find_Declaration
              (File   => File,
               Line   => Line,
               Column => To_Line_String_Index (File, Line, Column));

            if Decl /= Null_Entity_Access then
               Construct := Get_Construct
                 (To_Construct_Tree_Iterator (Decl));

               Put_Line
                 ("---> DECLARATION "
                  & (+Base_Name (Get_File_Path (Get_File (Decl))))
                  & ":" & Construct.Sloc_Start.Line'Img
                  & ":" & Construct.Sloc_Start.Column'Img
                  & ": " & Get (Construct.Name).all);
            else
               Put_Line ("---> DECLARATION [null]");
            end if;
         end;

      elsif Buffer (Word_Begin .. Word_End) = "RELATION" then
         Put_Line ("GET RELATION");

         declare
            It : constant Construct_Tree_Iterator := Get_Iterator_At
              (Tree              => Get_Tree (File),
               Location          =>
                 To_Location (String_Index_Type (Word_Begin)),
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
                  & (+Base_Name (Get_File_Path (Get_File (Relation_Entity))))
                  & ":" & Get_Construct (Relation_Entity).Sloc_Start.Line'Img
                  & ":" & Get_Construct
                    (Relation_Entity).Sloc_Start.Column'Img);
            end if;
         end;

      elsif Buffer (Word_Begin .. Word_End) = "TYPE" then
         declare
            It : constant Construct_Tree_Iterator := Get_Iterator_At
              (Tree              => Get_Tree (File),
               Location          =>
                 To_Location (String_Index_Type (Word_Begin)),
               From_Type         => Start_Construct,
               Position          => Before,
               Categories_Seeked => Null_Category_Array);
         begin
            Set_Active (Test_Trace, True);

            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            if Buffer (Word_Begin .. Word_End) = "PRIMITIVES" then
               Put_Line ("GET PRIMITIVES");

               declare
                  Primitives : constant Primitive_Array := Extract_Primitives
                    (Get_Ada_Type (To_Entity_Access (File, It)));
                  Entity     : Entity_Access;
               begin
                  for J in Primitives'Range loop
                     Entity := Get_Entity_Or_Overridden (Primitives (J));

                     Put_Line
                       ("---> PRIMITIVE: "
                        & Get (Get_Construct (Entity).Name).all
                        & " " & (+Base_Name
                          (Get_File_Path (Get_File (Entity))))
                        & " l" & Get_Construct (Entity).Sloc_Start.Line'Img);
                  end loop;
               end;
            elsif Buffer (Word_Begin .. Word_End) = "CHILDREN" then
               Put_Line ("GET CHILDREN");

               declare
                  Children : constant Entity_Persistent_Array := Get_Children
                    (Get_Ada_Type (To_Entity_Access (File, It)));
                  Entity     : Entity_Access;
               begin
                  for J in Children'Range loop
                     Entity := To_Entity_Access (Children (J));

                     Put_Line
                       ("---> CHILD: "
                        & Get (Get_Construct (Entity).Name).all
                        & " " & (+Base_Name
                          (Get_File_Path (Get_File (Entity))))
                        & " l" & Get_Construct (Entity).Sloc_Start.Line'Img);
                  end loop;
               end;
            elsif Buffer (Word_Begin .. Word_End) = "PARENTS" then
               Put_Line ("GET PARENTS");

               declare
                  Parents : constant Entity_Persistent_Array := Get_Parents
                    (Get_Ada_Type (To_Entity_Access (File, It)));
                  Entity     : Entity_Access;
               begin
                  for J in Parents'Range loop
                     Entity := To_Entity_Access (Parents (J));

                     Put_Line
                       ("---> PARENT: "
                        & Get (Get_Construct (Entity).Name).all
                        & " " & (+Base_Name
                          (Get_File_Path (Get_File (Entity))))
                        & " l" & Get_Construct (Entity).Sloc_Start.Line'Img);
                  end loop;
               end;
            elsif Buffer (Word_Begin .. Word_End) = "ANALYZE" then
               Read_Next_Word (Buffer, Index, Word_Begin, Word_End);
               Put_Line
                 ("ANALYZE TYPES FROM " & Buffer (Word_Begin .. Word_End));

               Analyze_All_Types
                 (Get_Or_Create
                    (Construct_Db,
                     GNATCOLL.VFS.Create_From_Dir
                       (Get_Current_Dir,
                        Filesystem_String (Buffer (Word_Begin .. Word_End)))));
            end if;

            Set_Active (Test_Trace, False);
         end;

      elsif Buffer (Word_Begin .. Word_End) = "REPLACE" then
         declare
            File_Modified : Virtual_File;
            File_W        : Writable_File;
            Contents      : String_Access;
         begin
            Put ("REPLACE ");

            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            Put (Buffer (Word_Begin .. Word_End));

            File_Modified :=
              Tree.Create (+Buffer (Word_Begin .. Word_End));

            File_W := Write_File (File_Modified);

            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

            Put_Line (" BY " & Buffer (Word_Begin .. Word_End));

            Contents := Read_File
              (Create_From_Dir
                 (Get_Current_Dir, +Buffer (Word_Begin .. Word_End)));

            --  Introduce a delay so that the timestamp of the file differs
            --  between two consecutive writes. A minimum delay of 2 seconds
            --  is needed under Windows.
            delay 2.1;
            Write (File_W, Contents.all);

            Free (Contents);

            Close (File_W);

            Update_Contents (Construct_Db, File_Modified);
         end;

      elsif Buffer (Word_Begin .. Word_End) = "ANALYZE" then
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         Put_Line ("ANALYZE " & Buffer (Word_Begin .. Word_End));

         Analyze_File (Tree.Create (+Buffer (Word_Begin .. Word_End)));

      elsif Buffer (Word_Begin .. Word_End) = "DIFF" then
         declare
            Left_Tree, Right_Tree : Construct_Tree;
            Left_File, Right_File : Virtual_File;

            procedure Diff_Callback
              (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

            procedure Put
              (Obj : Construct_Tree_Iterator);

            ---------
            -- Put --
            ---------

            procedure Put
              (Obj : Construct_Tree_Iterator)
            is
               C : access Simple_Construct_Information;
            begin
               C := Get_Construct (Obj);
               Put ("(l" & C.Sloc_Start.Line'Img & ")");

               if C.Name /= No_Symbol then
                  Put (" "  & Get (C.Name).all);
               end if;
            end Put;

            -------------------
            -- Diff_Callback --
            -------------------

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
              (Get_Current_Dir, +Buffer (Word_Begin .. Word_End));
            Read_Next_Word (Buffer, Index, Word_Begin, Word_End);
            Right_File := Create_From_Dir
              (Get_Current_Dir, +Buffer (Word_Begin .. Word_End));

            Left_Tree := Get_Tree (Get_Or_Create (Construct_Db, Left_File));
            Right_Tree := Get_Tree (Get_Or_Create (Construct_Db, Right_File));

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

            --------------
            -- Callback --
            --------------

            function Callback
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Entity, Sloc_Start, Partial_Entity);

               List : Entity_List;
               It   : Entity_Iterator;
            begin
               if Verbose then
                  Put ("[" & Sloc_End.Line'Img & ":" & Sloc_End.Column'Img);
                  Flush;
               end if;

               List := Find_Declarations
                 ((From_File,
                   Null_Instance_Info,
                   Full_File,
                   String_Index_Type (Sloc_End.Index)),
                  From_Visibility   => Null_Visibility_Context,
                  Expression        => Null_Parsed_Expression,
                  Filter            => Null_Filter,
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
                  Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

               return True;
            end Callback;

            Files     : constant GNATCOLL.VFS.File_Array_Access :=
                          Tree.Root_Project.Source_Files (Recursive => True);

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
                     & J'Img & " => " & (+Base_Name (Files.all (J))));

                  Flush;

                  declare
                     Lang_Name : constant String :=
                                   Tree.Info (Files (J)).Language;
                  begin
                     if To_Lower (Lang_Name) = "ada" then
                        Full_File := Get_Or_Create
                           (Construct_Db, Files.all (J));
                        Parse_Entities
                          (Lang     => Ada_Lang,
                           Buffer   => Get_Buffer (Full_File).all,
                           Callback => Callback'Unrestricted_Access);

                        Put_Line (" done.");
                     else
                        Put_Line (" skipped.");
                     end if;
                  end;
               end loop;

            else
               Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

               Put_Line
                 ("ALL_DECLARATIONS " & Buffer (Word_Begin .. Word_End));

               Full_File := Get_Or_Create
                 (Get_Database (File),
                  Tree.Create (+Buffer (Word_Begin .. Word_End)));

               Put_Line
                 ("---> ANALYZE " & (+Base_Name (Get_File_Path (Full_File))));

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

      elsif Buffer (Word_Begin .. Word_End) = "DATABASE_SEARCH" then
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         Put_Line
           ("DATABASE_SEARCH " & Buffer (Word_Begin .. Word_End));

         declare
            Expression : Parsed_Expression := Parse_Expression_Backward
              (Buffer       => Buffer,
               Start_Offset => String_Index_Type (Word_End),
               End_Offset   => String_Index_Type (Word_Begin));

            List : constant Entity_List := Find_Declarations
              (Context           =>
                 (From_Database,
                  Null_Instance_Info,
                  Construct_Db),
               From_Visibility   => Null_Visibility_Context,
               Expression        => Expression,
               Is_Partial        => False);

            List_It : Entity_Iterator;
         begin
            List_It := First (List);

            while not At_End (List_It) loop
               Put_Line
                 ("----> "
                  & (+Base_Name
                    (Get_File_Path (Get_File (Get_Entity (List_It)))))
                  & ":"
                  & Get_Construct (Get_Entity (List_It)).Sloc_Start.Line'Img
                  & ":"
                  & Get_Construct
                    (Get_Entity (List_It)).Sloc_Start.Column'Img);

               Next (List_It);
            end loop;

            Token_List.Clear (Expression.Tokens);
         end;

      elsif Buffer (Word_Begin .. Word_End) = "EXPORT" then
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         Put_Line ("EXPORT " & Buffer (Word_Begin .. Word_End));

         declare
            Assistant : constant Database_Assistant_Access :=
                          Ada_Semantic_Tree.Interfaces.Get_Assistant
                            (Construct_Db);
            Entity    : Entity_Access;
            Construct : access Simple_Construct_Information;
         begin
            Entity := Ada_Semantic_Tree.Interfaces.Get_Exported_Entity
              (Assistant, Buffer (Word_Begin .. Word_End));

            if Entity = Null_Entity_Access then
               Put_Line ("---> <not found>");
            else
               Construct := Get_Construct (Entity);
               Put_Line
                 ("---> " & Get (Construct.Name).all & ":"
                  & Construct.Sloc_Entity.Line'Img & ":"
                  & Construct.Sloc_Entity.Column'Img);
            end if;
         end;

      elsif Buffer (Word_Begin .. Word_End) = "IMPORT" then
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         Put_Line ("IMPORT " & Buffer (Word_Begin .. Word_End));

         declare
            List : Entity_List;
            It : Entity_Iterator;
         begin
            List := Find_Declarations
              ((From_Database,
                Null_Instance_Info,
                Construct_Db),
               Expression        =>
                 Parse_Expression_Backward
                   (Buffer,
                    String_Index_Type (Word_End),
                    String_Index_Type (Word_Begin)),
               Is_Partial        => True);

            It := First (List);

            if not At_End (It) then
               declare
                  Imported : constant Imported_Entity :=
                    Get_Imported_Entity (Get_Entity (It));
               begin
                  if Imported /= Null_Imported_Entity then
                     Put_Line
                       ("---> " & Get_Name (Imported)
                        & " [" & Get_Convention (Imported) & "]");
                  else
                     Put_Line ("---> <no import>");
                  end if;
               end;
            else
               Put_Line ("---> <entity not found>");
            end if;
         end;

      elsif Buffer (Word_Begin .. Word_End) = "DUMP_TREE" then
         Put_Line ("DUMP_TREE");

         declare
            Tree : constant Construct_Tree := Get_Tree (File);

            procedure Iterate
              (Indent : String; First : Construct_Tree_Iterator);

            -------------
            -- Iterate --
            -------------

            procedure Iterate
              (Indent : String; First : Construct_Tree_Iterator)
            is
               It : Construct_Tree_Iterator := First;
            begin
               while It /= Null_Construct_Tree_Iterator
                 and then Get_Parent_Scope (Tree, It)
                 = Get_Parent_Scope (Tree, First)
               loop
                  Put (Indent);

                  Put (Get_Construct (It).Category'Img);

                  if Get_Construct (It).Name /= No_Symbol then
                     Put (" - " & Get (Get_Construct (It).Name).all);
                  end if;

                  New_Line;

                  if Has_Children (It) then
                     Put (Indent & "[");
                     New_Line;
                     Iterate (Indent & "   ", Next (Tree, It, Jump_Into));
                     Put (Indent & "]");
                     New_Line;
                  end if;

                  It := Next (Tree, It, Jump_Over);
               end loop;
            end Iterate;

         begin
            Iterate ("", First (Tree));
         end;

      elsif Buffer (Word_Begin .. Word_End) = "ECHO" then
         declare
            Start_Text : constant Integer := Index;
            End_Text   : Integer := Start_Text;
         begin
            if Buffer (End_Text) /= ASCII.LF then
               while Buffer (End_Text + 1) /= ASCII.LF loop
                  End_Text := End_Text + 1;
               end loop;

               Put_Line (Buffer (Start_Text .. End_Text));
            else
               New_Line;
            end if;
         end;
      else
         Put_Line ("UNKOWN COMMAND " & Buffer (Word_Begin .. Word_End));
      end if;

   exception
      when E : others =>
         Put_Line ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
         Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   end Next_Test_Command;

   --------------------
   -- Read_Next_Word --
   --------------------

   procedure Read_Next_Word
     (Buffer     : access String;
      Index      : in out Natural;
      Word_Begin : out Natural;
      Word_End   : out Natural) is
   begin
      Word_Begin := Index;
      Skip_Blanks (Buffer.all, Word_Begin);
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

   ------------------
   -- Analyze_File --
   ------------------

   procedure Analyze_File (File : GNATCOLL.VFS.Virtual_File) is
      Index : Natural;
      File_Node : Structured_File_Access;
   begin
      Index := 1;

      File_Node := Get_Or_Create (Construct_Db, File);

      while Index /= 0 loop
         Next_Test_Command
           (Buffer => Get_Buffer (File_Node),
            File   => File_Node,
            Index  => Index);
      end loop;
   end Analyze_File;

   procedure Project_Error (Msg : String);

   -------------------
   -- Project_Error --
   -------------------

   procedure Project_Error (Msg : String) is
   begin
      Put ("ada_semantic_tree: Error loading project: " & Msg);
   end Project_Error;

begin
   GNATCOLL.Traces.Parse_Config_File;
   Set_Symbols (Construct_Db, Symbols);
   Set_Symbols (Ada_Lang, Symbols);

   Tree.Load
     (Root_Project_Path => Create_From_Dir (Get_Current_Dir, +Argument (1)),
      Errors            => Project_Error'Unrestricted_Access);

   Initialize (Construct_Db, new Ada_Language_Handler);
   Set_Provider (Construct_Db, new File_Buffer_Provider);

   Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
     (Construct_Db, GNATCOLL.VFS.No_File);

   declare
      Files     : constant GNATCOLL.VFS.File_Array_Access :=
                    Tree.Root_Project.Source_Files (True);
      File_Node : Structured_File_Access;

      pragma Unreferenced (File_Node);
   begin
      Sort (Files.all);

      --  First, generate the whole database
      Clear (Construct_Db);
      for J in Files.all'Range loop
         File_Node := Get_Or_Create (Construct_Db, Files.all (J));
      end loop;

      --  Then, execute the tests

      if Argument_Count > 1 then
         for J in 2 .. Argument_Count loop
            Analyze_File (Tree.Create (+Argument (J)));
         end loop;
      else
         for J in Files.all'Range loop
            Analyze_File (Files.all (J));
         end loop;
      end if;
   end;

   Destroy (Construct_Db);
exception
   when E : others =>
      Put_Line ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Ada_Semantic_Tree.Test_Driver;
