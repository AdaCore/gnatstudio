------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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

with Ada.Calendar;                   use Ada.Calendar;
with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;

with GNAT.Directory_Operations;      use GNAT.Directory_Operations;
with GNAT.OS_Lib;                    use GNAT.OS_Lib;
with GNAT.Strings;                   use GNAT.Strings;
with GNAT.Traceback.Symbolic;        use GNAT.Traceback.Symbolic;
with GNAT.Command_Line;              use GNAT.Command_Line;

with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.Symbols;               use GNATCOLL.Symbols;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;
with GNATCOLL.VFS;                   use GNATCOLL.VFS;
with GNATCOLL.Xref;

with Ada_Semantic_Tree.Assistants;   use Ada_Semantic_Tree.Assistants;
with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;
with Language.C;                     use Language.C;
with Language.Tree.Database;         use Language.Tree.Database;
with Language.Tree;                  use Language.Tree;
with Language;                       use Language;
with Projects;                       use Projects;
with Language_Handlers;              use Language_Handlers;
with Language.Ada;                   use Language.Ada;
with Xref;                           use Xref;

procedure Ada_Semantic_Tree.Crash_Test is
   use type GNATCOLL.Xref.Visible_Column;

   Constructs_Heuristics : constant Trace_Handle :=
     Create ("Entities.Constructs", On);
   --  Must match name in xref.adb

   Project_Name   : GNAT.Strings.String_Access;
   Unique_Project : Boolean := False;
   Unique_File    : GNAT.Strings.String_Access;
   Verbose        : Boolean := False;

   Result_File : File_Type;

   Line_To_Test   : Integer := -1;
   Column_To_Test : Visible_Column_Type := -1;

   procedure Log (Str : String);

   procedure Log (Str : String) is
   begin
      if Verbose then
         Put (Result_File, Str);
      end if;
   end Log;

   procedure Project_Error (Msg : String);

   Tree         : constant Project_Tree_Access := new Project_Tree;
   New_Registry : Project_Registry_Access := Create (Tree);

   Db  : General_Xref_Database;

   procedure Read_Next_Word
     (Buffer     : access String;
      Index      : in out Natural;
      Word_Begin : out Natural;
      Word_End   : out Natural);

   procedure Analyze_File (File : Virtual_File);

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

      while Word_Begin < Buffer'Last
        and then not
          (Is_Alphanumeric (Buffer (Word_Begin))
           or else Buffer (Word_Begin) = '_')
      loop
         Word_Begin := Word_Begin + 1;
      end loop;

      Word_End := Word_Begin;

      Word_End := Word_End + 1;

      while Word_End <= Buffer'Last
        and then
          (Is_Alphanumeric (Buffer (Word_End))
           or else Buffer (Word_End) = '_')
      loop
         Word_End := Word_End + 1;
      end loop;

      Word_End := Word_End - 1;

      Index := Word_End + 1;
   end Read_Next_Word;

   ------------------
   -- Analyze_File --
   ------------------

   package F_IO is new Ada.Text_IO.Float_IO (Float);

   type Result_Type is record
      Match   : Integer := 0;
      Diff    : Integer := 0;
      Unknown : Integer := 0;
      Phantom : Integer := 0;
      Exc     : Integer := 0;
   end record;

   procedure Print (R : Result_Type; Verbose : Boolean := True);

   Global_Result : Result_Type;

   procedure Print (R : Result_Type; Verbose : Boolean := True) is
      Total : constant Integer :=
        R.Match + R.Diff + R.Unknown + R.Phantom + R.Exc;
   begin
      if Verbose then
         Put (" {MATCHES =" & R.Match'Img & ", ");
         Put ("DIFFS =" & R.Diff'Img & ", ");
         Put ("UNKNOWN =" & R.Unknown'Img & ", ");
         Put ("PHANTOM =" & R.Phantom'Img & ", ");
         Put ("EXCEPTIONS =" & R.Exc'Img & ", ");
         Put ("RESOLUTION = ");
         F_IO.Put
           (Float (R.Match) / Float (Total) * 100.0, Exp => 0, Aft => 2);
         Put (" %}");
      else
         F_IO.Put
           (Float (R.Match) / Float (Total) * 100.0, Exp => 0, Aft => 2);
         Put (" %");
      end if;
   end Print;

   procedure Analyze_File (File : Virtual_File) is
      Local_Result : Result_Type;
      File_Node : Structured_File_Access;
      Index     : Integer := 1;
      Buffer    : GNAT.Strings.String_Access;
      ALI_Entity  : General_Entity;

      Construct_Entity : Entity_Access;

      Line   : Integer;
      Column : Visible_Column_Type;

      function Equals
        (E_Location  : General_Location;
         C_File      : Structured_File_Access;
         C_Construct : Simple_Construct_Information) return Boolean;

      procedure Test_Loc (Index : in out Integer);

      function Equals
        (E_Location  : General_Location;
         C_File      : Structured_File_Access;
         C_Construct : Simple_Construct_Information) return Boolean
      is
         ALI_File : constant Virtual_File := E_Location.File;
      begin
         if ALI_File = No_File then
            return False;
         else
            return
              Get_File_Path (C_File) = ALI_File
              and then C_Construct.Sloc_Entity.Line = E_Location.Line
              and then C_Construct.Sloc_Entity.Column =
                Integer (E_Location.Column);
         end if;
      end Equals;

      procedure Log_Entity (Decl : General_Location);

      procedure Log_Entity (Decl : General_Location) is
         ALI_File : constant Virtual_File := Decl.File;
      begin
         Log ("E[");

         if Decl = No_Location then
            Log ("<no_location>");
         else
            if ALI_File /= No_File then
               Log (Decl.File.Display_Base_Name);
            else
               Log ("<predefined>");
            end if;

            Log (":" & Decl.Line'Img & ":" & Decl.Column'Img & "]");
         end if;
      end Log_Entity;

      procedure Test_Loc (Index : in out Integer) is
         Word_Begin, Word_End : Integer;
      begin
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         To_Line_Column
           (File                 => File_Node,
            Absolute_Byte_Offset => String_Index_Type (Word_Begin),
            Line                 => Line,
            Column               => Column);

         if Verbose then
            Put (Line'Img & ", " & Column'Img & ASCII.CR);
         end if;

         if Word_End > Buffer'Last then
            Word_End := Buffer'Last;
         end if;

         --  Disable constructs temporarily to make sure the information is
         --  from the ALI files
         Set_Active (Constructs_Heuristics, False);
         ALI_Entity := Db.Get_Entity
           (Loc  => (File, Line, Column),
            Name => Buffer (Word_Begin .. Word_End));
         Set_Active (Constructs_Heuristics, True);

         Construct_Entity := Ada_Tree_Lang.Find_Declaration
           (File   => File_Node,
            Line   => Line,
            Column => String_Index_Type (Column));

         if Buffer (Word_Begin .. Word_End) = "all" then
            --  Things that we don't care about:
            --
            --  all keyword - doesn't correspond to anything in the ALI

            null;
         elsif ALI_Entity = No_General_Entity
           and then Construct_Entity = Null_Entity_Access
         then
            null;
         elsif ALI_Entity = No_General_Entity
           and then Construct_Entity /= Null_Entity_Access
         then
            Local_Result.Phantom := Local_Result.Phantom + 1;

            Log
              ("[" & Buffer (Word_Begin .. Word_End) & "] "
               & String (Base_Name (File)) & ":" & Line'Img & ":"
               & Column'Img & ":E[null],C["
               & String
                 (Base_Name (Get_File_Path (Get_File (Construct_Entity))))
               & ":"
               & Get_Construct
                 (Construct_Entity).Sloc_Entity.Line'Img & ":"
               & Get_Construct
                 (Construct_Entity).Sloc_Entity.Column'Img & "]"
               & ASCII.LF);
         elsif Construct_Entity = Null_Entity_Access then
            Local_Result.Unknown := Local_Result.Unknown + 1;

            Log
              ("[" & Buffer (Word_Begin .. Word_End) & "] "
               & String (Base_Name (File)) & ":" & Line'Img & ":"
               & Column'Img & ":");
            Log_Entity (Db.Get_Declaration (ALI_Entity).Loc);
            Log (",C[null]");
            Log ((1 => ASCII.LF));
         else
            declare
               Construct : Simple_Construct_Information :=
                 Get_Construct (Construct_Entity).all;

               Current_Col   : Integer := Construct.Sloc_Entity.Column;
               Current_Index : Integer := Construct.Sloc_Entity.Index;

               C_Buffer : constant GNAT.Strings.String_Access :=
                 Get_Buffer (Get_File (Construct_Entity));
               E_Location : constant General_Location :=
                 Db.Get_Declaration (ALI_Entity).Loc;
            begin
               --  The ALI database and the Construct database don't agree
               --  where names are starting for composite identifers, so
               --  just adapt it (A.B will start on A for the construct,
               --  and B for the database.

               while Current_Index < C_Buffer'Last loop
                  if Is_Alphanumeric (C_Buffer (Current_Index))
                    or else C_Buffer (Current_Index) = '_'
                  then
                     null;
                  elsif C_Buffer (Current_Index) = '.' then
                     Construct.Sloc_Entity.Column := Current_Col + 1;
                  else
                     exit;
                  end if;

                  Current_Index := Current_Index + 1;
                  Current_Col := Current_Col + 1;
               end loop;

               if Equals
                 (E_Location, Get_File (Construct_Entity), Construct)
               then
                  Local_Result.Match := Local_Result.Match + 1;
               else
                  Local_Result.Diff := Local_Result.Diff + 1;

                  Log
                    ("[" & Buffer (Word_Begin .. Word_End) & "] "
                     & String (Base_Name (File)) & ":" & Line'Img & ":"
                     & Column'Img & ":");
                  Log_Entity (E_Location);
                  Log (",C["
                    & String
                      (Base_Name
                         (Get_File_Path (Get_File (Construct_Entity))))
                    & ":"
                    & Construct.Sloc_Entity.Line'Img & ":"
                    & Construct.Sloc_Entity.Column'Img & "]");
                  Log ((1 => ASCII.LF));
               end if;
            end;
         end if;
      exception
         when E : others =>
            Put_Line
              ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
            Put_Line
              (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
            Local_Result.Exc := Local_Result.Exc + 1;
      end Test_Loc;

   begin
      File_Node := Get_Or_Create (Db.Constructs, File);
      Buffer := Get_Buffer (File_Node);

      if Line_To_Test = -1 then
         while Index < Buffer'Last loop
            Test_Loc (Index);
         end loop;
      else
         Index := Integer
           (To_String_Index (File_Node, Line_To_Test, Column_To_Test));

         Test_Loc (Index);
      end if;

      if Verbose then
         Print (Local_Result);
         New_Line;
      end if;

      Global_Result.Match := Global_Result.Match + Local_Result.Match;
      Global_Result.Diff := Global_Result.Diff + Local_Result.Diff;
      Global_Result.Unknown := Global_Result.Unknown + Local_Result.Unknown;
      Global_Result.Phantom := Global_Result.Phantom + Local_Result.Phantom;
      Global_Result.Exc := Global_Result.Exc + Local_Result.Exc;
   end Analyze_File;

   -------------------
   -- Project_Error --
   -------------------

   procedure Project_Error (Msg : String) is
   begin
      Put ("ada_semantic_tree: Error loading project: " & Msg);
   end Project_Error;

   Max_Files : Integer := -1;
   File_To_Analyze : Virtual_File;
   Files_Analyzed : Integer := 0;
   Handler : Language_Handler;

   procedure Compute_Predefined_Paths;

   procedure Compute_Predefined_Paths is
      Gnatls       : constant String := "gnatls";
      Gnatls_Args  : Argument_List_Access :=
        Argument_String_To_List (Gnatls & " -v");
      GNAT_Version : GNAT.Strings.String_Access;
      pragma Unreferenced (GNAT_Version);

   begin
      Projects.Compute_Predefined_Paths
        (New_Registry, GNAT_Version, Gnatls_Args);

      GNAT.OS_Lib.Free (Gnatls_Args);
   end Compute_Predefined_Paths;

   Start  : constant Time := Clock;
   Symbols : constant Symbol_Table_Access := GNATCOLL.Symbols.Allocate;
begin
   GNATCOLL.Traces.Parse_Config_File;

   loop
      case Getopt ("P: n: f: u v") is
         when ASCII.NUL =>
            exit;

         when 'P' =>
            Project_Name := new String'(Parameter);

         when 'u' =>
            Unique_Project := True;

         when 'n' =>
            Max_Files := Integer'Value (Parameter);

         when 'f' =>
            declare
               Loc : constant String := Parameter;
               Last_Col : Integer := Loc'First;
            begin
               for J in Loc'Range loop
                  if Loc (J) = ':' then
                     if GNAT.Strings."=" (Unique_File, null) then
                        Unique_File := new String'(Loc (Loc'First .. J - 1));
                        Put_Line (Unique_File.all);
                        Last_Col := J;
                     else
                        Line_To_Test :=
                          Integer'Value (Loc (Last_Col + 1 .. J - 1));
                        Column_To_Test :=
                          Visible_Column_Type'Value (Loc (J + 1 .. Loc'Last));

                        exit;
                     end if;
                  end if;
               end loop;

               if GNAT.Strings."=" (Unique_File, null) then
                  Unique_File := new String'(Loc);
               end if;
            end;

         when 'v' =>
            Verbose := True;

         when others =>
            raise Program_Error;
      end case;
   end loop;

   Compute_Predefined_Paths;
   New_Registry.Tree.Load
     (Root_Project_Path  =>
        Create (+Normalize_Pathname
           (Project_Name.all, GNAT.Directory_Operations.Get_Current_Dir)),
      Env                => New_Registry.Environment,
      Errors             => Project_Error'Unrestricted_Access);

   New_Registry.Tree.Recompute_View (Project_Error'Unrestricted_Access);

   Language_Handlers.Create_Handler (Handler, Symbols);
   Db := new General_Xref_Database_Record;
   Db.Initialize
     (Lang_Handler => Handler,
      Symbols      => Symbols,
      Registry     => New_Registry,
      Subprogram_Ref_Is_Call => False);
   Set_Provider
     (Db.Constructs,
      new File_Buffer_Provider);

   Register_Language (Handler, C_Lang, null);
   New_Registry.Environment.Register_Default_Language_Extension
     ("c", ".h", ".c", ".o");

   Set_Registry (Handler, New_Registry);

   Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
     (Db.Constructs, GNATCOLL.VFS.No_File);

   Register_Language
     (Handler   => Handler,
      Lang      => Ada_Lang,
      Tree_Lang => Ada_Tree_Lang);

   if GNAT.Strings."/=" (Unique_File, null) then
      File_To_Analyze := New_Registry.Tree.Create (+Unique_File.all);
   end if;

   Ada.Text_IO.Create (Result_File, Out_File, "result.txt");

   declare
      Files     : constant GNATCOLL.VFS.File_Array_Access :=
        New_Registry.Tree.Root_Project.Source_Files (Recursive => True);
      Files_To_Analyze : constant GNATCOLL.VFS.File_Array_Access :=
        New_Registry.Tree.Root_Project.Source_Files
          (Recursive => not Unique_Project);
      Predef_File : constant GNATCOLL.VFS.File_Array :=
        New_Registry.Environment.Predefined_Source_Files;
      File_Node : Structured_File_Access;

      pragma Unreferenced (File_Node);
   begin
      --  First, generate the whole database
      Clear (Db.Constructs);

      for J in Predef_File'Range loop
         File_Node := Get_Or_Create (Db.Constructs, Predef_File (J));
      end loop;

      for J in Files.all'Range loop
         File_Node := Get_Or_Create (Db.Constructs, Files (J));
      end loop;

      if File_To_Analyze = No_File then
         for J in Files_To_Analyze.all'Range loop
            File_To_Analyze := Files_To_Analyze (J);

            if Verbose then
               Put
                 ("-------------------- "
                  & String (Base_Name (File_To_Analyze)));

               Put (J'Img & " /");

               if Max_Files = -1 then
                  Put (Files_To_Analyze.all'Last'Img);
               else
                  Put (Max_Files'Img);
               end if;

               New_Line;
            end if;

            if Handler.Get_Language_From_File (File_To_Analyze) = Ada_Lang then
               Analyze_File (File_To_Analyze);
               Files_Analyzed := Files_Analyzed + 1;
            end if;

            if Files_Analyzed = Max_Files then
               exit;
            end if;
         end loop;
      else
         if Verbose then
            Put
              ("-------------------- "
               & String (Base_Name (File_To_Analyze)));

            New_Line;
         end if;

         Analyze_File (File_To_Analyze);
      end if;
   end;

   if Verbose then
      New_Line;
      Put ("GLOBAL: ");
      Print (Global_Result, True);
      New_Line;
      Put ("TIME = ");
      F_IO.Put (Float (Clock - Start), Exp => 0, Aft => 2);
      Put_Line ("s");
   else
      Put ("time: ");
      F_IO.Put (Float (Clock - Start), Exp => 0, Aft => 2);
      Put (" s (accuracy: ");
      Print (Global_Result, False);
      Put_Line (")");
   end if;

   Destroy (Db.Constructs);
   Projects.Destroy (New_Registry);
   Close (Result_File);
exception
   when E : others =>
      Put_Line ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Close (Result_File);
end Ada_Semantic_Tree.Crash_Test;
