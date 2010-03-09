-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Ada.Calendar;                   use Ada.Calendar;
with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;

with GNAT.OS_Lib;                    use GNAT.OS_Lib;
with GNAT.Strings;                   use GNAT.Strings;
with GNAT.Traceback.Symbolic;        use GNAT.Traceback.Symbolic;
with GNAT.Command_Line;              use GNAT.Command_Line;

with GNATCOLL.VFS;                   use GNATCOLL.VFS;

with Ada_Semantic_Tree.Assistants;   use Ada_Semantic_Tree.Assistants;
with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;
with Entities;                       use Entities;
with Entities.Queries;               use Entities.Queries;
with Language.Tree.Database;         use Language.Tree.Database;
with Language.Tree;                  use Language.Tree;
with Language;                       use Language;
with Projects.Registry;              use Projects.Registry;
with Projects;                       use Projects;
with String_Utils;                   use String_Utils;
with Language_Handlers;              use Language_Handlers;
with Language.Ada;                   use Language.Ada;
with ALI_Parser;                     use ALI_Parser;
with Projects.Registry.Queries;

procedure Ada_Semantic_Tree.Crash_Test is

   Project_Name   : GNAT.Strings.String_Access;
   Unique_Project : Boolean := False;
   Unique_File    : GNAT.Strings.String_Access;
   Verbose        : Boolean := False;

   Result_File : File_Type;

   procedure Log (Str : String);

   procedure Log (Str : String) is
   begin
      if Verbose then
         Put (Result_File, Str);
      end if;
   end Log;

   procedure Project_Error (Msg : String);

   New_Registry : aliased Project_Registry;

   Entities_Db  : Entities_Database;
   Construct_Db : constant Construct_Database_Access := new Construct_Database;

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
      Word_Begin, Word_End : Integer;
      ALI_Entity  : Entity_Information;

      Construct_Entity : Entity_Access;
      Status    : Find_Decl_Or_Body_Query_Status;

      Line   : Integer;
      Column : Visible_Column_Type;

      function Equals
        (E_Location  : File_Location;
         C_File      : Structured_File_Access;
         C_Construct : Simple_Construct_Information) return Boolean;

      function Equals
        (E_Location  : File_Location;
         C_File      : Structured_File_Access;
         C_Construct : Simple_Construct_Information) return Boolean
      is
         ALI_File : Source_File;
      begin
         ALI_File := Get_File (E_Location);

         if ALI_File = null then
            return False;
         else
            return
              Get_File_Path (C_File)
              = Get_Filename (ALI_File)
              and then C_Construct.Sloc_Entity.Line
              = Get_Line (E_Location)
              and then C_Construct.Sloc_Entity.Column
              = Integer (Get_Column (E_Location));
         end if;
      end Equals;

      procedure Log_Entity (E : Entity_Information);

      procedure Log_Entity (E : Entity_Information) is
         ALI_Decl : File_Location;
         ALI_File : Source_File;
      begin
         ALI_Decl := Get_Declaration_Of (E);
         ALI_File := Get_File (ALI_Decl);

         Log ("E[");

         if ALI_File /= null then
            Log (String (Base_Name (Get_Filename (Get_File (ALI_Decl)))));
         else
            Log ("<null>");
         end if;

         Log
           (":" & Get_Line (ALI_Decl)'Img
            & ":" & Get_Column (ALI_Decl)'Img & "]");
      end Log_Entity;

   begin
      File_Node := Get_Or_Create (Construct_Db, File);
      Buffer := Get_Buffer (File_Node);

      while Index < Buffer'Last loop
         Read_Next_Word (Buffer, Index, Word_Begin, Word_End);

         begin
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

            Find_Declaration
              (Db              => Entities_Db,
               File_Name       => File,
               Entity_Name     => Buffer (Word_Begin .. Word_End),
               Line            => Line,
               Column          => Column,
               Entity          => ALI_Entity,
               Status          => Status,
               Check_Decl_Only => False);
            Construct_Entity := Ada_Tree_Lang.Find_Declaration
              (File   => File_Node,
               Line   => Line,
               Column => String_Index_Type (Column));

            if Buffer (Word_Begin .. Word_End) = "all" then
               --  Things that we don't care about:
               --
               --  all keyword - doesn't correspond to anything in the ALI

               null;
            elsif Status /= Entities.Queries.Success
              and then Construct_Entity = Null_Entity_Access
            then
               null;
            elsif
              Status /= Entities.Queries.Success
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
               Log_Entity (ALI_Entity);
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
                  E_Location : File_Location :=
                    Get_Declaration_Of (ALI_Entity);
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

                  if Construct.Category = Cat_Parameter then
                     --  Parameters are not linked in the construct database -
                     --  all parameter references go to the reference in the
                     --  body while the ali goes to the spec. If we are on a
                     --  parameter, adjust the entity and extract the body
                     --  instead of the spec.
                     Entities.Queries.Find_Next_Body
                       (Entity   => ALI_Entity,
                        Location => E_Location);
                  end if;

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
                     Log_Entity (ALI_Entity);
                     Log (",C["
                       & String
                         (Base_Name
                            (Get_File_Path (Get_File (Construct_Entity))))
                       & ":"
                       & Get_Construct
                         (Construct_Entity).Sloc_Entity.Line'Img & ":"
                       & Get_Construct
                         (Construct_Entity).Sloc_Entity.Column'Img & "]");
                     Log ((1 => ASCII.LF));
                  end if;
               end;
            end if;
         exception
            when E : others =>
               Put_Line
                 (Result_File,
                  "UNEXPECTED EXCEPTION: " & Exception_Information (E));
               Put_Line
                 (Result_File,
                  GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
               Local_Result.Exc := Local_Result.Exc + 1;
         end;
      end loop;

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
      Put_Line ("Error loading project: " & Msg);
   end Project_Error;

   Loaded, Success : Boolean;

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
      Projects.Registry.Queries.Compute_Predefined_Paths
        (New_Registry'Unchecked_Access,
         GNAT_Version,
         Gnatls_Args);

      GNAT.OS_Lib.Free (Gnatls_Args);
   end Compute_Predefined_Paths;

   Start  : constant Time := Clock;
begin
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
            Unique_File := new String'(Parameter);

         when 'v' =>
            Verbose := True;

         when others =>
            raise Program_Error;
      end case;
   end loop;

   Projects.Registry.Initialize;

   Entities_Db := Create (New_Registry'Unchecked_Access, Construct_Db);

   Load
     (Registry           => New_Registry,
      Root_Project_Path  =>
        Create_From_Dir (Get_Current_Dir, +Project_Name.all),
      Errors             => Project_Error'Unrestricted_Access,
      New_Project_Loaded => Loaded,
      Status             => Success);

   Compute_Predefined_Paths;
   Recompute_View (New_Registry, Project_Error'Unrestricted_Access);

   Language_Handlers.Create_Handler (Handler);

   Set_Registry
     (Handler, New_Registry'Unchecked_Access);

   Register_Language_Handler (Entities_Db, Handler);

   Initialize
     (Construct_Db,
      new File_Buffer_Provider,
      Abstract_Language_Handler (Handler));

   Ada_Semantic_Tree.Assistants.Register_Ada_Assistants
     (Construct_Db, GNATCOLL.VFS.No_File);

   Register_Language
     (Handler   => Handler,
      Lang      => Ada_Lang,
      Tree_Lang => Ada_Tree_Lang,
      LI        => ALI_Parser.Create_ALI_Handler
        (Entities_Db, New_Registry));

   if GNAT.Strings."/=" (Unique_File, null) then
      File_To_Analyze := Create
        (Filesystem_String (Unique_File.all), New_Registry);
   end if;

   Ada.Text_IO.Create (Result_File, Out_File, "result.txt");

   declare
      Lock : Construct_Heuristics_Lock :=
        Lock_Construct_Heuristics (Entities_Db);
      Files     : constant GNATCOLL.VFS.File_Array_Access :=
        Get_Source_Files (Get_Root_Project (New_Registry), True);
      Files_To_Analyze : constant GNATCOLL.VFS.File_Array_Access :=
        Get_Source_Files (Get_Root_Project (New_Registry), not Unique_Project);
      Predef_File : constant GNATCOLL.VFS.File_Array :=
        Get_Predefined_Source_Files (New_Registry);
      File_Node : Structured_File_Access;

      pragma Unreferenced (File_Node);
   begin
      --  First, generate the whole database
      Clear (Construct_Db);

      for J in Predef_File'Range loop
         File_Node := Get_Or_Create (Construct_Db, Predef_File (J));
      end loop;

      for J in Files.all'Range loop
         File_Node := Get_Or_Create (Construct_Db, Files (J));
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

      Lock.Unlock_Construct_Heuristics;
   end;

   if Verbose then
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

   Destroy (Construct_Db);
   Destroy (New_Registry);
   Projects.Registry.Finalize;
   Close (Result_File);
exception
   when E : others =>
      Put_Line ("UNEXPECTED EXCEPTION: " & Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Close (Result_File);
end Ada_Semantic_Tree.Crash_Test;
