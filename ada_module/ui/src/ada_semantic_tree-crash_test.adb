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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;

with GNAT.Strings;                   use GNAT.Strings;

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

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Language.Ada; use Language.Ada;
with ALI_Parser; use ALI_Parser;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Projects.Registry.Queries;

procedure Ada_Semantic_Tree.Crash_Test is

   procedure Project_Error (Msg : String);

   New_Registry : aliased Project_Registry;

   Entities_Db  : Entities_Database;
   Construct_Db : constant Construct_Database_Access := new Construct_Database;

   procedure Read_Next_Word
     (Buffer     : access String;
      Index      : in out Natural;
      Word_Begin : out Natural;
      Word_End   : out Natural);

   Result_File : File_Type;

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
      Word_End := Word_Begin;

      Word_End := Word_End + 1;

      while Word_End <= Buffer'Last
        and then
          (Is_Alphanumeric (Buffer (Word_End))
           or else Buffer (Word_End) = '_'
           or else Buffer (Word_End) = '-')
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

   procedure Print (R : Result_Type);

   Global_Result : Result_Type;

   procedure Print (R : Result_Type) is
      Total : constant Integer :=
        R.Match + R.Diff + R.Unknown + R.Phantom + R.Exc;
   begin
      Put (" {MATCHES =" & R.Match'Img & ", ");
      Put ("DIFFS =" & R.Diff'Img & ", ");
      Put ("UNKNOWN =" & R.Unknown'Img & ", ");
      Put ("PHANTOM =" & R.Phantom'Img & ", ");
      Put ("EXCEPTIONS =" & R.Exc'Img & ", ");
      Put ("RESOLUTION = ");
      F_IO.Put (Float (R.Match) / Float (Total) * 100.0, Exp => 0);
      Put ("%}");
   end Print;

   procedure Analyze_File (File : Virtual_File) is
      Local_Result : Result_Type;
      File_Node : Structured_File_Access;
      Index     : Integer := 1;
      Buffer    : GNAT.Strings.String_Access;
      Word_Begin, Word_End : Integer;
      ALI_Entity       : Entity_Information;

      Construct_Entity : Entity_Access;
      Status    : Find_Decl_Or_Body_Query_Status;

      Line   : Integer;
      Column : Visible_Column_Type;

      function "="
        (Left : Entity_Information; Right : Entity_Access) return Boolean;

      function "="
        (Left : Entity_Information; Right : Entity_Access) return Boolean
      is
         ALI_Decl : File_Location;
         ALI_File : Source_File;
      begin
         ALI_Decl := Get_Declaration_Of (Left);
         ALI_File := Get_File (ALI_Decl);

         if ALI_File = null then
            return False;
         else
            return
              Get_File_Path (Get_File (Right))
              = Get_Filename (ALI_File)
              and then Get_Construct (Right).Sloc_Entity.Line
              = Get_Line (ALI_Decl)
              and then Get_Construct (Right).Sloc_Entity.Column
              = Integer (Get_Column (ALI_Decl));
         end if;
      end "=";

      procedure Put_Entity (E : Entity_Information);

      procedure Put_Entity (E : Entity_Information) is
         ALI_Decl : File_Location;
         ALI_File : Source_File;
      begin
         ALI_Decl := Get_Declaration_Of (E);
         ALI_File := Get_File (ALI_Decl);

         Put (Result_File, "E[");

         if ALI_File /= null then
            Put (Result_File,
                 String (Base_Name (Get_Filename (Get_File (ALI_Decl)))));
         else
            Put (Result_File, "<null>");
         end if;

         Put
           (Result_File, ":" & Get_Line (ALI_Decl)'Img
            & ":" & Get_Column (ALI_Decl)'Img & "]");
      end Put_Entity;
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

            Put (Line'Img & ", " & Column'Img & ASCII.CR);

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

            if Status /= Entities.Queries.Success
              and then Construct_Entity = Null_Entity_Access
            then
               null;
            elsif
              Status /= Entities.Queries.Success
              and then Construct_Entity /= Null_Entity_Access
            then
               Local_Result.Phantom := Local_Result.Phantom + 1;

               Put_Line
                 (Result_File,
                  +Base_Name (File) & ":" & Line'Img & ":"
                  & Column'Img & ":E[null],C["
                  & String
                    (Base_Name (Get_File_Path (Get_File (Construct_Entity))))
                  & ":"
                  & Get_Construct
                    (Construct_Entity).Sloc_Entity.Line'Img & ":"
                  & Get_Construct
                    (Construct_Entity).Sloc_Entity.Column'Img & "]");
            elsif Construct_Entity = Null_Entity_Access then
               Local_Result.Unknown := Local_Result.Unknown + 1;

               Put
                 (Result_File,
                  +Base_Name (File) & ":" & Line'Img & ":"
                  & Column'Img & ":");
               Put_Entity (ALI_Entity);
               Put (Result_File, ",C[null]");
               New_Line (Result_File);
            else
               if ALI_Entity = Construct_Entity then
                  Local_Result.Match := Local_Result.Match + 1;
               else
                  Local_Result.Diff := Local_Result.Diff + 1;

                  Put
                    (Result_File,
                     String (Base_Name (File)) & ":" & Line'Img & ":"
                     & Column'Img & ":");
                  Put_Entity (ALI_Entity);
                  Put (Result_File,
                       "C["
                    & String
                      (Base_Name
                         (Get_File_Path (Get_File (Construct_Entity)))) & ":"
                    & Get_Construct
                      (Construct_Entity).Sloc_Entity.Line'Img & ":"
                    & Get_Construct
                      (Construct_Entity).Sloc_Entity.Column'Img & "]");
                  New_Line (Result_File);
               end if;
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

      Print (Local_Result);
      New_Line;

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
begin
   Projects.Registry.Initialize;

   Entities_Db := Create (New_Registry'Unchecked_Access, Construct_Db);

   Load
     (Registry           => New_Registry,
      Root_Project_Path  => Create_From_Dir (Get_Current_Dir, +Argument (1)),
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

   Ada.Text_IO.Create (Result_File, Out_File, "result.txt");

   declare
      Lock : Construct_Heuristics_Lock :=
        Lock_Construct_Heuristics (Entities_Db);
      Files     : constant GNATCOLL.VFS.File_Array_Access :=
                    Get_Source_Files (Get_Root_Project (New_Registry), True);
      File_Node : Structured_File_Access;

      pragma Unreferenced (File_Node);
   begin
      --  First, generate the whole database
      Clear (Construct_Db);
      for J in Files.all'Range loop
         File_Node := Get_Or_Create (Construct_Db, Files.all (J));
      end loop;

      --  Then, execute the tests

      if Argument_Count > 1 then
         Max_Files := Integer'Value (Argument (2));
      end if;

      for J in Files.all'Range loop
         Put
           ("-------------------- "
            & String (Base_Name (Files.all (J))));

         Put (J'Img & " /");

         if Max_Files = -1 then
            Put (Files.all'Last'Img);
         else
            Put (Max_Files'Img);
         end if;

         New_Line;

         if Handler.Get_Language_From_File (Files.all (J)) = Ada_Lang then
            Analyze_File (Files.all (J));
            Files_Analyzed := Files_Analyzed + 1;
         end if;

         if Files_Analyzed = Max_Files then
            exit;
         end if;
      end loop;

      Lock.Unlock_Construct_Heuristics;
   end;

   Put ("GLOBAL: ");
   Print (Global_Result);
   New_Line;

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
