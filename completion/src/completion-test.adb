-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Calendar;                  use Ada.Calendar;
with Ada.Directories;               use Ada.Directories;

with GNAT.OS_Lib;

with Glib.Unicode;                    use Glib.Unicode;

with Completion.Ada;                  use Completion.Ada;
with Completion.History;              use Completion.History;

with Completion.Ada.Constructs_Extractor;
use Completion.Ada.Constructs_Extractor;

with Language;               use Language;
with String_Utils;           use String_Utils;
with Language.Tree;          use Language.Tree;
with Language.Tree.Ada;      use Language.Tree.Ada;
with Language.Tree.Database; use Language.Tree.Database;
with Ada_Semantic_Tree.Expression_Parser;
use Ada_Semantic_Tree.Expression_Parser;
with Ada_Semantic_Tree.Assistants; use Ada_Semantic_Tree.Assistants;
with Projects;               use Projects;
with Projects.Registry;      use Projects.Registry;
with Entities;               use Entities;
with VFS;                    use VFS;

procedure Completion.Test is
   use Standard.Ada;
   use Token_List;

   Max_Accepted_Time_For_Creation : constant Duration := 0.1;
   --  Maximum time for the resolution, in seconds

   Max_Accepted_Time_For_Iteration : constant Duration := 0.1;
   --  Maximum time for the iteration, in seconds

   Max_Accepted_Time_For_Initialization : constant Duration := 0.1;
   --  Maximum time for the initialization, in seconds

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

      Start_Date  : Time;
      Time_Passed : Duration;
   begin
      Put_Line (" *** " & Name & " *** ");

      Start_Date := Clock;

      Iter := First (List);

      --  This first dummy loop checks that we can iterate over the elements of
      --  the list in a reasonable time.

      while not At_End (Iter) loop
         declare
            Proposal : constant Completion_Proposal'Class :=
              Get_Proposal (Iter);
         begin
            if Get_Completion (Proposal) = "***" then
               Put_Line ("Dummy test");
            end if;
         end;

         Next (Iter);
      end loop;

      Time_Passed := Clock - Start_Date;

      if Time_Passed > Max_Accepted_Time_For_Iteration then
         Put_Line ("Iteration on completion is too long: " &
                   Duration'Image (Time_Passed));
      end if;

      Iter := First (List);

      --  This loop displays the contents of the list.

      while not At_End (Iter) loop
         Text_IO.Put (Get_Completion (Get_Proposal (Iter)) & " (");
         Text_IO.Put
           (Language_Category'Image
             (Get_Category (Get_Proposal (Iter))) & ")");
         New_Line;

         Next (Iter);
      end loop;
   end Display;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File (File : Virtual_File) is
      Result      : Parsed_Expression;

      Buffer : constant String_Access := String_Access (Read_File (File));

      procedure Display (List  : Token_List.List);
      procedure Display (Token : Token_Record);

      procedure Display (List : Token_List.List) is
         Current : Token_List.List_Node := First (List);
      begin
         while Current /= Token_List.Null_Node loop
            Display (Data (Current));
            Current := Next (Current);
         end loop;
      end Display;

      procedure Display (Token : Token_Record) is
      begin
         Put (Token_Type'Image (Token.Tok_Type));

         case Token.Tok_Type is
            when Tok_Identifier =>
               Put (" ");
               Put (Buffer
                    (Token.Token_First ..  Token.Token_Last));
            when Tok_Expression =>
               Put (" (");
               Put (Natural'Image (Token.Token_First));
               Put (",");
               Put (Natural'Image (Token.Token_Last));
               Put (")");
            when others =>
               null;
         end case;

         New_Line;
      end Display;

   begin
      Result := Parse_Current_List
        (Ada_Semantic_Tree.Expression_Parser.UTF8_String_Access
           (Buffer), UTF8_Find_Prev_Char (Buffer.all, Buffer'Last));
      Display (Result.Tokens);
   end Parse_File;

   ---------------------------------
   -- Get_New_Construct_Extractor --
   ---------------------------------

   New_Registry : aliased Project_Registry;
   Construct_Db : Construct_Database_Access := new Construct_Database;
   Db           : Entities_Database;
   pragma Unreferenced (Db);

   function Get_New_Construct_Extractor
     (File : Virtual_File; Project : String) return Completion_Resolver_Access
   is
      procedure Project_Error (Msg : String);

      procedure Project_Error (Msg : String) is
      begin
         Text_IO.Put_Line ("Error loading project: " & Msg);
      end Project_Error;

      Loaded  : Boolean;
      Success : Boolean;

      Current_File : Structured_File_Access;

   begin
      Initialize (Construct_Db.all, new File_Buffer_Provider);

      Ada_Semantic_Tree.Assistants.Register_Ada_Assistants (Construct_Db);

      if Project /= "" then
         Db := Create (New_Registry'Unchecked_Access);

         Load
           (Registry           => New_Registry,
            Root_Project_Path  => Create_From_Dir (Get_Current_Dir, Project),
            Errors             => Project_Error'Unrestricted_Access,
            New_Project_Loaded => Loaded,
            Status             => Success);

         Recompute_View (New_Registry, Project_Error'Unrestricted_Access);

         declare
            Files : constant VFS.File_Array_Access :=
              Get_Source_Files (Get_Root_Project (New_Registry), True);
         begin
            for J in Files.all'Range loop
               declare
                  Dummy_File_Node : Structured_File_Access;
                  pragma Unreferenced (Dummy_File_Node);
               begin
                  Dummy_File_Node := Get_Or_Create
                    (Construct_Db,
                     Files.all (J),
                     Ada_Tree_Lang);
               end;
            end loop;
         end;
      end if;

      Current_File := Get_Or_Create
        (Construct_Db, File, Ada_Tree_Lang);

      return New_Construct_Completion_Resolver
        (Construct_Db,
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

      Start_Date  : Time;
      Time_Passed : Duration;

      Buffer : constant String_Access := String_Access (Read_File (File));

   begin
      Tag_Index := 1;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Result := Get_Initial_Completion_List
           (Manager => Manager,
            Context =>
              Create_Context (Manager, File, Buffer, End_Word));

         Time_Passed := Clock - Start_Date;

         if Time_Passed > Max_Accepted_Time_For_Creation then
            Text_IO.Put_Line
              ("Completion is too long: "
               & Duration'Image (Time_Passed)
               & " seconds.");
         end if;

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

      Start_Date  : Time;
      Time_Passed : Duration;
      Buffer      : constant String_Access := String_Access (Read_File (File));
   begin
      Tag_Index := 1;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Result := Get_Initial_Completion_List
           (Manager => Manager,
            Context => Create_Context (Manager, File, Buffer, End_Word));

         Time_Passed := Clock - Start_Date;

         if Time_Passed > Max_Accepted_Time_For_Creation then
            Text_IO.Put_Line
              ("Completion is too long: "
               & Duration'Image (Time_Passed)
               & " seconds.");
         end if;

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

      Start_Date  : Time;
      Time_Passed : Duration;
      Buffer      : constant String_Access := String_Access (Read_File (File));
   begin
      Tag_Index := 1;

      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer.all, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Get_Completion_Root
           (Resolver => Resolver,
            Offset   => End_Word,
            Context  => Create_Context (Manager, File, Buffer, End_Word),
            Result   => Result);

         Time_Passed := Clock - Start_Date;

         if Time_Passed > Max_Accepted_Time_For_Creation then
            Text_IO.Put_Line
              ("Completion is too long: "
               & Duration'Image (Time_Passed)
               & " seconds.");
         end if;

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

      Start_Date  : Time;
      Time_Passed : Duration;

      Buffer  : constant String_Access := String_Access (Read_File (File));

      History_Resolver : Completion_History_Access;

      Apply_Number : Integer := -1;
   begin
      Start_Date := Clock;

      Resolver := Get_New_Construct_Extractor (File, Project);

      Time_Passed := Clock - Start_Date;

      if Time_Passed > Max_Accepted_Time_For_Initialization then
         Text_IO.Put_Line
           ("Initialization is too long: "
            & Duration'Image (Time_Passed)
            & " seconds.");
      end if;

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

         Start_Date := Clock;

         Result := Get_Initial_Completion_List
           (Manager => Manager,
            Context => Create_Context (Manager, File, Buffer, End_Word));

         Time_Passed := Clock - Start_Date;

         if Time_Passed > Max_Accepted_Time_For_Creation then
            Text_IO.Put_Line
              ("Completion is too long: "
               & Duration'Image (Time_Passed)
               & " seconds.");
         end if;

         Display (Result, Buffer (Start_Word .. End_Word));

         if Apply_Number /= -1 then
            declare
               Iter : Completion_Iterator;
            begin
               Iter := First (Result);

               for J in 1 .. Apply_Number loop
                  Next (Iter);
               end loop;

               if Get_Proposal (Iter) in Storable_Proposal'Class then
                  Prepend_Proposal (History_Resolver, Get_Proposal (Iter));
               end if;
            end;
         end if;
      end loop;
   end Full_Test;

   Given_File : Virtual_File;

begin
   if Argument_Count < 2 then
      Put_Line ("Usage : <command> <file_name> <mode>");
      return;
   end if;

   Projects.Registry.Initialize;

   Given_File := Create_From_Base
     (Current_Directory & GNAT.OS_Lib.Directory_Separator & Argument (1));

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

   Free (Construct_Db);
   Projects.Registry.Finalize;

exception
   when E : others =>
      Put_Line ("Unexpected exception: " & Exception_Information (E));
end Completion.Test;
