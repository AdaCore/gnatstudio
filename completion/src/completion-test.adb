-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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
with Ada.Strings.Unbounded;         use Ada.Strings;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Calendar;                  use Ada.Calendar;

with Completion.Ada;                  use Completion.Ada;
with Completion.Entities_Extractor;   use Completion.Entities_Extractor;
with Completion.Constructs_Extractor; use Completion.Constructs_Extractor;
with Completion.Expression_Parser;    use Completion.Expression_Parser;

with Language;          use Language;
with String_Utils;      use String_Utils;
with Ada_Analyzer;      use Ada_Analyzer;
with Case_Handling;     use Case_Handling;
with Language.Tree;     use Language.Tree;
with Language_Handlers; use Language_Handlers;
with Prj;               use Prj;
with Projects;          use Projects;
with Projects.Registry; use Projects.Registry;
with Entities;          use Entities;
with Entities.Queries;  use Entities.Queries;
with ALI_Parser;        use ALI_Parser;
with VFS;               use VFS;
with Language.Ada;      use Language.Ada;

procedure Completion.Test is
   use Standard.Ada;
   use Token_List;
   use type Strings.Unbounded.Unbounded_String;

   File   : File_Type;
   Buffer : Strings.Unbounded.Unbounded_String;

   Max_Accepted_Time_For_Creation : constant Duration := 0.1;
   --  Maximum time for the resolution, in seconds

   Max_Accepted_Time_For_Iteration : constant Duration := 0.1;
   --  Maximum time for the resolution, in seconds

   procedure Next_Complete_Tag
     (Buffer      : String;
      Index       : in out Natural;
      Word_Start  : out Natural;
      Word_End    : out Natural);

   procedure Display (List : Completion_List; Name : String);

   function Get_Construct_Completion_Resolver (Buffer : String)
     return Completion_Resolver_Access;

   function Get_Entity_Completion_Resolver (Buffer : String; Project : String)
     return Completion_Resolver_Access;

   procedure Parse_File (Buffer : String);
   procedure Extract_Constructs (Buffer : String);
   procedure Analyze_Proposal (Buffer : String);
   procedure Extract_Entities (Buffer : String; Project : String);
   procedure Full_Test (Buffer : String; Project : String);

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
            Proposal : Completion_Proposal'Class := Get_Proposal (Iter);
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

   procedure Parse_File (Buffer : String) is
      Result      : Token_List.List;

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
                    (Token.Token_Name_First ..  Token.Token_Name_Last));
            when Tok_Expression =>
               Put (" ");
               Put (Natural'Image (Token.Number_Of_Parameters));
            when others =>
               null;
         end case;

         New_Line;
      end Display;
   begin
      Result := Parse_Current_List (Buffer, Buffer'Last);
      Display (Result);
   end Parse_File;

   ---------------------------------------
   -- Get_Construct_Completion_Resolver --
   ---------------------------------------

   function Get_Construct_Completion_Resolver (Buffer : String)
     return Completion_Resolver_Access
   is
      Constructs  : constant Construct_List_Access := new Construct_List;
   begin
      Analyze_Ada_Source
        (Buffer          => Buffer,
         Indent_Params   => Default_Indent_Parameters,
         Format          => False,
         From            => 0,
         To              => 0,
         Replace         => null,
         Constructs      => Constructs,
         Callback        => null,
         Indent_Offset   => 0,
         Case_Exceptions => No_Casing_Exception);

      return new Construct_Completion_Resolver'
        (New_Construct_Completion_Resolver
           (new Construct_Tree'(To_Construct_Tree
            (Ada_Lang, Buffer, Constructs.all)),
            Create_From_Base (Argument (1))));
   end Get_Construct_Completion_Resolver;

   ------------------------------------
   -- Get_Entity_Completion_Resolver --
   ------------------------------------

   Registry : aliased Project_Registry;
   Db       : Entities_Database;
   ALI      : LI_Handler;

   function Get_Entity_Completion_Resolver (Buffer : String; Project : String)
     return Completion_Resolver_Access
   is
      Constructs  : constant Construct_List_Access := new Construct_List;

      -------------------------
      -- Create_Lang_Handler --
      -------------------------

      function Create_Lang_Handler return Language_Handler;

      function Create_Lang_Handler return Language_Handler is
         Handler : Language_Handler;
      begin
         Create_Handler (Handler);
         Prj.Initialize (Get_Tree (Get_Root_Project (Registry)));

         Register_Language_Handler (Db, Handler);

         ALI := Create_ALI_Handler (Db, Registry);
         Register_Language (Handler, Ada_Lang, ALI);
         Register_Default_Language_Extension (Registry, "ada", ".ads", ".adb");

--       CPP_LI := Create_CPP_Handler (Db, Registry);
--       Register_Language (Handler, C_Lang, CPP_LI);
--       Register_Default_Language_Extension (Registry, "c", ".h", ".c");
--
--       Register_Language (Handler, Cpp_Lang, CPP_LI);
--       Register_Default_Language_Extension (Registry, "c++", ".h", ".cc");

         Set_Registry (Handler, Registry'Unrestricted_Access);
         return Handler;
      end Create_Lang_Handler;

      procedure Project_Error (Msg : String);

      procedure Project_Error (Msg : String) is
      begin
         Text_IO.Put_Line ("Error loading project: " & Msg);
      end Project_Error;

      Loaded  : Boolean;
      Success : Boolean;

      Handler : Language_Handler;
   begin
      Analyze_Ada_Source
        (Buffer          => Buffer,
         Indent_Params   => Default_Indent_Parameters,
         Format          => False,
         From            => 0,
         To              => 0,
         Replace         => null,
         Constructs      => Constructs,
         Callback        => null,
         Indent_Offset   => 0,
         Case_Exceptions => No_Casing_Exception);

      Projects.Registry.Initialize;

      Db := Create (Registry'Unchecked_Access);

      Load
        (Registry           => Registry,
         Root_Project_Path  => Create_From_Dir (Get_Current_Dir, Project),
         Errors             => Project_Error'Unrestricted_Access,
         New_Project_Loaded => Loaded,
         Status             => Success);

      Recompute_View (Registry, Project_Error'Unrestricted_Access);

      Handler := Create_Lang_Handler;
      --  It's important to have called that before the update of the xrefs !

      declare
         Files : constant VFS.File_Array_Access :=
           Get_Source_Files (Get_Root_Project (Registry), True);
      begin
         for J in Files.all'Range loop
            Update_Xref (Get_Or_Create (Db, Files (J)));
         end loop;
      end;

      Compute_All_Call_Graphs (Db);

      return new Entity_Completion_Resolver'
        (New_Entity_Completion_Resolver
           (new Construct_Tree'(To_Construct_Tree
            (Ada_Lang, Buffer, Constructs.all)),
            Get_Root_Project (Registry),
            Handler));
   end Get_Entity_Completion_Resolver;

   ------------------------
   -- Extract_Constructs --
   ------------------------

   procedure Extract_Constructs (Buffer : String) is
      Result      : Completion_List;
      Tag_Index   : Natural;

      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : constant Completion_Resolver_Access :=
        Get_Construct_Completion_Resolver (Buffer);

      Start_Date  : Time;
      Time_Passed : Duration;
   begin
      Tag_Index := 1;

      Set_Buffer (Manager.all, new String'(Buffer));
      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Get_Possibilities
           (Resolver => Resolver,
            Identifier => Buffer (Start_Word .. End_Word),
            Is_Partial => True,
            Offset     => Start_Word - 1,
            Filter     => All_Visible_Entities,
            Result     => Result);

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

   procedure Analyze_Proposal (Buffer : String) is
      Result      : Completion_List;
      Tag_Index   : Natural;

      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : constant Completion_Resolver_Access :=
        Get_Construct_Completion_Resolver (Buffer);

      Start_Date  : Time;
      Time_Passed : Duration;
   begin
      Tag_Index := 1;

      Set_Buffer (Manager.all, new String'(Buffer));
      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Result := Get_Initial_Completion_List
           (Manager      => Manager.all,
            Start_Offset => End_Word);

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

   procedure Extract_Entities (Buffer : String; Project : String) is
      Result : Completion_List;

      Tag_Index   : Natural;

      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;
      Resolver : constant Completion_Resolver_Access :=
        Get_Entity_Completion_Resolver (Buffer, Project);

      Start_Date  : Time;
      Time_Passed : Duration;
   begin
      Tag_Index := 1;

      Set_Buffer (Manager.all, new String'(Buffer));
      Register_Resolver (Manager, Resolver);

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Get_Possibilities
           (Resolver   => Resolver,
            Identifier => Buffer (Start_Word .. End_Word),
            Is_Partial => True,
            Offset     => End_Word,
            Filter     => All_Visible_Entities,
            Result     => Result);

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

   procedure Full_Test (Buffer : String; Project : String) is
      Result      : Completion_List;

      Tag_Index   : Natural;

      Start_Word  : Natural;
      End_Word    : Natural;

      Manager  : constant Completion_Manager_Access :=
        new Ada_Completion_Manager;

      Start_Date  : Time;
      Time_Passed : Duration;
   begin
      Tag_Index := 1;

      Set_Buffer (Manager.all, new String'(Buffer));
      Register_Resolver
        (Manager, Get_Construct_Completion_Resolver (Buffer));
      Register_Resolver
        (Manager, Get_Entity_Completion_Resolver
         (Buffer, Project));

      while Tag_Index < Buffer'Last loop

         Next_Complete_Tag (Buffer, Tag_Index, Start_Word, End_Word);

         exit when Tag_Index = 0;

         Start_Date := Clock;

         Result := Get_Initial_Completion_List
           (Manager      => Manager.all,
            Start_Offset => End_Word);

         Time_Passed := Clock - Start_Date;

         if Time_Passed > Max_Accepted_Time_For_Creation then
            Text_IO.Put_Line
              ("Completion is too long: "
               & Duration'Image (Time_Passed)
               & " seconds.");
         end if;

         Display (Result, Buffer (Start_Word .. End_Word));
      end loop;
   end Full_Test;

begin

   if Argument_Count < 2 then
      Put_Line ("Usage : <command> <file_name> <mode>");
      return;
   end if;

   Open (File, In_File, Argument (1));

   while not End_Of_File (File) loop
      Strings.Unbounded.Append
        (Buffer,
         Strings.Unbounded.Text_IO.Get_Line (File) & ASCII.LF);
   end loop;

   Close (File);

   declare
      S : constant String := Strings.Unbounded.To_String (Buffer);
   begin
      if Argument (2) = "parse" then
         Parse_File (S);
      elsif Argument (2) = "construct" then
         Extract_Constructs (S);
      elsif Argument (2) = "analyze" then
         Analyze_Proposal (S);
      elsif Argument (2) = "entity" then
         if Argument_Count < 3 then
            Put_Line ("Usage : <command> <file_name> analyze <project_name>");
            return;
         end if;

         Extract_Entities (S, Argument (3));

      elsif Argument (2) = "full" then
         if Argument_Count < 3 then
            Put_Line ("Usage : <command> <file_name> full <project_name>");
            return;
         end if;

         Full_Test (S, Argument (3));
      end if;
   end;

   Flush;

exception
   when E : others =>
      Put_Line ("Unexpected exception: " & Exception_Information (E));

end Completion.Test;
