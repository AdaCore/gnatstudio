------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011, AdaCore                          --
--                                                                          --
-- GPS is free software;  you can  redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
------------------------------------------------------------------------------

with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;      use GNATCOLL.VFS_Utils;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure GNATSpark is

   type Action_Kind is
     (Examiner, MetaExaminer, Pogs, Simplifier, SPARKMake, SPARKFormat,
      SPARKSimp, ZombieScope, ViCToR);

   Tool  : Unbounded_String;
   --  Action requested (i.e. spark tool to run), verbatim (case sensitive).
   --  See Action_Kind above for possible spark tools.

   Action  : Action_Kind;
   --  Same as Tool, in enum form

   Project : Unbounded_String;
   --  GNAT Project file

   File : Virtual_File;
   --  The file to be processed, if any

   procedure Append (List : in out String_List_Access; S : String);
   --  Append S to List.
   --  List should never be null

   procedure Append (List : in out String_List_Access; S : String_List_Access);
   --  Append S to List
   --  List and S should never be null

   function Contains
     (Switches : GNAT.Strings.String_List;
      Str      : String) return Boolean;
   --  Return True if some Switches (x) starts with Str

   procedure Parse_Command_Line;
   --  Parse command line arguments

   procedure Usage;
   --  Display command line usage and terminate the application

   procedure Parse_Project (Project : String; Tree : out Project_Tree);
   --  Parse project file into tree

   function Run
     (Command : String; Switches : String_List_Access) return Integer;
   --  Spawn command <switches> and display the command line on stdout
   --  Return exit code of command.

   function Tool_Name (Action : Action_Kind) return String;
   --  Return the name of the executable corresponding to action

   ------------
   -- Append --
   ------------

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (String_List, String_List_Access);

   procedure Append (List : in out String_List_Access; S : String) is
      Result : String_List_Access;
   begin
      Result := new String_List (1 .. List'Length + 1);
      Result (1 .. List'Length) := List.all;
      Result (Result'Last) := new String'(S);
      Unchecked_Free (List);
      List := Result;
   end Append;

   procedure Append
     (List : in out String_List_Access; S : String_List_Access)
   is
      Result : String_List_Access;
   begin
      Result := new String_List (1 .. List'Length + S'Length);
      Result (1 .. List'Length) := List.all;
      Result (List'Length + 1 .. Result'Last) := S.all;
      Unchecked_Free (List);
      List := Result;
   end Append;

   ---------------
   -- Tool_Name --
   ---------------

   function Tool_Name (Action : Action_Kind) return String is
   begin
      case Action is
         when Examiner | MetaExaminer => return "spark";
         when Simplifier => return "spadesimp";
         when others     => return To_Lower (Action_Kind'Image (Action));
      end case;
   end Tool_Name;

   -------------------
   -- Parse_Project --
   -------------------

   procedure Parse_Project (Project : String; Tree : out Project_Tree) is
      Proj_Env     : Project_Environment_Access;
      GNAT_Version : GNAT.Strings.String_Access;
   begin
      Initialize (Proj_Env);
      Set_Path_From_Gnatls (Proj_Env.all, "gnatls", GNAT_Version);
      Tree.Load (GNATCOLL.VFS.Create (Filesystem_String (Project)), Proj_Env);

   exception
      when others =>
         Put_Line ("cannot parse project file: " & Project);
         OS_Exit (2);
   end Parse_Project;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      Count : constant Natural := Argument_Count;
   begin
      if Count < 2 or else Count > 3 then
         Usage;
         return;
      end if;

      declare
         Str : constant String := Argument (1);
      begin
         if Str = "-h" or else Str = "-help" or else Str = "--help" then
            Usage;
            return;
         end if;

         Set_Unbounded_String (Tool, Str);
         Action := Action_Kind'Value (To_String (Tool));
      end;

      declare
         S : constant String := Argument (2);
      begin
         if S'Length > 2 and then S (1 .. 2) = "-P" then
            --  Strip -P from -Pproject
            Set_Unbounded_String (Project, S (3 .. S'Last));
         else
            Usage;
         end if;
      end;

      if Count = 3 then
         File := Create_From_Base (Filesystem_String (Argument (3)));
      end if;

   exception
      when others =>
         Usage;
   end Parse_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("usage: gnatspark <action> -P<project> [file]");
      New_Line;
      Put_Line ("action: spark tool to run, among:");
      Put_Line ("  examiner, metaexaminer, pogs, simplifier, sparkmake,");
      Put_Line ("  sparkformat, sparksimp, zombiescope, victor");
      Put_Line ("project: project file (.gpr)");
      Put_Line ("file: optional file to be processed");
      OS_Exit (1);
   end Usage;

   ---------
   -- Run --
   ---------

   function Run
     (Command : String; Switches : String_List_Access) return Integer
   is
      Exec   : GNAT.OS_Lib.String_Access;
   begin
      Put (Command);

      for J in Switches'Range loop
         Put (" " & Switches (J).all);
      end loop;

      New_Line;
      Exec := Locate_Exec_On_Path (Command);

      if Exec = null then
         Put_Line (Command & " executable not found, exiting.");
         OS_Exit (3);
      end if;

      return Spawn (Exec.all, Switches.all);
   end Run;

   --------------
   -- Contains --
   --------------

   function Contains
     (Switches : GNAT.Strings.String_List;
      Str      : String) return Boolean is
   begin
      for J in Switches'Range loop
         declare
            S : String renames Switches (J).all;
         begin
            if S'Length >= Str'Length
              and then S (S'First .. S'First + Str'Length - 1) = Str
            then
               return True;
            end if;
         end;
      end loop;

      return False;
   end Contains;

   Tree     : Project_Tree;
   Switches : GNAT.Strings.String_List_Access;
   IDE_Switches : constant Attribute_Pkg_List :=
     Build ("IDE", "Default_Switches");
   Project_Path : Virtual_File;
   Status       : Integer;

begin
   Parse_Command_Line;
   Parse_Project (To_String (Project), Tree);

   Project_Path := Tree.Root_Project.Project_Path;
   Change_Dir (Project_Path.Dir_Name);

   if Action = MetaExaminer then
      --  Examiner and MetaExaminer share the same switches
      Switches := Tree.Root_Project.Attribute_Value
        (Attribute => IDE_Switches,
         Index     => "Examiner");
   else
      Switches := Tree.Root_Project.Attribute_Value
        (Attribute => IDE_Switches,
         Index     => To_String (Tool));
   end if;

   --  Ensure Switches is never null when calling Append

   if Switches = null then
      Switches := new String_List (1 .. 0);
   end if;

   case Action is
      when Examiner =>
         Append (Switches, "-brief");
         Append
           (Switches, "-ou=" & Tree.Root_Project.Object_Dir.Display_Full_Name);
         Append (Switches, File.Display_Full_Name);

      when MetaExaminer =>
         Append (Switches, "-brief");
         Append
           (Switches, "-ou=" & Tree.Root_Project.Object_Dir.Display_Full_Name);
         Append (Switches, "@" & File.Display_Full_Name);

      when Pogs =>
         --  Do not append -o if already specified

         if not Contains (Switches.all, "-o=") then
            Append
              (Switches,
               "-o=" &
               Tree.Root_Project.Object_Dir.Display_Dir_Name
               & Project_Path.Display_Base_Name (".gpr") & ".sum");
         end if;

      when Simplifier =>
         --  Spadesimp only supports analyzing file in the current directory,
         --  with a basename.
         Append (Switches, File.Display_Base_Name);
         Change_Dir (File.Dir_Name);

      when SPARKMake | SPARKFormat =>
         Append (Switches, File.Display_Full_Name);

      when SPARKSimp =>
         declare
            Simplifier_Switches : constant String_List_Access :=
              Tree.Root_Project.Attribute_Value
                (Attribute => IDE_Switches,
                 Index     => "Simplifier");
            Zombie_Switches : constant String_List_Access :=
              Tree.Root_Project.Attribute_Value
                (Attribute => IDE_Switches,
                 Index     => "ZombieScope");
            Victor_Switches : constant String_List_Access :=
              Tree.Root_Project.Attribute_Value
                (Attribute => IDE_Switches,
                 Index     => "ViCToR");

         begin
            if Simplifier_Switches /= null then
               Append (Switches, "-sargs");
               Append (Switches, Simplifier_Switches);
            end if;

            if Zombie_Switches /= null then
               Append (Switches, "-zargs");
               Append (Switches, Zombie_Switches);
            end if;

            if Victor_Switches /= null then
               Append (Switches, "-vargs");
               Append (Switches, Victor_Switches);
            end if;
         end;

         Change_Dir (Tree.Root_Project.Object_Dir);

      when ZombieScope =>
         Append (Switches, File.Display_Full_Name);

      when ViCToR =>
         declare
            S : constant Filesystem_String := File.Base_Name (".vcg");
         begin
            --  Victor only supports analyzing file in the current directory,
            --  with a basename.
            Change_Dir (File.Dir_Name);

            if S'Length > 4 and then S (S'Last - 3 .. S'Last) = ".siv" then
               Append (Switches, String (S (S'First .. S'Last - 4)));
            else
               Append (Switches, String (S));
            end if;
         end;
   end case;

   Status := Run (Tool_Name (Action), Switches);
   OS_Exit (Status);
end GNATSpark;
