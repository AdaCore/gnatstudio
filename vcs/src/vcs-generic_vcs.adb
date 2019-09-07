------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Regpat;                  use GNAT.Regpat;
with GNATCOLL.Arg_Lists;           use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;            use GNATCOLL.Projects;
with GNATCOLL.Scripts;             use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Utils;       use GNATCOLL.Scripts.Utils;

with Basic_Types;                  use Basic_Types;
with Commands;                     use Commands;
with Commands.Custom;              use Commands.Custom;
with Commands.Interactive;         use Commands.Interactive;
with GPS.Customizable_Modules;     use GPS.Customizable_Modules;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Modules;           use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;           use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;      use GPS.Kernel.Task_Manager;
with String_Utils;                 use String_Utils;
with GNATCOLL.Traces;                       use GNATCOLL.Traces;
with VCS.Branching_Commands;       use VCS.Branching_Commands;
with VCS_Activities;               use VCS_Activities;
with VCS_Module;                   use VCS_Module;
with VCS_Status;                   use VCS_Status;
with VCS_View.Activities;          use VCS_View.Activities;
with VCS_View.Explorer;            use VCS_View.Explorer;
with XML_Utils;                    use XML_Utils;

package body VCS.Generic_VCS is

   --  A note about launching of commands: in this module, the launching of
   --  commands through the Launch_Background_Command interface should be
   --  done in general with Active => True for commands that execute something
   --  and with Active => False for commands that wait on an external process.

   use type GNAT.Strings.String_Access;

   Me : constant Trace_Handle := Create ("GPS.VCS.GENERIC_VCS");
   Revision_Handling_Queue : constant String := "Revision View";

   Max_Matches : constant := 64;
   --  The number of matches in each parsing loop

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type VCS_Generic_Module_ID_Record is new Module_ID_Record with null record;

   overriding procedure Customize
     (Module : access VCS_Generic_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   type VCS_Generic_Module_ID_Access is access
     all VCS_Generic_Module_ID_Record'Class;

   VCS_Generic_Module_Name : constant String := "Generic VCS connectivity";
   VCS_Generic_Module_ID   : VCS_Generic_Module_ID_Access;

   function Create_File_Context
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Selection_Context;
   --  Create a file context corresponding to File

   procedure Generic_Command
     (Ref        : access Generic_VCS_Record;
      Files      : File_Array;
      First_Args : GNAT.Strings.String_List_Access;
      Action     : VCS_Action;
      Show_Bar   : Boolean := True);
   --  Launch a generic command corresponding to Action.
   --  User must free Files.
   --  User must free First_Args.
   --  Action is the action that must be executed for every file,
   --  Dir_Action is the action that must be executed for every directory.
   --  Dir_Action only takes the name of the directory as parameter.

   procedure Generic_Dir_Command
     (Ref        : access Generic_VCS_Record;
      Dirs       : File_Array;
      First_Args : GNAT.Strings.String_List_Access;
      Dir_Action : VCS_Action;
      Show_Bar   : Boolean := True);
   --  Same as above, but work on dirs

   procedure Generic_Command
     (Ref        : access Generic_VCS_Record;
      File       : GNATCOLL.VFS.Virtual_File;
      First_Args : GNAT.Strings.String_List_Access;
      Action     : VCS_Action);
   --  Launch a generic command corresponding to Action

   function Lookup_Action
     (Kernel : Kernel_Handle;
      Action : GNAT.Strings.String_Access) return Action_Access;
   --  Wrapper for Lookup_Action

   procedure Generic_Parse_Status
     (Rep        : access Generic_VCS_Record;
      Parser     : Status_Parser_Record;
      Text       : String;
      Clear_Logs : Boolean;
      Dir        : String;
      Local      : Boolean);
   --  Parse the status for Text using Parser.
   --  See Parse_Status for description of the parameters.

   function Describe_Action
     (Ref    : access Generic_VCS_Record;
      Action : VCS_Action) return String;
   --  Return a description of Action

   -- Parser command --

   type Parser_Command_Type is new Root_Command with record
      Clear_Logs : Boolean;
      Text       : GNAT.Strings.String_Access;
      Start      : Integer;
      Prev_Start : Integer;
      Parser     : Status_Parser_Record;
      Status     : File_Status_List.Vector;
      Rep        : Generic_VCS_Access;
      Dir        : GNAT.Strings.String_Access;
      Is_Local   : Boolean;
   end record;

   --  ??? Need to implement destroy

   overriding function Execute
     (Command : access Parser_Command_Type) return Command_Return_Type;
   overriding function Name
     (Command : access Parser_Command_Type) return String;

   type Parser_Command_Access is access all Parser_Command_Type;

   overriding procedure Primitive_Free (Command : in out Parser_Command_Type);
   --  Free memory associated to Command

   --  Simple Hook command

   type Run_Hook_Command_Type is new Root_Command with record
      Kernel : Kernel_Handle;
      Hook   : access Simple_Hooks;   --  The hook to run
   end record;

   overriding function Execute
     (Command : access Run_Hook_Command_Type) return Command_Return_Type;

   ------------------------------
   -- Administrative_Directory --
   ------------------------------

   overriding function Administrative_Directory
     (Ref : access Generic_VCS_Record) return Filesystem_String is
   begin
      if Ref.Administrative_Dir = null then
         return "";
      else
         return Ref.Administrative_Dir.all;
      end if;
   end Administrative_Directory;

   ---------------------
   -- Create_From_VCS --
   ---------------------

   overriding function Create_From_VCS
     (Ref  : access Generic_VCS_Record;
      Name : String) return GNATCOLL.VFS.Virtual_File is
   begin
      if Ref.Absolute_Names and then Ref.Path_Style = Cygwin then
         --  Then make sure that the PATH is converted back from Cygwin if
         --  necessary.
         return Create
           (Format_Pathname
              (Filesystem_String (Name), Style => System_Default), Ref.Kernel);
      else
         return Create (Filesystem_String (Name), Ref.Kernel);
      end if;
   end Create_From_VCS;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Run_Hook_Command_Type) return Command_Return_Type is
   begin
      Command.Hook.Run (Command.Kernel);
      return Success;
   end Execute;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
     (Command : in out Parser_Command_Type) is
   begin
      GNAT.Strings.Free (Command.Text);
      GNAT.Strings.Free (Command.Dir);
      Command.Status.Clear;
   end Primitive_Free;

   ---------------------
   -- Describe_Action --
   ---------------------

   function Describe_Action
     (Ref    : access Generic_VCS_Record;
      Action : VCS_Action) return String is
   begin
      if Ref.Action_Labels (Action) = null then
         return -"VCS command";
      else
         return Ref.Action_Labels (Action).all;
      end if;
   end Describe_Action;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Kernel : Kernel_Handle;
      Action : GNAT.Strings.String_Access) return Action_Access is
   begin
      if Action = null then
         return null;
      else
         return Lookup_Action (Kernel, Action.all);
      end if;
   end Lookup_Action;

   -------------------------
   -- Create_File_Context --
   -------------------------

   function Create_File_Context
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Selection_Context
   is
      Context : Selection_Context := New_Context
        (Kernel, VCS_Generic_Module_ID);
   begin
      Set_File_Information (Context, Files => (1 => File));
      return Context;
   end Create_File_Context;

   ----------
   -- Free --
   ----------

   procedure Free (S : in out Status_Parser_Record) is
   begin
      Basic_Types.Unchecked_Free (S.Regexp);
      GNAT.Strings.Free (S.Pattern);
      S.Status_Identifiers.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Ref : in out Generic_VCS_Record) is

      procedure Free (A : in out Action_Array);
      procedure Free (S : in out Status_Array_Access);

      ----------
      -- Free --
      ----------

      procedure Free (A : in out Action_Array) is
      begin
         for J in A'Range loop
            GNAT.Strings.Free (A (J));
         end loop;
      end Free;

      procedure Free (S : in out Status_Array_Access) is
      begin
         for J in S'Range loop
            Free (S (J));
         end loop;
         Unchecked_Free (S);
      end Free;

   begin
      GNAT.Strings.Free (Ref.Id);
      Free (Ref.Administrative_Dir);
      Basic_Types.Unchecked_Free (Ref.Parent_Revision_Regexp);
      Basic_Types.Unchecked_Free (Ref.Branch_Root_Revision_Regexp);
      Free (Ref.Status);
      Free (Ref.Commands);
      Free (Ref.Status_Parser);
      Free (Ref.Local_Status_Parser);
      Free (Ref.Annotations_Parser);
      Free (Ref.Update_Parser);
      Free (Ref.Log_Parser);
      Free (Ref.Revision_Parser);

      Free (VCS_Record (Ref));
   end Free;

   ----------
   -- Name --
   ----------

   overriding function Name (Ref : access Generic_VCS_Record) return String is
   begin
      if Ref.Id /= null then
         return Ref.Id.all;
      else
         return "";
      end if;
   end Name;

   -------------------------
   -- Generic_Dir_Command --
   -------------------------

   procedure Generic_Dir_Command
     (Ref        : access Generic_VCS_Record;
      Dirs       : File_Array;
      First_Args : GNAT.Strings.String_List_Access;
      Dir_Action : VCS_Action;
      Show_Bar   : Boolean := True)
   is
      Kernel     : Kernel_Handle renames Ref.Kernel;
      The_Action : constant Action_Access :=
                     Lookup_Action (Kernel, Ref.Commands (Dir_Action));

      Custom     : Command_Access;
      Index      : Natural := 1;

      use type GNAT.Strings.String_List_Access;

   begin
      if The_Action = null then
         return;
      end if;

      for J in Dirs'Range loop
         declare
            Args : GNAT.Strings.String_List_Access;

         begin
            --  Args and Dir freed when the command proxy is destroyed

            if First_Args = null then
               Args := new GNAT.Strings.String_List (1 .. 1);
            else
               Args := new GNAT.Strings.String_List
                 (1 .. 1 + First_Args'Length);
            end if;

            Index := 1;

            if First_Args /= null then
               for J in First_Args'Range loop
                  Args (Index) := new String'(First_Args (J).all);
                  Index := Index + 1;
               end loop;
            end if;

            Custom := Create_Proxy
              (Get_Command (The_Action),
               (Event       => null,
                Context     => No_Context,
                Synchronous => False,
                Dir         => Dirs (J),
                Args        => Args,
                Via_Menu    => False,
                Label       => new String'(Describe_Action (Ref, Dir_Action)),
                Repeat_Count     => 1,
                Remaining_Repeat => 0));

            Launch_Background_Command
              (Kernel          => Kernel,
               Command         => Custom,
               Active          => False,
               Show_Bar        => Show_Bar,
               Queue_Id        => Ref.Id.all);
         end;
      end loop;
   end Generic_Dir_Command;

   ---------------------
   -- Generic_Command --
   ---------------------

   procedure Generic_Command
     (Ref        : access Generic_VCS_Record;
      Files      : File_Array;
      First_Args : GNAT.Strings.String_List_Access;
      Action     : VCS_Action;
      Show_Bar   : Boolean := True)
   is
      Kernel            : Kernel_Handle renames Ref.Kernel;

      The_Action        : constant Action_Access :=
                            Lookup_Action (Kernel, Ref.Commands (Action));

      File              : Natural := Files'First;
      Custom            : Command_Access;
      Index             : Natural := 1;
      First_Args_Length : Natural := 0;
      Dir               : Virtual_File;

      use type GNAT.Strings.String_List_Access;
   begin
      if The_Action = null or else Files'Length = 0 then
         return;
      end if;

      if First_Args /= null then
         First_Args_Length := First_Args'Length;
      end if;

      --  Handles files

      while File <= Files'Last loop
         declare
            Args : GNAT.Strings.String_List_Access;
            --  Set to True when the current working directory already part of
            --  the command line.
         begin
            --  Args, Dir and Dir_Args are freed when the command proxy is
            --  destroyed.

            Args := new GNAT.Strings.String_List
              (1 .. Files'Length + First_Args_Length);

            Index := 1;

            if First_Args /= null then
               for J in First_Args'Range loop
                  Args (Index) := new String'(First_Args (J).all);
                  Index := Index + 1;
               end loop;
            end if;

            if Ref.Absolute_Names then
               Dir := No_File;
            else
               Dir := Files (File).Get_Parent;
            end if;

            --  Group files by directories

            while File <= Files'Last loop
               exit when
                 (not Ref.Absolute_Names
                  and then Files (File).Get_Parent /= Dir)
                  or else Index - First_Args_Length >= Command_Line_Limit;

               if Ref.Absolute_Names then
                  --  The VCS works on absolute names
                  if Ref.Path_Style = System_Default then
                     Args (Index) := new String'(+Files (File).Full_Name);
                  else
                     Args (Index) := new String'
                       (+Format_Pathname
                          (Files (File).Full_Name, Ref.Path_Style));
                  end if;

               else
                  --  The VCS works on relative names
                  declare
                     Suffix : constant Filesystem_String :=
                                Files (File).Base_Name;
                  begin
                     if Suffix'Length = 0 then
                        --  Empty, we have a directory that is the base dir
                        Args (Index) := new String'(".");

                     elsif Ref.Path_Style = System_Default then
                        Args (Index) := new String'(+Suffix);
                     else
                        Args (Index) := new String'
                          (+Format_Pathname (Suffix, Ref.Path_Style));
                     end if;
                  end;
               end if;

               Index := Index + 1;
               File := File + 1;
            end loop;

            Custom := Create_Proxy
              (Get_Command (The_Action),
               (Event            => null,
                Context          => No_Context,
                Synchronous      => False,
                Dir              => Dir,
                Args             => Args,
                Label            => new String'(Describe_Action (Ref, Action)),
                Via_Menu         => False,
                Repeat_Count     => 1,
                Remaining_Repeat => 0));

            Launch_Background_Command
              (Kernel          => Kernel,
               Command         => Custom,
               Active          => False,
               Show_Bar        => Show_Bar,
               Queue_Id        => Ref.Id.all);
         end;
      end loop;
   end Generic_Command;

   procedure Generic_Command
     (Ref        : access Generic_VCS_Record;
      File       : GNATCOLL.VFS.Virtual_File;
      First_Args : GNAT.Strings.String_List_Access;
      Action     : VCS_Action)
   is
      Kernel     : Kernel_Handle renames Ref.Kernel;
      The_Action : constant Action_Access :=
                     Lookup_Action (Kernel, Ref.Commands (Action));
      Custom     : Command_Access;
      Args       : GNAT.Strings.String_List_Access;
      Dir        : Virtual_File;

      use GNAT.Strings;
      use type GNAT.Strings.String_List_Access;

   begin
      if The_Action = null then
         return;
      end if;

      Dir := Get_Parent (File);

      if First_Args /= null then
         Args := new GNAT.Strings.String_List (1 .. First_Args'Length + 1);

         for J in First_Args'Range loop
            Args (J - First_Args'First + 1) :=
              new String'(First_Args (J).all);
         end loop;

      else
         Args := new GNAT.Strings.String_List (1 .. 1);
      end if;

      if Ref.Absolute_Names then
         if Ref.Path_Style = System_Default then
            Args (Args'Last) :=
              new String'(+Full_Name (File));
         else
            Args (Args'Last) := new String'
              (+Format_Pathname (Full_Name (File),
               Ref.Path_Style));
         end if;

      else
         Args (Args'Last) := new String'(+Base_Name (File));
      end if;

      Custom := Create_Proxy
        (Get_Command (The_Action),
         (Event            => null,
          Context          => Create_File_Context (Kernel, File),
          Synchronous      => False,
          Dir              => Dir,
          Args             => Args,
          Via_Menu         => False,
          Label            => new String'(Describe_Action (Ref, Action)),
          Repeat_Count     => 1,
          Remaining_Repeat => 0));

      Launch_Background_Command
        (Kernel          => Kernel,
         Command         => Custom,
         Active          => False,
         Show_Bar        => True,
         Queue_Id        => Ref.Id.all);
   end Generic_Command;

   ----------------
   -- Get_Status --
   ----------------

   overriding procedure Get_Status
     (Rep        : access Generic_VCS_Record;
      Filenames  : File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      Args   : GNAT.Strings.String_List_Access;
      Sorted : File_Array := Filenames;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Clear_Logs'Img);

      if Rep.Current_Query_Files /= null then
         Unchecked_Free (Rep.Current_Query_Files);
      end if;

      Sort (Sorted);

      if Local then
         if Rep.Local_Status_Parser.File_Index = 0 then
            Rep.Current_Query_Files := new File_Array'(Sorted);
         end if;

         Generic_Command (Rep, Sorted, Args, Local_Status_Files, False);

      else
         if Rep.Status_Parser.File_Index = 0 then
            Rep.Current_Query_Files := new File_Array'(Sorted);
         end if;

         Generic_Command (Rep, Sorted, Args, Status_Files);
      end if;

      GNAT.Strings.Free (Args);
   end Get_Status;

   ---------------------
   -- Get_Status_Dirs --
   ---------------------

   overriding procedure Get_Status_Dirs
     (Rep        : access Generic_VCS_Record;
      Dirs       : File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Clear_Logs'Img);

      if Rep.Current_Query_Files /= null then
         Unchecked_Free (Rep.Current_Query_Files);
      end if;

      if Local then
         Generic_Dir_Command (Rep, Dirs, Args, Local_Status_Dir, False);
      else
         Generic_Dir_Command (Rep, Dirs, Args, Status_Dir);
      end if;
   end Get_Status_Dirs;

   -------------------------------
   -- Get_Status_Dirs_Recursive --
   -------------------------------

   overriding procedure Get_Status_Dirs_Recursive
     (Rep        : access Generic_VCS_Record;
      Dirs       : File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      Args  : GNAT.Strings.String_List_Access;

      Files : File_Array_Access := null;

      procedure Add_Directory_Recursively (D : Virtual_File);
      --  Add Dir and all subdirectories in Dir to Dirs, and make Node point
      --  to the next node.

      -------------------------------
      -- Add_Directory_Recursively --
      -------------------------------

      procedure Add_Directory_Recursively (D : Virtual_File) is
         List   : File_Array_Access;
         Tmp    : File_Array_Access;
         Length : Natural;
      begin
         List := D.Read_Dir (Dirs_Only);
         Length := List'Length;

         --  Filter the hidden directories
         for J in List'Range loop
            if Rep.Kernel.Is_Hidden (List (J)) then
               List (J) := No_File;
               Length := Length - 1;
            end if;
         end loop;

         --  Recreate the list whithout those hidden directories

         if Length < List'Length then
            Tmp := new File_Array (1 .. Length);
            Length := Tmp'First;

            for J in List'Range loop
               if List (J) /= No_File then
                  Tmp (Length) := List (J);
                  Length := Length + 1;
               end if;
            end loop;

            Unchecked_Free (List);
            List := Tmp;
         end if;

         --  Append to the global list
         --  Note that 'Files' is never null, as it has already been
         --  initialized from the initial list of directories.
         Tmp := new File_Array'(Files.all & List.all);
         Unchecked_Free (Files);
         Files := Tmp;

         --  And call recursively

         for L in List'Range loop
            Add_Directory_Recursively (List (L));
         end loop;

         Unchecked_Free (List);

      exception
         when VFS_Directory_Error =>
            null;
      end Add_Directory_Recursively;

   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Clear_Logs'Img);

      if Rep.Current_Query_Files /= null then
         Unchecked_Free (Rep.Current_Query_Files);
      end if;

      if Local then
         Generic_Dir_Command (Rep, Dirs, Args, Local_Status_Dir, False);
         --  ??? add recursive behavior for this?
      else
         declare
            The_Action : constant Action_Access :=
              Lookup_Action (Rep.Kernel, Rep.Commands (Status_Dir_Recursive));
         begin
            if The_Action = null then
               --  There is no action to perform on a hierarchy of directories:
               --  do the work manually.

               Files := new File_Array'(Dirs);

               for J in Dirs'Range loop
                  Add_Directory_Recursively (Dirs (J));
               end loop;

               Generic_Dir_Command (Rep, Files.all, Args, Status_Dir);
               Unchecked_Free (Files);
            else
               Generic_Dir_Command (Rep, Dirs, Args, Status_Dir_Recursive);
            end if;
         end;
      end if;
   end Get_Status_Dirs_Recursive;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   overriding function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array) return File_Status_List.Vector
   is
      pragma Unreferenced (Rep);

      Result           : File_Status_List.Vector;
      Blank_Status     : File_Status_Record;
      Current_Status   : File_Status_Record := Blank_Status;
   begin
      for J in Filenames'Range loop
         Current_Status := Blank_Status;
         Current_Status.File := Filenames (J);

         Result.Append (Current_Status);
      end loop;

      return Result;
   end Local_Get_Status;

   ----------------
   -- Create_Tag --
   ----------------

   overriding procedure Create_Tag
     (Rep       : access Generic_VCS_Record;
      Dir       : GNATCOLL.VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 2);
      Args (1) := new String'(+Base_Dir_Name (Dir)); -- root dir
      Args (2) := new String'(Tag);                 -- tag name

      if As_Branch then
         Generic_Dir_Command (Rep, (1 => Dir), Args, Create_Branch);
      else
         Generic_Dir_Command (Rep, (1 => Dir), Args, Create_Tag);
      end if;

      GNAT.Strings.Free (Args);
   end Create_Tag;

   ----------
   -- Open --
   ----------

   overriding procedure Open
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array;
      User_Name : String := "")
   is
      pragma Unreferenced (User_Name);
   begin
      Generic_Command (Rep, Filenames, null, Open);
   end Open;

   ------------
   -- Commit --
   ------------

   overriding procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array;
      Log       : String)
   is
      Args    : GNAT.Strings.String_List_Access;
      Escaped : String (Log'First .. Log'First + Log'Length * 2);
      Last    : Integer := Escaped'First;

   begin
      for J in Log'Range loop
         if Log (J) = '%' then
            Escaped (Last .. Last + 1) := "%%";
            Last := Last + 2;
         else
            Escaped (Last) := Log (J);
            Last := Last + 1;
         end if;
      end loop;

      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Escaped (Escaped'First .. Last - 1));

      Generic_Command (Rep, Filenames, Args, Commit);

      GNAT.Strings.Free (Args);
   end Commit;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array) is
   begin
      Generic_Command (Rep, Filenames, null, Update);
   end Update;

   ------------
   -- Switch --
   ------------

   overriding procedure Switch
     (Rep : access Generic_VCS_Record;
      Dir : GNATCOLL.VFS.Virtual_File;
      Tag : String)
   is
      Args     : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 2);
      Args (1) := new String'(+Base_Dir_Name (Dir)); -- root dir
      Args (2) := new String'(Tag);                 -- tag name

      Generic_Dir_Command (Rep, (1 => Dir), Args, Switch);

      GNAT.Strings.Free (Args);
   end Switch;

   --------------
   -- Resolved --
   --------------

   overriding procedure Resolved
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array) is
   begin
      Generic_Command (Rep, Filenames, null, Resolved);
   end Resolved;

   -----------
   -- Merge --
   -----------

   overriding procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array;
      Tag       : String)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Tag);                 -- tag name

      --  Iterate through all files, the merge is file oriented and some VCS
      --  won't accept multiple files to merge.

      for J in Filenames'Range loop
         Generic_Command (Rep, Filenames (J), Args, Merge);
      end loop;

      GNAT.Strings.Free (Args);
   end Merge;

   -------------------
   -- File_Revision --
   -------------------

   overriding procedure File_Revision
     (Rep      : access Generic_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Revision : String)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Revision);

      Generic_Command (Rep, File, Args, VCS.Revision);
   end File_Revision;

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array;
      Log       : String;
      Commit    : Boolean := True)
   is
      Args : GNAT.Strings.String_List_Access;

   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Log);

      if Commit then
         Generic_Command (Rep, Filenames, Args, Add);
      else
         Generic_Command (Rep, Filenames, Args, Add_No_Commit);
      end if;

      GNAT.Strings.Free (Args);
   end Add;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array;
      Log       : String;
      Commit    : Boolean := True)
   is
      Args : GNAT.Strings.String_List_Access;

   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Log);

      if Commit then
         Generic_Command (Rep, Filenames, Args, Remove);
      else
         Generic_Command (Rep, Filenames, Args, Remove_No_Commit);
      end if;

      GNAT.Strings.Free (Args);
   end Remove;

   ------------
   -- Revert --
   ------------

   overriding procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : File_Array) is
   begin
      Generic_Command (Rep, Filenames, null, Revert);
   end Revert;

   ----------
   -- Diff --
   ----------

   overriding procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : GNATCOLL.VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      Args : GNAT.Strings.String_List_Access;

   begin
      if Version_1 = "" and then Version_2 = "" then
         Generic_Command (Rep, File, null, Diff_Head);

      elsif Version_1 /= "" then
         if Version_2 = "" then
            Args := new GNAT.Strings.String_List (1 .. 1);
            Args (1) := new String'(Version_1);
            Generic_Command (Rep, File, Args, Diff);
         else
            Args := new GNAT.Strings.String_List (1 .. 2);
            Args (1) := new String'(Version_1);
            Args (2) := new String'(Version_2);
            Generic_Command (Rep, File, Args, Diff2);
         end if;

         GNAT.Strings.Free (Args);
      end if;
   end Diff;

   ----------------
   -- Diff_Patch --
   ----------------

   overriding procedure Diff_Patch
     (Rep    : access Generic_VCS_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Output : GNATCOLL.VFS.Virtual_File)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(+Full_Name (Output));

      Generic_Command (Rep, File, Args, Diff_Patch);
      GNAT.Strings.Free (Args);
   end Diff_Patch;

   --------------------
   -- Diff_Base_Head --
   --------------------

   overriding procedure Diff_Base_Head
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Generic_Command (Rep, File, null, Diff_Base_Head);
   end Diff_Base_Head;

   ------------------
   -- Diff_Working --
   ------------------

   overriding procedure Diff_Working
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Generic_Command (Rep, File, null, Diff_Working);
   end Diff_Working;

   --------------
   -- Diff_Tag --
   --------------

   overriding procedure Diff_Tag
     (Rep      : access Generic_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Tag_Name : String)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 2);
      Args (1) := new String'(Tag_Name);
      Args (2) := new String'(+Base_Name (File));

      Generic_Command (Rep, File, Args, Diff_Tag);

      GNAT.Strings.Free (Args);
   end Diff_Tag;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Rep     : access Generic_VCS_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True)
   is
      Args : GNAT.Strings.String_List_Access;

   begin
      if Rev /= "" then
         Args := new GNAT.Strings.String_List (1 .. 1);
         Args (1) := new String'(Rev);

         Generic_Command (Rep, File, Args, History_Revision);
         GNAT.Strings.Free (Args);

      else
         if As_Text then
            Generic_Command (Rep, File, Args, History_Text);
         else
            Generic_Command (Rep, File, Args, History);
         end if;
      end if;
   end Log;

   --------------
   -- Annotate --
   --------------

   overriding procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Generic_Command (Rep, File, null, Annotate);
   end Annotate;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access VCS_Generic_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);
      Kernel : constant Kernel_Handle := Get_Kernel (Module.all);

      function Parse_Node (M : Node_Ptr) return Boolean;
      --  Parse one node that contains VCS information

      ----------------
      -- Parse_Node --
      ----------------

      function Parse_Node (M : Node_Ptr) return Boolean is
         Name   : constant String := Get_Attribute (M, "name");
         Child  : constant Node_Ptr := M.Child;
         Ref    : Generic_VCS_Access;
         P      : Node_Ptr;
         Num    : Natural := 0;
         Node   : Node_Ptr;
         Value  : String_Ptr;

         function Parse_Status_Parser
           (N : Node_Ptr) return Status_Parser_Record;
         --  Parse and return a status matcher record

         function To_Natural (X : String) return Natural;
         --  Safe function to convert a string to a Natural

         function Parse_Regexp (N : Node_Ptr) return Pattern_Matcher_Access;
         --  Parse and return a Pattern_Matcher

         ----------------
         -- To_Natural --
         ----------------

         function To_Natural (X : String) return Natural is
         begin
            return Natural'Value (X);
         exception
            when others =>
               Insert (Kernel, -"Warning: numeric value expected");
               return 0;
         end To_Natural;

         -------------------------
         -- Parse_Status_Parser --
         -------------------------

         function Parse_Status_Parser
           (N : Node_Ptr) return Status_Parser_Record
         is
            procedure Set_Field
              (Parser : in out Status_Parser_Record;
               Value  : String_Ptr;
               Field  : in out Natural);
            --  Set field with the value supplied in Value

            ---------------
            -- Set_Field --
            ---------------

            procedure Set_Field
              (Parser : in out Status_Parser_Record;
               Value  : String_Ptr;
               Field  : in out Natural) is
            begin
               if Value /= null then
                  Field := To_Natural (Value.all);
                  Parser.Matches_Num := Natural'Max
                    (Parser.Matches_Num, Field);
               end if;
            end Set_Field;

            Parser : Status_Parser_Record;
            M      : Node_Ptr;

         begin
            if N = null then
               return Parser;
            end if;

            M := N.Child;

            while M /= null loop
               if M.Tag.all = "status_matcher" then
                  declare
                     Label  : constant String :=
                                Get_Attribute (M, "label", -"unnamed status");
                     Num    : Natural;
                     Regexp : Pattern_Matcher_Access;
                  begin
                     Num := 0;

                     --  Find the index of this status

                     for J in Ref.Status'Range loop
                        if Ref.Status (J).Label.all = Label then
                           Num := J;
                           exit;
                        end if;
                     end loop;

                     --  Store the regexp corresponding to this status

                     if M.Value = null then
                        Insert
                          (Kernel,
                           -"No regexp specified for status_matcher " & Label);
                        Regexp := new Pattern_Matcher'(Compile ("(.*)"));
                     else
                        Regexp := new Pattern_Matcher'(Compile (M.Value.all));
                     end if;

                     if Num /= 0 then
                        Parser.Status_Identifiers.Append
                          (Regexp_Status_Record'(Regexp, Num));
                     else
                        Basic_Types.Unchecked_Free (Regexp);
                     end if;

                  exception
                     when E : GNAT.Regpat.Expression_Error =>
                        Insert
                          (Kernel,
                           -"Error when registering VCS """ & Name & """:"
                           & ASCII.LF
                           & (-"could not parse regular expression: ")
                           & ASCII.LF & Value.all,
                           Mode => Error);
                        Trace (Me, E);
                  end;
               end if;

               M := M.Next;
            end loop;

            Value := Get_Field (N, "regexp");

            if Value /= null then
               declare
               begin
                  Parser.Regexp := new Pattern_Matcher'(Compile (Value.all));
               exception
                  when E : GNAT.Regpat.Expression_Error =>
                     Insert
                       (Kernel,
                        "Error when registering VCS """ & Name & """:"
                        & ASCII.LF & "could not parse regular expression: "
                        & ASCII.LF & Value.all,
                        Mode => Error);
                     Trace (Me, E);
               end;
            end if;

            pragma Warnings (Off);
            ---  Warning about writable actual for "Parser" overlaps
            ---  with actual for "Field" is harmless here

            Value := Get_Field (N, "file_index");
            Set_Field (Parser, Value, Parser.File_Index);

            Value := Get_Field (N, "status_index");
            Set_Field (Parser, Value, Parser.Status_Index);

            Value := Get_Field (N, "local_revision_index");
            Set_Field (Parser, Value, Parser.Local_Rev_Index);

            Value := Get_Field (N, "repository_revision_index");
            Set_Field (Parser, Value, Parser.Repository_Rev_Index);

            Value := Get_Field (N, "author_index");
            Set_Field (Parser, Value, Parser.Author_Index);

            Value := Get_Field (N, "date_index");
            Set_Field (Parser, Value, Parser.Date_Index);

            Value := Get_Field (N, "log_index");
            Set_Field (Parser, Value, Parser.Log_Index);

            Value := Get_Field (N, "sym_index");
            Set_Field (Parser, Value, Parser.Sym_Index);

            pragma Warnings (On);

            Value := Get_Field (N, "tooltip_pattern");

            if Value /= null then
               Parser.Pattern := new String'(Value.all);

               declare
                  K : Natural := Value'First;
               begin
                  loop
                     K := Index (Value (K .. Value'Last), "\");
                     exit when K = 0
                       or else K = Value'Last
                       or else Value (K + 1) not in '0' .. '9';
                     Parser.Matches_Num := Natural'Max
                       (Parser.Matches_Num,
                        Natural'Value ((1 => Value (K + 1))));
                     K := K + 2;
                  end loop;
               end;
            end if;

            return Parser;
         end Parse_Status_Parser;

         ------------------
         -- Parse_Regexp --
         ------------------

         function Parse_Regexp (N : Node_Ptr) return Pattern_Matcher_Access is
         begin
            if N = null then
               return null;
            end if;

            declare
               Regexp : constant String := Get_Attribute (N, "regexp", "");
            begin
               if Regexp = "" then
                  Insert
                    (Kernel,
                     -"No regexp specified for " & N.Tag.all);
                  return null;
               else
                  return new Pattern_Matcher'(Compile (Regexp));
               end if;
            end;
         end Parse_Regexp;

      begin
         if Name = "" then
            Trace (Me, "Error: name not specified");
            return False;
         end if;

         Trace (Me, "Generic VCS information found for " & Name);

         if Child = null then
            Trace (Me, "Error: xml children missing");
            return False;
         end if;

         Ref := new Generic_VCS_Record;
         Ref.Id := new String'(Name);

         Ref.Query_Status_By_Dir := Boolean'Value
           (Get_Attribute (M, "group_queries_by_directory", "FALSE"));
         Ref.Absolute_Names := Boolean'Value
           (Get_Attribute (M, "absolute_names", "FALSE"));
         Ref.Atomic_Commands := Boolean'Value
           (Get_Attribute (M, "atomic_commands", "FALSE"));
         Ref.Commit_Directory := Boolean'Value
           (Get_Attribute (M, "commit_directory", "FALSE"));

         for R in Revision_Type loop
            Ref.Default_Revisions (R) := To_Unbounded_String
              (Get_Attribute (M, To_Lower (R'Img) & "_revision", "n/a"));
         end loop;

         Ref.Administrative_Dir := new Filesystem_String'
           (+Get_Attribute (M, "administrative_directory", ""));
         --  ??? Potentially non-utf8 string should not be
         --  stored in an XML attribute.

         Ref.Require_Log := Boolean'Value
           (Get_Attribute (M, "require_log", "TRUE"));

         --  dir_sep is an alias for path_style and is obsolescent, if both
         --  path_style and dir_sep are set, parth_style value is used.

         if Get_Attribute (M, "path_style", "@") = "@" then
            Ref.Path_Style := OS_Utils.Path_Style'Value
              (Get_Attribute (M, "dir_sep", "System_Default"));
         else
            Ref.Path_Style := OS_Utils.Path_Style'Value
              (Get_Attribute (M, "path_style", "System_Default"));
         end if;

         Ref.Ignore_Filename := new Filesystem_String'
           (+Get_Attribute (M, "ignore_file", ""));
         --  ??? Potentially non-utf8 string should not be
         --  stored in an XML attribute.

         --  Find the command descriptions

         for A in VCS_Action loop
            Node := Find_Tag (Child, To_Lower (A'Img));

            if Node /= null then
               Ref.Commands (A) := new String'
                 (Get_Attribute (Node, "action", ""));

               Ref.Action_Labels (A) := new String'
                 (Get_Attribute (Node, "label", To_Lower (A'Img)));
               --  ??? Should we use better than To_Lower here ?
            else
               Trace (Me, "Warning: no command provided for action " & A'Img);
            end if;
         end loop;

         --  Find all the declared status

         P := Child;

         while P /= null loop
            if P.Tag.all = "status" then
               Num := Num + 1;
            end if;

            P := P.Next;
         end loop;

         Ref.Status := new Status_Array (1 .. Num);

         Num := 1;

         P := Child;

         while P /= null loop
            if P.Tag.all = "status" then
               declare
                  Icon : constant String := Get_Attribute (P, "iconname");
               begin
                  if Icon = "" then
                     --  Rollback to old 'stock' attribute to keep
                     --  compability for a while
                     Ref.Status (Num).Icon_Name := new String'
                       (Get_Attribute (P, "stock", "unknown-icon"));
                  else
                     Ref.Status (Num).Icon_Name := new String'(Icon);
                  end if;

                  Ref.Status (Num).Label := new String'
                    (Get_Attribute (P, "label", -"unnamed status"));

                  Num := Num + 1;
               end;
            end if;

            P := P.Next;
         end loop;

         --  Parse the status parsers

         Ref.Status_Parser := Parse_Status_Parser
           (Find_Tag (Child, "status_parser"));

         Ref.Local_Status_Parser := Parse_Status_Parser
           (Find_Tag (Child, "local_status_parser"));

         Ref.Update_Parser := Parse_Status_Parser
           (Find_Tag (Child, "update_parser"));

         --  Parse the annotations parser data

         Ref.Annotations_Parser := Parse_Status_Parser
           (Find_Tag (Child, "annotations_parser"));

         Ref.Log_Parser := Parse_Status_Parser
           (Find_Tag (Child, "log_parser"));

         Ref.Revision_Parser := Parse_Status_Parser
           (Find_Tag (Child, "revision_parser"));

         Ref.Parent_Revision_Regexp := Parse_Regexp
           (Find_Tag (Child, "parent_revision"));

         Ref.Branch_Root_Revision_Regexp := Parse_Regexp
           (Find_Tag (Child, "branch_root_revision"));

         Register_VCS (Name, VCS_Access (Ref));

         Ref.Kernel := Kernel;

         return True;
      end Parse_Node;

   begin
      if Node.Tag /= null
        and then To_Lower (Node.Tag.all) = "vcs"
      then
         if not Parse_Node (Node) then
            Trace (Me, "Could not parse generic VCS information");
         end if;
      end if;
   end Customize;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Parser_Command_Type) return Command_Return_Type
   is
      S             : String renames Command.Text.all;
      Matches       : Match_Array (0 .. Command.Parser.Matches_Num);
      Num_Matches   : Natural := 0;
      Status_Update : Boolean := False;

      use File_Status_List;
   begin
      if S'Last = 0 then
         --  Empty text, nothing to do, this happen when doing a diff on a
         --  file without modification for example.
         return Success;
      end if;

      Command.Prev_Start := Command.Start;

      while Num_Matches < Max_Matches loop
         Match (Command.Parser.Regexp.all, S, Matches, Command.Start, S'Last);

         if Matches (0) = No_Match then
            --  Update the status in the VCS Explorer

            Display_File_Status
              (Command.Rep.Kernel,
               Command.Status,
               VCS_Access (Command.Rep),
               Override_Cache => True,
               Force_Display  => True,
               Clear_Logs     => Command.Clear_Logs,
               Display        => True);

            --  Update also the status in the VCS Activities explorer

            Display_File_Status
              (Command.Rep.Kernel,
               No_Activity,
               Command.Status,
               VCS_Access (Command.Rep),
               Override_Cache => True,
               Force_Display  => True,
               Clear_Logs     => Command.Clear_Logs,
               Display        => True);

            --  Remove any error messages in the Locations view for all the
            --  files that have an 'Up to date' status.

            for R of Command.Status loop
               if R.Status = Up_To_Date then
                  declare
                     Msgs : constant Message_Array :=
                       GPS.Kernel.Messages.Get_Messages
                         (Self     => Get_Messages_Container
                              (Command.Rep.Kernel),
                          Category => To_Unbounded_String
                            (Command.Rep.Name & " errors"),
                          File     => R.File);
                  begin
                     for J in Msgs'Range loop
                        Msgs (J).Remove;
                     end loop;
                  end;
               end if;
            end loop;

            Status_Parsed_Hook.Run (Command.Rep.Kernel);

            return Success;
         end if;

         declare
            Filename : constant String :=
                         S (Matches (Command.Parser.File_Index).First
                            .. Matches (Command.Parser.File_Index).Last);
            St       : File_Status_Record;
         begin
            if Command.Parser.File_Index /= 0 then
               if Command.Dir = null or else Command.Dir.all = "" then
                  St.File := GPS.Kernel.Create
                    (+Filename, Command.Rep.Kernel, True, False);

               else
                  if Command.Rep.Absolute_Names
                    and then GNAT.OS_Lib.Is_Absolute_Path (Filename)
                  then
                     St.File := Create (+Filename);
                  else
                     St.File := Create (+(Command.Dir.all & Filename));
                  end if;
               end if;

               --  Initialize with the current status now

               declare
                  LR : constant Line_Record :=
                         Get_Cache (Get_Status_Cache, St.File);
               begin
                  if LR /= No_Data then
                     St := Copy_File_Status (LR.Status);
                  end if;
               end;

            elsif Command.Rep.Current_Query_Files /= null
              and then Command.Rep.Current_Query_Files'Length > 0
            then
               St.File := Command.Rep.Current_Query_Files
                 (Command.Rep.Current_Query_Files'First);

               if Command.Rep.Current_Query_Files'Length = 1 then
                  Unchecked_Free (Command.Rep.Current_Query_Files);
               else
                  declare
                     Tmp : File_Array_Access;
                  begin
                     Tmp := new File_Array'
                       (Command.Rep.Current_Query_Files
                         (Command.Rep.Current_Query_Files'First + 1 ..
                              Command.Rep.Current_Query_Files'Last));
                     Unchecked_Free (Command.Rep.Current_Query_Files);
                     Command.Rep.Current_Query_Files := Tmp;
                  end;
               end if;

            else
               --  ??? There should be an error message here
               return Failure;
            end if;

            if Command.Parser.Status_Index /= 0 then
               declare
                  Status_String : constant String := S
                    (Matches (Command.Parser.Status_Index).First
                     .. Matches (Command.Parser.Status_Index).Last);
                  Matches       : Match_Array (0 .. 1);

                  use Status_Parser;
               begin
                  for Item of Command.Parser.Status_Identifiers loop
                     Match (Item.Regexp.all, Status_String, Matches);

                     if Matches (0) /= No_Match then
                        declare
                           New_Status : constant VCS_File_Status :=
                             Command.Rep.Status (Item.Index);
                        begin
                           --  Do not update the status if current command is
                           --  local and we have recorded that a modification
                           --  is present on repository.
                           if Command.Is_Local
                             and then not Is_Local_Status (St.Status)
                             and then Is_Local_Status (New_Status)
                           then
                              Status_Update := False;
                           else
                              St.Status := New_Status;
                              Status_Update := True;
                           end if;
                        end;

                        exit;
                     end if;
                  end loop;
               end;
            end if;

            if Status_Update then
               if Command.Parser.Local_Rev_Index /= 0 then
                  Replace
                    (St.Working_Revision,
                     S (Matches (Command.Parser.Local_Rev_Index).First
                       .. Matches (Command.Parser.Local_Rev_Index).Last));
               else
                  Replace (St.Working_Revision, "n/a");
               end if;

               if Command.Parser.Repository_Rev_Index /= 0 then
                  Replace
                    (St.Repository_Revision,
                     S (Matches (Command.Parser.Repository_Rev_Index).First
                       .. Matches (Command.Parser.Repository_Rev_Index).Last));
               else
                  Replace (St.Repository_Revision, "n/a");
               end if;

               File_Status_List.Append (Command.Status, St);
            end if;
         end;

         Command.Start := Matches (0).Last + 1;

         --  Prevent infinite loop that could occur if users allow matches
         --  on empty strings.

         if Command.Prev_Start = Command.Start then
            return Success;
         end if;

         Num_Matches := Num_Matches + 1;
      end loop;

      return Execute_Again;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Parser_Command_Type) return String
   is
      pragma Unreferenced (Command);
   begin
      return -"Parsing status";
   end Name;

   --------------------------
   -- Generic_Parse_Status --
   --------------------------

   procedure Generic_Parse_Status
     (Rep        : access Generic_VCS_Record;
      Parser     : Status_Parser_Record;
      Text       : String;
      Clear_Logs : Boolean;
      Dir        : String;
      Local      : Boolean)
   is
      Command : Parser_Command_Access;
   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no status parser defined for " & Rep.Id.all);
         return;
      end if;

      Command := new Parser_Command_Type;
      Command.Parser     := Parser;
      Command.Text       := new String'(Text);
      Command.Start      := Text'First;
      Command.Prev_Start := Text'First - 1;
      Command.Clear_Logs := Clear_Logs;
      Command.Rep        := Generic_VCS_Access (Rep);
      Command.Dir        := new String'(Dir);
      Command.Is_Local   := Local;

      Launch_Background_Command
        (Rep.Kernel, Command_Access (Command), True, False, "");
   end Generic_Parse_Status;

   ------------------
   -- Parse_Status --
   ------------------

   overriding procedure Parse_Status
     (Rep        : access Generic_VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean;
      Dir        : String) is
   begin
      if Local then
         Generic_Parse_Status
           (Rep, Rep.Local_Status_Parser, Text, Clear_Logs, Dir, Local);
      else
         Generic_Parse_Status
           (Rep, Rep.Status_Parser, Text, Clear_Logs, Dir, Local);
      end if;
   end Parse_Status;

   ------------------
   -- Parse_Update --
   ------------------

   overriding procedure Parse_Update
     (Rep  : access Generic_VCS_Record;
      Text : String;
      Dir  : String)
   is
      Command : Parser_Command_Access;
   begin
      if Rep.Update_Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no update parser defined for " & Rep.Id.all);
         return;
      end if;

      Command := new Parser_Command_Type;
      Command.Parser     := Rep.Update_Parser;
      Command.Text       := new String'(Text);
      Command.Start      := Text'First;
      Command.Prev_Start := Text'First - 1;
      Command.Clear_Logs := False;
      Command.Rep        := Generic_VCS_Access (Rep);
      Command.Dir        := new String'(Dir);
      Command.Is_Local   := False;

      Launch_Background_Command
        (Rep.Kernel, Command_Access (Command), True, False, "");
   end Parse_Update;

   -----------------------
   -- Parse_Annotations --
   -----------------------

   overriding procedure Parse_Annotations
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String)
   is
      Kernel : Kernel_Handle renames Rep.Kernel;

      function Build_Text_Info (Rev, Author, Date : String) return String;
      --  Returns the text information

      Last_Rev : String (1 .. 10);
      --  Keep last revision to remove entries for same rev

      ---------------------
      -- Build_Text_Info --
      ---------------------

      function Build_Text_Info (Rev, Author, Date : String) return String is

         function Field (Str : String; Width : Natural := 0) return String;
         --  Returns ASCII.HT & Str if Str not empty, otherwise returns "". If
         --  Width is set the returned string will have exactly Width
         --  characters (padding to the left with spaces).

         -----------
         -- Field --
         -----------

         function Field (Str : String; Width : Natural := 0) return String is
         begin
            if Str = "" then
               return "";

            elsif Width = 0 then
               return Str;

            elsif Str'Length >= Width then
               return Str (Str'First .. Str'First + Width - 1);

            else
               return Str & (Width - Str'Length) * ' ';
            end if;
         end Field;

         N_Rev : constant String (1 .. 10) := ((10 - Rev'Length) * ' ') & Rev;

      begin
         if N_Rev = Last_Rev then
            return "             . . . ";

         else
            Last_Rev := N_Rev;
            return Date & ' ' & Field (Author, 10)
              & "<span size=""small"">" & "<span underline=""single"" "
              & "foreground=""blue"">"
              & Rev & "</span></span>";
         end if;
      end Build_Text_Info;

      Parser  : constant Status_Parser_Record := Rep.Annotations_Parser;
      Line    : Editable_Line_Type := 1;
      Max     : Editable_Line_Type;
      S       : String renames Text;
      Matches : Match_Array (0 .. Parser.Matches_Num);
      Start   : Integer := S'First;
      Script  : Scripting_Language;

   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no annotations parser defined for " & Rep.Id.all);
      end if;

      if Is_Open (Kernel, File) then
         Open_File_Action_Hook.Run
            (Kernel, File, Project => No_Project, Line => 0);
      else
         Open_File_Action_Hook.Run (Kernel, File, Project => No_Project);
      end if;

      Script := Kernel.Scripts.Lookup_Scripting_Language (GPS_Shell_Name);

      declare
         CL : Arg_List;
      begin
         CL := Create ("Editor.get_last_line");
         Append_Argument (CL, +Full_Name (File), One_Arg);
         declare
            Res : constant String := Execute_GPS_Shell_Command (Kernel, CL);
         begin
            Max := Editable_Line_Type'Value (Res);
         exception
            when others =>
               Trace
                 (Me, "Could not get last line of "
                  & Display_Full_Name (File) & ", result='" & Res & ''');
               return;
         end;
      end;

      declare
         use Ada.Strings.Unbounded;
         Max_Length : Integer := 0;
         A : Line_Information_Array (1 .. Max);
      begin
         loop
            Match (Parser.Regexp.all, S, Matches, Start, S'Last);

            exit when Matches (0) = No_Match or else Line > Max;

            if Parser.Repository_Rev_Index /= 0 then
               declare
                  Command : Custom_Command_Access;
                  Rev     : constant String :=
                             S (Matches (Parser.Repository_Rev_Index).First
                                .. Matches (Parser.Repository_Rev_Index).Last);
                  Author  : Unbounded_String;
                  Date    : Unbounded_String;
               begin
                  if Parser.Author_Index /= 0 then
                     Author := To_Unbounded_String
                       (S (Matches (Parser.Author_Index).First
                        .. Matches (Parser.Author_Index).Last));
                  end if;

                  if Parser.Date_Index /= 0 then
                     Date := To_Unbounded_String
                       (S (Matches (Parser.Date_Index).First
                        .. Matches (Parser.Date_Index).Last));
                  end if;

                  A (Line).Text := To_Unbounded_String
                    (Build_Text_Info
                       (Rev, To_String (Author), To_String (Date)));

                  Max_Length := Integer'Max
                    (Max_Length, Rev'Length + 12 + Length (Date));

                  if Parser.Pattern /= null then
                     declare
                        Str : Unbounded_String :=
                                To_Unbounded_String (Parser.Pattern.all);
                        K   : Natural;
                        N   : Natural;
                     begin
                        loop
                           K := Index (Str, "\");
                           exit when K = 0
                             or else K = Length (Str)
                             or else Element (Str, K + 1) not in '0' .. '9';

                           N := Natural'Value ((1 => Element (Str, K + 1)));

                           Replace_Slice
                             (Str, K, K + 1,
                              "<b>" &
                              S (Matches (N).First .. Matches (N).Last)
                              & "</b>");
                        end loop;
                        A (Line).Tooltip_Text := Str;
                     end;
                  end if;

                  Create
                    (Command, -"query log", Kernel,
                     "VCS.log "
                     & Argument_To_Quoted_String (+Full_Name (File))
                     & " """ & Rev & """",
                     Script);

                  A (Line).Associated_Command := Command_Access (Command);
               end;
            end if;

            Line  := Line + 1;
            Start := Matches (0).Last + 1;
         end loop;

         Create_Line_Information_Column
            (Kernel,
             File       => File,
             Identifier => Annotation_Id,
             Info       =>
               (Text               =>
                    To_Unbounded_String ((1 .. Max_Length => ' ')),
                Tooltip_Text       => Null_Unbounded_String,
                Image              => Null_Unbounded_String,
                Message            => <>,
                Associated_Command => null),
             Every_Line => False);
         Add_Line_Information
            (Kernel,
             File       => File,
             Identifier => Annotation_Id,
             Info       => A);
      end;

      Annotation_Parsed_Hook.Run (Kernel);
   end Parse_Annotations;

   ---------------
   -- Parse_Log --
   ---------------

   overriding procedure Parse_Log
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String)
   is
      use Ada.Strings.Unbounded;

      Kernel : Kernel_Handle renames Rep.Kernel;
      Script : Scripting_Language;

      function Parse
        (Rev : String; Regexp : Pattern_Matcher_Access)
         return Unbounded_String;
      --  Parse Rev using Regexp and return the first match

      procedure Create_Link (P_Rev, Rev : String);
      --  Create a link between P_Rev and Rev in the revision browser

      -----------------
      -- Create_Link --
      -----------------

      procedure Create_Link (P_Rev, Rev : String) is
         Command : Custom_Command_Access;
      begin
         Create
           (Command, -"add link", Kernel,
            "Revision.add_link " &
            Argument_To_Quoted_String (+Full_Name (File)) &
            " """ & P_Rev & """ """ & Rev & """", Script);

         Launch_Background_Command
           (Kernel, Command_Access (Command), True, False,
            Revision_Handling_Queue);
      end Create_Link;

      -----------
      -- Parse --
      -----------

      function Parse
        (Rev : String; Regexp : Pattern_Matcher_Access)
         return Unbounded_String
      is
         Result  : Unbounded_String;
         Matches : Match_Array (0 .. 1);
      begin
         if Regexp /= null then
            Match (Regexp.all, Rev, Matches);

            if Matches (0) /= No_Match then
               Result := To_Unbounded_String
                 (Rev (Matches (1).First .. Matches (1).Last));
            end if;
         end if;

         return Result;
      end Parse;

      Parser   : constant Status_Parser_Record := Rep.Log_Parser;
      S        : String renames Text;
      Matches  : Match_Array (0 .. Parser.Matches_Num);
      Start    : Integer := S'First;
      Parent   : Unbounded_String;
      Branch   : Unbounded_String;
      P_Branch : Unbounded_String;
      P_Rev    : Unbounded_String;
      First    : Boolean := True;
      Commands : Branching_Command;
   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no log parser defined for " & Rep.Id.all);
      end if;

      Script := Kernel.Scripts.Lookup_Scripting_Language (GPS_Shell_Name);

      loop
         Match (Parser.Regexp.all, S, Matches, Start, S'Last);

         exit when Matches (0) = No_Match;

         declare
            Rev     : constant String :=
                        S (Matches (Parser.Repository_Rev_Index).First
                           .. Matches (Parser.Repository_Rev_Index).Last);
            Author  : constant String :=
                        S (Matches (Parser.Author_Index).First
                           .. Matches (Parser.Author_Index).Last);
            Date    : constant String :=
                        S (Matches (Parser.Date_Index).First
                           .. Matches (Parser.Date_Index).Last);
            Log     : constant String :=
                        S (Matches (Parser.Log_Index).First
                           .. Matches (Parser.Log_Index).Last);
            Command : Custom_Command_Access;
         begin
            Create
              (Command, -"add log", Kernel,
               "Revision.add_log " &
               Argument_To_Quoted_String (+Full_Name (File)) &
               " """ & Rev & """ """ & Author & """ """ &
               Date & """ """ & String_Utils.Protect (Log) & """ """ &
               Boolean'Image (First) & """",
               Script);

            if Commands /= null then
               VCS.Branching_Commands.Add_Consequence_Action
                 (Commands, Command_Access (Command));
            else
               Commands := Create
                 (Kernel, Command_Access (Command), Name (Rep));
            end if;

            First := False;

            Branch := Parse (Rev, Rep.Branch_Root_Revision_Regexp);

            Parent := Parse (Rev, Rep.Parent_Revision_Regexp);

            --  Set previous depending on the branch information

            if Parent /= Null_Unbounded_String
              and then Branch /= Null_Unbounded_String
              and then P_Branch /= Branch
            then
               --  This node is a new branch, link to the corresponding parent
               Create_Link (Rev, To_String (Parent));

            elsif P_Rev /= Null_Unbounded_String then
               Create_Link (To_String (P_Rev), Rev);
            end if;

            P_Rev    := To_Unbounded_String (Rev);
            P_Branch := Branch;
         end;

         Start := Matches (0).Last + 1;
      end loop;

      declare
         C : constant Command_Access :=
               new Run_Hook_Command_Type'
                 (Root_Command with
                  Kernel => Rep.Kernel, Hook => Log_Parsed_Hook'Access);
      begin
         if Commands /= null then
            VCS.Branching_Commands.Add_Consequence_Action (Commands, C);
         end if;

         if Commands /= null then
            Launch_Background_Command
              (Kernel   => Rep.Kernel,
               Command  => Commands,
               Active   => True,
               Show_Bar => False,
               Queue_Id => Revision_Handling_Queue);
         end if;
      end;
   end Parse_Log;

   --------------------
   -- Parse_Revision --
   --------------------

   overriding procedure Parse_Revision
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String)
   is
      Kernel   : Kernel_Handle renames Rep.Kernel;
      Parser   : constant Status_Parser_Record := Rep.Revision_Parser;
      S        : String renames Text;
      Matches  : Match_Array (0 .. Parser.Matches_Num);
      Script   : Scripting_Language;
      Start    : Integer := S'First;
      Commands : Branching_Command;
   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no revision parser defined for " & Rep.Id.all);
      end if;

      Script := Kernel.Scripts.Lookup_Scripting_Language (GPS_Shell_Name);

      loop
         Match (Parser.Regexp.all, S, Matches, Start, S'Last);

         exit when Matches (0) = No_Match;

         declare
            Rev     : constant String :=
                        S (Matches (Parser.Repository_Rev_Index).First
                           .. Matches (Parser.Repository_Rev_Index).Last);
            Sym     : constant String :=
                        S (Matches (Parser.Sym_Index).First
                           .. Matches (Parser.Sym_Index).Last);
            Command : Custom_Command_Access;
         begin
            Create
              (Command, -"add revision", Kernel,
               "Revision.add_revision " &
               Argument_To_Quoted_String (+Full_Name (File)) &
               " """ & Rev & """ """ & Sym & """",
               Script);

            if Commands /= null then
               VCS.Branching_Commands.Add_Consequence_Action
                 (Commands, Command_Access (Command));
            else
               Commands := Create
                 (Kernel, Command_Access (Command), Name (Rep));
            end if;
         end;

         Start := Matches (0).Last + 1;
      end loop;

      declare
         C : constant Command_Access :=
               new Run_Hook_Command_Type'
                 (Root_Command with
                  Kernel => Rep.Kernel, Hook => Revision_Parsed_Hook'Access);
      begin
         if Commands /= null then
            VCS.Branching_Commands.Add_Consequence_Action (Commands, C);
         end if;

         if Commands /= null then
            Launch_Background_Command
              (Kernel   => Rep.Kernel,
               Command  => Commands,
               Active   => True,
               Show_Bar => False,
               Queue_Id => Revision_Handling_Queue);
         end if;
      end;
   end Parse_Revision;

   ---------------------------
   -- Get_Registered_Status --
   ---------------------------

   overriding function Get_Registered_Status
     (Rep : access Generic_VCS_Record) return Status_Array
   is
      Result : Status_Array (1 .. Rep.Status'Length + 1);
   begin
      Result (Result'First) := Unknown;
      Result (Result'First + 1 .. Result'First + Rep.Status'Length) :=
        Rep.Status.all;

      return Result;
   end Get_Registered_Status;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      VCS_Generic_Module_ID := new VCS_Generic_Module_ID_Record;
      Register_Module
        (Module      => Module_ID (VCS_Generic_Module_ID),
         Kernel      => Kernel,
         Module_Name => VCS_Generic_Module_Name,
         Priority    => Default_Priority);
   end Register_Module;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Regexp_Status_Record) is
   begin
      Basic_Types.Unchecked_Free (X.Regexp);
   end Free;

   --------------------------
   -- Get_Default_Revision --
   --------------------------

   overriding function Get_Default_Revision
     (Ref      : access Generic_VCS_Record;
      Revision : Revision_Type) return String is
   begin
      return To_String (Ref.Default_Revisions (Revision));
   end Get_Default_Revision;

end VCS.Generic_VCS;
