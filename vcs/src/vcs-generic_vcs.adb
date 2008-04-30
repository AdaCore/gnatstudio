-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Commands.Custom;           use Commands.Custom;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Generic_List;
with Traces;                    use Traces;
with VCS_Activities;            use VCS_Activities;
with VCS_View.Activities;       use VCS_View.Activities;
with VCS_View.Explorer;         use VCS_View.Explorer;
with VCS_Module;                use VCS_Module;
with VCS_Status;                use VCS_Status;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;
with String_Utils;              use String_Utils;

package body VCS.Generic_VCS is

   use type GNAT.Strings.String_Access;

   Me : constant Debug_Handle := Create ("Generic_VCS");
   Revision_Handling_Queue : constant String := "Revision View";

   Max_Matches : constant := 64;
   --  The number of matches in each parsing loop

   ----------------------------
   -- Local type definitions --
   ----------------------------

   procedure Free (X : in out Generic_VCS_Access);
   --  Free memory associated to X

   package VCS_Info_List is new Generic_List (Generic_VCS_Access);

   type VCS_Generic_Module_ID_Record is new Module_ID_Record with record
      VCS_List : VCS_Info_List.List;
   end record;

   procedure Destroy (Id : in out VCS_Generic_Module_ID_Record);
   procedure Customize
     (Module : access VCS_Generic_Module_ID_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  See inherited documentation

   type VCS_Generic_Module_ID_Access is access
     all VCS_Generic_Module_ID_Record'Class;

   VCS_Generic_Module_Name : constant String := "Generic VCS connectivity";
   VCS_Generic_Module_ID   : VCS_Generic_Module_ID_Access;

   use String_List_Utils.String_List;

   function Identify_VCS (S : String) return VCS_Access;
   --  Utility function to identify the Generic VCS from a given string

   function Get_Info (Id : String) return Generic_VCS_Access;
   --  Return the information corresponding to Id

   function Create_File_Context
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Selection_Context;
   --  Create a file context corresponding to File

   procedure Generic_Command
     (Ref        : access Generic_VCS_Record;
      Files      : String_List.List;
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
      Dirs       : String_List.List;
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
      Action : GNAT.Strings.String_Access) return Action_Record_Access;
   --  Wrapper for Lookup_Action

   procedure Generic_Parse_Status
     (Rep            : access Generic_VCS_Record;
      Parser         : Status_Parser_Record;
      Text           : String;
      Override_Cache : Boolean;
      Clear_Logs     : Boolean;
      Dir            : String);
   --  Parse the status for Text using Parser.
   --  See Parse_Status for description of the parameters.

   function Describe_Action
     (Ref    : access Generic_VCS_Record;
      Action : VCS_Action) return String;
   --  Return a description of Action

   -- Parser command --

   type Parser_Command_Type is new Root_Command with record
      Override_Cache : Boolean;
      Clear_Logs     : Boolean;
      Text           : GNAT.Strings.String_Access;
      Start          : Integer;
      Prev_Start     : Integer;
      Parser         : Status_Parser_Record;
      Status         : File_Status_List.List;
      Rep            : Generic_VCS_Access;
      Dir            : GNAT.Strings.String_Access;
   end record;

   --  ??? Need to implement destroy

   function Execute
     (Command : access Parser_Command_Type) return Command_Return_Type;
   function Name (Command : access Parser_Command_Type) return String;

   type Parser_Command_Access is access all Parser_Command_Type;

   procedure Free (Command : in out Parser_Command_Type);
   --  Free memory associated to Command

   ----------
   -- Free --
   ----------

   procedure Free (Command : in out Parser_Command_Type) is
   begin
      GNAT.Strings.Free (Command.Text);
      GNAT.Strings.Free (Command.Dir);
      File_Status_List.Free (Command.Status);
   end Free;

   ---------------------
   -- Describe_Action --
   ---------------------

   function Describe_Action
     (Ref    : access Generic_VCS_Record;
      Action : VCS_Action) return String
   is
   begin
      if Ref.Labels (Action) = null then
         return -"VCS command";
      else
         return Ref.Labels (Action).all;
      end if;
   end Describe_Action;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Kernel : Kernel_Handle;
      Action : GNAT.Strings.String_Access) return Action_Record_Access is
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
      Context : Selection_Context := New_Context;
   begin
      Set_Context_Information
        (Context => Context,
         Kernel  => Kernel,
         Creator => Abstract_Module_ID (VCS_Generic_Module_ID));

      Set_File_Information (Context, Files => (1 => File));

      return Context;
   end Create_File_Context;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (Id : String) return Generic_VCS_Access is
      use VCS_Info_List;

      Node : VCS_Info_List.List_Node := VCS_Info_List.First
        (VCS_Generic_Module_ID.VCS_List);
   begin
      while Node /= VCS_Info_List.Null_Node loop
         if Data (Node).Id /= null
           and then To_Lower (Data (Node).Id.all) = To_Lower (Id)
         then
            return Data (Node);
         end if;

         Node := Next (Node);
      end loop;

      return null;
   end Get_Info;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Generic_VCS_Access) is
      pragma Warnings (Off, X);
   begin
      GNAT.Strings.Free (X.Id);

      for J in X.Commands'Range loop
         GNAT.Strings.Free (X.Commands (J));
      end loop;

      for J in X.Labels'Range loop
         GNAT.Strings.Free (X.Labels (J));
      end loop;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VCS_Generic_Module_ID_Record) is
      pragma Unreferenced (Id);
   begin
      VCS_Info_List.Free (VCS_Generic_Module_ID.VCS_List);
   end Destroy;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
   begin
      return VCS_Access (Get_Info (S));
   end Identify_VCS;

   ----------
   -- Name --
   ----------

   function Name (Ref : access Generic_VCS_Record) return String is
   begin
      if Ref.Id /= null then
         return Ref.Id.all;
      else
         return "";
      end if;
   end Name;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access Generic_VCS_Record) is
      pragma Unreferenced (Ref);
   begin
      null;
   end Free;

   -------------------------
   -- Generic_Dir_Command --
   -------------------------

   procedure Generic_Dir_Command
     (Ref        : access Generic_VCS_Record;
      Dirs       : String_List.List;
      First_Args : GNAT.Strings.String_List_Access;
      Dir_Action : VCS_Action;
      Show_Bar   : Boolean := True)
   is
      Kernel     : Kernel_Handle renames Ref.Kernel;
      The_Action : constant Action_Record_Access :=
                     Lookup_Action (Kernel, Ref.Commands (Dir_Action));

      Node       : List_Node;
      Custom     : Command_Access;
      Index      : Natural := 1;

      use type GNAT.Strings.String_List_Access;

   begin
      if The_Action = null then
         return;
      end if;

      Node := First (Dirs);

      while Node /= Null_Node loop
         declare
            Args : GNAT.Strings.String_List_Access;
            Dir  : GNAT.Strings.String_Access;

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

            Dir := new String'(Locale_From_UTF8 (Data (Node)));

            Custom := Create_Proxy
              (The_Action.Command,
               (Event       => null,
                Context     => No_Context,
                Synchronous => False,
                Dir         => Dir,
                Args        => Args,
                Label      => new String'(Describe_Action (Ref, Dir_Action)),
                Repeat_Count     => 1,
                Remaining_Repeat => 0));

            Launch_Background_Command
              (Kernel          => Kernel,
               Command         => Custom,
               Active          => False,
               Show_Bar        => Show_Bar,
               Queue_Id        => Ref.Id.all,
               Destroy_On_Exit => True);
         end;

         Node := Next (Node);
      end loop;
   end Generic_Dir_Command;

   ---------------------
   -- Generic_Command --
   ---------------------

   procedure Generic_Command
     (Ref        : access Generic_VCS_Record;
      Files      : String_List.List;
      First_Args : GNAT.Strings.String_List_Access;
      Action     : VCS_Action;
      Show_Bar   : Boolean := True)
   is
      Kernel            : Kernel_Handle renames Ref.Kernel;

      The_Action        : constant Action_Record_Access :=
                            Lookup_Action (Kernel, Ref.Commands (Action));
      Length            : constant Natural := String_List.Length (Files);

      Node              : List_Node;
      Custom            : Command_Access;
      Index             : Natural := 1;
      First_Args_Length : Natural := 0;
      Dir               : GNAT.Strings.String_Access;

      use type GNAT.Strings.String_List_Access;
   begin
      if The_Action = null or else Is_Empty (Files) then
         return;
      end if;

      if First_Args /= null then
         First_Args_Length := First_Args'Length;
      end if;

      --  Handles files

      Node := First (Files);

      while Node /= Null_Node loop
         declare
            Args : GNAT.Strings.String_List_Access;
            --  Set to True when the current working directory already part of
            --  the command line.
         begin
            --  Args, Dir and Dir_Args are freed when the command proxy is
            --  destroyed.

            Args := new GNAT.Strings.String_List
              (1 .. Length + First_Args_Length);

            Index := 1;

            if First_Args /= null then
               for J in First_Args'Range loop
                  Args (Index) := new String'(First_Args (J).all);
                  Index := Index + 1;
               end loop;
            end if;

            if Ref.Absolute_Names then
               Dir := null;
            else
               Dir := new String'(Dir_Name (Locale_From_UTF8 (Data (Node))));
            end if;

            while Node /= Null_Node loop
               exit when (not Ref.Absolute_Names
                          and then GNAT.Directory_Operations.Dir_Name
                            (Locale_From_UTF8 (Data (Node))) /= Dir.all)
                 or else Index - First_Args_Length >= Command_Line_Limit;

               declare
                  Filename : constant String := Data (Node);
               begin
                  if Ref.Absolute_Names then
                     --  The VCS works on absolute names
                     if Ref.Path_Style = System_Default then
                        Args (Index) := new String'(Filename);
                     else
                        Args (Index) := new String'
                          (Format_Pathname (Filename, Ref.Path_Style));
                     end if;

                  else
                     --  The VCS works on relative names
                     declare
                        Suffix : constant String := Filename
                          (Filename'First + Dir.all'Length .. Filename'Last);
                     begin
                        if Suffix = "" then
                           --  Empty, we have a directory that is the base dir
                           Args (Index) := new String'(".");

                        elsif Ref.Path_Style = System_Default then
                           Args (Index) := new String'(Suffix);
                        else
                           Args (Index) := new String'
                             (Format_Pathname (Suffix, Ref.Path_Style));
                        end if;
                     end;
                  end if;
               end;

               Index := Index + 1;
               Node := Next (Node);
            end loop;

            Custom := Create_Proxy
              (The_Action.Command,
               (Event            => null,
                Context          => No_Context,
                Synchronous      => False,
                Dir              => Dir,
                Args             => Args,
                Label            => new String'(Describe_Action (Ref, Action)),
                Repeat_Count     => 1,
                Remaining_Repeat => 0));

            Launch_Background_Command
              (Kernel          => Kernel,
               Command         => Custom,
               Active          => False,
               Show_Bar        => Show_Bar,
               Queue_Id        => Ref.Id.all,
               Destroy_On_Exit => True);
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
      The_Action : constant Action_Record_Access :=
                     Lookup_Action (Kernel, Ref.Commands (Action));
      Custom     : Command_Access;
      Args       : GNAT.Strings.String_List_Access;
      Dir        : GNAT.Strings.String_Access;

      use type GNAT.Strings.String_List_Access;
      use GNAT.Strings;

   begin
      if The_Action = null then
         return;
      end if;

      Dir := new String'(Locale_From_UTF8 (Dir_Name (File).all));

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
              new String'(Locale_From_UTF8 (Full_Name (File).all));
         else
            Args (Args'Last) := new String'
              (Format_Pathname
                 (Locale_From_UTF8 (Full_Name (File).all), Ref.Path_Style));
         end if;

      else
         Args (Args'Last) := new String'(Locale_From_UTF8 (Base_Name (File)));
      end if;

      Custom := Create_Proxy
        (The_Action.Command,
         (Event            => null,
          Context          => Create_File_Context (Kernel, File),
          Synchronous      => False,
          Dir              => Dir,
          Args             => Args,
          Label            => new String'(Describe_Action (Ref, Action)),
          Repeat_Count     => 1,
          Remaining_Repeat => 0));

      Launch_Background_Command
        (Kernel          => Kernel,
         Command         => Custom,
         Active          => False,
         Show_Bar        => True,
         Queue_Id        => Ref.Id.all,
         Destroy_On_Exit => True);
   end Generic_Command;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep        : access Generic_VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      Args   : GNAT.Strings.String_List_Access;
      Sorted : String_List.List := Copy_String_List (Filenames);
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Clear_Logs'Img);

      if Rep.Current_Query_Files /= Null_List then
         Free (Rep.Current_Query_Files);
      end if;

      Sort (Sorted);

      if Local then
         if Rep.Local_Status_Parser.File_Index = 0 then
            Rep.Current_Query_Files := Copy_String_List (Sorted);
         end if;

         Generic_Command (Rep, Sorted, Args, Local_Status_Files, False);

      else
         if Rep.Status_Parser.File_Index = 0 then
            Rep.Current_Query_Files := Copy_String_List (Sorted);
         end if;

         Generic_Command (Rep, Sorted, Args, Status_Files);
      end if;

      String_List.Free (Sorted);
      GNAT.Strings.Free (Args);
   end Get_Status;

   ---------------------
   -- Get_Status_Dirs --
   ---------------------

   procedure Get_Status_Dirs
     (Rep        : access Generic_VCS_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Clear_Logs'Img);

      if Rep.Current_Query_Files /= Null_List then
         Free (Rep.Current_Query_Files);
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

   procedure Get_Status_Dirs_Recursive
     (Rep        : access Generic_VCS_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      Args : GNAT.Strings.String_List_Access;

      Files : String_List.List;

      procedure Add_Directory_Recursively;
      --  Add Dir and all subdirectories in Dir to Dirs, and make Node point
      --  to the next node.

      -------------------------------
      -- Add_Directory_Recursively --
      -------------------------------

      procedure Add_Directory_Recursively is
         Node : String_List.List_Node;
         File : String (1 .. 1024);
         Last : Natural;
         D    : Dir_Type;
      begin
         Node := First (Files);

         while Node /= Null_Node loop
            begin
               Open (D, Data (Node));

               loop
                  begin
                     Read (D, File, Last);

                     if Last = 0 then
                        Close (D);
                        exit;
                     else
                        if File (1 .. Last) /= "."
                          and then File (1 .. Last) /= ".."
                          and then GNAT.OS_Lib.Is_Directory
                            (Data (Node) & File (1 .. Last))
                          and then not Is_Hidden (Rep.Kernel, File (1 .. Last))
                        then
                           Append
                             (Files,
                              Data (Node) & File (1 .. Last)
                              & GNAT.OS_Lib.Directory_Separator);
                        end if;
                     end if;

                  exception
                     when Directory_Error =>
                        Close (D);
                        exit;
                  end;
               end loop;

            exception
               when Directory_Error =>
                  null;
            end;

            Node := Next (Node);
         end loop;
      end Add_Directory_Recursively;

   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Clear_Logs'Img);

      if Rep.Current_Query_Files /= Null_List then
         Free (Rep.Current_Query_Files);
      end if;

      if Local then
         Generic_Dir_Command (Rep, Dirs, Args, Local_Status_Dir, False);
         --  ??? add recursive behavior for this?
      else
         declare
            The_Action : constant Action_Record_Access :=
              Lookup_Action (Rep.Kernel, Rep.Commands (Status_Dir_Recursive));
         begin
            if The_Action = null then
               --  There is no action to perform on a hierarchy of directories:
               --  do the work manually.

               Files := Copy_String_List (Dirs);
               Add_Directory_Recursively;
               Generic_Dir_Command (Rep, Files, Args, Status_Dir);
               String_List.Free (Files);
            else
               Generic_Dir_Command (Rep, Dirs, Args, Status_Dir_Recursive);
            end if;
         end;
      end if;
   end Get_Status_Dirs_Recursive;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
      return File_Status_List.List
   is
      pragma Unreferenced (Rep);

      Current_Filename : List_Node := First (Filenames);
      Result           : File_Status_List.List;
      Blank_Status     : File_Status_Record;
      Current_Status   : File_Status_Record := Blank_Status;
   begin
      while Current_Filename /= Null_Node loop
         Current_Status := Blank_Status;
         Current_Status.File := Create (Data (Current_Filename));

         File_Status_List.Append (Result, Current_Status);

         Current_Filename := Next (Current_Filename);
      end loop;

      return Result;
   end Local_Get_Status;

   ----------------
   -- Create_Tag --
   ----------------

   procedure Create_Tag
     (Rep       : access Generic_VCS_Record;
      Dir       : GNATCOLL.VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean)
   is
      Args     : GNAT.Strings.String_List_Access;
      Dir_List : String_List.List;
   begin
      Args := new GNAT.Strings.String_List (1 .. 2);
      Args (1) := new String'(Base_Dir_Name (Dir)); -- root dir
      Args (2) := new String'(Tag);                 -- tag name

      Append (Dir_List, Full_Name (Dir).all);

      if As_Branch then
         Generic_Dir_Command (Rep, Dir_List, Args, Create_Branch);
      else
         Generic_Dir_Command (Rep, Dir_List, Args, Create_Tag);
      end if;

      String_List.Free (Dir_List);
      GNAT.Strings.Free (Args);
   end Create_Tag;

   ----------
   -- Open --
   ----------

   procedure Open
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      User_Name : String := "")
   is
      pragma Unreferenced (User_Name);
   begin
      Generic_Command (Rep, Filenames, null, Open);
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
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

   procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List) is
   begin
      Generic_Command (Rep, Filenames, null, Update);
   end Update;

   ------------
   -- Switch --
   ------------

   procedure Switch
     (Rep : access Generic_VCS_Record;
      Dir : GNATCOLL.VFS.Virtual_File;
      Tag : String)
   is
      Args     : GNAT.Strings.String_List_Access;
      Dir_List : String_List.List;
   begin
      Args := new GNAT.Strings.String_List (1 .. 2);
      Args (1) := new String'(Base_Dir_Name (Dir)); -- root dir
      Args (2) := new String'(Tag);                 -- tag name

      Append (Dir_List, Full_Name (Dir).all);

      Generic_Dir_Command (Rep, Dir_List, Args, Switch);

      String_List.Free (Dir_List);
      GNAT.Strings.Free (Args);
   end Switch;

   --------------
   -- Resolved --
   --------------

   procedure Resolved
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List) is
   begin
      Generic_Command (Rep, Filenames, null, Resolved);
   end Resolved;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Tag       : String)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Tag);                 -- tag name

      --  Iterate through all files, the merge is file oriented and some VCS
      --  won't accept multiple files to merge.

      declare
         Node : List_Node;
      begin
         Node := First (Filenames);
         while Node /= Null_Node loop
            Generic_Command (Rep, Create (Data (Node)), Args, Merge);
            Node := Next (Node);
         end loop;
      end;

      GNAT.Strings.Free (Args);
   end Merge;

   -------------------
   -- File_Revision --
   -------------------

   procedure File_Revision
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

   procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
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

   procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
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

   procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List) is
   begin
      Generic_Command (Rep, Filenames, null, Revert);
   end Revert;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : GNATCOLL.VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      Args : GNAT.Strings.String_List_Access;

   begin
      if Version_1 = ""
        and then Version_2 = ""
      then
         Generic_Command (Rep, File, null, Diff_Head);

      elsif Version_1 /= ""
        and then Version_2 = ""
      then
         Args := new GNAT.Strings.String_List (1 .. 1);
         Args (1) := new String'(Version_1);
         Generic_Command (Rep, File, Args, Diff);
         GNAT.Strings.Free (Args);

      elsif Version_1 /= ""
        and then Version_2 /= ""
      then
         Args := new GNAT.Strings.String_List (1 .. 2);
         Args (1) := new String'(Version_1);
         Args (2) := new String'(Version_2);
         Generic_Command (Rep, File, Args, Diff2);
         GNAT.Strings.Free (Args);
      end if;
   end Diff;

   ----------------
   -- Diff_Patch --
   ----------------

   procedure Diff_Patch
     (Rep    : access Generic_VCS_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Output : GNATCOLL.VFS.Virtual_File)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 1);
      Args (1) := new String'(Full_Name (Output).all);

      Generic_Command (Rep, File, Args, Diff_Patch);
      GNAT.Strings.Free (Args);
   end Diff_Patch;

   --------------------
   -- Diff_Base_Head --
   --------------------

   procedure Diff_Base_Head
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Generic_Command (Rep, File, null, Diff_Base_Head);
   end Diff_Base_Head;

   ------------------
   -- Diff_Working --
   ------------------

   procedure Diff_Working
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Generic_Command (Rep, File, null, Diff_Working);
   end Diff_Working;

   --------------
   -- Diff_Tag --
   --------------

   procedure Diff_Tag
     (Rep      : access Generic_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Tag_Name : String)
   is
      Args : GNAT.Strings.String_List_Access;
   begin
      Args := new GNAT.Strings.String_List (1 .. 2);
      Args (1) := new String'(Tag_Name);
      Args (2) := new String'(Base_Name (File));

      Generic_Command (Rep, File, Args, Diff_Tag);

      GNAT.Strings.Free (Args);
   end Diff_Tag;

   ---------
   -- Log --
   ---------

   procedure Log
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

   procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File) is
   begin
      Generic_Command (Rep, File, null, Annotate);
   end Annotate;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
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
         Field  : String_Ptr;

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
                        (Get_Attribute (M, "label", -"unnamed status"));
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
                        Status_Parser.Append
                          (Parser.Status_Identifiers, (Regexp, Num));
                     else
                        Unchecked_Free (Regexp);
                     end if;

                  exception
                     when E : GNAT.Regpat.Expression_Error =>
                        Insert
                          (Kernel,
                           -"Error when registering VCS """ & Name & """:"
                           & ASCII.LF
                           & (-"could not parse regular expression: ")
                           & ASCII.LF & Field.all,
                           Mode => Error);
                        Trace (Exception_Handle, E);
                  end;
               end if;

               M := M.Next;
            end loop;

            Field := Get_Field (N, "regexp");

            if Field /= null then
               declare
               begin
                  Parser.Regexp := new Pattern_Matcher'(Compile (Field.all));
               exception
                  when E : GNAT.Regpat.Expression_Error =>
                     Insert
                       (Kernel,
                        "Error when registering VCS """ & Name & """:"
                        & ASCII.LF & "could not parse regular expression: "
                        & ASCII.LF & Field.all,
                        Mode => Error);
                     Trace (Exception_Handle, E);
               end;
            end if;

            Field := Get_Field (N, "file_index");
            Set_Field (Parser, Field, Parser.File_Index);

            Field := Get_Field (N, "status_index");
            Set_Field (Parser, Field, Parser.Status_Index);

            Field := Get_Field (N, "local_revision_index");
            Set_Field (Parser, Field, Parser.Local_Rev_Index);

            Field := Get_Field (N, "repository_revision_index");
            Set_Field (Parser, Field, Parser.Repository_Rev_Index);

            Field := Get_Field (N, "author_index");
            Set_Field (Parser, Field, Parser.Author_Index);

            Field := Get_Field (N, "date_index");
            Set_Field (Parser, Field, Parser.Date_Index);

            Field := Get_Field (N, "log_index");
            Set_Field (Parser, Field, Parser.Log_Index);

            Field := Get_Field (N, "sym_index");
            Set_Field (Parser, Field, Parser.Sym_Index);

            Field := Get_Field (N, "tooltip_pattern");

            if Field /= null then
               Parser.Pattern := new String'(Field.all);

               declare
                  K : Natural := Field'First;
               begin
                  loop
                     K := Index (Field (K .. Field'Last), "\");
                     exit when K = 0
                       or else K = Field'Last
                       or else Field (K + 1) not in '0' .. '9';
                     Parser.Matches_Num := Natural'Max
                       (Parser.Matches_Num,
                        Natural'Value ((1 => Field (K + 1))));
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

         --  dir_sep is an alias for path_style and is obsolescent, if both
         --  path_style and dir_sep are set, parth_style value is used.

         if Get_Attribute (M, "path_style", "@") = "@" then
            Ref.Path_Style := OS_Utils.Path_Style'Value
              (Get_Attribute (M, "dir_sep", "System_Default"));
         else
            Ref.Path_Style := OS_Utils.Path_Style'Value
              (Get_Attribute (M, "path_style", "System_Default"));
         end if;

         Ref.Ignore_Filename := new String'
           (Get_Attribute (M, "ignore_file", ""));

         --  Find the command descriptions

         for A in VCS_Action loop
            Node := Find_Tag (Child, To_Lower (A'Img));

            if Node /= null then
               Ref.Commands (A) := new String'
                 (Get_Attribute (Node, "action", ""));

               Ref.Labels (A) := new String'
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
               Ref.Status (Num).Stock_Id := new String'
                 (Get_Attribute (P, "stock", "gtk-new"));
               Ref.Status (Num).Label := new String'
                 (Get_Attribute (P, "label", -"unnamed status"));

               Num := Num + 1;
            end if;

            P := P.Next;
         end loop;

         --  Parse the status parsers

         Ref.Status_Parser := Parse_Status_Parser
           (Find_Tag (Child, "status_parser"));

         Ref.Local_Status_Parser := Parse_Status_Parser
           (Find_Tag (Child, "local_status_parser"));

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

         Register_VCS (Module_ID (VCS_Module_ID), Name);

         Ref.Kernel := Kernel;

         VCS_Info_List.Append (VCS_Generic_Module_ID.VCS_List, Ref);

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

   function Execute
     (Command : access Parser_Command_Type) return Command_Return_Type
   is
      S           : String renames Command.Text.all;
      Matches     : Match_Array (0 .. Command.Parser.Matches_Num);
      Num_Matches : Natural := 0;

   begin
      Command.Prev_Start := Command.Start;

      while Num_Matches < Max_Matches loop
         Match (Command.Parser.Regexp.all, S, Matches, Command.Start, S'Last);

         if Matches (0) = No_Match then
            --  Update the status in the VCS Explorer

            Display_File_Status
              (Command.Rep.Kernel,
               Command.Status,
               VCS_Access (Command.Rep),
               Override_Cache => Command.Override_Cache,
               Force_Display  => True,
               Clear_Logs     => Command.Clear_Logs,
               Display        => Command.Override_Cache);

            --  Update also the status in the VCS Activities explorer

            Display_File_Status
              (Command.Rep.Kernel,
               No_Activity,
               Command.Status,
               VCS_Access (Command.Rep),
               Override_Cache => Command.Override_Cache,
               Force_Display  => True,
               Clear_Logs     => Command.Clear_Logs,
               Display        => Command.Override_Cache);

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
                    (Filename, Command.Rep.Kernel, True, False);

               else
                  if Command.Rep.Absolute_Names
                    and then GNAT.OS_Lib.Is_Absolute_Path (Filename)
                  then
                     St.File := Create (Filename);
                  else
                     St.File := Create (Command.Dir.all & Filename);
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

            elsif not Is_Empty (Command.Rep.Current_Query_Files) then
               St.File := GPS.Kernel.Create
                 (String_List.Head (Command.Rep.Current_Query_Files),
                  Command.Rep.Kernel,
                  True, False);

               String_List.Next (Command.Rep.Current_Query_Files);

            else
               --  ??? There should be an error message here
               return Failure;
            end if;

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

            if Command.Parser.Status_Index /= 0 then
               declare
                  Status_String : constant String := S
                    (Matches (Command.Parser.Status_Index).First
                     .. Matches (Command.Parser.Status_Index).Last);
                  Matches       : Match_Array (0 .. 1);

                  use Status_Parser;

                  Node : Status_Parser.List_Node :=
                           First (Command.Parser.Status_Identifiers);
               begin
                  while Node /= Status_Parser.Null_Node loop
                     Match (Data (Node).Regexp.all, Status_String, Matches);

                     if Matches (0) /= No_Match then
                        St.Status := Command.Rep.Status (Data (Node).Index);
                        exit;
                     end if;

                     Node := Next (Node);
                  end loop;
               end;
            end if;

            File_Status_List.Append (Command.Status, St);
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

   function Name (Command : access Parser_Command_Type) return String is
      pragma Unreferenced (Command);
   begin
      return -"Parsing status";
   end Name;

   --------------------------
   -- Generic_Parse_Status --
   --------------------------

   procedure Generic_Parse_Status
     (Rep            : access Generic_VCS_Record;
      Parser         : Status_Parser_Record;
      Text           : String;
      Override_Cache : Boolean;
      Clear_Logs     : Boolean;
      Dir            : String)
   is
      Command : Parser_Command_Access;
   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no status parser defined for " & Rep.Id.all);
         return;
      end if;

      Command := new Parser_Command_Type;
      Command.Parser         := Parser;
      Command.Text           := new String'(Text);
      Command.Start          := Text'First;
      Command.Prev_Start     := Text'First - 1;
      Command.Override_Cache := Override_Cache;
      Command.Clear_Logs     := Clear_Logs;
      Command.Rep            := Generic_VCS_Access (Rep);
      Command.Dir            := new String'(Dir);

      Launch_Background_Command
        (Rep.Kernel, Command_Access (Command), True, False, "");
   end Generic_Parse_Status;

   ------------------
   -- Parse_Status --
   ------------------

   procedure Parse_Status
     (Rep        : access Generic_VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean;
      Dir        : String) is
   begin
      if Local then
         Generic_Parse_Status
           (Rep, Rep.Local_Status_Parser, Text, False, Clear_Logs, Dir);
      else
         Generic_Parse_Status
           (Rep, Rep.Status_Parser, Text, True, Clear_Logs, Dir);
      end if;
   end Parse_Status;

   -----------------------
   -- Parse_Annotations --
   -----------------------

   procedure Parse_Annotations
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
               return (Width - Str'Length) * ' ' & Str;
            end if;
         end Field;

         N_Rev : constant String (1 .. 10) := ((10 - Rev'Length) * ' ') & Rev;

      begin
         if N_Rev = Last_Rev then
            return "   . . .   ";

         else
            Last_Rev := N_Rev;
            return "<span size=""small"">" & "<span underline=""single"" "
              & "foreground=""blue"">"
              & Rev & "</span>" & ASCII.HT
              & Field (Author, 10) & ' ' & Field (Date) & " </span>";
         end if;
      end Build_Text_Info;

      Parser  : constant Status_Parser_Record := Rep.Annotations_Parser;
      Line    : Natural := 1;
      Max     : Natural;
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
         Open_File_Editor (Kernel, File, Line => 0);
      else
         Open_File_Editor (Kernel, File);
      end if;

      Script := Lookup_Scripting_Language
        (Get_Scripts (Kernel), GPS_Shell_Name);

      declare
      begin
         Max := Natural'Value
           (Execute_GPS_Shell_Command
              (Kernel,
               "Editor.get_last_line " & Full_Name (File).all));
      exception
         when others =>
            Trace (Me, "Could not get last line of " & Full_Name (File).all);
            return;
      end;

      declare
         use Ada.Strings.Unbounded;
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

                  A (Line).Text := new String'
                    (Build_Text_Info
                       (Rev, To_String (Author), To_String (Date)));

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
                        A (Line).Tooltip_Text := new String'(To_String (Str));
                     end;
                  end if;

                  Create
                    (Command, -"query log", Kernel,
                     "VCS.log " & Full_Name (File).all & " """ & Rev & """",
                     Script);

                  A (Line).Associated_Command := Command_Access (Command);
               end;
            end if;

            Line  := Line + 1;
            Start := Matches (0).Last + 1;
         end loop;

         Add_Line_Information
           (Kernel,
            File,
            Annotation_Id,
            new Line_Information_Array'(A));
      end;
   end Parse_Annotations;

   ---------------
   -- Parse_Log --
   ---------------

   procedure Parse_Log
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
            "Revision.add_link " & Full_Name (File).all &
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
   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no log parser defined for " & Rep.Id.all);
      end if;

      Script := Lookup_Scripting_Language
        (Get_Scripts (Kernel), GPS_Shell_Name);

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
               "Revision.add_log " & Full_Name (File).all &
               " """ & Rev & """ """ & Author & """ """ &
               Date & """ """ & String_Utils.Protect (Log) & """ """ &
               Boolean'Image (First) & """",
               Script);

            First := False;

            Launch_Background_Command
              (Kernel, Command_Access (Command), True, False,
               Revision_Handling_Queue);

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
   end Parse_Log;

   --------------------
   -- Parse_Revision --
   --------------------

   procedure Parse_Revision
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String)
   is
      Kernel  : Kernel_Handle renames Rep.Kernel;
      Parser  : constant Status_Parser_Record := Rep.Revision_Parser;
      S       : String renames Text;
      Matches : Match_Array (0 .. Parser.Matches_Num);
      Script  : Scripting_Language;
      Start   : Integer := S'First;
   begin
      if Parser.Regexp = null then
         Insert (Rep.Kernel,
                 -"Error: no revision parser defined for " & Rep.Id.all);
      end if;

      Script := Lookup_Scripting_Language
        (Get_Scripts (Kernel), GPS_Shell_Name);

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
               "Revision.add_revision " & Full_Name (File).all &
               " """ & Rev & """ """ & Sym & """",
               Script);

            Launch_Background_Command
              (Rep.Kernel, Command_Access (Command), True, False,
               Revision_Handling_Queue);
         end;

         Start := Matches (0).Last + 1;
      end loop;
   end Parse_Revision;

   ----------------------------
   -- Get_Identified_Actions --
   ----------------------------

   function Get_Identified_Actions
     (Rep : access Generic_VCS_Record) return Action_Array is
   begin
      return Rep.Labels;
   end Get_Identified_Actions;

   ---------------------------
   -- Get_Registered_Status --
   ---------------------------

   function Get_Registered_Status
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
      Register_VCS_Identifier (Identify_VCS'Access);
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
      Unchecked_Free (X.Regexp);
   end Free;

end VCS.Generic_VCS;
