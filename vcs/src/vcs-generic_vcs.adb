-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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

with Glib;                      use Glib;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Actions;      use Glide_Kernel.Actions;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Contexts;     use Glide_Kernel.Contexts;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;

with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with VCS_Module;                use VCS_Module;
with Traces;                    use Traces;
with VFS;                       use VFS;

with Basic_Types;               use Basic_Types;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.OS_Lib;

with Generic_List;

package body VCS.Generic_VCS is

   Me : constant Debug_Handle := Create ("Generic_VCS");

   ----------------------------
   -- Local type definitions --
   ----------------------------

   procedure Free (X : in out Generic_VCS_Access);
   --  Free memory associated to X.

   package VCS_Info_List is new Generic_List (Generic_VCS_Access);

   type VCS_Generic_Module_ID_Record is new Module_ID_Record with record
      VCS_List : VCS_Info_List.List;
      Kernel   : Kernel_Handle;
   end record;

   procedure Destroy (Id : in out VCS_Generic_Module_ID_Record);
   --  Free memory used by the module.

   type VCS_Generic_Module_ID_Access is access
     all VCS_Generic_Module_ID_Record'Class;

   VCS_Generic_Module_Name : constant String := "Generic VCS connectivity";
   VCS_Generic_Module_ID   : VCS_Generic_Module_ID_Access;

   use String_List_Utils.String_List;

   function Identify_VCS (S : String) return VCS_Access;
   --  Utility function to identify the Generic VCS from a given string.

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Called when a new customization in parsed

   function Get_Info (Id : String) return Generic_VCS_Access;
   --  Return the information corresponding to Id.

   function Create_File_Context
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Selection_Context_Access;
   --  Create a file context corresponding to File.

   procedure Generic_Command
     (Ref    : access Generic_VCS_Record;
      Files  : String_List.List;
      Action : VCS_Action);
   --  Launch a generic command corresponding to Action.

   procedure Generic_Command
     (Ref    : access Generic_VCS_Record;
      File   : VFS.Virtual_File;
      Action : VCS_Action);
   --  Launch a generic command corresponding to Action.

   function Lookup_Action
     (Kernel : Kernel_Handle;
      Action : String_Access) return Action_Record;
   --  Wrapper for Lookup_Action.

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Kernel : Kernel_Handle;
      Action : String_Access) return Action_Record is
   begin
      if Action = null then
         return No_Action;
      else
         return Lookup_Action (Kernel, Action.all);
      end if;
   end Lookup_Action;

   -------------------------
   -- Create_File_Context --
   -------------------------

   function Create_File_Context
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Selection_Context_Access
   is
      Context : File_Selection_Context_Access;
   begin
      Context := new File_Selection_Context;

      Set_Context_Information
        (Context => Context,
         Kernel  => Kernel,
         Creator => Module_ID (VCS_Generic_Module_ID));

      Set_File_Information
        (Context,
         File => File);

      return Selection_Context_Access (Context);
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
           and then Data (Node).Id.all = Id
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
   begin
      Free (X.Id);

      for J in X.Commands'Range loop
         Free (X.Commands (J));
      end loop;

      for J in X.Labels'Range loop
         Free (X.Labels (J));
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

   ---------------------
   -- Generic_Command --
   ---------------------

   procedure Generic_Command
     (Ref    : access Generic_VCS_Record;
      Files  : String_List.List;
      Action : VCS_Action)
   is
      Kernel : Kernel_Handle renames Ref.Kernel;

      The_Action : constant Action_Record :=
        Lookup_Action (Kernel, Ref.Commands (Action));

      Node   : List_Node := First (Files);

      Custom : Command_Access;

      Args   : GNAT.OS_Lib.String_List_Access;
      Index  : Natural := 1;
   begin
      if The_Action = No_Action
        or else Is_Empty (Files)
      then
         return;
      end if;

      --  ??? Where is Args freed ?
      Args := new GNAT.OS_Lib.String_List (1 .. Length (Files));

      while Node /= Null_Node loop
         Args (Index) := new String'(Data (Node));
         Index := Index + 1;
         Node := Next (Node);
      end loop;

      Custom := Create_Proxy
        (The_Action.Command,
         (null,
          null,
          Args));

      Launch_Background_Command
        (Kernel,
         Custom,
         True,
         True,
         Ref.Id.all,
         True);
   end Generic_Command;

   procedure Generic_Command
     (Ref    : access Generic_VCS_Record;
      File   : VFS.Virtual_File;
      Action : VCS_Action)
   is
      Kernel     : Kernel_Handle renames Ref.Kernel;
      The_Action : constant Action_Record :=
        Lookup_Action (Kernel, Ref.Commands (Action));
      Custom     : Command_Access;
   begin
      if The_Action = No_Action then
         return;
      end if;

      Custom := Create_Proxy
        (The_Action.Command,
         (null,
          Create_File_Context (Kernel, File),
          new GNAT.OS_Lib.String_List'
            ((1 => new String'(Full_Name (File).all)))));

      Launch_Background_Command
        (Kernel,
         Custom,
         True,
         True,
         Ref.Id.all,
         True);
   end Generic_Command;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access Generic_VCS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False)
   is
      pragma Unreferenced (Clear_Logs);
   begin
      Generic_Command (Rep, Filenames, Status);
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
      return File_Status_List.List
   is
      pragma Unreferenced (Rep);

      use String_List;
      Current_Filename : List_Node := First (Filenames);
      Result           : File_Status_List.List;
      Blank_Status     : File_Status_Record;
      Current_Status   : File_Status_Record := Blank_Status;
   begin
      while Current_Filename /= Null_Node loop
         Current_Status := Blank_Status;
         Current_Status.File :=
           Create (Data (Current_Filename));

         Current_Filename := Next (Current_Filename);
      end loop;

      return Result;
   end Local_Get_Status;

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
      Generic_Command (Rep, Filenames, Open);
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Log       : String)
   is
      Kernel : Kernel_Handle renames Rep.Kernel;

      The_Action : constant Action_Record :=
        Lookup_Action (Kernel, Rep.Commands (Commit));

      Node   : List_Node := First (Filenames);

      Custom : Command_Access;

      Args   : GNAT.OS_Lib.String_List_Access;
      Index  : Natural := 2;
   begin
      if The_Action = No_Action
        or else Is_Empty (Filenames)
      then
         return;
      end if;

      --  ??? Where is Args freed ?
      Args := new GNAT.OS_Lib.String_List (1 .. Length (Filenames) + 1);

      Args (1) := new String'(Log);

      while Node /= Null_Node loop
         Args (Index) := new String'(Data (Node));
         Index := Index + 1;
         Node := Next (Node);
      end loop;

      Custom := Create_Proxy
        (The_Action.Command,
         (null,
          null,
          Args));

      Launch_Background_Command
        (Kernel,
         Custom,
         True,
         True,
         Rep.Id.all,
         True);
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List) is
   begin
      Generic_Command (Rep, Filenames, Update);
   end Update;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Revert;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      pragma Unreferenced (Version_2, Version_1);
      Filenames : String_List.List;
   begin
      --  ??? This will only diff against the head revision.
      Generic_Command (Rep, File, Diff_Head);
      Free (Filenames);
   end Diff;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Rev  : String)
   is
      pragma Unreferenced (Rev);
   begin
      --  ??? Will not work for specified revision history
      Generic_Command (Rep, File, History);
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Annotate;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);

      N : Node_Ptr := Node;

      function Parse_Node (M : Node_Ptr) return Boolean;
      --  Parse one node that contains VCS information.

      function Parse_Node (M : Node_Ptr) return Boolean is
         Name   : constant String := Get_Attribute (M, "name");
         Ref    : Generic_VCS_Access;
         Child  : Node_Ptr := M.Child;
         Node   : Node_Ptr;
         Field  : String_Ptr;

         function To_Natural (X : String) return Natural;
         --  Safe function to convert a string to a Natural.

         function To_Natural (X : String) return Natural is
         begin
            return Natural'Value (X);
         exception
            when others =>
               Insert (Kernel, -"Warning: numeric value expected");
               return 0;
         end To_Natural;

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

         --  Parse the status analyze data.

         Child := Find_Tag (Child, "status_parser");


         if Child /= null then
            for A in File_Status loop
               Field := Get_Field (Child, To_Lower (A'Img));

               if Field /= null then
                  Ref.Status (A) := new String'(Field.all);
               end if;
            end loop;

            Field := Get_Field (Child, "regexp");

            if Field /= null then
               Ref.Regexp := new Pattern_Matcher'(Compile (Field.all));
            end if;

            Field := Get_Field (Child, "file_name_index");

            if Field /= null then
               Ref.File_Index := To_Natural (Field.all);
            end if;

            Field := Get_Field (Child, "status_index");

            if Field /= null then
               Ref.Status_Index := To_Natural (Field.all);
            end if;

            Field := Get_Field (Child, "local_revision_index");

            if Field /= null then
               Ref.Local_Rev_Index := To_Natural (Field.all);
            end if;

            Field := Get_Field (Child, "repository_revision_index");

            if Field /= null then
               Ref.Repository_Rev_Index := To_Natural (Field.all);
            end if;
         end if;

         Register_VCS (VCS_Module_ID, Name);

         Ref.Kernel := Kernel_Handle (Kernel);
         Ref.Queue  := New_Queue;

         VCS_Info_List.Append (VCS_Generic_Module_ID.VCS_List, Ref);

         return True;
      end Parse_Node;

   begin
      while N /= null loop
         if N.Tag /= null
           and then To_Lower (N.Tag.all) = "vcs"
         then
            if not Parse_Node (N) then
               Trace (Me, "Could not parse generic VCS information");
            end if;
         end if;

         N := N.Next;
      end loop;
   end Customize;

   ------------------
   -- Parse_Status --
   ------------------

   function Parse_Status
     (Rep  : access Generic_VCS_Record;
      Text : String) return File_Status_List.List
   is
      Status : File_Status_List.List;

      S : String renames Text;
      Matches : Match_Array (0 .. 4);
      Start   : Integer := S'First;

   begin
      if Rep.Regexp = null
        or else Rep.File_Index = 0
      then
         Insert (Rep.Kernel,
                 -"Error: no status parser defined for " & Rep.Id.all);
         return Status;
      end if;

      loop
         Match (Rep.Regexp.all, S, Matches, Start, S'Last);

         exit when Matches (0) = No_Match;

         declare
            St : File_Status_Record;
         begin
            Trace (Me, "File: #"
                   & S (Matches (Rep.File_Index).First
                        .. Matches (Rep.File_Index).Last) & "#");

            St.File := Glide_Kernel.Create
              (S (Matches (Rep.File_Index).First
                  .. Matches (Rep.File_Index).Last),
               Rep.Kernel,
               True, False);

            Start := Integer'Max (Matches (Rep.File_Index).Last + 1, Start);

            if Rep.Local_Rev_Index /= 0 then
               String_List_Utils.String_List.Append
                 (St.Working_Revision,
                  S (Matches (Rep.Local_Rev_Index).First
                     .. Matches (Rep.Local_Rev_Index).Last));

               Start := Integer'Max
                 (Matches (Rep.Local_Rev_Index).Last + 1, Start);
            end if;

            if Rep.Repository_Rev_Index /= 0 then
               String_List_Utils.String_List.Append
                 (St.Repository_Revision,
                  S (Matches (Rep.Repository_Rev_Index).First
                     .. Matches (Rep.Repository_Rev_Index).Last));

               Start := Integer'Max
                 (Matches (Rep.Repository_Rev_Index).Last + 1, Start);
            end if;

            if Rep.Status_Index /= 0 then
               declare
                  Status_String : constant String :=
                    S (Matches (Rep.Status_Index).First
                       .. Matches (Rep.Status_Index).Last);
               begin
                  Trace (Me, "Status: #" & Status_String & "#");

                  for A in File_Status'Range loop
                     if Rep.Status (A) /= null
                       and then Status_String = Rep.Status (A).all
                     then
                        St.Status := A;
                        exit;
                     end if;
                  end loop;
               end;
            end if;

            File_Status_List.Append (Status, St);
         end;
      end loop;

      return Status;
   end Parse_Status;

   ----------------------------
   -- Get_Identified_Actions --
   ----------------------------

   function Get_Identified_Actions
     (Rep : access Generic_VCS_Record) return Action_Array is
   begin
      return Rep.Labels;
   end Get_Identified_Actions;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      VCS_Generic_Module_ID := new VCS_Generic_Module_ID_Record;
      VCS_Generic_Module_ID.Kernel := Kernel_Handle (Kernel);

      Register_VCS_Identifier (Identify_VCS'Access);
      Register_Module
        (Module                  => Module_ID (VCS_Generic_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => VCS_Generic_Module_Name,
         Priority                => Default_Priority,
         Customization_Handler   => Customize'Access,
         Contextual_Menu_Handler => null);
   end Register_Module;

end VCS.Generic_VCS;
