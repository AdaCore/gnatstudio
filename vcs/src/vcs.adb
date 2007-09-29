-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2007, AdaCore                  --
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

with Unchecked_Deallocation;

with Commands.VCS;            use Commands.VCS;
with Commands;                use Commands;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with VCS.Unknown_VCS;         use VCS.Unknown_VCS;

package body VCS is

   procedure Free (Identifier : in out VCS_Id_Identifier);
   --  Dummy function used to instanciate Identifiers list

   package Identifiers is new Generic_List (VCS_Id_Identifier);

   Identifiers_List : Identifiers.List;
   --  Global variable to store all the registered handlers

   ----------------------
   -- Commit_Directory --
   ----------------------

   function Commit_Directory (Ref : access VCS_Record) return Boolean is
   begin
      return Ref.Commit_Directory;
   end Commit_Directory;

   ----------------------
   -- Copy_File_Status --
   ----------------------

   function Copy_File_Status
     (F : in File_Status_Record) return File_Status_Record
   is
      Result : File_Status_Record;
   begin
      Result.File := F.File;
      Result.Working_Revision := Copy_String_List (F.Working_Revision);
      Result.Repository_Revision := Copy_String_List (F.Repository_Revision);
      Result.Tags := Copy_String_List (F.Tags);
      Result.Users := Copy_String_List (F.Users);
      Result.Status := F.Status;
      return Result;
   end Copy_File_Status;

   ----------
   -- Free --
   ----------

   procedure Free (Identifier : in out VCS_Id_Identifier) is
      pragma Unreferenced (Identifier);
   begin
      null;
   end Free;

   -----------------------------
   -- Register_VCS_Identifier --
   -----------------------------

   procedure Register_VCS_Identifier (Identifier : VCS_Id_Identifier) is
   begin
      Identifiers.Append (Identifiers_List, Identifier);
   end Register_VCS_Identifier;

   -------------------------------
   -- Unregister_VCS_Identifier --
   -------------------------------

   procedure Unregister_VCS_Identifier (Identifier : VCS_Id_Identifier) is
      use Identifiers;
      Prev, Current : Identifiers.List_Node;
   begin
      Current := First (Identifiers_List);
      while Current /= Null_Node loop
         if Data (Current) = Identifier then
            Remove_Nodes (Identifiers_List, Prev, Current);
            return;
         end if;

         Prev := Current;
         Current := Next (Current);
      end loop;
   end Unregister_VCS_Identifier;

   ---------------------
   -- Get_VCS_From_Id --
   ---------------------

   function Get_VCS_From_Id (Id : String) return VCS_Access is
      use Identifiers;

      Result : VCS_Access;
      Temp   : List_Node  := First (Identifiers_List);

   begin
      while Temp /= Null_Node loop
         Result := Data (Temp) (Id);

         if Result /= null then
            return Result;
         end if;

         Temp := Next (Temp);
      end loop;

      return Unknown_VCS_Reference;
   end Get_VCS_From_Id;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Rep     : access VCS_Record;
      Message : String;
      Add_LF  : Boolean := True) is
   begin
      if Rep.Kernel = null then
         return;
      end if;

      Insert (Rep.Kernel, Message, Add_LF, Mode => Error);
   end Set_Error;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : in out VCS_Record) is
      pragma Unreferenced (Ref);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : in out VCS_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (VCS_Record'Class, VCS_Access);
   begin
      Free (Ref.all);
      Unchecked_Free (Ref);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (F : in out File_Status_Record) is
      use String_List;
      use File_Status_List;
   begin
      Free (F.Working_Revision);
      Free (F.Repository_Revision);
      Free (F.Tags);
      Free (F.Users);
   end Free;

   ----------------------------------
   -- Absolute_Filenames_Supported --
   ----------------------------------

   function Absolute_Filenames_Supported
     (Ref : access VCS_Record) return Boolean is
   begin
      return Ref.Absolute_Names;
   end Absolute_Filenames_Supported;

   -------------------------------
   -- Atomic_Commands_Supported --
   -------------------------------

   function Atomic_Commands_Supported
     (Ref : access VCS_Record) return Boolean is
   begin
      return Ref.Atomic_Commands;
   end Atomic_Commands_Supported;

   ---------------------
   -- Ignore_Filename --
   ---------------------

   function Ignore_Filename (Ref : access VCS_Record) return String is
      use GNAT.Strings;
   begin
      if Ref.Ignore_Filename = null then
         return "";
      else
         return Ref.Ignore_Filename.all;
      end if;
   end Ignore_Filename;

   -----------------
   -- Check_Files --
   -----------------

   procedure Check_Files
     (Rep       : access VCS_Record;
      Filenames : String_List_Utils.String_List.List)
   is
      C : Update_Files_Command_Access;
   begin
      Create (C, Rep.Kernel, Filenames);
      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, True, Name (VCS_Access (Rep)));
   end Check_Files;

   ------------------
   -- Parse_Status --
   ------------------

   procedure Parse_Status
     (Rep        : access VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean;
      Dir        : String)
   is
      pragma Unreferenced (Rep, Text, Local, Clear_Logs, Dir);
   begin
      null;
   end Parse_Status;

   -----------------------
   -- Parse_Annotations --
   -----------------------

   procedure Parse_Annotations
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File;
      Text : String)
   is
      pragma Unreferenced (Rep, File, Text);
   begin
      null;
   end Parse_Annotations;

   ---------------
   -- Parse_Log --
   ---------------

   procedure Parse_Log
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File;
      Text : String)
   is
      pragma Unreferenced (Rep, File, Text);
   begin
      null;
   end Parse_Log;

   --------------------
   -- Parse_Revision --
   --------------------

   procedure Parse_Revision
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File;
      Text : String)
   is
      pragma Unreferenced (Rep, File, Text);
   begin
      null;
   end Parse_Revision;

   ----------------------------
   -- Get_Identified_Actions --
   ----------------------------

   function Get_Identified_Actions
     (Rep : access VCS_Record) return Action_Array
   is
      pragma Unreferenced (Rep);
      Result : Action_Array;
   begin
      return Result;
   end Get_Identified_Actions;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out File_Status) is
   begin
      GNAT.Strings.Free (X.Label);
      GNAT.Strings.Free (X.Stock_Id);
   end Free;

   ---------------------------
   -- Get_Registered_Status --
   ---------------------------

   function Get_Registered_Status
     (Rep : access VCS_Record) return Status_Array
   is
      pragma Unreferenced (Rep);
   begin
      return
        (1 => Unknown,
         2 => Up_To_Date,
         3 => Modified,
         4 => Added,
         5 => Removed,
         6 => Needs_Merge,
         7 => Needs_Update,
         8 => Not_Registered);
   end Get_Registered_Status;

   ---------------------
   -- Get_File_Status --
   ---------------------

   function Get_File_Status
     (Ref    : access VCS_Record'Class;
      Status : Status_Id) return File_Status
   is
      function Status_For (Status : File_Status) return File_Status;
      --  Return the File_Status for the corresponding VCS, uses the Stock_Id
      --  as a key.

      Ref_Status : constant Status_Array := Get_Registered_Status (Ref);

      ----------------
      -- Status_For --
      ----------------

      function Status_For (Status : File_Status) return File_Status is
      begin
         for K in Ref_Status'Range loop
            if Ref_Status (K).Stock_Id.all = Status.Stock_Id.all then
               return Ref_Status (K);
            end if;
         end loop;
         return Ref_Status (Ref_Status'First);
      end Status_For;

   begin
      case Status is
         when Unknown_Id =>
            return Status_For (Unknown);
         when Up_To_Date_Id =>
            return Status_For (Up_To_Date);
         when Modified_Id =>
            return Status_For (Modified);
         when Removed_Id =>
            return Status_For (Removed);
         when Added_Id =>
            return Status_For (Added);
         when Needs_Merge_Id =>
            return Status_For (Needs_Merge);
         when Needs_Update_Id =>
            return Status_For (Needs_Update);
         when Not_Registered_Id =>
            return Status_For (Not_Registered);
      end case;
   end Get_File_Status;

   ------------------------
   -- Get_File_Status_Id --
   ------------------------

   function Get_File_Status_Id (Status : File_Status) return Status_Id is
   begin
      if Status.Stock_Id.all = Up_To_Date_Stock then
         return Up_To_Date_Id;
      elsif Status.Stock_Id.all = Added_Stock then
         return Added_Id;
      elsif Status.Stock_Id.all = Removed_Stock then
         return Removed_Id;
      elsif Status.Stock_Id.all = Modified_Stock then
         return Modified_Id;
      elsif Status.Stock_Id.all = Needs_Merge_Stock then
         return Needs_Merge_Id;
      elsif Status.Stock_Id.all = Needs_Update_Stock then
         return Needs_Update_Id;
      elsif Status.Stock_Id.all = Not_Registered_Stock then
         return Not_Registered_Id;
      else
         return Unknown_Id;
      end if;
   end Get_File_Status_Id;

   -------------------------------
   -- Group_Query_Status_By_Dir --
   -------------------------------

   function Group_Query_Status_By_Dir
     (Ref : access VCS_Record) return Boolean is
   begin
      return Ref.Query_Status_By_Dir;
   end Group_Query_Status_By_Dir;

end VCS;
