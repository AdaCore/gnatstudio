------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Commands.VCS;            use Commands.VCS;
with Commands;                use Commands;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with String_Utils;            use String_Utils;

package body VCS is

   ------------------------------
   -- Administrative_Directory --
   ------------------------------

   function Administrative_Directory
     (Ref : access VCS_Record) return Filesystem_String is
      pragma Unreferenced (Ref);
   begin
      return "";
   end Administrative_Directory;

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
     (F : File_Status_Record) return File_Status_Record
   is
      Result : File_Status_Record;
   begin
      Result.File := F.File;
      Replace (Result.Working_Revision, F.Working_Revision);
      Replace (Result.Repository_Revision, F.Repository_Revision);
      Result.Tags := Copy_String_List (F.Tags);
      Result.Users := Copy_String_List (F.Users);
      Result.Status := F.Status;
      return Result;
   end Copy_File_Status;

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

      procedure Free (A : in out Action_Array);

      ----------
      -- Free --
      ----------

      procedure Free (A : in out Action_Array) is
      begin
         for J in A'Range loop
            GNAT.Strings.Free (A (J));
         end loop;
      end Free;

   begin
      Free (Ref.Ignore_Filename);
      Free (Ref.Action_Labels);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : in out VCS_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (VCS_Record'Class, VCS_Access);
   begin
      Free (Ref.all);
      Unchecked_Free (Ref);
   end Free;

   ---------
   -- "=" --
   ---------

   overriding function "=" (S1, S2 : VCS_File_Status) return Boolean is
      use type GNAT.Strings.String_Access;
   begin
      return (S1.Label = null and then S2.Label = null
              and then S1.Icon_Name = null and then S2.Icon_Name = null)
        or else
          (S1.Label /= null
           and then S2.Label /= null
           and then S1.Label.all = S2.Label.all
           and then S1.Icon_Name /= null
           and then S2.Icon_Name /= null
           and then S1.Icon_Name.all = S2.Icon_Name.all);
   end "=";

   ---------------------
   -- Is_Local_Status --
   ---------------------

   function Is_Local_Status (Status : VCS_File_Status) return Boolean is
   begin
      return Status /= Needs_Update and then Status /= Needs_Merge;
   end Is_Local_Status;

   ----------
   -- Free --
   ----------

   procedure Free (F : in out File_Status_Record) is
   begin
      GNAT.Strings.Free (F.Working_Revision);
      GNAT.Strings.Free (F.Repository_Revision);
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

   function Ignore_Filename
     (Ref : access VCS_Record) return Filesystem_String
   is
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
      Filenames : GNATCOLL.VFS.File_Array)
   is
      C : Update_Files_Command_Access;
   begin
      Create (C, Rep.Kernel, Filenames);
      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, True, Name (VCS_Access (Rep)));
   end Check_Files;

   ---------------------
   -- Create_From_VCS --
   ---------------------

   function Create_From_VCS
     (Ref  : access VCS_Record;
      Name : String) return GNATCOLL.VFS.Virtual_File is
   begin
      return Create (Filesystem_String (Name), Ref.Kernel);
   end Create_From_VCS;

   ----------------------------
   -- Get_Identified_Actions --
   ----------------------------

   function Get_Identified_Actions
     (Rep : access VCS_Record'Class) return Action_Array is
   begin
      if Rep = null then
         return No_Action;
      else
         return Rep.Action_Labels;
      end if;
   end Get_Identified_Actions;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out VCS_File_Status) is
   begin
      GNAT.Strings.Free (X.Label);
      GNAT.Strings.Free (X.Icon_Name);
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
      Status : Status_Id) return VCS_File_Status
   is
      function Status_For (Status : VCS_File_Status) return VCS_File_Status;
      --  Return the File_Status for the corresponding VCS, uses the Stock_Id
      --  as a key.

      Ref_Status : constant Status_Array := Get_Registered_Status (Ref);

      ----------------
      -- Status_For --
      ----------------

      function Status_For (Status : VCS_File_Status) return VCS_File_Status is
      begin
         for K in Ref_Status'Range loop
            if Ref_Status (K).Icon_Name.all = Status.Icon_Name.all then
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

   function Get_File_Status_Id (Status : VCS_File_Status) return Status_Id is
   begin
      if Status.Icon_Name.all = Up_To_Date_Stock then
         return Up_To_Date_Id;
      elsif Status.Icon_Name.all = Added_Stock then
         return Added_Id;
      elsif Status.Icon_Name.all = Removed_Stock then
         return Removed_Id;
      elsif Status.Icon_Name.all = Modified_Stock then
         return Modified_Id;
      elsif Status.Icon_Name.all = Needs_Merge_Stock then
         return Needs_Merge_Id;
      elsif Status.Icon_Name.all = Needs_Update_Stock then
         return Needs_Update_Id;
      elsif Status.Icon_Name.all = Not_Registered_Stock then
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

   ----------
   -- Used --
   ----------

   procedure Used (Ref : in out VCS_Record'Class) is
   begin
      Ref.Used := True;
   end Used;

   -------------
   -- Is_Used --
   -------------

   function Is_Used (Ref : VCS_Record'Class) return Boolean is
   begin
      return Ref.Used;
   end Is_Used;

   -----------------------
   -- Is_Action_Defined --
   -----------------------

   function Is_Action_Defined
     (Ref : access VCS_Record'Class; Action : VCS_Action) return Boolean
   is
      use type GNAT.Strings.String_Access;
   begin
      return Ref.Action_Labels (Action) /= null;
   end Is_Action_Defined;

   -----------------
   -- Require_Log --
   -----------------

   function Require_Log (Ref : access VCS_Record) return Boolean is
   begin
      return Ref.Require_Log;
   end Require_Log;

   --------------------------
   -- Get_Default_Revision --
   --------------------------

   function Get_Default_Revision
     (Ref      : access VCS_Record;
      Revision : Revision_Type) return String
   is
      pragma Unreferenced (Ref, Revision);
   begin
      return "n/a";
   end Get_Default_Revision;

end VCS;
