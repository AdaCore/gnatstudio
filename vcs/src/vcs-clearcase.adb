-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with String_Utils;              use String_Utils;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;

package body VCS.ClearCase is

   type VCS_Clearcase_Module_ID_Record is new Module_ID_Record with record
      ClearCase_Reference : VCS_Access;
   end record;
   type VCS_Clearcase_Module_ID_Access is access all
     VCS_Clearcase_Module_ID_Record'Class;


   VCS_ClearCase_Module_Name : constant String := "ClearCase_Connectivity";
   VCS_ClearCase_Module_ID   : VCS_Clearcase_Module_ID_Access;
   ClearCase_Identifier      : constant String := "ClearCase";

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Destroy (Id : in out VCS_Clearcase_Module_ID_Record);
   --  Free the memory occupied by this module

   function Identify_VCS (S : String) return VCS_Access;
   --  Return an access to VCS_Record if S describes a ClearCase system.

   ----------
   -- Name --
   ----------

   function Name (Ref : access ClearCase_Record) return String is
      pragma Unreferenced (Ref);
   begin
      return ClearCase_Identifier;
   end Name;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
      Id         : String := S;
      Identifier : String := ClearCase_Identifier;
   begin
      Lower_Case (Id);
      Lower_Case (Identifier);

      if Strip_Quotes (Id) = Identifier then
         return VCS_ClearCase_Module_ID.ClearCase_Reference;
      end if;

      return null;
   end Identify_VCS;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access ClearCase_Record;
      Filenames   : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
     return File_Status_List.List
   is
      pragma Unreferenced (Rep, Filenames);
      Result : File_Status_List.List;
   begin
      return Result;
   end Local_Get_Status;

   ----------
   -- Open --
   ----------

   procedure Open
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      User_Name : String := "")
   is
      pragma Unreferenced (Rep, Filenames, User_Name);
   begin
      null;
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Logs      : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames, Logs);
   begin
      null;
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Update;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access ClearCase_Record;
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
     (Rep       : access ClearCase_Record;
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
     (Rep       : access ClearCase_Record;
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
     (Rep       : access ClearCase_Record;
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
     (Rep       : access ClearCase_Record;
      File      : String;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      pragma Unreferenced (Rep, File, Version_1, Version_2);
   begin
      null;
   end Diff;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access ClearCase_Record;
      File : String)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access ClearCase_Record;
      File : String)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Annotate;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VCS_Clearcase_Module_ID_Record) is
   begin
      Free (Id.ClearCase_Reference);
      Unregister_VCS_Identifier (Identify_VCS'Access);
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      VCS_ClearCase_Module_ID := new VCS_Clearcase_Module_ID_Record;
      Register_VCS_Identifier (Identify_VCS'Access);
      Register_Module
        (Module                  => Module_ID (VCS_ClearCase_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => VCS_ClearCase_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

      VCS_ClearCase_Module_ID.ClearCase_Reference := new ClearCase_Record;

      Register_VCS (Kernel, ClearCase_Identifier);
   end Register_Module;

end VCS.ClearCase;
