-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with GNAT.Case_Util;            use GNAT.Case_Util;

with String_Utils;              use String_Utils;

with Commands;                  use Commands;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with VCS_Module;                use VCS_Module;
with VFS;                       use VFS;

package body VCS.Generic_VCS is

   type VCS_Generic_Module_ID_Record is new Module_ID_Record with record
      Generic_Reference : VCS_Access;
   end record;
   type VCS_Generic_Module_ID_Access is access
     all VCS_Generic_Module_ID_Record'Class;

   Generic_VCS_Name        : constant String := "Generic VCS";
   VCS_Generic_Module_Name : constant String := "Generic VCS connectivity";
   VCS_Generic_Module_ID   : VCS_Generic_Module_ID_Access;

   use String_List;

   function Identify_VCS (S : String) return VCS_Access;
   --  Utility function to identify the Generic VCS from a given string.

   procedure Error (File : String);
   --  Convenience function to display an error message in the console.

   -----------
   -- Error --
   -----------

   procedure Error (File : String) is
   begin
      Insert
        (VCS_Generic_Module_ID.Generic_Reference.Kernel,
         -"Warning: no VCS set in project properties for " & File);
   end Error;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
      Id         : String := S;
      Identifier : String := Generic_VCS_Name;
   begin
      --  ??? Should fix implementation to allow multiple names.
      To_Lower (Id);
      To_Lower (Identifier);

      if Strip_Quotes (Id) = Identifier then
         return VCS_Generic_Module_ID.Generic_Reference;
      end if;

      return null;
   end Identify_VCS;

   ----------
   -- Name --
   ----------

   function Name (Ref : access Generic_VCS_Record) return String is
      pragma Unreferenced (Ref);
   begin
      return Generic_VCS_Name;
   end Name;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access Generic_VCS_Record) is
      pragma Unreferenced (Ref);
   begin
      null;
   end Free;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access Generic_VCS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False)
   is
      pragma Unreferenced (Rep, Clear_Logs);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
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
         Append (Current_Status.File_Name,
                 Data (Current_Filename));

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
      pragma Unreferenced (Rep, User_Name);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Logs      : String_List.List)
   is
      pragma Unreferenced (Rep, Logs);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Update;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
      null;
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
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
      pragma Unreferenced (Rep, File, Version_2, Version_1);
   begin
      null;
   end Diff;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Rev  : String)
   is
      pragma Unreferenced (Rep, Rev);
   begin
      Error (Full_Name (File).all);
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File)
   is
      pragma Unreferenced (Rep);
   begin
      Error (Full_Name (File).all);
   end Annotate;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      VCS_Generic_Module_ID := new VCS_Generic_Module_ID_Record;
      Register_VCS_Identifier (Identify_VCS'Access);
      Register_Module
        (Module                  => Module_ID (VCS_Generic_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => VCS_Generic_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

      VCS_Generic_Module_ID.Generic_Reference := new Generic_VCS_Record;
      VCS_Generic_Module_ID.Generic_Reference.Kernel := Kernel_Handle (Kernel);
      VCS_Generic_Module_ID.Generic_Reference.Queue  := New_Queue;

      Register_VCS (VCS_Module_ID, Generic_VCS_Name);
   end Register_Module;

end VCS.Generic_VCS;
