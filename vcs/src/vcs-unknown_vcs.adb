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

package body VCS.Unknown_VCS is

   Unknown_VCS_Reference : VCS_Access;

   function Identify_VCS (S : String) return VCS_Access;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
   begin
      if S = "" then
         return Unknown_VCS_Reference;
      else
         return null;
      end if;
   end Identify_VCS;

   ----------
   -- Name --
   ----------

   function Name (Ref : access Unknown_VCS_Record) return String is
      pragma Unreferenced (Ref);
   begin
      return "";
   end Name;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access Unknown_VCS_Record)
   is
      pragma Unreferenced (Ref);
   begin
      null;
   end Free;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
      File      : String;
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
     (Rep  : access Unknown_VCS_Record;
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
     (Rep  : access Unknown_VCS_Record;
      File : String)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Annotate;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Register_VCS_Identifier (Identify_VCS'Access);

      Unknown_VCS_Reference := new Unknown_VCS_Record;
   end Register_Module;

end VCS.Unknown_VCS;
