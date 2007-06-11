-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2006                      --
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

with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Console; use GPS.Kernel.Console;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with VCS_Module;         use VCS_Module;
with VFS;                use VFS;

package body VCS.Unknown_VCS is

   Unknown_VCS_Name : constant String := "Unknown VCS";

   use String_List;

   function Identify_VCS (S : String) return VCS_Access;
   --  Utility function to identify the Unknown VCS from a given string

   procedure Error (File : String);
   --  Convenience function to display an error message in the console

   -----------
   -- Error --
   -----------

   procedure Error (File : String) is
   begin
      Insert
        (Unknown_VCS_Reference.Kernel,
         -"Warning: no VCS set in project properties for " & File);
   end Error;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
   begin
      if S = ""
        or else S = Unknown_VCS_Name
      then
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
      return Unknown_VCS_Name;
   end Name;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access Unknown_VCS_Record) is
      pragma Unreferenced (Ref);
   begin
      null;
   end Free;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep        : access Unknown_VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      pragma Unreferenced (Rep, Clear_Logs);
   begin
      if Filenames /= Null_List
        and then not Local
      then
         Error (Head (Filenames));
      end if;
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
     (Rep       : access Unknown_VCS_Record;
      Dir       : VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean)
   is
      pragma Unreferenced (Rep, Tag, As_Branch);
   begin
      if Dir /= No_File then
         Error (Base_Name (Dir));
      end if;
   end Create_Tag;

   ----------
   -- Open --
   ----------

   procedure Open
     (Rep       : access Unknown_VCS_Record;
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
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Log       : String)
   is
      pragma Unreferenced (Rep, Log);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Update;

   ------------
   -- Switch --
   ------------

   procedure Switch
     (Rep : access Unknown_VCS_Record;
      Dir : VFS.Virtual_File;
      Tag : String)
   is
      pragma Unreferenced (Rep, Tag);
   begin
      if Dir /= No_File then
         Error (Base_Name (Dir));
      end if;
   end Switch;

   --------------
   -- Resolved --
   --------------

   procedure Resolved
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Resolved;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Tag       : String)
   is
      pragma Unreferenced (Rep, Tag);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True)
   is
      pragma Unreferenced (Rep, Log, Commit);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True)
   is
      pragma Unreferenced (Rep, Log, Commit);
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
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames /= Null_List then
         Error (Head (Filenames));
      end if;
      null;
   end Revert;

   -------------------
   -- File_Revision --
   -------------------

   procedure File_Revision
     (Rep      : access Unknown_VCS_Record;
      File     : VFS.Virtual_File;
      Revision : String)
   is
      pragma Unreferenced (Rep, File, Revision);
   begin
      null;
   end File_Revision;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access Unknown_VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      pragma Unreferenced (Rep, File, Version_2, Version_1);
   begin
      null;
   end Diff;

   ----------------
   -- Diff_Patch --
   ----------------

   procedure Diff_Patch
     (Rep    : access Unknown_VCS_Record;
      File   : VFS.Virtual_File;
      Output : VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File, Output);
   begin
      null;
   end Diff_Patch;

   --------------------
   -- Diff_Base_Head --
   --------------------

   procedure Diff_Base_Head
     (Rep  : access Unknown_VCS_Record;
      File : VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Diff_Base_Head;

   ------------------
   -- Diff_Working --
   ------------------

   procedure Diff_Working
     (Rep  : access Unknown_VCS_Record;
      File : VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Diff_Working;

   --------------
   -- Diff_Tag --
   --------------

   procedure Diff_Tag
     (Rep      : access Unknown_VCS_Record;
      File     : VFS.Virtual_File;
      Tag_Name : String)
   is
      pragma Unreferenced (Rep, File, Tag_Name);
   begin
      null;
   end Diff_Tag;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep     : access Unknown_VCS_Record;
      File    : VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True)
   is
      pragma Unreferenced (Rep, Rev, As_Text);
   begin
      Error (Full_Name (File).all);
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access Unknown_VCS_Record;
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      --  ??? Where should this be unregistered. Memory leak otherwise.
      Register_VCS_Identifier (Identify_VCS'Access);
      Register_VCS (Module_ID (VCS_Module_ID), "");

      --  ??? Where is this freed. Memory leak
      Unknown_VCS_Reference := new Unknown_VCS_Record;
      Unknown_VCS_Reference.Kernel := Kernel_Handle (Kernel);
   end Register_Module;

end VCS.Unknown_VCS;
