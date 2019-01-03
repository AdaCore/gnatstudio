------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GPS.Intl;           use GPS.Intl;
with VCS_Module;         use VCS_Module;

package body VCS.Unknown_VCS is

   Unknown_VCS_Name : constant String := "Unknown VCS";

   procedure Error (File : Virtual_File);
   --  Convenience function to display an error message in the console

   -----------
   -- Error --
   -----------

   procedure Error (File : Virtual_File) is
   begin
      Insert
        (Unknown_VCS_Reference.Kernel,
         -"Warning: no VCS set in project properties for " &
         File.Display_Full_Name);
   end Error;

   ----------
   -- Name --
   ----------

   overriding function Name (Ref : access Unknown_VCS_Record) return String is
      pragma Unreferenced (Ref);
   begin
      return Unknown_VCS_Name;
   end Name;

   -----------------
   -- Require_Log --
   -----------------

   overriding function Require_Log
     (Ref : access Unknown_VCS_Record) return Boolean
   is
      pragma Unreferenced (Ref);
   begin
      return False;
   end Require_Log;

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

   overriding procedure Get_Status
     (Rep        : access Unknown_VCS_Record;
      Filenames  : File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False)
   is
      pragma Unreferenced (Rep, Clear_Logs);
   begin
      if Filenames'Length > 0
        and then not Local
      then
         Error (Filenames (Filenames'First));
      end if;
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   overriding function Local_Get_Status
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array)
      return File_Status_List.Vector
   is
      pragma Unreferenced (Rep);

      Result           : File_Status_List.Vector;
      Blank_Status     : File_Status_Record;
      Current_Status   : File_Status_Record := Blank_Status;
   begin
      for J in Filenames'Range loop
         Current_Status := Blank_Status;
         Current_Status.File := Filenames (J);

         File_Status_List.Append (Result, Current_Status);
      end loop;

      return Result;
   end Local_Get_Status;

   ----------------
   -- Create_Tag --
   ----------------

   overriding procedure Create_Tag
     (Rep       : access Unknown_VCS_Record;
      Dir       : GNATCOLL.VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean)
   is
      pragma Unreferenced (Rep, Tag, As_Branch);
   begin
      if Dir /= No_File then
         Error (Dir);
      end if;
   end Create_Tag;

   ----------
   -- Open --
   ----------

   overriding procedure Open
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array;
      User_Name : String := "")
   is
      pragma Unreferenced (Rep, User_Name);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Open;

   ------------
   -- Commit --
   ------------

   overriding procedure Commit
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array;
      Log       : String)
   is
      pragma Unreferenced (Rep, Log);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Commit;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Update;

   ------------
   -- Switch --
   ------------

   overriding procedure Switch
     (Rep : access Unknown_VCS_Record;
      Dir : GNATCOLL.VFS.Virtual_File;
      Tag : String)
   is
      pragma Unreferenced (Rep, Tag);
   begin
      if Dir /= No_File then
         Error (Dir);
      end if;
   end Switch;

   --------------
   -- Resolved --
   --------------

   overriding procedure Resolved
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Resolved;

   -----------
   -- Merge --
   -----------

   overriding procedure Merge
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array;
      Tag       : String)
   is
      pragma Unreferenced (Rep, Tag);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Merge;

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array;
      Log       : String;
      Commit    : Boolean := True)
   is
      pragma Unreferenced (Rep, Log, Commit);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Add;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array;
      Log       : String;
      Commit    : Boolean := True)
   is
      pragma Unreferenced (Rep, Log, Commit);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Remove;

   ------------
   -- Revert --
   ------------

   overriding procedure Revert
     (Rep       : access Unknown_VCS_Record;
      Filenames : File_Array)
   is
      pragma Unreferenced (Rep);
   begin
      if Filenames'Length > 0 then
         Error (Filenames (Filenames'First));
      end if;
   end Revert;

   -------------------
   -- File_Revision --
   -------------------

   overriding procedure File_Revision
     (Rep      : access Unknown_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Revision : String)
   is
      pragma Unreferenced (Rep, File, Revision);
   begin
      null;
   end File_Revision;

   ----------
   -- Diff --
   ----------

   overriding procedure Diff
     (Rep       : access Unknown_VCS_Record;
      File      : GNATCOLL.VFS.Virtual_File;
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

   overriding procedure Diff_Patch
     (Rep    : access Unknown_VCS_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Output : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File, Output);
   begin
      null;
   end Diff_Patch;

   --------------------
   -- Diff_Base_Head --
   --------------------

   overriding procedure Diff_Base_Head
     (Rep  : access Unknown_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Diff_Base_Head;

   ------------------
   -- Diff_Working --
   ------------------

   overriding procedure Diff_Working
     (Rep  : access Unknown_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Diff_Working;

   --------------
   -- Diff_Tag --
   --------------

   overriding procedure Diff_Tag
     (Rep      : access Unknown_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Tag_Name : String)
   is
      pragma Unreferenced (Rep, File, Tag_Name);
   begin
      null;
   end Diff_Tag;

   ---------
   -- Log --
   ---------

   overriding procedure Log
     (Rep     : access Unknown_VCS_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True)
   is
      pragma Unreferenced (Rep, Rev, As_Text);
   begin
      Error (File);
   end Log;

   --------------
   -- Annotate --
   --------------

   overriding procedure Annotate
     (Rep  : access Unknown_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Rep);
   begin
      Error (File);
   end Annotate;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Unknown_VCS_Reference := new Unknown_VCS_Record;
      Unknown_VCS_Reference.Kernel := Kernel_Handle (Kernel);

      Register_VCS (Unknown_VCS_Name, Unknown_VCS_Reference);
   end Register_Module;

end VCS.Unknown_VCS;
