-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2008, AdaCore             --
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

package body Filesystem.Unix is

   -------------
   -- To_Unix --
   -------------

   function To_Unix
     (FS         : Unix_Filesystem_Record;
      Path       : String;
      Use_Cygwin : Boolean := False) return String
   is
      pragma Unreferenced (FS, Use_Cygwin);
   begin
      return Path;
   end To_Unix;

   ---------------
   -- From_Unix --
   ---------------

   function From_Unix
     (FS   : Unix_Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (FS);
   begin
      return Path;
   end From_Unix;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path
     (FS   : Unix_Filesystem_Record;
      Path : String) return Boolean
   is
      pragma Unreferenced (FS);
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute_Path;

   --------------
   -- Get_Root --
   --------------

   function Get_Root
     (FS   : Unix_Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (FS, Path);
   begin
      return "/";
   end Get_Root;

   -----------------
   -- Device_Name --
   -----------------

   function Device_Name
     (FS   : Unix_Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (Path, FS);
   begin
      return "";
   end Device_Name;

   ----------
   -- Path --
   ----------

   function Path
     (FS : Unix_Filesystem_Record;
      Device : String;
      Dir    : String;
      File   : String) return String
   is
      pragma Unreferenced (FS, Device);
   begin
      return Dir & File;
   end Path;

   -----------------------
   -- Is_Case_Sensitive --
   -----------------------

   function Is_Case_Sensitive (FS : Unix_Filesystem_Record) return Boolean is
      pragma Unreferenced (FS);
   begin
      return True;
   end Is_Case_Sensitive;

   -----------------
   -- Has_Devices --
   -----------------

   function Has_Devices (FS : Unix_Filesystem_Record) return Boolean is
      pragma Unreferenced (FS);
   begin
      return False;
   end Has_Devices;

   ------------------------
   -- Get_Logical_Drives --
   ------------------------

   procedure Get_Logical_Drives
     (FS     : Unix_Filesystem_Record;
      Host   : String;
      Buffer : in out String;
      Len    : out Integer)
   is
      pragma Unreferenced (FS, Host);
   begin
      Buffer := (others => ' ');
      Len := 0;
   end Get_Logical_Drives;

end Filesystem.Unix;
