-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with String_Utils;           use String_Utils;

package body Filesystem is

   ----------------
   -- Is_Subtree --
   ----------------

   function Is_Subtree
     (FS        : Filesystem_Record;
      Directory : String;
      Full_Path : String) return Boolean is
   begin
      --  Path length shall be greater or equal to directory length
      if Directory'Length > Full_Path'Length then
         return False;
      end if;

      --  Do not try to compare last character: on VMS, you will compare
      --  a closing bracket with a dot (disk:[path] with disk:[path.subpath])
      return Equal
        (Full_Path (Full_Path'First .. Full_Path'First + Directory'Length - 1),
         Directory,
         Is_Case_Sensitive (Filesystem_Record'Class (FS)));
   end Is_Subtree;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension
     (FS   : Filesystem_Record;
      Path : String) return String
   is
      pragma Unreferenced (FS);
   begin
      for J in reverse Path'Range loop
         if Path (J) = '.' then
            return Path (J + 1 .. Path'Last);
         end if;
      end loop;
      return "";
   end File_Extension;

   ------------
   -- Concat --
   ------------

   function Concat
     (FS   : Filesystem_Record;
      Root : String;
      Sub  : String) return String
   is
      pragma Unreferenced (FS);
   begin
      return Root & Sub;
   end Concat;

   ---------------------------
   -- Multi_Unit_Index_Char --
   ---------------------------

   function Multi_Unit_Index_Char
     (FS : Filesystem_Record) return Character
   is
      pragma Unreferenced (FS);
   begin
      return '~';
   end Multi_Unit_Index_Char;

end Filesystem;
