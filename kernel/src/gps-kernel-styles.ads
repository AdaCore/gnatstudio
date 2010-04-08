-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2005-2010, AdaCore                 --
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

--  This package implements the kernel functions needed to retrieve registered
--  styles or load/save styles to disk.

with String_Hash;

with GPS.Styles; use GPS.Styles;

package GPS.Kernel.Styles is

   procedure Save_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Save the currently registered Styles to File.

   procedure Load_Styles
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Load the currently registered Styles from File.

   function Get_Or_Create_Style
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Create : Boolean := True) return Style_Access;
   --  Lookup the style Name.
   --  If it doesn't exist and Create = True, then create it.

   function Get_Or_Create_Style_Copy
     (Kernel     : Kernel_Handle;
      Name       : String;
      From_Style : Style_Access) return Style_Access;
   --  Lookups the style Name. Creates new style as copy of Style when it
   --  doesn't exists.

private
   package Style_Htable is new String_Hash (Style_Access, Free, null);

   type Style_Htable_Record is new Root_Table with record
      Table : Style_Htable.String_Hash_Table.Instance;
   end record;
   type Style_Htable_Access is access all Style_Htable_Record'Class;

   overriding procedure Reset (X : access Style_Htable_Record);
   --  Reset the table.

end GPS.Kernel.Styles;
