-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Main;
with GNAT.Expect;
with GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;

package Glide_Kernel.Timeout is

   type Process_Data is record
      Kernel     : Kernel_Handle;
      Descriptor : GNAT.Expect.Process_Descriptor_Access;
      Name       : GNAT.OS_Lib.String_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (GNAT.Expect.Process_Descriptor'Class,
      GNAT.Expect.Process_Descriptor_Access);

   package Process_Timeout is new Gtk.Main.Timeout (Process_Data);

end Glide_Kernel.Timeout;
