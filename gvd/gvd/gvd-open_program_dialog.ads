-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with GVD.Types; use GVD.Types;
with Open_Program_Pkg; use Open_Program_Pkg;

package GVD.Open_Program_Dialog is

   type GVD_Open_Program_Record is new Open_Program_Record with record
      Valid : Boolean;
   end record;
   type GVD_Open_Program is access all GVD_Open_Program_Record'Class;

   procedure Gtk_New (Open : out GVD_Open_Program);

   procedure Open_Program
     (Open       : in out GVD_Open_Program;
      Descriptor : out Program_Descriptor);
   --  Open a program window and launch a main loop until the ok or cancel
   --  button has been pressed.
   --  Open if null is set to the created window, that is hidden on return.
   --  If non null, Open_Program will show it instead of creating a new one.
   --  Return the program descriptor. If Launch is None,
   --  this means a cancellation from the user.
   --  Note that this is your responsibility to free the memory associated with
   --  Descriptor, using Free below.

   procedure Free (Descriptor : in out Program_Descriptor);
   --  Free the dynamic memory associated with program.

end GVD.Open_Program_Dialog;
