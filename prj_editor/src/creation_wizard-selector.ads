-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
--                            AdaCore                                --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a dialog to help the user choose among the
--  several project wizards provided by GPS

with GPS.Kernel;
with Glib.Object;

package Creation_Wizard.Selector is

   function Create_New_Project
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean;
   --  Open a new dialog that lists all the possible wizards provided by
   --  GPS. The user can then immediately start creating a project.
   --  Return True if a project could be created, False if there was an error
   --  or the user cancelled the operation.

   procedure On_New_Project
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Same as above, but suitable for use from a callback.

end Creation_Wizard.Selector;
