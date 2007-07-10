-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007, AdaCore             --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

--  This package defines the module for code analysis storage structure
--  It defines shell commands that allow to create graphical interfaces for
--  coverage informations inside GPS.

with Gdk.Pixbuf;
with GPS.Kernel;

package Code_Analysis_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module

   type Code_Analysis_Icons is record
      Prj_Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Subp_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;

end Code_Analysis_Module;
