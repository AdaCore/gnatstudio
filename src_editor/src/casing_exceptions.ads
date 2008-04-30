-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2005                      --
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

--  Add support routines for the casing exceptions feature. It has services
--  to add or remove a casing exception and to initialize the casing
--  GPS feature (create menu, read/write casing XML files).

with Glib.Xml_Int;  use Glib.Xml_Int;
with GPS.Kernel;    use GPS.Kernel;
with Case_Handling; use Case_Handling;
with GNATCOLL.VFS;

package Casing_Exceptions is

   procedure Add_Exception (Ident : String);
   --  Add Ident into the case exception table

   procedure Add_Substring_Exception (Ident : String);
   --  Add Ident into the case exception table

   procedure Remove_Exception (Ident : String);
   --  Remove Ident from the case exception table

   procedure Remove_Substring_Exception (Ident : String);
   --  Remove Ident from the case exception table

   function Get_Case_Exceptions return Case_Handling.Casing_Exceptions;
   --  Return the current case exception table

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Initialize the casing support, must be called before other calls, it
   --  reads the user's casing_exceptions.xml files.

   procedure Casing_Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Customization routine for the casing feature, this is a callback to
   --  be used with a Register_Module.

end Casing_Exceptions;
