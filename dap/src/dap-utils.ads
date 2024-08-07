------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2024, AdaCore                  --
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

with Ada.Strings.UTF_Encoding;

with GNATCOLL.VFS;   use GNATCOLL.VFS;

with VSS.Strings.Conversions;

with GPS.Kernel;

package DAP.Utils is

   procedure Highlight_Current_File_And_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer);
   --  Mark current debugging line in editors

   procedure Unhighlight_Current_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Clear current debugging line in editors

   procedure Goto_Location
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer);
   --  Open the file and set the cursor on the line.

   function To_UTF8
     (Item : VSS.Strings.Virtual_String'Class)
      return Ada.Strings.UTF_Encoding.UTF_8_String
      renames VSS.Strings.Conversions.To_UTF_8_String;

   function To_UTF_8_String
     (Item : VSS.Strings.Virtual_String'Class)
      return Ada.Strings.UTF_Encoding.UTF_8_String
      renames VSS.Strings.Conversions.To_UTF_8_String;

   function To_Virtual_String
     (Item : Ada.Strings.UTF_Encoding.UTF_8_String)
      return VSS.Strings.Virtual_String
      renames VSS.Strings.Conversions.To_Virtual_String;

   function To_File
     (Item : VSS.Strings.Virtual_String'Class)
      return GNATCOLL.VFS.Virtual_File;

end DAP.Utils;
