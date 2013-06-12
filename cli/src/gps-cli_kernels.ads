------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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
--  Kernel for CLI

with GPS.Core_Kernels;
with Projects;
with Xref;
with GPS.Messages_Windows;

package GPS.CLI_Kernels is

   type CLI_Kernel_Record is
     new GPS.Core_Kernels.Core_Kernel_Record with null record;

   type CLI_Kernel is access all CLI_Kernel_Record;

   overriding procedure Create_Registry
     (Self   : not null access CLI_Kernel_Record;
      Result : out Projects.Project_Registry_Access);

   overriding procedure Create_Database
     (Self   : not null access CLI_Kernel_Record;
      Result : out Xref.General_Xref_Database);

   overriding function Messages_Window
     (Self : not null access CLI_Kernel_Record)
      return GPS.Messages_Windows.Abstract_Messages_Window_Access;

end GPS.CLI_Kernels;
