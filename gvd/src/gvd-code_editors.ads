------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

--  This package provides various utilities to interface the debugger with
--  editors.

with GNATCOLL.VFS;
with GPS.Debuggers;            use GPS.Debuggers;
with GPS.Kernel;               use GPS.Kernel;

package GVD.Code_Editors is

   procedure Set_Current_File_And_Line
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Process   : access Base_Visual_Debugger'Class := null;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Highlight : Boolean := True);
   --  Set the file and line which the debugger is stopped at.
   --  Process might be null if no debugger is running
   --  Highlight line as current debugging line if Highlight parameter is true

   procedure Goto_Current_Line
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Process : not null access Base_Visual_Debugger'Class);
   --  Move the cursor to the current line for Process

   procedure Unhighlight_Current_Line
     (Kernel  : not null access Kernel_Handle_Record'Class);
   --  Unhighlight the current line in the editor if it is highlighted.
   --  Do nothing otherwise.

end GVD.Code_Editors;
