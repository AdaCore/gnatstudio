-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
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

private package GNATStack.Module.Editors is

   procedure Show_Stack_Usage
     (Module : not null access GNATStack_Module_Id_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Shows stack usage information in the specified file.

   procedure Hide_Stack_Usage
     (Module : not null access GNATStack_Module_Id_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Hides stack usage information in the specified file. Do nothing when
   --  specified file is No_File.

   procedure Show_Stack_Usage_In_Opened_Editors
     (Module : not null access GNATStack_Module_Id_Record'Class);
   --  Shows stack usage information is all opened editors.

   procedure Register_Module
     (Module : not null access GNATStack_Module_Id_Record'Class);
   --  Register preferences, styles, hooks for editor integration.

end GNATStack.Module.Editors;
