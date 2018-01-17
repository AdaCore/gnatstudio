------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

   procedure Hide_Stack_Usage_In_Opened_Editors
     (Module : not null access GNATStack_Module_Id_Record'Class);
   --  Hides stack usage information is all opened editors.

   procedure Register_Module
     (Module : not null access GNATStack_Module_Id_Record'Class);
   --  Register preferences, styles, hooks for editor integration.

end GNATStack.Module.Editors;
