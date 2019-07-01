------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with GPS.Kernel;
with GPS.Editors; use GPS.Editors;

package Src_Editor_Module.Shell is

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the commands with the shells

   function Create_Editor_Mark
     (Script : access Scripting_Language_Record'Class;
      Mark   : Editor_Mark'Class) return Class_Instance;
   --  Return an instance of EditorMark encapsulating Mark

   function Create_Editor_Location
     (Script   : access Scripting_Language_Record'Class;
      Location : Editor_Location'Class) return Class_Instance;
   --  Return an instance of EditorLocation

   -------------------
   -- Find_All_Refs --
   -------------------

   type Find_All_Refs_Handler_Procedure is access procedure
     (Kernel   : Kernel_Handle;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Integer;
      Column   : Visible_Column_Type;
      Name     : String;
      Implicit : Boolean;
      In_File  : Virtual_File;
      Data     : Callback_Data_Access);

   procedure Find_All_Refs
     (Kernel   : Kernel_Handle;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Integer;
      Column   : Visible_Column_Type;
      Name     : String;
      Implicit : Boolean;
      In_File  : Virtual_File;
      Data     : Callback_Data_Access);
   --  This procedure calls GPS.Entities, e.g. redirecting
   --  Should be removed when we have totally switched to LSP

   type Refactoring_Rename_Handler_Procedure is access procedure
     (Kernel            : Kernel_Handle;
      File              : GNATCOLL.VFS.Virtual_File;
      Line              : Integer;
      Column            : Basic_Types.Visible_Column_Type;
      Name              : String;
      New_Name          : String;
      Make_Writable     : Boolean;
      Auto_Save         : Boolean;
      Rename_Primitives : Boolean);

   Find_All_Refs_Handler : Find_All_Refs_Handler_Procedure :=
     Find_All_Refs'Access;

   Refactoring_Rename_Handler : Refactoring_Rename_Handler_Procedure := null;

end Src_Editor_Module.Shell;
