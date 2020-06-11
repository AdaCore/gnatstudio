------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2020, AdaCore                     --
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

with GNATCOLL.VFS;

with Completion;  use Completion;
with GPS.Editors; use GPS.Editors;
with GPS.Kernel;
with Language;

package Completion_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module.
   --  Note this isn't a real module, and therefore shouldn't be register by
   --  gps-main.adb, only by the source editor itself. There are just too
   --  many links with the rest of the source editor.

   type Completion_Manager_Factory_Type is access
     function (Kernel : not null GPS.Kernel.Kernel_Handle;
               File   : GNATCOLL.VFS.Virtual_File;
               Lang   : Language.Language_Access)
               return Completion_Manager_Access;
   --  Type for completion manager factories.
   --  This should return the completion manager that should handle the given
   --  file with the given associated language.

   procedure Set_Completion_Manager_Factory
     (Factory : Completion_Manager_Factory_Type);
   --  Set the completion manager factory that will be used when completion
   --  gets triggered.

   type Completion_Trigger_Chars_Func_Type is access
   function (Editor : Editor_Buffer'Class;
             C      : Character) return Boolean;
   --  Type for the function used to determine if the given character typed in
   --  Editor should trigger auto completion.
   --  Note that this function is only called for special characters that are
   --  not valid for identifiers in the editor's language (e.g: like '.' or ':'
   --  for C/C++).

   procedure Set_Completion_Trigger_Chars_Func
     (Func : Completion_Trigger_Chars_Func_Type);
   --  Set the function that should determine whether a character should
   --  trigger auto completion or not.

   function Get_Completion_Display return Completion_Display_Interface_Access;
   --  Return the completion display (i.e: the completion window) or null when
   --  there is no completion.

   procedure Reset_Completion_Data;
   --  Reset the current completion data. It should be called only when
   --  the user performs a completion operation.

   procedure Remove_Completion;
   --  Remove the completion window

   function In_Smart_Completion return Boolean;
   --  Return True if we are currently showing a smart completion window

end Completion_Module;
