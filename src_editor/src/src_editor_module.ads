-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Src_Editor_Box;
with Glide_Kernel;
with Gtkada.MDI;

package Src_Editor_Module is

   Src_Editor_Module_Id : Glide_Kernel.Module_ID;
   Src_Editor_Module_Name : constant String := "Source_Editor";

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   function Find_Current_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Return the source editor that has currently the focus in the MDI.
   --  If the focus in the MDI is not set on a source editor, then the top most
   --  editor is returned.

   function Find_Editor
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String) return Gtkada.MDI.MDI_Child;
   --  Return the first child that contains an editor that edits file.
   --  null is returned if there are no such editor

   function Get_Source_Box_From_MDI
     (Child : Gtkada.MDI.MDI_Child) return Src_Editor_Box.Source_Editor_Box;
   --  Return the source editor contained in a MDI_Child. Constraint_Error if
   --  Child doesn't contain an editor. null is returned Child is null.

end Src_Editor_Module;
