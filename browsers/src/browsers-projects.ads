-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package implements the project hierarchy browsers

with Browsers.Canvas;
with Glide_Kernel;
with Prj.Tree;

package Browsers.Projects is

   procedure Examine_Project_Hierarchy
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      In_Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Project    : Prj.Tree.Project_Node_Id);
   --  Display the project hierarchy for Project in the canvas.

end Browsers.Projects;
