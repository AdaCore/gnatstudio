-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Src_Info;
with Prj;
with Prj.Tree;
with Language_Handlers;

package Docgen.ALI_Utils is

   --  ??? This whole package should be replaced by Glide_Kernel.Project and
   --  Prj_API

   procedure Load_Project
     (Name : String;
      Handler      : access Language_Handlers.Language_Handler_Record'Class;
      Project_Tree : out Prj.Tree.Project_Node_Id;
      Project_View : out Prj.Project_Id);
   --  Load a project file

   function Predefined_Source_Path return String;
   function Predefined_Object_Path return String;
   --  Return the predefined source paths for the current compiler

   procedure Load_LI_File
     (Source_Info_List : in out Src_Info.LI_File_List;
      Handler          : Language_Handlers.Language_Handler;
      Project_View     : Prj.Project_Id;
      Source_Filename  : String;
      LI               : out Src_Info.LI_File_Ptr);
   --  Find, Load and Parse the LI file for the corresponding source
   --  file

   function Create_Lang_Handler return Language_Handlers.Language_Handler;
   --  Create a language handler

end Docgen.ALI_Utils;
