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
with Language_Handlers;
with Projects.Registry;

package Docgen.ALI_Utils is

   --  ??? This whole package should be replaced by Glide_Kernel.Project and
   --  Prj_API

   procedure Load_Project
     (Name : String;
      Registry     : in out Projects.Registry.Project_Registry'Class;
      Project      : out Projects.Project_Type);
   --  Load a project file

   function Predefined_Source_Path return String;
   function Predefined_Object_Path return String;
   --  Return the predefined source paths for the current compiler

   procedure Load_LI_File
     (Source_Info_List : in out Src_Info.LI_File_List;
      Handler          : Language_Handlers.Language_Handler;
      Registry         : Projects.Registry.Project_Registry'Class;
      Source_Filename  : String;
      LI               : out Src_Info.LI_File_Ptr);
   --  Find, Load and Parse the LI file for the corresponding source
   --  file

   function Create_Lang_Handler
     (Registry : access Projects.Registry.Project_Registry'Class)
      return Language_Handlers.Language_Handler;
   --  Create a language handler

end Docgen.ALI_Utils;
