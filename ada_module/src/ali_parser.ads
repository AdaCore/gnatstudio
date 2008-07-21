-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2008, AdaCore              --
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

with Entities;
with GNATCOLL.VFS;
with Projects.Registry;

package ALI_Parser is

   function Create_ALI_Handler
     (Db       : Entities.Entities_Database;
      Registry : Projects.Registry.Project_Registry)
      return Entities.LI_Handler;
   --  Create a new ALI handler

   type ALI_Handler_Record is new Entities.LI_Handler_Record with record
      Db       : Entities.Entities_Database;
      Registry : Projects.Registry.Project_Registry;
   end record;
   type ALI_Handler is access all ALI_Handler_Record'Class;
   --  Generic ALI handler. Can be overriden for e.g. GCC .gli files.

   function Get_Name (LI : access ALI_Handler_Record) return String;
   function Get_Source_Info
     (Handler               : access ALI_Handler_Record;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : Entities.File_Error_Reporter := null)
      return Entities.Source_File;
   function Case_Insensitive_Identifiers
     (Handler : access ALI_Handler_Record) return Boolean;
   function Parse_All_LI_Information
     (Handler   : access ALI_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return Integer;
   function Generate_LI_For_Project
     (Handler      : access ALI_Handler_Record;
      Lang_Handler : access Entities.Abstract_Language_Handler_Record'Class;
      Project      : Projects.Project_Type;
      Errors       : Projects.Error_Report;
      Recursive    : Boolean := False)
      return Entities.LI_Handler_Iterator'Class;
   --  See doc for inherited subprograms

   function Get_ALI_Ext (LI : access ALI_Handler_Record) return String;
   --  Return the ali file extension (e.g. ".ali") for the given handler

   function Get_ALI_Filename
     (Handler   : access ALI_Handler_Record;
      Base_Name : String) return String;
   --  Return the most likely candidate for an ALI file, given a source name

end ALI_Parser;
