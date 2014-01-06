------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with Basic_Types;
with Old_Entities;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Projects;
with Language.Tree.Database;

pragma Warnings (Off);
with GNAT.Expect.TTY;
pragma Warnings (On);

package ALI_Parser is
   package Old renames Old_Entities;

   function Create_ALI_Handler
     (Db           : Old.Entities_Database;
      Registry     : Projects.Project_Registry'Class;
      Lang_Handler :
         access Language.Tree.Database.Abstract_Language_Handler_Record'Class)
      return Old_Entities.LI_Handler;
   --  Create a new ALI handler

   type ALI_Handler_Record is new Old.LI_Handler_Record with record
      Db           : Old.Entities_Database;
      Registry     : Projects.Project_Registry;

      Lang_Handler : Language.Tree.Database.Abstract_Language_Handler;
      --  Field used to store the languages handler of the kernel; used to
      --  obtain the LI handler of entities imported from other languages
   end record;

   type ALI_Handler is access all ALI_Handler_Record'Class;
   --  Generic ALI handler. Can be overriden for e.g. GCC .gli files.

   overriding procedure Destroy (Handler : in out ALI_Handler_Record);
   overriding function Get_Name (LI : access ALI_Handler_Record) return String;
   overriding function Get_Source_Info
     (Handler               : access ALI_Handler_Record;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : Basic_Types.File_Error_Reporter := null)
      return Old.Source_File;
   overriding function Parse_All_LI_Information
     (Handler   : access ALI_Handler_Record;
      Project   : GNATCOLL.Projects.Project_Type)
      return Old.LI_Information_Iterator'Class;
   overriding function Generate_LI_For_Project
     (Handler      : access ALI_Handler_Record;
      Lang_Handler : access
        Language.Tree.Database.Abstract_Language_Handler_Record'Class;
      Project      : GNATCOLL.Projects.Project_Type;
      Errors       : GNATCOLL.Projects.Error_Report;
      Recursive    : Boolean := False)
      return Old.LI_Handler_Iterator'Class;
   --  See doc for inherited subprograms

   type ALI_Information_Iterator
     is new Old.LI_Information_Iterator with private;
   overriding procedure Free (Iter : in out ALI_Information_Iterator);
   overriding procedure Next
     (Iter   : in out ALI_Information_Iterator;
      Steps  : Natural := Natural'Last;
      Count  : out Natural;
      Total  : out Natural);
   --  See doc for inherited subprograms

private
   type ALI_Information_Iterator
     is new Old.LI_Information_Iterator with
      record
         Handler : ALI_Handler;
         Files   : GNATCOLL.VFS.File_Array_Access;  --  in current dir
         Current : Natural;            --  current file
         Project : GNATCOLL.Projects.Project_Type;
      end record;
end ALI_Parser;
