------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Entities;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Projects;
with Language.Tree.Database;
with Language_Handlers;

pragma Warnings (Off);
with GNAT.Expect.TTY;
pragma Warnings (On);

package ALI_Parser is

   function Create_ALI_Handler
     (Db           : Entities.Entities_Database;
      Registry     : Projects.Project_Registry'Class;
      Lang_Handler : Language_Handlers.Language_Handler)
      return Entities.LI_Handler;
   --  Create a new ALI handler

   type ALI_Handler_Record is new Entities.LI_Handler_Record with record
      Db           : Entities.Entities_Database;
      Registry     : Projects.Project_Registry;

      Lang_Handler : Language_Handlers.Language_Handler;
      --  Field used to store the languages handler of the kernel; used to
      --  obtain the LI handler of entities imported from other languages

      Unmangle_Pd     : access GNAT.Expect.TTY.TTY_Process_Descriptor;
      --  Descriptor of process used to unmangle names; null if not required
      --  or not available.

      Launch_Unmangle_Subprocess : Boolean;
      --  Flag used to avoid retrying launching the process used to unmangle
      --  names when its associated program is not available in the system
   end record;

   type ALI_Handler is access all ALI_Handler_Record'Class;
   --  Generic ALI handler. Can be overriden for e.g. GCC .gli files.

   overriding procedure Destroy (Handler : in out ALI_Handler_Record);
   overriding function Get_Name (LI : access ALI_Handler_Record) return String;
   overriding function Get_Source_Info
     (Handler               : access ALI_Handler_Record;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : Entities.File_Error_Reporter := null)
      return Entities.Source_File;
   overriding function Case_Insensitive_Identifiers
     (Handler : access ALI_Handler_Record) return Boolean;
   overriding function Parse_All_LI_Information
     (Handler   : access ALI_Handler_Record;
      Project   : GNATCOLL.Projects.Project_Type)
      return Entities.LI_Information_Iterator'Class;
   overriding function Generate_LI_For_Project
     (Handler      : access ALI_Handler_Record;
      Lang_Handler : access
        Language.Tree.Database.Abstract_Language_Handler_Record'Class;
      Project      : GNATCOLL.Projects.Project_Type;
      Errors       : GNATCOLL.Projects.Error_Report;
      Recursive    : Boolean := False)
      return Entities.LI_Handler_Iterator'Class;
   --  See doc for inherited subprograms

   function Get_ALI_Ext
     (LI : access ALI_Handler_Record)
      return GNATCOLL.VFS.Filesystem_String;
   --  Return the ali file extension (e.g. ".ali") for the given handler

   function Get_ALI_Filename
     (Handler   : access ALI_Handler_Record;
      Base_Name : GNATCOLL.VFS.Filesystem_String)
      return GNATCOLL.VFS.Filesystem_String;
   --  Return the most likely candidate for an ALI file, given a source name

   type ALI_Information_Iterator
     is new Entities.LI_Information_Iterator with private;
   overriding procedure Free (Iter : in out ALI_Information_Iterator);
   overriding procedure Next
     (Iter  : in out ALI_Information_Iterator;
      Steps : Natural := Natural'Last;
      Count : out Natural;
      Total : out Natural);
   --  See doc for inherited subprograms

private
   type ALI_Information_Iterator
     is new Entities.LI_Information_Iterator with
      record
         Handler : ALI_Handler;
         Files   : GNATCOLL.VFS.File_Array_Access;  --  in current dir
         Current : Natural;            --  current file
         Project : GNATCOLL.Projects.Project_Type;
      end record;
end ALI_Parser;
