------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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
--  Persistent reference to the project view safe for project reloading
--  operation. It can be resolved to GNATCOLL.Projects.Project_Type using
--  currently loaded project.

limited with GPS.Core_Kernels;

package Projects.Views is

   type Project_View_Reference is tagged private;
   --  Reference to the project view.

   Empty_Project_View_Reference : constant Project_View_Reference;

   function Get_Project_Type
     (Self : Project_View_Reference'Class)
      return GNATCOLL.Projects.Project_Type;
   --  Returns object of GNATCOLL.Projects.Project_Type type. Should be used
   --  with caution, all such objects will be invalidated on project
   --  reloading.
   --
   --  ??? This subprogram is in interest during migration to libgpr2 and
   --  need to be removed later.

   function Create_Project_View_Reference
     (Kernel  : not null access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Project : GNATCOLL.Projects.Project_Type) return Project_View_Reference;
   --  Creates reference to project view of given project.
   --
   --  ??? This subprogram is in interest during migration to libgpr2 and
   --  need to be removed later.

   function Name (Self : Project_View_Reference'Class) return String;
   --  Return the name of the project.

   function Source_Files
     (Self                     : Project_View_Reference'Class;
      Recursive                : Boolean := False;
      Include_Externally_Built : Boolean := True)
      return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of source files belonging to the project.
   --  If Recursive is False, only the direct sources of the project are
   --  returned. Otherwise, the sources from imported projects are returned as
   --  well.
   --
   --  The returned value must be freed by the user
   --
   --  The sources that are returned are not necessarily the ones that are used
   --  when compiling the root project, since some of them might be overridden
   --  by extending projects. Instead, they are the sources that would be used
   --  when compiling from Project ("gnatmake -PProject"). Base names of
   --  returned files may not be unique in case when root project is an
   --  aggregate project. For languages other than Ada multiple sources with
   --  same base name can also be returned.
   --  If Include_Externally_Built is False then source directories belonging
   --  to project marked "Externally_Built" will not be returned.

   function Has_Attribute
     (Self      : Project_View_Reference'Class;
      Attribute : GNATCOLL.Projects.Attribute_Pkg_String;
      Index     : String := "") return Boolean;
   --  True if the attribute was explicitly defined in the project through
   --      for Attribute (Index) use ...
   --  or  for Attribute use ...

   function Get_Attribute_Value
     (Self         : Project_View_Reference'Class;
      Attribute    : GNATCOLL.Projects.Attribute_Pkg_String;
      Index        : String := "";
      Default      : String := "";
      Use_Extended : Boolean := False) return String;
   --  Return the value for a string attribute.
   --  Default is returned if the attribute wasn't set by the user and
   --  has no default value.
   --  The corresponding attribute would have been set in the project as:
   --      for Attribute use "value";
   --  or
   --      for Attribute (Index) use "value";
   --
   --  If Use_Extended is true and the attribute is not defined in Project
   --  itself, then the attribute is looked up in the project extended by
   --  Project (if any).

private

   type Project_View_Reference is tagged record
      Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
   end record;

   Empty_Project_View_Reference : constant Project_View_Reference :=
                                    (Kernel => null,
                                     File   => GNATCOLL.VFS.No_File);

end Projects.Views;
