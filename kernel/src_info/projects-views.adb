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

with GPS.Core_Kernels;

package body Projects.Views is

   -----------------------------------
   -- Create_Project_View_Reference --
   -----------------------------------

   function Create_Project_View_Reference
     (Kernel  : not null access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Project : GNATCOLL.Projects.Project_Type)
      return Project_View_Reference is
   begin
      return
        (Kernel => Kernel,
         File   => Project.Project_Path);
   end Create_Project_View_Reference;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Self         : Project_View_Reference'Class;
      Attribute    : GNATCOLL.Projects.Attribute_Pkg_String;
      Index        : String := "";
      Default      : String := "";
      Use_Extended : Boolean := False) return String
   is
      Aux : constant GNATCOLL.Projects.Project_Type := Self.Get_Project_Type;

   begin
      if Aux /= No_Project then
         return Aux.Attribute_Value (Attribute, Index, Default, Use_Extended);

      else
         return "";
      end if;
   end Get_Attribute_Value;

   ----------------------
   -- Get_Project_Type --
   ----------------------

   function Get_Project_Type
     (Self : Project_View_Reference'Class)
      return GNATCOLL.Projects.Project_Type is
   begin
      if Self.Kernel /= null
        and then Self.File /= No_File
      then
         return Self.Kernel.Get_Project_Tree.Project_From_Path (Self.File);

      else
         return GNATCOLL.Projects.No_Project;
      end if;
   end Get_Project_Type;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Self      : Project_View_Reference'Class;
      Attribute : GNATCOLL.Projects.Attribute_Pkg_String;
      Index     : String := "") return Boolean
   is
      Aux : constant GNATCOLL.Projects.Project_Type := Self.Get_Project_Type;

   begin
      if Aux /= No_Project then
         return Aux.Has_Attribute (Attribute, Index);

      else
         return False;
      end if;
   end Has_Attribute;

   ----------
   -- Name --
   ----------

   function Name (Self : Project_View_Reference'Class) return String is
      Aux : constant GNATCOLL.Projects.Project_Type := Self.Get_Project_Type;

   begin
      if Aux /= No_Project then
         return Aux.Name;

      else
         return "";
      end if;
   end Name;

   ------------------
   -- Source_Files --
   ------------------

   function Source_Files
     (Self                     : Project_View_Reference'Class;
      Recursive                : Boolean := False;
      Include_Externally_Built : Boolean := True)
      return GNATCOLL.VFS.File_Array_Access
   is
      Aux : constant GNATCOLL.Projects.Project_Type := Self.Get_Project_Type;

   begin
      if Aux /= No_Project then
         return
           Aux.Source_Files
             (Recursive                => Recursive,
              Include_Externally_Built => Include_Externally_Built);

      else
         return null;
      end if;
   end Source_Files;

end Projects.Views;
