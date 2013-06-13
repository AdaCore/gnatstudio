------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Docgen3.Backend.Simple;  use Docgen3.Backend.Simple;

package body Docgen3.Backend is

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory
     (Kernel : Core_Kernel) return Virtual_File
   is
      Project  : Project_Type renames Kernel.Registry.Tree.Root_Project;
      Attr     : constant String :=
                   Project.Attribute_Value (Documentation_Dir_Attribute);
      Base_Dir : Virtual_File;

   begin
      if Attr /= "" then
         Base_Dir := Create_From_Base (+Attr);
         Base_Dir.Ensure_Directory;

         return Base_Dir;
      end if;

      if Project.Object_Dir /= No_File then
         Base_Dir := Project.Object_Dir;
      else
         Base_Dir := Project.Project_Path.Get_Parent;
      end if;

      return Create_From_Dir (Base_Dir, +"doc/");
   end Get_Doc_Directory;

   -----------------
   -- New_Backend --
   -----------------

   function New_Backend return Docgen3_Backend'Class is
      pragma Warnings (Off);
      Obj : Simple_Backend;
      pragma Warnings (On);
   begin
      return Obj;
   end New_Backend;

end Docgen3.Backend;
