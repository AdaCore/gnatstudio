-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with GNAT.Strings;

with Entities;     use Entities;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Language;     use Language;
with Docgen2.Tags;

package Docgen2.Entities is

   type Entity_Info_Category is
     (Cat_File,
      Cat_Package,
      Cat_Class,
      Cat_Task,
      Cat_Protected,
      Cat_Type,
      Cat_Variable,
      Cat_Parameter,
      Cat_Subprogram,
      Cat_Entry,
      Cat_Unknown);

   function Image (Cat : Entity_Info_Category) return String;
   function Image (Cat : Language_Category) return String;
   --  Returns a printable image of the category

   type Entity_Info_Record (Category : Entity_Info_Category := Cat_Unknown);

   type Entity_Info is access all Entity_Info_Record;
   --  This type fully describe an entity. This is the central docgen type that
   --  is filled during analysis, then used to generate the documentation.

   type Location_Type is record
      File_Loc : File_Location;
      Pkg_Nb   : Natural;
   end record;

   Null_Location : constant Location_Type :=
                     (File_Loc => No_File_Location,
                      Pkg_Nb   => 0);

   type Cross_Ref_Record;
   type Cross_Ref is access all Cross_Ref_Record;

   type Cross_Ref_Record is record
      Location      : File_Location;
      Name          : GNAT.Strings.String_Access;
      Xref          : Entity_Info := null;
      Inherited     : Boolean := False; -- Primitive operation cross-ref
      Overriding_Op : Cross_Ref;        -- Primitive operation cross-ref
   end record;

   package Cross_Ref_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Cross_Ref);

   package Entity_Info_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Entity_Info);

   package Entity_Ref_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Entity_Reference);

   package Files_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => GNATCOLL.VFS.Virtual_File);

   function Less_Than (Left, Right : Cross_Ref) return Boolean;
   function Less_Than_Short_Name (Left, Right : Entity_Info) return Boolean;
   function Less_Than_Full_Name (Left, Right : Entity_Info) return Boolean;
   function Less_Than (Left, Right : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Used to sort the children lists

   package Vector_Sort is new Cross_Ref_List.Generic_Sorting
     ("<" => Less_Than);

   package EInfo_Vector_Sort_Short is new Entity_Info_List.Generic_Sorting
     ("<" => Less_Than_Short_Name);

   package EInfo_Vector_Sort_Full is new Entity_Info_List.Generic_Sorting
     ("<" => Less_Than_Full_Name);

   package Files_Vector_Sort is new Files_List.Generic_Sorting
     ("<" => Less_Than);

   procedure Free (List : in out Cross_Ref_List.Vector);
   procedure Free (List : in out Entity_Info_List.Vector);
   --  Free memory used by List

   type Entity_Info_Record (Category : Entity_Info_Category := Cat_Unknown)
      is record
         Lang_Category        : Language_Category;
         --  Entity category as returned by the language package

         Name                 : GNAT.Strings.String_Access;
         --  Full name (with namespace)

         Short_Name           : GNAT.Strings.String_Access;
         --  Short name

         Description          : aliased Docgen2.Tags.Comment_Type;
         --  Associated comment

         Printout             : GNAT.Strings.String_Access;
         --  Code display

         Entity_Loc           : Source_Location;
         --  Location of the entity in line/column/index format

         Location             : Location_Type;
         --  Location of the entity in line/column/file/package number format

         Printout_Loc         : Source_Location;
         --  Printout location (includes preceding keywords as 'package',
         --  'procedure' and so on)

         Body_Location        : File_Location;
         --  Entity's body location in line/column/file format

         Is_Abstract          : Boolean := False;
         Is_Private           : Boolean := False;
         Is_Generic           : Boolean := False;
         Is_Renaming          : Boolean := False;
         Is_Instantiation     : Boolean := False;
         Is_Partial           : Boolean := False;
         --  Entity flags.

         Generic_Params       : Entity_Info_List.Vector;
         --  For generics, xref to the list of generic parameters

         Renamed_Entity       : Cross_Ref := null;
         --  For renamings, xref to the renamed entity

         Instantiated_Entity  : Cross_Ref := null;
         --  For instantiations, xref to their instantiated entity

         Full_Declaration     : Cross_Ref := null;
         --  For partial entities, xref to their full declarations

         Hidden               : Boolean := False;
         --  Mark the entity information as not shown in the final document
         --  This is used in particular for partial views of a type, to
         --  determine if the full view or partial view should be used.

         Children             : Entity_Info_List.Vector;
         --  For tagged types, the list of immediate children

         References           : Entity_Ref_List.Vector;
         Calls                : Cross_Ref_List.Vector;
         Called               : Cross_Ref_List.Vector;
         --  Lists for call graph

         case Category is
            when Cat_Package | Cat_File =>
               Language       : Language_Access;
               --  File language

               File           : Source_File;
               --  The file

               Pkg_Nb         : Natural;
               --  Package id in this file

            when Cat_Task | Cat_Protected =>
               Is_Type        : Boolean;
               --  Tells if this is a type definition or actual tasks/protected
               --  objects

            when Cat_Class =>
               Parents        : Cross_Ref_List.Vector;
               --  List of all parents
               Class_Children : Cross_Ref_List.Vector;
               --  List of immediate children
               Primitive_Ops  : Cross_Ref_List.Vector;
               --  List of primitive operations

            when Cat_Variable =>
               Variable_Type  : Cross_Ref := null;
               --  The variable's type definition

            when Cat_Parameter =>
               Parameter_Type : Cross_Ref := null;
               --  The parameter's type definition

            when Cat_Subprogram | Cat_Entry =>
               Return_Type    : Cross_Ref := null;
               --  The returned type definition

            when others =>
               null;
         end case;
      end record;

   function Hash (Key : File_Location) return Ada.Containers.Hash_Type;
   function Equivalent_Keys (Left, Right : File_Location)
                             return Boolean;
   package Entity_Info_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (File_Location, Entity_Info, Hash, Equivalent_Keys);
   --  A hashed set of nodes, identified by their 'loc' attribute

   function To_Category (Category : Language_Category)
                         return Entity_Info_Category;
   --  Translate language category into entity_info_category

   procedure Set_Printout
     (Construct   : Simple_Construct_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info);
   --  Retrieve the Source extract representing the construct, and
   --  set the printout field of E_Info

   procedure Set_Pkg_Printout
     (Construct   : Simple_Construct_Information;
      Entity      : Entity_Information;
      File_Buffer : GNAT.Strings.String_Access;
      E_Info      : Entity_Info);
   --  Retrieve the Source extract representing the header of the package, or
   --  the full construct if the package is an instantiation or a renaming.

end Docgen2.Entities;
