-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada_Semantic_Tree.List_Resolver; use Ada_Semantic_Tree.List_Resolver;

package Ada_Semantic_Tree.Generics is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   type Generic_Instance_Information is private;

   Null_Generic_Instance_Information : constant Generic_Instance_Information;

   procedure Ref (This : in out Generic_Instance_Information);
   --  Adds 1 to the reference counter.

   procedure Unref (This : in out Generic_Instance_Information);
   --  Removes 1 from the reference counter. Frees the instance when reaches 0.

   type Generic_Instance_Array is array
     (Integer range <>) of Generic_Instance_Information;

   function Is_Generic_Instance (Entity : Entity_Access) return Boolean;
   --  Return true if the entity given in parameter is a generic instance

   function Get_Generic_Instance_Information
     (Entity : Entity_Access) return Generic_Instance_Information;
   --  Return the generic instance information corresponding to this instance.
   --  Return Null_Generic_Instance_Information if this is not an instance.
   --  This information is not persistent, and the data store will be invalid
   --  after changes done in the construct database.

   function Get_Generic_Instances
     (Entity     : Entity_Access;
      Visibility : Visibility_Context) return Generic_Instance_Array;
   --  Return the generic instances that can be found for this entity from
   --  the visibility information given in parameter.

   function Is_Viewed_From_Generic
     (Entity : Entity_Access; Visibility : Visibility_Context) return Boolean;
   --  Return true if the entity is viewed from its generic context. Otherwise,
   --  it's viewed from an instance, and will need to be processed with generic
   --  instance information.

   function Is_Generic_Entity (Entity : Entity_Access) return Boolean;
   --  Return true if the entity is declared in a generic entity.

   function Get_Actual_For_Generic_Param
     (Info   : Generic_Instance_Information;
      Formal : Entity_Access) return Entity_Access;
   --  Return the actual entity for this formal, according to the generic
   --  instance information

   function Get_Generic_Package
     (Info : Generic_Instance_Information)
      return Entity_Access;
   --  Return the generic package instantiated by this information

private

   type Generic_Instance_Information_Record is record
      Instance_Package : Entity_Access;
      Generic_Package  : Entity_Access;
      Resolver         : Actual_Parameter_Resolver_Access;
      Refs             : Integer := 0;
   end record;

   type Generic_Instance_Information is
     access all Generic_Instance_Information_Record;

   Null_Generic_Instance_Information : constant Generic_Instance_Information :=
     null;

end Ada_Semantic_Tree.Generics;
