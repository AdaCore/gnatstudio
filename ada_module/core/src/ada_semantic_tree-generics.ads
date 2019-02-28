------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;

with Ada_Semantic_Tree.List_Resolver; use Ada_Semantic_Tree.List_Resolver;

package Ada_Semantic_Tree.Generics is

   procedure Register_Assistant (Db : Construct_Database_Access);
   --  This assistant has to be registered to the database before any of the
   --  queries in this file can work.

   type Instance_Info is private;
   --  Represents a generic context. The information store in this type can't
   --  be kept after a construct database update.

   Null_Instance_Info : constant Instance_Info;

   function "&"
     (Left, Right : Instance_Info)
      return Instance_Info;
   --  Return a new generic instance information resulting of the concatenation
   --  of the parameters. If one of the two parameters is null, then the other
   --  one will be returned without modifications.

   procedure Ref (This : Instance_Info);
   --  Adds 1 to the reference counter.

   procedure Unref (This : in out Instance_Info);
   --  Removes 1 from the reference counter. Frees the instance when reaches 0.

   type Generic_Instance_Array is array
     (Integer range <>) of Instance_Info;

   function Is_Generic_Instance (Entity : Entity_Access) return Boolean;
   --  Return true if the entity given in parameter is a generic instance

   function Get_Generic_Instance_Information
     (Entity : Entity_Access) return Instance_Info;
   --  Return the generic instance information corresponding to this instance.
   --  Return Null_Generic_Instance_Information if this is not an instance.
   --  This information is not persistent, and the data store will be invalid
   --  after changes done in the construct database.

   function Is_Viewed_From_Generic
     (Entity : Entity_Access; Visibility : Visibility_Context) return Boolean;
   --  Return true if the entity is viewed from its generic context. Otherwise,
   --  it's viewed from an instance, and will need to be processed with generic
   --  instance information.

   function Is_Generic_Entity (Entity : Entity_Access) return Boolean;
   --  Return true if the entity is declared in a generic entity.

   function Get_Actual_For_Generic_Param
     (Info   : Instance_Info; Formal : Entity_Access) return Entity_Access;
   --  Return the actual entity for this formal, according to the generic
   --  instance information

   function Get_Generic_Entity (Info : Instance_Info) return Entity_Access;
   --  Return the generic entity instantiated by this information
   --  ??? see where to remove calls to that guy...

   function Get_Generic_Entity (Info : Entity_Access) return Entity_View;
   --  Same as above, but computes only the generic package without retreiving
   --  the entire generic context.

   type Persistent_Instance_Info is private;
   --  Type for a generic instance information able to outlive database update.

   Null_Persistent_Instance_Info : constant Persistent_Instance_Info;

   function To_Persistent
     (Instance : Instance_Info) return Persistent_Instance_Info;
   --  Generate an persistent generic information from an active one. This
   --  can then be stored and kept across database updates.

   function To_Active
     (Instance : Persistent_Instance_Info) return Instance_Info;
   --  Generate an active generic information from a persistent one.

   function Is_Up_To_Date
     (This : Persistent_Instance_Info) return Boolean;
   --  Return true if the information is up to date, false if it points to
   --  certain entites that doesn't exist anymore.

   procedure Free (This : in out Persistent_Instance_Info);
   --  Frees the memory associated to this persistent entity information

private

   type Instance_Info_Record;
   type Persistent_Instance_Info_Record;

   type Instance_Info is
     access all Instance_Info_Record;
   type Persistent_Instance_Info is
     access all Persistent_Instance_Info_Record;

   package Generic_Info_List is new Ada.Containers.Doubly_Linked_Lists
     (Instance_Info);
   package Persistent_Generic_Info_List is new
     Ada.Containers.Doubly_Linked_Lists (Persistent_Instance_Info);

   type Instance_Info_Record is record
      Instance_Package : Entity_Access;
      Generic_Package  : Entity_Access;
      Resolver         : Actual_Parameter_Resolver_Access;
      Refs             : Integer := 0;

      Pre_Contexts  : Generic_Info_List.List;
      Post_Contexts : Generic_Info_List.List;
   end record;

   type Persistent_Instance_Info_Record is record
      Instance_Package : Entity_Persistent_Access;
      Generic_Package  : Entity_Persistent_Access;

      Pre_Contexts  : Persistent_Generic_Info_List.List;
      Post_Contexts : Persistent_Generic_Info_List.List;
   end record;

   Null_Instance_Info : constant Instance_Info :=
     null;

   Null_Persistent_Instance_Info : constant Persistent_Instance_Info := null;

end Ada_Semantic_Tree.Generics;
