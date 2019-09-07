------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package provides an annotation container - this is used to efficiently
--  add annotation without having to carry heavy containers. On the other hand,
--  it's not suitable for large number of annotations.

--  It is generic in order to allow the creation of incompatible annotations
--  types.

generic
package Annotations is

   type Annotation_Kind is
     (Nothing, Integer_Kind, Boolean_Kind, Other_Kind);

   type General_Annotation_Record is tagged null record;
   --  A general annotation record has to be derived by the implemented in
   --  order to store complex data structure.

   procedure Free
     (Obj : in out General_Annotation_Record) is null;
   --  This function will be called when the enclosing annotation if freed.
   --  The implementer is responsible of doing all the necessary deallocations
   --  here.

   type General_Annotation is access all General_Annotation_Record'Class;

   type Annotation (Kind : Annotation_Kind := Nothing) is record
      case Kind is
         when Nothing =>
            null;

         when Integer_Kind =>
            Integer_Val : Integer;

         when Boolean_Kind =>
            Boolean_Val : Boolean;

         when Other_Kind =>
            Other_Val : General_Annotation;
            --  This value will be freed when the tree containing the object
            --  is freed.
      end case;
   end record;

   procedure Free (Annot : in out Annotation);
   --  Free the given annotation, and its subsequent Other_Val if relevant.

   Null_Annotation : constant Annotation := (Kind => Nothing);

   type Annotation_Key is private;
   --  An annotation key is used to store and retreive an annotation from the
   --  container. Only one annotation can be stored in a given tree.

   Null_Annotation_Key : constant Annotation_Key;

   type Annotation_Key_Registry is private;
   --  A key registry holds the map of the created annotations keys, and
   --  ensures that the new created keys won't conflict with existing ones.
   --  All keys used for a given annotation container should come from a unique
   --  annotation key registry.

   function Create_Annotation_Key_Registry return Annotation_Key_Registry;
   --  Creates a new annotation key registry.

   procedure Free (Registry : in out Annotation_Key_Registry);
   --  Free the memory associated to this key registry.

   Null_Annotation_Key_Registry : constant Annotation_Key_Registry;

   procedure Get_Annotation_Key
     (Registry : in out Annotation_Key_Registry;
      New_Key  : out Annotation_Key);
   --  Creates a new key out of this registry.

   type Annotation_Container is private;
   --  Container for annoations. This container is potentially using a node for
   --  each annotation, even if they are not actually set. Accesses are
   --  extremely efficient, but if too many keys have been created from the
   --  registry, then memory will get lost.

   Null_Annotation_Container : constant Annotation_Container;

   procedure Free (Container : in out Annotation_Container);
   --  Free the memory associated to this container and its annotations.

   procedure Get_Annotation
     (Container : Annotation_Container;
      Key       : Annotation_Key;
      Result    : out Annotation);
   pragma Inline (Get_Annotation);
   --  Return the annotation located at the key given in parameter. If such an
   --  annotation doesn't exist, return Null_Annotation.

   procedure Set_Annotation
     (Container : in out Annotation_Container;
      Key       : Annotation_Key;
      Value     : Annotation);
   --  Set the value of an annotation. If there's already an annotation stored
   --  here, it will first be freed.

   function Is_Set
     (Container : Annotation_Container; Key : Annotation_Key)
      return Boolean;
   --  Return true if the annotation at the key given in parameter is set.

   procedure Free_Annotation
     (Container : Annotation_Container; Key : Annotation_Key);
   --  Free the annotation located at the key given in parameter, and replace
   --  its value by Null_Annotation in the container.

   function Copy (Obj : Annotation) return Annotation;
   --  Performs a deep copy of the annotation, including the "Other_Val" if
   --  any.

private

   type Annotation_Key is new Integer;

   Null_Annotation_Key : constant Annotation_Key := 0;

   type Annotation_Array is
     array (Annotation_Key range <>) of Annotation;

   type Annotation_Array_Access is access Annotation_Array;

   type Annotation_Key_Registry_Record is record
      Last_Key : Annotation_Key := 0;
   end record;

   type Annotation_Key_Registry is
     access all Annotation_Key_Registry_Record;

   Null_Annotation_Key_Registry : constant Annotation_Key_Registry := null;

   type Annotation_Container is record
      Annotations : Annotation_Array_Access;
   end record;

   Null_Annotation_Container : constant Annotation_Container :=
     (Annotations => null);

end Annotations;
