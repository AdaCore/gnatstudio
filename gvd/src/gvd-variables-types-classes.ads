------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

--  Items used for simple types, ie whose value is a simple string.
--  See the package Items for more information on all the private subprograms.

package GVD.Variables.Types.Classes is

   -------------
   -- Classes --
   -------------

   type GVD_Class_Type (<>) is new GVD_Generic_Type with private;
   type GVD_Class_Type_Access is access all GVD_Class_Type'Class;
   --  This type represents an object, such as a C++ class, an Ada tagged type
   --  or a Java object.
   --  It can have one or more ancestors, whose contents is also displayed when
   --  the value of the variable is shown.

   function New_Class_Type
     (Num_Ancestors : Natural) return GVD_Type_Holder;
   --  Create a new class type, with a specific number of ancestors (parent
   --  classes).

   procedure Add_Ancestor
     (Self     : not null access GVD_Class_Type;
      Num      : Positive;
      Ancestor : GVD_Type_Holder);
   --  Define one of the ancestors of item.
   --  When the value of item, its components are parsed in the following
   --  order: first, all the fields of the first ancestor, then all the fields
   --  of the second ancestor, ..., then the fields of Item.
   --  No copy of Ancestor is made, we just keep the pointer.

   procedure Set_Child
     (Self  : not null access GVD_Class_Type;
      Child : GVD_Type_Holder);
   --  Record the child component of Item (where the fields of Item are
   --  defined).

   function Get_Child
     (Self : not null access GVD_Class_Type)
      return GVD_Type_Holder;
   --  Return a pointer to the child.

   function Get_Ancestor
     (Self : not null access GVD_Class_Type;
      Num  : Positive)
      return GVD_Type_Holder;
   --  Return a pointer to the Num-th ancestor.

   function Get_Num_Ancestors
     (Self : not null access GVD_Class_Type)
      return Natural;
   --  Return the number of ancestors.

   function Get_Value_Command
     (Self          : not null access GVD_Class_Type;
      Unused_Entity : String)
      return String is ("");
   --  Returns command for retrieving value

   procedure Set_Value
     (Self  : not null access GVD_Class_Type;
      Value : String) is null;
   --  Parse and set value

private

   type Class_Type_Array is array (Positive range <>) of GVD_Type_Holder;

   type GVD_Class_Type (Num_Ancestors : Natural) is
     new GVD_Generic_Type with record
      Child          : GVD_Type_Holder;
      Ancestors      : Class_Type_Array (1 .. Num_Ancestors) :=
        (others => Empty_GVD_Type_Holder);
   end record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Class_Type) return String is ("Class");

   overriding procedure Clear (Self : not null access GVD_Class_Type);

   overriding procedure Free (Self : not null access GVD_Class_Type);

   overriding procedure Clone
     (Self : not null access GVD_Class_Type;
      Item : not null GVD_Generic_Type_Access);

   overriding function Replace
     (Self         : not null access GVD_Class_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class;

   overriding procedure Set_Type_Name
     (Self : not null access GVD_Class_Type;
      Name : String);

   overriding function Get_Type_Name
     (Self : not null access GVD_Class_Type)
      return String;

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Class_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   overriding function Start
     (Self : not null access GVD_Class_Type) return Generic_Iterator'Class;

end GVD.Variables.Types.Classes;
