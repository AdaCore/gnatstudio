------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Items.Records;

package Items.Classes is

   -------------
   -- Classes --
   -------------

   type Class_Type (<>) is new Generic_Type with private;
   type Class_Type_Access is access all Class_Type'Class;
   --  This type represents an object, such as a C++ class, an Ada tagged type
   --  or a Java object.
   --  It can have one or more ancestors, whose contents is also displayed when
   --  the value of the variable is shown.

   function New_Class_Type
     (Num_Ancestors : Natural) return Generic_Type_Access;
   --  Create a new class type, with a specific number of ancestors (parent
   --  classes).

   procedure Add_Ancestor
     (Item     : in out Class_Type;
      Num      : Positive;
      Ancestor : Class_Type_Access);
   --  Define one of the ancestors of item.
   --  When the value of item, its components are parsed in the following
   --  order: first, all the fields of the first ancestor, then all the fields
   --  of the second ancestor, ..., then the fields of Item.
   --  No copy of Ancestor is made, we just keep the pointer.

   procedure Set_Child
     (Item  : in out Class_Type;
      Child : Items.Records.Record_Type_Access);
   --  Record the child component of Item (where the fields of Item are
   --  defined).

   function Get_Child (Item : Class_Type) return Generic_Type_Access;
   --  Return a pointer to the child.

   function Get_Ancestor
     (Item : Class_Type;
      Num  : Positive) return Generic_Type_Access;
   --  Return a pointer to the Num-th ancestor.

   function Get_Num_Ancestors (Item : Class_Type) return Natural;
   --  Return the number of ancestors.

   procedure Draw_Border
     (Item : access Class_Type;
      Draw : Boolean := True);
   --  If Draw is True (the default for new items), a border is drawn around
   --  the item when it is displayed on the screen.

   overriding procedure Propagate_Width
     (Item  : in out Class_Type;
      Width : Glib.Gint);

private

   type Class_Type_Array is array (Positive range <>) of Class_Type_Access;

   type Class_Type (Num_Ancestors : Natural) is new Generic_Type with record
     Child     : Items.Records.Record_Type_Access;
     Border_Spacing : Glib.Gint := Items.Border_Spacing;
     Ancestors : Class_Type_Array (1 .. Num_Ancestors) := (others => null);
   end record;
   overriding procedure Print (Value : Class_Type; Indent : Natural := 0);
   overriding procedure Free
     (Item : access Class_Type;
      Only_Value : Boolean := False);
   overriding procedure Clone_Dispatching
     (Item  : Class_Type;
      Clone : in out Generic_Type_Access);
   overriding procedure Paint
     (Item    : in out Class_Type;
      Context : Drawing_Context;
      Cr      : Cairo.Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Glib.Gint := 0);
   overriding procedure Size_Request
     (Item           : in out Class_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False);
   overriding function Get_Component_Name
     (Item : access Class_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String;
   overriding function Get_Component_Name
     (Item : access Class_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      Comp : Generic_Type_Access) return String;
   overriding function Get_Component
     (Item : access Class_Type;
      X, Y : Glib.Gint) return Generic_Type_Access;
   overriding function Replace
     (Parent       : access Class_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access;
   overriding procedure Set_Type_Name
     (Item : access Class_Type;
      Name : String);
   overriding function Structurally_Equivalent
     (Item1 : access Class_Type; Item2 : access Generic_Type'Class)
     return Boolean;

   type Class_Iterator is new Generic_Iterator with record
      Item     : Class_Type_Access;
      Ancestor : Natural;
   end record;
   overriding function Start
     (Item : access Class_Type) return Generic_Iterator'Class;
   overriding procedure Next (Iter : in out Class_Iterator);
   overriding function At_End (Iter : Class_Iterator) return Boolean;
   overriding function Data (Iter : Class_Iterator) return Generic_Type_Access;

end Items.Classes;
