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

--  Items used for repeated types.
--  See the package Items for more information on all the private subprograms.

package Items.Repeats is

   ------------
   -- Repeat --
   ------------

   type Repeat_Type is new Generic_Type with private;
   type Repeat_Type_Access is access all Repeat_Type'Class;

   function New_Repeat_Type return Generic_Type_Access;

   function Get_Repeat_Num (Item : access Repeat_Type) return Integer;
   --  Return the number of times the item is repeated.

   procedure Set_Repeat_Num
     (Item : in out Repeat_Type;
      Num  : Integer);
   --  Set the repeat number of the item.

   function Get_Value (Item : Repeat_Type) return Generic_Type_Access;
   --  Return the repeated value.

   procedure Set_Value
     (Item  : in out Repeat_Type;
      Value : Generic_Type_Access);
   --  Set the repeated value.
   --  It does not copy Value itself.

private

   --  To handle the '0 <repeats .. times>' case.

   type Repeat_Type is new Generic_Type with record
      Repeat_Num       : Integer;
      Repeat_Str_Width : Glib.Gint := 0;
      Value            : Generic_Type_Access := null;
   end record;

   overriding procedure Print (Value : Repeat_Type; Indent : Natural := 0);
   overriding procedure Free
     (Item : access Repeat_Type;
      Only_Value : Boolean := False);
   overriding procedure Clone_Dispatching
     (Item  : Repeat_Type;
      Clone : in out Generic_Type_Access);

   overriding procedure Paint
     (Item    : in out Repeat_Type;
      Context : Drawing_Context;
      Cr      : Cairo.Cairo_Context;
      Lang    : Language.Language_Access;
      Mode    : Display_Mode;
      X, Y    : Glib.Gint := 0);

   overriding procedure Size_Request
     (Item           : in out Repeat_Type;
      Context        : Drawing_Context;
      Lang           : Language.Language_Access;
      Mode           : Display_Mode;
      Hide_Big_Items : Boolean := False);
   overriding function Get_Component_Name
     (Item : access Repeat_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String;
   overriding function Get_Component_Name
     (Item : access Repeat_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      Comp : Generic_Type_Access) return String;
   overriding function Get_Component
     (Item : access Repeat_Type;
      X, Y : Glib.Gint) return Generic_Type_Access;
   overriding function Replace
     (Parent       : access Repeat_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access;
   overriding function Structurally_Equivalent
     (Item1 : access Repeat_Type; Item2 : access Generic_Type'Class)
     return Boolean;

   type Repeat_Iterator is new Generic_Iterator with record
      Item   : Repeat_Type_Access;
      At_End : Boolean;
   end record;
   overriding function Start
     (Item : access Repeat_Type) return Generic_Iterator'Class;
   overriding procedure Next (Iter : in out Repeat_Iterator);
   overriding function At_End (Iter : Repeat_Iterator) return Boolean;
   overriding function Data
     (Iter : Repeat_Iterator) return Generic_Type_Access;

end Items.Repeats;
