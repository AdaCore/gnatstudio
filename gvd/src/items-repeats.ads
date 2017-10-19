------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

   overriding function Get_Type_Descr
     (Self : not null access Repeat_Type) return String is ("Repeat");
   overriding function Get_Simple_Value
     (Self : not null access Repeat_Type) return String;
   overriding function Get_Type_Name
     (Self    : access Repeat_Type;
      Lang    : Language.Language_Access) return String;
   overriding procedure Free
     (Item : access Repeat_Type;
      Only_Value : Boolean := False);
   overriding procedure Clone_Dispatching
     (Item  : Repeat_Type;
      Clone : in out Generic_Type_Access);
   overriding function Build_Display
     (Self : not null access Repeat_Type;
      Name : String;
      View : not null access GVD.Canvas.Debugger_Data_View_Record'Class;
      Lang : Language.Language_Access;
      Mode : GVD.Canvas.Display_Mode) return GVD.Canvas.Component_Item;
   overriding function Replace
     (Parent       : access Repeat_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access;
   overriding function Structurally_Equivalent
     (Item1 : access Repeat_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   overriding function Start
     (Item : access Repeat_Type) return Generic_Iterator'Class;

end Items.Repeats;
