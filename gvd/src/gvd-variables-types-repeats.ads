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

--  Items used for repeated types.
--  See the package Items for more information on all the private subprograms.

package GVD.Variables.Types.Repeats is

   ------------
   -- Repeat --
   ------------

   type GVD_Repeat_Type is new GVD_Generic_Type with private;
   type GVD_Repeat_Type_Access is access all GVD_Repeat_Type'Class;

   function New_Repeat_Type return GVD_Type_Holder;

   function Get_Repeat_Num
     (Self : not null access GVD_Repeat_Type)
      return Integer;
   --  Return the number of times the item is repeated.

   procedure Set_Repeat_Num
     (Self : not null access GVD_Repeat_Type;
      Num  : Integer);
   --  Set the repeat number of the item.

   function Get_Value
     (Self : not null access GVD_Repeat_Type)
      return GVD_Type_Holder;
   --  Return the repeated value.

   procedure Set_Value
     (Self  : not null access GVD_Repeat_Type;
      Value : GVD_Type_Holder);
   --  Set the repeated value.
   --  It does not copy Value itself.

private

   --  To handle the '0 <repeats .. times>' case.

   type GVD_Repeat_Type is new GVD_Generic_Type with record
      Repeat_Num       : Integer         := 0;
      Repeat_Str_Width : Glib.Gint       := 0;
      Value            : GVD_Type_Holder := Empty_GVD_Type_Holder;
   end record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Repeat_Type) return String is ("Repeat");

   overriding function Get_Simple_Value
     (Self : not null access GVD_Repeat_Type) return String;

   overriding function Get_Type_Name
     (Self : not null access GVD_Repeat_Type) return String;

   overriding procedure Clone
     (Self : not null access GVD_Repeat_Type;
      Item : not null GVD_Generic_Type_Access);

   overriding procedure Free
     (Self : not null access GVD_Repeat_Type);

   overriding function Replace
     (Self         : not null access GVD_Repeat_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class;

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Repeat_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   overriding function Start
     (Self : not null access GVD_Repeat_Type)
      return Generic_Iterator'Class;

end GVD.Variables.Types.Repeats;
