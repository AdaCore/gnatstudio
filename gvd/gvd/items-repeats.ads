-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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
      Repeat_Num : Integer;
      Value      : Generic_Type_Access := null;
   end record;

   procedure Print (Value : Repeat_Type; Indent : Natural := 0);
   procedure Free (Item : access Repeat_Type;
                   Only_Value : Boolean := False);
   function Clone (Value : Repeat_Type) return Generic_Type_Access;

   procedure Paint (Item    : in out Repeat_Type;
                    Context : Drawing_Context;
                    X, Y    : Glib.Gint := 0);

   procedure Size_Request
     (Item           : in out Repeat_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name (Item : access Repeat_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String;
   function Get_Component (Item : access Repeat_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access;
   function Replace
     (Parent       : access Repeat_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
     return Generic_Type_Access;
   procedure Reset_Recursive (Item : access Repeat_Type);

end Items.Repeats;
