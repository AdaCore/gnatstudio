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

with Debugger; use Debugger;
with Generic_Values; use Generic_Values;

package Language.Debugger is

   type Language_Debugger is abstract new Language_Root with private;
   type Language_Debugger_Access is access all Language_Debugger'Class;

   procedure Set_Debugger
     (The_Language : access Language_Debugger;
      The_Debugger : Debugger_Access);
   --  Set the debugger associated with a language.

   function Get_Debugger
     (The_Language : access Language_Debugger) return Debugger_Access;
   --  Return the debugger associate with a language.

   procedure Parse_Type
     (Lang     : access Language_Debugger;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access) is abstract;
   --  Parse the type of Entity.
   --  Type_Str should contain the type as returned by the debugger.
   --  Entity is used to get the type of the fields or array items.

   procedure Parse_Value
     (Lang       : access Language_Debugger;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Values.Generic_Type_Access;
      Repeat_Num : out Positive) is abstract;
   --  Parse the value of an entity, for the Ada language.
   --  Type_Str should contain the value, as returned by the debugger itself.
   --  Repeat_Num is the number of times the item is repeated in the output.

private
   type Language_Debugger is abstract new Language_Root with record
      The_Debugger : Debugger_Access;
   end record;
end Language.Debugger;
