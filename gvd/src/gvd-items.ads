------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

--  This type encapsulates information about an entity (from the source),
--  its type and current value (from the debugger), and display properties
--  (from views).

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Debugger;               use Debugger;
with Items;                  use Items;
with GVD.Process;            use GVD.Process;

package GVD.Items is

   type Item_Info is tagged record
      Varname      : Unbounded_String;       --  tree display varname
      Cmd          : Unbounded_String;       --  tree display `cmd`
      Entity       : Generic_Type_Access;    --  parsed type info
      Split_Lines  : Boolean;                --  for commands
      Auto_Refresh : Boolean := True;
      Mode         : Display_Mode := Value;  --  what to display
      Format       : Debugger.Value_Format := Debugger.Default_Format;
   end record;

   function Is_Same (Info1, Info2 : Item_Info) return Boolean;
   --  Whether the two parameters represent the same variable or command

   procedure Free (Self : in out Item_Info);
   --  Free the memory used by self

   function Is_A_Variable (Self : Item_Info) return Boolean
      is (Self.Varname /= "");
   --  Whether this item relates to a variable (as opposed to a gdb command)

   function Wrap_Debugger_Command
     (Cmd         : String;
      Split_Lines : Boolean := False) return Item_Info;
   function Wrap_Variable
     (Varname  : String;
      Format   : Debugger.Value_Format := Default_Format)
      return Item_Info;
   --  Two ways to create items: either they wrap a debugger command (for
   --  instance to show local variables), or an actual variable.
   --  If Split_Lines is true, then each of the line output by the debugger is
   --  displayed as a separate component.

   function Name (Self : Item_Info) return String
      is (if Self.Varname /= ""
          then To_String (Self.Varname) else To_String (Self.Cmd));
   --  Return the display name for this item

   procedure Update
     (Self     : in out Item_Info;
      Process  : not null access Visual_Debugger_Record'Class);
   --  Update type info and value of the item

   procedure Mark_As_Up_To_Date (Self : in out Item_Info);
   --  Mark the entity as up-to-date (i.e. no longer mark is in red in the
   --  views, as if it had just been updated).

   function Has_Address (Self : Item_Info) return Boolean
      is (Self.Auto_Refresh and then Self.Is_A_Variable);
   --  Whether we should check the address of Self when computing aliases.

   No_Item_Info : constant Item_Info := (others => <>);

end GVD.Items;
