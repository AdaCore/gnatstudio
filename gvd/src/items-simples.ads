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

--  Items used for simple types, ie whose value is a simple string.
--  See the package Items for more information on all the private subprograms.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Items.Simples is

   ------------------
   -- Simple Types --
   ------------------

   type Simple_Type is new Generic_Type with private;
   type Simple_Type_Access is access all Simple_Type'Class;
   --  For simple values, like integers and floats, whose value we don't need
   --  to analyze.

   function New_Simple_Type return Generic_Type_Access;
   --  Create a new simple value.

   function Get_Value (Item : Simple_Type) return Unbounded_String;
   --  Return the current value of Item (or null if it is not known).

   procedure Set_Value (Item : in out Simple_Type; Value : String);
   --  Assign a new value to Item.

   -----------------
   -- Range Types --
   -----------------

   type Range_Type is new Simple_Type with private;
   type Range_Type_Access is access all Range_Type'Class;
   --  A range value, as in Ada 'range 0 .. 10', ie a number between the min
   --  and max value.

   function New_Range_Type
     (Min, Max : Long_Integer) return Generic_Type_Access;
   --  Create a new range item, whose specific range is given in parameter.

   ---------------
   -- Mod Types --
   ---------------

   type Mod_Type is new Simple_Type with private;
   type Mod_Type_Access is access all Mod_Type'Class;
   --  A modular type whose values are module a given value.

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access;
   --  Create a new mod type which a given modulo value.

   ------------------
   -- Access Types --
   ------------------

   type Access_Type is new Simple_Type with private;
   type Access_Type_Access is access all Access_Type'Class;
   --  A simple address value, that can be derefenced by the user.

   function New_Access_Type return Generic_Type_Access;
   --  Create a new access type.

   ----------------
   -- Enum Types --
   ----------------

   type Enum_Type is new Simple_Type with private;
   type Enum_Type_Access is access all Enum_Type'Class;
   --  An enumeration value, ie an item whose value must be in a set of given
   --  elements.
   --  Currently, the set of possible values is not stored in the item.

   function New_Enum_Type return Generic_Type_Access;
   --  Create a new enum type.

   --------------------
   -- Debugger types --
   --------------------

   type Debugger_Output_Type is new Generic_Type with private;
   type Debugger_Output_Type_Access is access all Debugger_Output_Type'Class;
   --  General types, used to display directly the output of the debugger
   --  (no processing is done in that case).

   function New_Debugger_Type
     (Cmd         : String;
      Split_Lines : Boolean := False) return Generic_Type_Access;
   --  Create a new Debugger item, which a specific command to send to the
   --  debugger to get the new value.
   --  If Split_Lines is true, then each line will be returned as a separate
   --  component when iterating, for instance for the Variables view.

   function Refresh_Command (Item : Debugger_Output_Type) return String;
   --  Return the command to send to the debugger to refresh the value.

   procedure Set_Value (Item : in out Debugger_Output_Type; Value : String);
   --  Set the value to be displayed

   overriding function Build_Display
     (Self   : not null access Debugger_Output_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item;
   overriding function Start
     (Self   : not null access Debugger_Output_Type)
      return Generic_Iterator'Class;

private

   type Base_Simple_Type is abstract new Generic_Type with null record;
   overriding function Replace
     (Parent       : access Base_Simple_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class)
      return Generic_Type_Access
   is (null);

   type Simple_Type is new Base_Simple_Type with record
      Value : Unbounded_String := Null_Unbounded_String;
      --  The value, as displayed by the debugger

      Has_Changed : Boolean := False;
      --  Whether the value of the item has changed.
   end record;

   overriding function Get_Type_Descr
     (Self : not null access Simple_Type) return String is ("Simple");
   overriding function Get_Simple_Value
     (Self : not null access Simple_Type) return String;
   overriding procedure Free
     (Item : access Simple_Type;
      Only_Value : Boolean := False);
   overriding procedure Clone_Dispatching
     (Item  : Simple_Type;
      Clone : in out Generic_Type_Access);
   overriding procedure Reset_Recursive (Item : access Simple_Type);
   overriding function Structurally_Equivalent
     (Item1 : access Simple_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   overriding function Build_Display
     (Self   : not null access Simple_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item;
   overriding function Is_Changed
     (Self : not null access Simple_Type) return Boolean;

   type Range_Type is new Simple_Type with record
      Min, Max : Long_Integer;
   end record;
   overriding function Get_Type_Descr
     (Self : not null access Range_Type) return String
     is ("Range " & Self.Min'Img & " .." & Self.Max'Img);
   overriding function Structurally_Equivalent
     (Item1 : access Range_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   --  Free is inherited from Simple_Type.

   type Mod_Type is new Simple_Type with record
      Modulo : Long_Integer;
   end record;
   overriding function Get_Type_Descr
     (Self : not null access Mod_Type) return String
     is ("Modulo " & Self.Modulo'Img);
   overriding function Structurally_Equivalent
     (Item1 : access Mod_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   --  Free is inherited from Simple_Type.

   type Access_Type is new Simple_Type with null record;
   overriding function Get_Type_Descr
     (Self : not null access Access_Type) return String  is ("Access");
   overriding function Structurally_Equivalent
     (Item1 : access Access_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   overriding function Build_Display
     (Self   : not null access Access_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item;

   type Enum_Type is new Simple_Type with null record;
   overriding function Get_Type_Descr
     (Self : not null access Enum_Type) return String  is ("Enumeration");
   overriding function Structurally_Equivalent
     (Item1 : access Enum_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   --  Free is inherited from Simple_Type.

   type Line_Value is record
      Value    : Unbounded_String;
      Modified : Boolean;
   end record;

   package Line_Vector is new Ada.Containers.Vectors (Positive, Line_Value);

   type Debugger_Output_Type is new Base_Simple_Type with record
      Value       : Line_Vector.Vector;
      Refresh_Cmd : Unbounded_String;
      Split_Lines : Boolean;

      As_Record   : Type_Vector.Vector;
      --  When we split lines, the value is interpreted as a record for which
      --  all fields are strings. This is unused if we do not split lines
   end record;
   --  Since Value can be a multiple-line string, and we want to consider each
   --  line separately as far as highlighting is concerned, we in fact insert
   --  a special character at the beginning of each line to indicate whether
   --  the line should be highlighted or not.

   overriding function Get_Type_Descr
     (Self : not null access Debugger_Output_Type) return String
     is ("Debugger_Type");
   overriding function Get_Simple_Value
     (Self : not null access Debugger_Output_Type) return String;
   overriding procedure Clone_Dispatching
     (Item  : Debugger_Output_Type;
      Clone : in out Generic_Type_Access);
   overriding procedure Free
     (Item : access Debugger_Output_Type; Only_Value : Boolean := False);
   overriding procedure Reset_Recursive (Item : access Debugger_Output_Type);
   overriding function Structurally_Equivalent
     (Item1 : access Debugger_Output_Type; Item2 : access Generic_Type'Class)
     return Boolean;

end Items.Simples;
