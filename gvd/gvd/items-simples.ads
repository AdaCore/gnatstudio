-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  Items used for simple types, ie whose value is a simple string.
--  See the package Items for more information on all the private subprograms.

with Basic_Types;

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

   function Get_Value (Item : Simple_Type) return Basic_Types.String_Access;
   --  Return the current value of Item (or null if it is not known).

   procedure Set_Value (Item : in out Simple_Type; Value : String);
   --  Assign a new value to Item.
   --  String is copied internally.

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

   type Debugger_Output_Type is new Simple_Type with private;
   type Debugger_Output_Type_Access is access all Debugger_Output_Type'Class;
   --  General types, used to display directly the output of the debugger
   --  (no processing is done in that case).

   function New_Debugger_Type (Cmd : String) return Generic_Type_Access;
   --  Create a new Debugger item, which a specific command to send to the
   --  debugger to get the new value.

   function Refresh_Command (Item : Debugger_Output_Type) return String;
   --  Return the command to send to the debugger to refresh the value.

   procedure Set_Value (Item : in out Debugger_Output_Type; Value : String);
   --  Set a new value for Item.
   --  Since there is no pre-processing done on Value, we filter out ^Ms for
   --  Value as well.

private
   type Simple_Type is new Generic_Type with record
      Value : Basic_Types.String_Access := null;
      --  The value, as displayed by the debugger

      Has_Changed : Boolean := False;
      --  Whether the value of the item has changed.
   end record;

   procedure Print (Value : Simple_Type; Indent : Natural := 0);
   procedure Free
     (Item : access Simple_Type;
      Only_Value : Boolean := False);
   procedure Clone_Dispatching
     (Item  : Simple_Type;
      Clone : out Generic_Type_Access);
   procedure Paint
     (Item    : in out Simple_Type;
      Context : Drawing_Context;
      X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Simple_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name
     (Item : access Simple_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String;
   function Get_Component
     (Item : access Simple_Type;
      X, Y : Glib.Gint) return Generic_Type_Access;
   function Replace
     (Parent       : access Simple_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access;
   procedure Reset_Recursive (Item : access Simple_Type);
   function Structurally_Equivalent
     (Item1 : access Simple_Type; Item2 : access Generic_Type'Class)
     return Boolean;

   type Range_Type is new Simple_Type with record
      Min, Max : Long_Integer;
   end record;
   procedure Print (Value : Range_Type; Indent : Natural := 0);
   function Structurally_Equivalent
     (Item1 : access Range_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   --  Free is inherited from Simple_Type.

   type Mod_Type is new Simple_Type with record
      Modulo : Long_Integer;
   end record;
   procedure Print (Value : Mod_Type; Indent : Natural := 0);
   function Structurally_Equivalent
     (Item1 : access Mod_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   --  Free is inherited from Simple_Type.

   type Access_Type is new Simple_Type with null record;
   procedure Print (Value : Access_Type; Indent : Natural := 0);
   --  Free is inherited from Simple_Type.
   procedure Paint
     (Item    : in out Access_Type;
      Context : Drawing_Context;
      X, Y    : Glib.Gint := 0);
   function Structurally_Equivalent
     (Item1 : access Access_Type; Item2 : access Generic_Type'Class)
     return Boolean;

   type Enum_Type is new Simple_Type with null record;
   procedure Print (Value : Enum_Type; Indent : Natural := 0);
   function Structurally_Equivalent
     (Item1 : access Enum_Type; Item2 : access Generic_Type'Class)
     return Boolean;
   --  Free is inherited from Simple_Type.

   type Debugger_Output_Type is new Simple_Type with record
      Refresh_Cmd : Basic_Types.String_Access;
   end record;
   --  Since Value can be a multiple-line string, and we want to consider each
   --  line separately as far as highlighting is concerned, we in fact insert
   --  a special character at the beginning of each line to indicate whether
   --  the line should be highlighted or not.

   procedure Print (Value : Debugger_Output_Type; Indent : Natural := 0);
   procedure Clone_Dispatching
     (Item  : Debugger_Output_Type;
      Clone : out Generic_Type_Access);
   procedure Free (Item : access Debugger_Output_Type;
                   Only_Value : Boolean := False);
   procedure Size_Request
     (Item           : in out Debugger_Output_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False);
   procedure Paint (Item    : in out Debugger_Output_Type;
                    Context : Drawing_Context;
                    X, Y    : Glib.Gint := 0);
   procedure Reset_Recursive (Item : access Debugger_Output_Type);
   function Structurally_Equivalent
     (Item1 : access Debugger_Output_Type; Item2 : access Generic_Type'Class)
     return Boolean;

end Items.Simples;
