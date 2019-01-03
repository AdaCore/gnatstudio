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

private with Ada.Containers.Vectors;

package GVD.Variables.Types.Simples is

   ---------------------
   -- GVD_Simple_Type --
   ---------------------

   type GVD_Simple_Type is new GVD_Generic_Type with private;
   type GVD_Simple_Type_Access is access all GVD_Simple_Type'Class;
   --  For simple values, like integers and floats, whose value we don't need
   --  to analyze.

   function New_Simple_Type return GVD_Type_Holder;
   --  Create a new simple value.

   function Get_Value
     (Self : not null access GVD_Simple_Type)
      return Unbounded_String;
   --  Return the current value of Item (or null if it is not known).

   procedure Set_Value
     (Self  : not null access GVD_Simple_Type;
      Value : String);
   --  Assign a new value to Item.

   ---------------------
   -- GVD_Range Types --
   ---------------------

   type GVD_Range_Type is new GVD_Simple_Type with private;
   type GVD_Range_Type_Access is access all GVD_Range_Type'Class;
   --  A range value, as in Ada 'range 0 .. 10', ie a number between the min
   --  and max value.

   function New_Range_Type
     (Min, Max : Long_Integer)
      return GVD_Type_Holder;
   --  Create a new range item, whose specific range is given in parameter.

   -------------------
   -- GVD_Mod Types --
   -------------------

   type GVD_Mod_Type is new GVD_Simple_Type with private;
   type GVD_Mod_Type_Access is access all GVD_Mod_Type'Class;
   --  A modular type whose values are module a given value.

   function New_Mod_Type
     (Modulo : Long_Integer)
      return GVD_Type_Holder;
   --  Create a new mod type which a given modulo value.

   ----------------------
   -- GVD_Access Types --
   ----------------------

   type GVD_Access_Type is new GVD_Simple_Type with private;
   type GVD_Access_Type_Access is access all GVD_Access_Type'Class;
   --  A simple address value, that can be derefenced by the user.

   function New_Access_Type return GVD_Type_Holder;
   --  Create a new access type.

   --------------------
   -- GVD_Enum Types --
   --------------------

   type GVD_Enum_Type is new GVD_Simple_Type with private;
   type GVD_Enum_Type_Access is access all GVD_Enum_Type'Class;
   --  An enumeration value, ie an item whose value must be in a set of given
   --  elements.
   --  Currently, the set of possible values is not stored in the item.

   function New_Enum_Type return GVD_Type_Holder;
   --  Create a new enum type.

   ------------------------------
   -- GVD_Debugger_Output_Type --
   ------------------------------

   type GVD_Debugger_Output_Type is new GVD_Generic_Type with private;
   type GVD_Debugger_Output_Type_Access is
     access all GVD_Debugger_Output_Type'Class;
   --  General types, used to display directly the output of the debugger
   --  (no processing is done in that case).

   function New_Debugger_Type
     (Cmd         : String;
      Split_Lines : Boolean := False)
      return GVD_Type_Holder;
   --  Create a new Debugger item, which a specific command to send to the
   --  debugger to get the new value.
   --  If Split_Lines is true, then each line will be returned as a separate
   --  component when iterating, for instance for the Variables view.

   function Refresh_Command
     (Self : not null access GVD_Debugger_Output_Type)
      return String;
   --  Return the command to send to the debugger to refresh the value.

   procedure Set_Value
     (Self  : not null access GVD_Debugger_Output_Type;
      Value : String);
   --  Set the value to be displayed

   overriding function Start
     (Self   : not null access GVD_Debugger_Output_Type)
      return Generic_Iterator'Class;

private

   type GVD_Base_Simple_Type is abstract new GVD_Generic_Type with null record;

   overriding function Replace
     (Parent             : not null access GVD_Base_Simple_Type;
      Dummy_Current      : GVD_Type_Holder'Class;
      Dummy_Replace_With : GVD_Type_Holder'Class)
      return GVD_Type_Holder'Class
   is (Empty_GVD_Type_Holder);

   type GVD_Simple_Type is new GVD_Base_Simple_Type with record
      Value : Unbounded_String := Null_Unbounded_String;
      --  The value, as displayed by the debugger

      Has_Changed : Boolean := False;
      --  Whether the value of the item has changed.
   end record;

   overriding procedure Clear (Self : not null access GVD_Simple_Type);

   overriding procedure Clone
     (Self : not null access GVD_Simple_Type;
      Item : not null GVD_Generic_Type_Access);

   overriding function Get_Type_Descr
     (Self : not null access GVD_Simple_Type) return String is ("Simple");

   overriding function Get_Simple_Value
     (Self : not null access GVD_Simple_Type) return String;

   overriding procedure Reset_Recursive
     (Self : not null access GVD_Simple_Type);

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Simple_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   overriding function Is_Changed
     (Self : not null access GVD_Simple_Type) return Boolean;

   type GVD_Range_Type is new GVD_Simple_Type with record
      Min, Max : Long_Integer;
   end record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Range_Type) return String
   is ("Range " & Self.Min'Img & " .." & Self.Max'Img);

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Range_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   type GVD_Mod_Type is new GVD_Simple_Type with record
      Modulo : Long_Integer;
   end record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Mod_Type) return String
   is ("Modulo " & Self.Modulo'Img);

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Mod_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   type GVD_Access_Type is new GVD_Simple_Type with null record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Access_Type) return String  is ("Access");

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Access_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   type GVD_Enum_Type is new GVD_Simple_Type with null record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Enum_Type) return String  is ("Enumeration");

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Enum_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   type Line_Value is record
      Value    : Unbounded_String;
      Modified : Boolean;
   end record;

   package Line_Vector is new Ada.Containers.Vectors (Positive, Line_Value);

   type Field_Descr is record
      Name : Unbounded_String := Null_Unbounded_String;
      Typ  : GVD_Type_Holder;
   end record;
   package Type_Vector is new Ada.Containers.Vectors (Positive, Field_Descr);

   function Start (Self : Type_Vector.Vector) return Generic_Iterator'Class;
   --  Return an iterator on Self

   type GVD_Debugger_Output_Type is new GVD_Base_Simple_Type with record
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
     (Self : not null access GVD_Debugger_Output_Type) return String
   is ("Debugger_Type");

   overriding function Get_Simple_Value
     (Self : not null access GVD_Debugger_Output_Type) return String;

   overriding procedure Clear
     (Self : not null access GVD_Debugger_Output_Type);

   overriding procedure Clone
     (Self : not null access GVD_Debugger_Output_Type;
      Item : not null GVD_Generic_Type_Access);

   overriding procedure Free (Self : not null access GVD_Debugger_Output_Type);

   overriding procedure Reset_Recursive
     (Self : not null access GVD_Debugger_Output_Type);

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Debugger_Output_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   type Field_Iterator is new Generic_Iterator with record
      Fields : Type_Vector.Vector;
      Idx    : Integer;
   end record;
   overriding function At_End (Self : Field_Iterator) return Boolean;
   overriding procedure Next (Self : in out Field_Iterator);
   overriding function Field_Name
     (Self : Field_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;
   overriding function Data
     (Self : Field_Iterator) return GVD_Type_Holder'Class;
   --  An iterator on an array of fields

end GVD.Variables.Types.Simples;
