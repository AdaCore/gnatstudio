------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with Glib;       use Glib;

with GPS.Kernel; use GPS.Kernel;

package Aliases_Module is
   package SU renames Ada.Strings.Unbounded;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   -------------
   -- Aliases --
   -------------

   type Alias_Type is tagged private;
   --  Type representing an alias

   No_Alias : constant Alias_Type;

   function Get_Name (Alias : Alias_Type) return String;
   --  Return the given alias' name

   function Get_Expansion (Alias : Alias_Type) return String;
   --  Return the given alias' expansion text "as is".

   function Expand_Macro (Alias : Alias_Type) return String;
   --  Return the given alias' expansion text with macros (like %O) expanded.

   function Get_Default_Value
     (Alias : Alias_Type; Name : String) return String;
   --  Return default value for given parameter of the Alias

   function Has_Same_Parameters
     (Left, Right : Alias_Type) return Boolean;
   --  Return True if Left and Right has the same number of parameters and if
   --  the name of their parameters match.

   package Alias_Parameter_Substitution_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => "=");
   --  Used to associate alias parameters' names and their corresponding
   --  values.

   type Alias_Option_Type is private;
   --  Type representing an option in the alias expanding dialog.

   No_Option : constant Alias_Option_Type;

   function Create
     (Label         : String;
      Default_Value : Boolean := False;
      Doc           : String := "") return Alias_Option_Type;
   --  Create an option will be displayed in the Expand_Alias function's
   --  dialog, with the associated Doc, if any.

   function Is_Enabled (Option : Alias_Option_Type) return Boolean;
   --  True if the option has been enabled, False otherwise.

   type Alias_Filter_Type is
     access function
       (Text      : String;
        Error_Msg : out Ada.Strings.Unbounded.Unbounded_String) return Boolean;
   --  Type representing an alias filter.
   --  Filters can be used to prevent users from entering invalid alias
   --  parameters.
   --  The filter should return True when the entered Text is valid and False
   --  otherwise. When returning False, Error_Msg will be displated as a
   --  tooltip of the modified parameter entry.

   function Expand_Alias
     (Alias                : Alias_Type;
      Kernel               : not null access Kernel_Handle_Record'Class;
      Cursor               : out Integer;
      Must_Reindent        : out Boolean;
      Params_Substitutions : out Alias_Parameter_Substitution_Map.Map;
      Offset_Column        : Gint := 0;
      Dialog_Title         : String := "Alias Parameters Selection";
      Option               : access Alias_Option_Type := null;
      Filter               : Alias_Filter_Type := null)
      return String;
   --  Return the expanded version of Alias, displaying, if needed, a dialog
   --  asking the user to enter values for its parameters.
   --
   --  Cursor is the index in the returned string for the cursor position.
   --
   --  Must_Reindent is set to True if the editor should be reindented after
   --  insertion.
   --
   --  Params_Substitutions is filled with the values retrieved for each
   --  alias parameter.
   --
   --  Dialog_Title is used to set the dialog's title.
   --
   --  Option is used to create an optional checkbox with the option's label.
   --  Use the Is_Enabled function to know whether the given option has been
   --  checked or not.
   --
   --  Filter is used to filter the entered alias parameters.

   function Expand_Alias
     (Alias         : Alias_Type;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Cursor        : out Integer;
      Must_Reindent : out Boolean;
      Offset_Column : Gint := 0;
      Dialog_Title  : String := "Alias Parameters Selection")
      return String;
   --  Same as above, but without the Params_Subsitutions parameter.

   function Expand_Alias_With_Values
     (Alias                : Alias_Type;
      Kernel               : not null access Kernel_Handle_Record'Class;
      Params_Substitutions : Alias_Parameter_Substitution_Map.Map;
      Cursor               : out Integer;
      Offset_Column        : Gint := 0)
      return String;
   --  Same as above but uses the values already provided in
   --  Params_Substitutions instead of asking the user to enter values for the
   --  parameters.

   type Alias_List is array (Positive range <>) of Alias_Type;

   function Get_Aliases_List return Alias_List;
   --  Get the list of all the registered aliases

   function Get_Alias (Name : String) return Alias_Type;
   --  Get the alias corresponding to the given Name.
   --  Return No_Alias if not found.

   ------------------------
   -- Aliases Parameters --
   ------------------------

   type Alias_Param_Type is tagged private;
   --  Type representing an alias parameter

   function Get_Name (Param : Alias_Param_Type) return String;
   --  Return the given alias parameter's name

   function Get_Description (Param : Alias_Param_Type) return String;
   --  Return the given alias parameter's description

   -----------------------
   -- Aliases Expansion --
   -----------------------

   Invalid_Expansion : constant String := "!@#$%";

   type Alias_Expansion_Function is access
     function (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
               Expansion : String;
               Special   : Character) return String;
   --  Function called to test alias expansion.
   --  The context in which the expansion takes place can be found by calling
   --  GPS.Kernel.Get_Current_Focus_Widget.
   --  Special is the special entity seen in the alias expansion, without the
   --  leading '%'.
   --  The expansion of the alias, as computed up to the encounter with the
   --  special character, is given in Expansion. For instance, if the alias is
   --  defined as "foo%o bar", Expansion will be set to "foo" when the
   --  expansion function for 'o' is called.
   --  This function should return the new expansion of the alias (ie Expansion
   --  & the expansion for Special). If Special needs to replace some text in
   --  the alias, it doesn't need to return the whole of Expansion.
   --  If Invalid_Expansion is returned, then this special character will
   --  simply be ignored.

   procedure Register_Special_Alias_Entity
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Description : String;
      Entity      : Character;
      Func        : Alias_Expansion_Function);
   --  Register a new special entity that can be used in aliases expansion.
   --  Special is the text for that special entity, without the leading '%'.
   --  Func is the function called to get the replacement text for Special.
   --  Description is used in the contextual menu in the aliases editor to
   --  describe this special character.

private

   type Alias_Param_Type is tagged record
      Name        : SU.Unbounded_String;
      --  The parameter's name

      Description : SU.Unbounded_String;
      --  The parameter's description

      Initial     : SU.Unbounded_String;
      --  The parameter's initial value

      From_Env    : Boolean;
      --  True if the parameter's value is retrieved from an environment
      --  variable, False otherwise.
   end record;

   package Params_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Alias_Param_Type);
   --  Used to store the list of parameters of a given alias

   type Alias_Type is tagged record
      Name          : SU.Unbounded_String;
      --  The alias' name

      Expansion     : SU.Unbounded_String;
      --  The alias' expansion text

      Params        : Params_List.List;
      --  The alias' parameters' list

      Read_Only     : Boolean;
      --  True if the alias' should not be editable, False otherwise

      Must_Reindent : Boolean;
      --  Whether the editor should be reindented after insertion of the macro
   end record;

   type Alias_Option_Type is record
      Label   : SU.Unbounded_String;
      Doc     : SU.Unbounded_String;
      Enabled : Boolean;
   end record;

   No_Option : constant Alias_Option_Type :=
                 Alias_Option_Type'(Label   => SU.Null_Unbounded_String,
                                    Doc     => SU.Null_Unbounded_String,
                                    Enabled => False);

   No_Alias : constant Alias_Type :=
                Alias_Type'(Name          => SU.Null_Unbounded_String,
                            Expansion     => SU.Null_Unbounded_String,
                            Params        => Params_List.Empty_List,
                            Read_Only     => False,
                            Must_Reindent => False);

end Aliases_Module;
