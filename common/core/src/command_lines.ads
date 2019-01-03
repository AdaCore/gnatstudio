------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

--  This is simplified version of GNAT.Command_Line package.
--  The reason of this package in that it doesn't strip duplicated switches
--  from a command line if any.

with GNAT.Strings;

with Ada.Strings.Unbounded;                use Ada.Strings.Unbounded;
private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with GNATCOLL.Refcount;

package Command_Lines is

   -----------------
   -- Configuring --
   -----------------

   --  The following subprograms are used to manipulate a command line
   --  represented as a string (for instance "-g -O2"), as well as parsing
   --  the switches from such a string. They provide high-level configurations
   --  to define aliases (a switch is equivalent to one or more other switches)
   --  or grouping of switches ("-gnatyac" is equivalent to "-gnatya" and
   --  "-gnatyc").

   --  See GNAT.Command_Line for examples on how to use these subprograms

   type Command_Line_Configuration is tagged private;

   function "=" (Left, Right : Command_Line_Configuration'Class)
     return Boolean;

   procedure Define_Section
     (Config  : in out Command_Line_Configuration;
      Section : String);
   --  Indicates a new switch section. All switches belonging to the same
   --  section are ordered together, preceded by the section. They are placed
   --  at the end of the command line (as in "gnatmake somefile.adb -cargs -g")

   procedure Define_Alias
     (Config   : in out Command_Line_Configuration;
      Switch   : String;
      Expanded : String;
      Section  : String := "");
   --  Indicates that whenever Switch appears on the command line, it should
   --  be expanded as Expanded. For instance, for the GNAT compiler switches,
   --  we would define "-gnatwa" as an alias for "-gnatwcfijkmopruvz", ie some
   --  default warnings to be activated.
   --
   --  This expansion is only done within the specified section, which must
   --  have been defined first through a call to [Define_Section].

   procedure Define_Prefix
     (Config : in out Command_Line_Configuration;
      Prefix : String);
   --  Indicates that all switches starting with the given prefix should be
   --  grouped. For instance, for the GNAT compiler we would define "-gnatw" as
   --  a prefix, so that "-gnatwu -gnatwv" can be grouped into "-gnatwuv" It is
   --  assumed that the remainder of the switch ("uv") is a set of characters
   --  whose order is irrelevant. In fact, this package will sort them
   --  alphabetically.
   --
   --  When grouping switches that accept arguments (for instance "-gnatyL!"
   --  as the definition, and "-gnatyaL12b" as the command line), only
   --  numerical arguments are accepted. The above is equivalent to
   --  "-gnatya -gnatyL12 -gnatyb".

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Switch      : String;
      Section     : String := "");
   --  Indicates a new switch.
   --
   --  Switch should also start with the leading '-' (or any other characters).
   --
   --  The switches defined in the command_line_configuration object are used
   --  when ungrouping switches with more that one character after the prefix.
   --
   --  Switch can be set to "*" to indicate that any switch is supported.
   --
   --  Section indicates in which section the switch is valid (you need to
   --  first define the section through a call to Define_Section).

   procedure Define_Switch_With_Parameter
     (Config      : in out Command_Line_Configuration;
      Switch      : String;
      Section     : String := "";
      Optional    : Boolean := False);
   --  Indicates a new switch with a parameter.

   procedure Define_Switch_With_Parameter
     (Config      : in out Command_Line_Configuration;
      Switch      : String;
      Section     : String := "";
      Separator   : Character;
      Optional    : Boolean := False);
   --  Indicates a new switch with a parameter and separator between them.

   procedure Free (Config : in out Command_Line_Configuration);
   --  Free the memory used by Config

   ------------------------------
   -- Generating command lines --
   ------------------------------

   --  Once the command line configuration has been created, you can build your
   --  own command line. This will be done in general because you need to spawn
   --  external tools from your application.

   --  Although it could be done by concatenating strings, the following
   --  subprograms will properly take care of grouping switches when possible,
   --  so as to keep the command line as short as possible. They also provide a
   --  way to remove a switch from an existing command line.

   --  For instance:

   --      declare
   --         Config : Command_Line_Configuration;
   --         Line : Command_Line;
   --         Args : Argument_List_Access;

   --      begin
   --         Define_Switch (Config, "-gnatyc");
   --         Define_Switch (Config, ...);  --  for all valid switches
   --         Define_Prefix (Config, "-gnaty");

   --         Set_Configuration (Line, Config);
   --         Add_Switch (Line, "-O2");
   --         Add_Switch (Line, "-gnatyc");
   --         Add_Switch (Line, "-gnatyd");
   --
   --         Build (Line, Args);
   --         --   Args is now  ["-O2", "-gnatycd"]
   --      end;

   type Command_Line is tagged private;

   function "=" (Left, Right : Command_Line'Class) return Boolean;

   procedure Set_Configuration
     (Cmd    : in out Command_Line'Class;
      Config : Command_Line_Configuration);
   function Get_Configuration
     (Cmd : Command_Line'Class) return Command_Line_Configuration;
   --  Set or retrieve the configuration used for that command line. The Config
   --  must have been initialized first, by calling one of the Define_Switches
   --  subprograms.

   procedure Set_Command_Line
     (Cmd      : in out Command_Line;
      Switches : String);
   --  Set the new content of the command line, by replacing the current
   --  version with Switches.

   procedure Append_Switches
     (Cmd  : in out Command_Line;
      List : GNAT.Strings.String_List);
   --  Append given switches to command line

   procedure Append_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False);
   procedure Append_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False;
      Success    : out Boolean);
   --  Add a new switch to the command line, and combine/group it with existing
   --  switches if possible.
   --
   --  If the Switch takes a parameter, the latter should be specified
   --  separately, so that the association between the two is always correctly
   --  recognized even if the order of switches on the command line changes.
   --  For instance, you should pass "--check=full" as ("--check", "full") so
   --  that Remove_Switch below can simply take "--check" in parameter. That
   --  will automatically remove "full" as well. The value of the parameter is
   --  never modified by this package.
   --
   --  On the other hand, you could decide to simply pass "--check=full" as
   --  the Switch above, and then pass no parameter. This means that you need
   --  to pass "--check=full" to Remove_Switch as well.
   --
   --  A Switch with a parameter will never be grouped with another switch to
   --  avoid ambiguities as to what the parameter applies to.
   --
   --  If the switch is part of a section, then it should be specified so that
   --  the switch is correctly placed in the command line, and the section
   --  added if not already present. For example, to add the -g switch into the
   --  -cargs section, you need to call (Cmd, "-g", Section => "-cargs").
   --
   --  [Separator], if specified, overrides the separator that was defined
   --  through Define_Switch_With_Parameter.
   --
   --  Invalid_Section is raised if Section was not defined in the
   --  configuration of the command line.
   --
   --  Add_Before allows insertion of the switch at the beginning of the
   --  command line.

   procedure Append
     (Cmd   : in out Command_Line;
      Value : Command_Line'Class);
   --  Append switches of Value to given command line. Configuration of Cmd
   --  will be updated to include section definitions from Value configuration

   function Append
     (Cmd   : Command_Line'Class;
      Value : Command_Line'Class) return Command_Line;
   --  Append switches of Value to given command line and return the result.
   --  The function keeps both arguments unchanged.

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Has_Parameter : Boolean := False;
      Section       : String  := "");
   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Has_Parameter : Boolean := False;
      Section       : String  := "";
      Success       : out Boolean);
   --  Remove Switch from the command line, and ungroup existing switches if
   --  necessary.
   --
   --  If Has_Parameter is set to True, then only switches having a parameter
   --  are removed.
   --
   --  If the switch belongs to a section, then this section should be
   --  specified: Remove_Switch (Cmd_Line, "-g", Section => "-cargs") called
   --  on the command line "-g -cargs -g" will result in "-g", while if
   --  called with (Cmd_Line, "-g") this will result in "-cargs -g".

   procedure Clear (Self : in out Command_Line);
   --  Reset command line to empty state. The Configuration isn't altered.

   function Is_Empty (Self : Command_Line) return Boolean;
   --  Check if there is some switch on given command line

   function Has_Switch
     (Cmd     : Command_Line;
      Switch  : String;
      Section : String  := "") return Boolean;
   --  Check if there is the Switch in given Section

   function To_String_List
     (Cmd      : Command_Line;
      Expanded : Boolean) return GNAT.Strings.String_List_Access;
   --  Return the arguments of the command line. Expanded indicates whether
   --  the expanded command line, or the shortest command line, is returned.
   --  Result should be freed by caller after use.

   --  This represents separator between switch and its argument
   type Separator (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Character;
         when False =>
            null;
      end case;
   end record;

   --  This represents argument of some switch
   type Argument (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Separator : Command_Lines.Separator;
            Value     : Ada.Strings.Unbounded.Unbounded_String;
         when False =>
            null;
      end case;
   end record;

   function Get_Parameter
     (Cmd     : Command_Line;
      Switch  : String;
      Section : String  := "") return Argument;
   --  Return parameter of given Switch in given Section

   function Map
     (Cmd    : Command_Line;
      Update : access procedure
        (Switch    : in out Unbounded_String;
         Section   : in out Unbounded_String;
         Parameter : in out Argument))
      return Command_Line;
   --  This function creates a copy of given command line where some switches
   --  are modified by the given Update procedure

   function Filter
     (Cmd    : Command_Line;
      Delete : access function
        (Switch    : String;
         Section   : String;
         Parameter : Argument) return Boolean)
      return Command_Line;
   --  This function creates a copy of given command line where some switсhes
   --  are deleted according to Delete result.

   Invalid_Section : exception;

   ---------------
   -- Iteration --
   ---------------

   --  When a command line was created with the above, you can then iterate
   --  over its contents using the following iterator.

   type Command_Line_Iterator is private;

   procedure Start
     (Cmd      : Command_Line;
      Iter     : in out Command_Line_Iterator;
      Expanded : Boolean := False);
   --  Start iterating over the command line arguments. If Expanded is true,
   --  then the arguments are not grouped and no alias is used. For instance,
   --  "-gnatwv" and "-gnatwu" would be returned instead of "-gnatwuv".
   --
   --  The iterator becomes invalid if the command line is changed through a
   --  call to Add_Switch, Remove_Switch or Set_Command_Line.

   function Current_Switch    (Iter : Command_Line_Iterator) return String;
   function Is_New_Section    (Iter : Command_Line_Iterator) return Boolean;
   function Current_Section   (Iter : Command_Line_Iterator) return String;
   function Current_Separator (Iter : Command_Line_Iterator) return String;
   function Current_Parameter (Iter : Command_Line_Iterator) return String;
   --  Return the current switch and its parameter (or the empty string if
   --  there is no parameter or the switch was added through Add_Switch
   --  without specifying the parameter.
   --
   --  Separator is the string that goes between the switch and its separator.
   --  It could be the empty string if they should be concatenated, or a space
   --  for instance. When printing, you should not add any other character.

   function Has_More (Iter : Command_Line_Iterator) return Boolean;
   --  Return True if there are more switches to be returned

   procedure Next (Iter : in out Command_Line_Iterator);
   --  Move to the next switch

private

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);

   type Parameter_Configuration (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Optional  : Boolean := False;
            Separator : Command_Lines.Separator;
         when False =>
            null;
      end case;
   end record;

   type Switch_Configuration is record
      Switch       : Unbounded_String;
      Parameter    : Parameter_Configuration;
   end record;

   package Switch_Configuration_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Switch_Configuration,
      "<"          => "<");

   package Unbounded_String_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,  --  Alias (short switch)
      Element_Type => Unbounded_String,  --  Extended switch
      "<"          => "<",
      "="          => "=");

   type Section_Configuration is record
      Name     : Unbounded_String;
      Aliases  : Unbounded_String_Maps.Map;
      Expanded : Unbounded_String_Maps.Map;  --  Reversed Aliases map
      Switches : Switch_Configuration_Maps.Map;
   end record;

   package Section_Configuration_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,
      Element_Type => Section_Configuration,
      "<"          => "<");

   type Configuration is record
      Prefixes : String_Vectors.Vector;
      Sections : Section_Configuration_Maps.Map;
   end record;

   package Configuration_References is new GNATCOLL.Refcount.Shared_Pointers
     (Configuration);

   type Command_Line_Configuration is new Configuration_References.Ref
     with null record;

   type Switch is record
      Switch       : Unbounded_String;
      Parameter    : Argument;
   end record;

   package Switch_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Switch);

   package Argument_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,  -- Switch
      Element_Type => Argument,
      "<"          => "<",
      "="          => "=");

   package Prefixed_Switch_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,  --  Prefix
      Element_Type => Argument_Maps.Map,
      "<"          => "<",
      "="          => Argument_Maps."=");
   --  Switches with common prefix stored together ordered alphabetically
   --  together with their arguments.

   type Section is record
      Prefixes : Prefixed_Switch_Maps.Map;
      Switches : Switch_Vectors.Vector;
   end record;

   Empty_Section : constant Section :=
     (Prefixed_Switch_Maps.Empty_Map, Switch_Vectors.Empty_Vector);

   package Section_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Unbounded_String,  --  Prefix
      Element_Type => Section,
      "<"          => "<",
      "="          => "=");

   package Section_Map_References is new GNATCOLL.Refcount.Shared_Pointers
     (Section_Maps.Map);

   type Command_Line is tagged record
      Configuration : Command_Line_Configuration;
      Sections      : Section_Map_References.Ref;
      --  We store command line as sequence of sections, such as -cargs,
      --  -largs, etc. Default section has empty name ("").
      --  Each section has 'prefixed' switches grouped by prefix, such as
      --  -gnaty1M79ab, that means (-gnaty1 -gnatyM79 -gnatya -gnatyb)
      --  Each section also has others switch as a vector of them.
      --  Any switch by it self can have a parameter.
      --  Switches stored with any alias is expanded.
   end record;

   type Command_Line_Iterator (Expanded : Boolean := False) is record
      Line           : Command_Line;
      Section        : Section_Maps.Cursor;  --  Iterate over sections
      Switch         : Switch_Vectors.Cursor;  -- Iterate over switches
      Prefixed       : Prefixed_Switch_Maps.Cursor;  --  then over prefixes
      Is_New_Section : Boolean;  --  We've stepped into a new section

      case Expanded is
         when True =>
            Argument : Argument_Maps.Cursor;
            --  if Prefixed is not null then iterate over prefixed switches
         when False =>
            null;
      end case;
   end record;

end Command_Lines;
