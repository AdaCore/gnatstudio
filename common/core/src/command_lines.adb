------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

with GNAT.OS_Lib;
with GNATCOLL.Scripts.Utils;  use GNATCOLL.Scripts.Utils;
with Ada.Containers.Ordered_Sets;

package body Command_Lines is

   procedure Make_Default_Section (Config : in out Command_Line_Configuration);
   --  Make sure that there is a section with empty name in Config

   procedure Check_Initialized (Cmd : in out Command_Line'Class);
   --  Check if Cmd is initialized and initialize it otherwise

   function Starts_With (Value, Prefix : Unbounded_String) return Boolean;
   --  Check if Value starts with given prefix

   function Find_Prefix
     (Conf   : Configuration_References.Element_Access;
      Switch : Unbounded_String) return Unbounded_String;
   --  Look for prefix matching Switch in Configuration

   procedure Append
     (Cmd        : in out Command_Line;
      Item       : Switch;
      Section    : Unbounded_String;
      Add_Before : Boolean := False);
   --  Append given Switch to command line.

   procedure Update (Self : in out Command_Line_Iterator);
   --  Initialize iterator based on its current section

   function Current_Switch (Iter : Command_Line_Iterator) return Switch;
   --  Return current switch of iterator. It expects Iter in Expanded mode

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : Unbounded_String;
      Has_Parameter : Boolean := False;
      Section       : Unbounded_String;
      Success       : out Boolean);
   --  The same as Remove_Switch but with Unbounded_String

   function Has_Switch
     (Cmd     : Command_Line;
      Switch  : Unbounded_String;
      Section : Unbounded_String) return Boolean;
   --  The same as Has_Switch but with Unbounded_String

   generic
      with procedure Process_Switch (Item : Switch);
   procedure Parse_One_Switch
     (Config  : Command_Line_Configuration;
      Item    : Switch;
      Section : Unbounded_String;
      Prefix  : out Unbounded_String);
   --  If Item.Switch is an alias, expand it and call Process_Switch.
   --  Otherwise if Item.Switch has a known prefix,
   --  for each switch found after prefix call Process_Switch.
   --  Otherwise do nothing (so Process_Switch won't be called as all).
   --  Process_Switch won't be called neither if after prefix there is only
   --  one switch.

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Command_Line_Configuration'Class)
                 return Boolean
   is
      Empty : Configuration;
   begin
      if Left.Is_Null then
         return Right.Is_Null or else Right.Unchecked_Get.all = Empty;
      elsif Right.Is_Null then
         return Left.Unchecked_Get.all = Empty;
      end if;

      return Left.Unchecked_Get.all = Right.Unchecked_Get.all;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Command_Line'Class) return Boolean is
      use type Section_Maps.Map;
   begin
      if Left.Sections.Is_Null then
         return Right.Is_Empty;
      elsif Right.Sections.Is_Null then
         return Left.Is_Empty;
      else
         return Left.Sections.Unchecked_Get.all =
           Right.Sections.Unchecked_Get.all;
      end if;
   end "=";

   --------------------
   -- Define_Section --
   --------------------

   procedure Define_Section
     (Config  : in out Command_Line_Configuration;
      Section : String)
   is
      Name : constant Unbounded_String := To_Unbounded_String (Section);
   begin
      Make_Default_Section (Config);

      if not Config.Get.Sections.Contains (Name) then
         Config.Get.Sections.Insert (Name, (Name, others => <>));
      end if;
   end Define_Section;

   ------------------
   -- Define_Alias --
   ------------------

   procedure Define_Alias
     (Config   : in out Command_Line_Configuration;
      Switch   : String;
      Expanded : String;
      Section  : String := "")
   is
      package Unbounded_String_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Unbounded_String,
         "<"          => "<");

      procedure Process_Switch (Item : Command_Lines.Switch);
      --  Append prefixed switch to ordered set

      Set   : Unbounded_String_Sets.Set;

      --------------------
      -- Process_Switch --
      --------------------

      procedure Process_Switch (Item : Command_Lines.Switch) is
      begin
         if not Item.Parameter.Is_Set then
            Set.Include (Item.Switch);
         elsif Item.Parameter.Separator.Is_Set then
            Set.Include
              (Item.Switch
               & Item.Parameter.Separator.Value
               & Item.Parameter.Value);
         else
            Set.Include (Item.Switch & Item.Parameter.Value);
         end if;
      end Process_Switch;

      procedure Parse_Switch is new Parse_One_Switch (Process_Switch);

      Name   : constant Unbounded_String := To_Unbounded_String (Section);
      Conf   : Configuration_References.Element_Access;
      Short  : constant Unbounded_String := To_Unbounded_String (Switch);
      Long   : Unbounded_String := To_Unbounded_String (Expanded);
      Prefix : Unbounded_String;
   begin
      if Expanded'Length < Switch'Length then
         Define_Alias
           (Config   => Config,
            Switch   => Expanded,
            Expanded => Switch,
            Section  => Section);

         return;
      end if;

      Make_Default_Section (Config);
      Conf := Config.Unchecked_Get;

      if not Conf.Sections.Contains (Name) then
         raise Invalid_Section;
      end if;

      --  Try to find prefix and reorder switches after prefix. If we keep
      --  Expanded unordered we won't be able to match it
      Parse_Switch
        (Config  => Config,
         Item    => (Switch => Long, Parameter => (Is_Set => False)),
         Section => Name,
         Prefix  => Prefix);

      if not Set.Is_Empty then
         Long := Prefix;

         for Item of Set loop
            Append (Long, Delete (Item, 1, Length (Prefix)));
         end loop;
      end if;

      declare
         Value  : Section_Configuration renames Conf.Sections (Name);
      begin
         Value.Aliases.Include (Short, Long);
         Value.Expanded.Include (Long, Short);
      end;
   end Define_Alias;

   -------------------
   -- Define_Prefix --
   -------------------

   procedure Define_Prefix
     (Config : in out Command_Line_Configuration;
      Prefix : String)
   is
      Value : constant Unbounded_String := To_Unbounded_String (Prefix);
   begin
      Make_Default_Section (Config);

      if not Config.Get.Prefixes.Contains (Value) then
         Config.Get.Prefixes.Append (Value);
      end if;
   end Define_Prefix;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Switch      : String;
      Section     : String := "")
   is
      Key   : constant Unbounded_String := To_Unbounded_String (Switch);
      Name  : constant Unbounded_String := To_Unbounded_String (Section);
      Conf  : Configuration_References.Element_Access;
   begin
      Make_Default_Section (Config);
      Conf := Config.Unchecked_Get;

      if not Conf.Sections.Contains (Name) then
         raise Invalid_Section;
      end if;

      declare
         Value : Section_Configuration renames Conf.Sections (Name);
      begin
         if not Value.Switches.Contains (Key) then
            Value.Switches.Insert
              (Key,
               (Switch    => Key,
                Parameter => (Is_Set => False)));
         end if;
      end;
   end Define_Switch;

   ----------------------------------
   -- Define_Switch_With_Parameter --
   ----------------------------------

   procedure Define_Switch_With_Parameter
     (Config      : in out Command_Line_Configuration;
      Switch      : String;
      Section     : String := "";
      Optional    : Boolean := False)
   is
      Key   : constant Unbounded_String := To_Unbounded_String (Switch);
      Name  : constant Unbounded_String := To_Unbounded_String (Section);
      Conf  : Configuration_References.Element_Access;
   begin
      Make_Default_Section (Config);
      Conf := Config.Unchecked_Get;

      if not Conf.Sections.Contains (Name) then
         raise Invalid_Section;
      end if;

      declare
         Value : Section_Configuration renames Conf.Sections (Name);
      begin
         if not Value.Switches.Contains (Key) then
            Value.Switches.Insert
              (Key,
               (Switch    => Key,
                Parameter => (Is_Set => True,
                              Optional => Optional,
                              Separator => (Is_Set => False))));
         end if;
      end;
   end Define_Switch_With_Parameter;

   ----------------------------------
   -- Define_Switch_With_Parameter --
   ----------------------------------

   procedure Define_Switch_With_Parameter
     (Config      : in out Command_Line_Configuration;
      Switch      : String;
      Section     : String := "";
      Separator   : Character;
      Optional    : Boolean := False)
   is
      Key   : constant Unbounded_String := To_Unbounded_String (Switch);
      Name  : constant Unbounded_String := To_Unbounded_String (Section);
      Conf  : Configuration_References.Element_Access;
   begin
      Make_Default_Section (Config);
      Conf := Config.Unchecked_Get;

      if not Conf.Sections.Contains (Name) then
         raise Invalid_Section;
      end if;

      declare
         Value : Section_Configuration renames Conf.Sections (Name);
      begin
         if not Value.Switches.Contains (Key) then
            Value.Switches.Insert
              (Key,
               (Switch    => Key,
                Parameter => (Is_Set => True,
                              Optional => Optional,
                              Separator => (Is_Set => True,
                                            Value  => Separator))));
         end if;
      end;
   end Define_Switch_With_Parameter;

   ----------
   -- Free --
   ----------

   procedure Free (Config : in out Command_Line_Configuration) is
   begin
      Config := (Configuration_References.Null_Ref with null record);
   end Free;

   --------------------------
   -- Make_Default_Section --
   --------------------------

   procedure Make_Default_Section (Config : in out Command_Line_Configuration)
   is
   begin
      if Config.Is_Null then
         Config.Set (Configuration'(others => <>));

         Config.Get.Sections.Insert
           (Null_Unbounded_String, (Null_Unbounded_String, others => <>));
      end if;
   end Make_Default_Section;

   -----------------------
   -- Check_Initialized --
   -----------------------

   procedure Check_Initialized (Cmd : in out Command_Line'Class) is
   begin
      if Cmd.Sections.Is_Null then
         Cmd.Sections.Set (Section_Maps.Empty_Map);
      end if;
   end Check_Initialized;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration
     (Cmd    : in out Command_Line'Class;
      Config : Command_Line_Configuration) is
   begin
      Cmd.Configuration := Config;
      Make_Default_Section (Cmd.Configuration);
      Check_Initialized (Cmd);
   end Set_Configuration;

   -----------------------
   -- Get_Configuration --
   -----------------------

   function Get_Configuration
     (Cmd : Command_Line'Class) return Command_Line_Configuration is
   begin
      return Cmd.Configuration;
   end Get_Configuration;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Command_Line) is
   begin
      Self.Sections.Set (Section_Maps.Empty_Map);
   end Clear;

   ----------------------
   -- Set_Command_Line --
   ----------------------

   procedure Set_Command_Line
     (Cmd      : in out Command_Line;
      Switches : String)
   is
      --  Do not use GNAT.OS_Lib.Argument_String_To_List, since it doesn't
      --  properly handle quotes in arguments. For instance,
      --     %python("foo")  becomes    1=> %python("foo" , 2 => ")"
      List : GNAT.OS_Lib.Argument_List_Access :=
        Argument_String_To_List_With_Triple_Quotes (Switches);
   begin
      Cmd.Clear;
      Cmd.Append_Switches (List.all);
      GNAT.OS_Lib.Free (List);
   end Set_Command_Line;

   ---------------------
   -- Append_Switches --
   ---------------------

   procedure Append_Switches
     (Cmd  : in out Command_Line;
      List : GNAT.Strings.String_List)
   is
      function Is_Section (Name : String) return Boolean;
      --  Search for section with given Name

      procedure Find_Switch
        (Switch  : String;
         Section : Unbounded_String;
         Result  : out Switch_Configuration;
         Found   : out Boolean);
      --  Look for given Switch under Section in Conf.
      --  Special case if switch has an argument appended without separator
      --  like -gnatyM72. Treat prefixed switch (like -gnatyabcd) as unknown.

      Conf : Configuration_References.Element_Access;

      ----------------
      -- Is_Section --
      ----------------

      function Is_Section (Name : String) return Boolean is
         Section : constant Unbounded_String := To_Unbounded_String (Name);
      begin
         return Conf.Sections.Contains (Section);
      end Is_Section;

      -----------------
      -- Find_Switch --
      -----------------

      procedure Find_Switch
        (Switch  : String;
         Section : Unbounded_String;
         Result  : out Switch_Configuration;
         Found   : out Boolean)
      is
         Arg     : constant Unbounded_String := To_Unbounded_String (Switch);
         Prefix  : constant Unbounded_String := Find_Prefix (Conf, Arg);
         Value   : Section_Configuration renames Conf.Sections (Section);
         Pos     : Switch_Configuration_Maps.Cursor := Value.Switches.First;
         Length  : Natural;  --  Length of current switch
         Current : Switch_Configuration;
      begin
         Found := False;

         if Prefix /= "" then
            --  Treat any prefixed switch as unknown
            return;
         end if;

         while Switch_Configuration_Maps.Has_Element (Pos) loop
            Current := Switch_Configuration_Maps.Element (Pos);
            Length  := Ada.Strings.Unbounded.Length (Current.Switch);

            --  Check if argument exectly matches switch
            if Current.Switch = Switch then
               Result := Current;
               Found  := True;
               return;

            --  Otherwise check if switch has parameter embeded in argument
            elsif Current.Parameter.Is_Set
              and then Switch'Length > Length
              and then Starts_With (Arg, Current.Switch)
              and then (not Current.Parameter.Separator.Is_Set
                        or else Current.Parameter.Separator.Value =
                          Switch (Switch'First + Length))
            then
               if Found then
                  --  One parameter is already found,
                  --  select one with the biggest length

                  if Ada.Strings.Unbounded.Length (Result.Switch) < Length then
                     Result := Current;
                  end if;

               else
                  Result := Current;
                  Found  := True;
               end if;
            end if;

            Switch_Configuration_Maps.Next (Pos);
         end loop;
      end Find_Switch;

      Arg          : GNAT.Strings.String_Access;
      Switch_Conf  : Switch_Configuration;
      Section      : Unbounded_String;
      Found        : Boolean;
      Is_Parameter : Boolean := False;

   begin
      Check_Initialized (Cmd);
      Make_Default_Section (Cmd.Configuration);
      Conf := Cmd.Configuration.Unchecked_Get;

      for J in List'Range loop
         Arg := List (J);

         if Is_Parameter then
            Is_Parameter := False;

         elsif Is_Section (Arg.all) then
            Section := To_Unbounded_String (Arg.all);

         else
            declare
               Item : Switch;
            begin
               Find_Switch (Arg.all, Section, Switch_Conf, Found);

               --  Check if next argument is parameter
               Is_Parameter :=
                 (Found
                  and then J < List'Last
                  and then Switch_Conf.Parameter.Is_Set
                  and then Switch_Conf.Parameter.Separator.Is_Set
                  and then Switch_Conf.Parameter.Separator.Value = ' ');

               if Is_Parameter then
                  Item.Parameter :=
                    (Is_Set    => True,
                     Separator => Switch_Conf.Parameter.Separator,
                     Value     => To_Unbounded_String
                       (List (J + 1).all));
               end if;

               if not Found then
                  Item.Switch := To_Unbounded_String (Arg.all);

               elsif Length (Switch_Conf.Switch) = Arg'Length then
                  Item.Switch := Switch_Conf.Switch;

               elsif Switch_Conf.Parameter.Separator.Is_Set then
                  Item.Switch := Switch_Conf.Switch;
                  Item.Parameter :=
                    (Is_Set    => True,
                     Separator => Switch_Conf.Parameter.Separator,
                     Value     => To_Unbounded_String
                       (Arg (Arg'First + Length (Item.Switch) + 1
                             .. Arg'Last)));

               else
                  Item.Switch := Switch_Conf.Switch;
                  Item.Parameter :=
                    (Is_Set    => True,
                     Separator => (Is_Set => False),
                     Value     => To_Unbounded_String
                       (Arg (Arg'First + Length (Item.Switch) .. Arg'Last)));

               end if;

               Append (Cmd, Item, Section);
            end;
         end if;
      end loop;
   end Append_Switches;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With (Value, Prefix : Unbounded_String) return Boolean is
   begin
      return Length (Prefix) <= Length (Value)
        and then Prefix = Head (Value, Length (Prefix));
   end Starts_With;

   -----------------
   -- Find_Prefix --
   -----------------

   function Find_Prefix
     (Conf   : Configuration_References.Element_Access;
      Switch : Unbounded_String) return Unbounded_String is
   begin
      for J of Conf.Prefixes loop
         if Starts_With (Switch, J) then
            return J;
         end if;
      end loop;

      return Null_Unbounded_String;
   end Find_Prefix;

   ------------
   -- Append --
   ------------

   procedure Append
     (Cmd   : in out Command_Line;
      Value : Command_Line'Class)
   is
      List : GNAT.Strings.String_List_Access :=
        Value.To_String_List (Expanded => False);
   begin
      Make_Default_Section (Cmd.Configuration);

      if not Value.Configuration.Is_Null then
         --  Append absent section definitions
         for J of Value.Configuration.Unchecked_Get.Sections loop
            if not Cmd.Configuration.Unchecked_Get.Sections.Contains (J.Name)
            then
               Cmd.Configuration.Unchecked_Get.Sections.Insert
                 (J.Name, J);
            end if;
         end loop;
      end if;

      Cmd.Append_Switches (List.all);
      GNAT.OS_Lib.Free (List);
   end Append;

   ------------
   -- Append --
   ------------

   function Append
     (Cmd   : Command_Line'Class;
      Value : Command_Line'Class) return Command_Line is
   begin
      return Result : Command_Line do
         Result.Set_Configuration (Cmd.Get_Configuration);
         Check_Initialized (Result);

         if not Cmd.Sections.Is_Null then
            Result.Sections.Unchecked_Get.all := Cmd.Sections.Get;
         end if;

         Result.Append (Value);
      end return;
   end Append;

   ----------------------
   -- Parse_One_Switch --
   ----------------------

   procedure Parse_One_Switch
     (Config  : Command_Line_Configuration;
      Item    : Switch;
      Section : Unbounded_String;
      Prefix  : out Unbounded_String)
   is
      function Find_Parameter
        (Switch : Unbounded_String;
         Prefix : Unbounded_String;
         Pos    : Positive) return Unbounded_String;
      --  Lookup for switch argument in Item.Switch starting from Pos

      function Find_Switch
        (Prefix : Unbounded_String;
         Pos    : Positive) return Unbounded_String;
      --  Lookup for switch in Item.Switch starting from Pos

      Conf : constant Configuration_References.Element_Access :=
        Config.Unchecked_Get;
      Sect : Section_Configuration renames Conf.Sections (Section);

      --------------------
      -- Find_Parameter --
      --------------------

      function Find_Parameter
        (Switch : Unbounded_String;
         Prefix : Unbounded_String;
         Pos    : Positive) return Unbounded_String
      is
         Result : Unbounded_String;
         Param  : Parameter_Configuration;
      begin
         if Sect.Switches.Contains (Switch)
           and Pos <= Length (Item.Switch)
         then
            Param := Sect.Switches (Switch).Parameter;

            if Param.Is_Set and then not Param.Separator.Is_Set then
               Result := Delete (Item.Switch, 1, Pos - 1);

               --  Look for first switch after Pos
               for J in Pos .. Length (Item.Switch) loop
                  if Find_Switch (Prefix, J)
                       not in Null_Unbounded_String | Prefix
                  then
                     Result := Unbounded_Slice (Item.Switch, Pos, J - 1);
                     exit;
                  end if;
               end loop;
            end if;
         end if;

         return Result;
      end Find_Parameter;

      -----------------
      -- Find_Switch --
      -----------------

      function Find_Switch
        (Prefix : Unbounded_String;
         Pos    : Positive) return Unbounded_String
      is
         Switch : constant Unbounded_String :=
           Prefix & Delete (Item.Switch, 1, Pos - 1);
         Cursor : Switch_Configuration_Maps.Cursor :=
           Sect.Switches.Ceiling (Switch);
      begin
         if Switch_Configuration_Maps.Has_Element (Cursor) and then
           Switch_Configuration_Maps.Key (Cursor) /= Switch
         then
            Switch_Configuration_Maps.Previous (Cursor);
         end if;

         if Switch_Configuration_Maps.Has_Element (Cursor) and then
           Starts_With (Switch, Switch_Configuration_Maps.Key (Cursor))
         then
            return Switch_Configuration_Maps.Key (Cursor);
         else
            return Null_Unbounded_String;
         end if;
      end Find_Switch;

      Arg    : Unbounded_String;
      Switch : Unbounded_String;
      From   : Positive;
   begin
      --  Expand alias if found
      if not Item.Parameter.Is_Set
        and then Sect.Aliases.Contains (Item.Switch)
        and then not Sect.Aliases.Contains (Sect.Aliases (Item.Switch))
      then
         Process_Switch
           ((Switch    => Sect.Aliases (Item.Switch),
             Parameter => Item.Parameter));

         return;
      end if;

      --  Keep switches with parameters with separators outside of prefix group
      if not Item.Parameter.Is_Set
        or else not Item.Parameter.Separator.Is_Set
      then
         Prefix := Find_Prefix (Conf, Item.Switch);
      end if;

      --  Expand prefixed switches if found matched prefix. For example turn
      --  -gnaty3M72b into (-gnaty(3), -gnatyM(72), -gnatyb)
      if Prefix /= "" and not Item.Parameter.Is_Set then
         --  Start parsing switches just after prefix end
         From := Length (Prefix) + 1;

         --  If there is a switch exactly as prefix
         --  try to find its parameter (as -gnaty3)
         if Sect.Switches.Contains (Prefix) then
            Arg := Find_Parameter (Prefix, Prefix, From);
            From := From + Length (Arg);  --  Skip argument

            if Arg /= "" then
               Process_Switch
                 ((Switch    => Prefix,
                   Parameter => (Is_Set    => True,
                                 Separator => (Is_Set => False),
                                 Value     => Arg)));
            end if;
         end if;

         while From <= Length (Item.Switch) loop
            Switch := Find_Switch (Prefix, From);

            if Switch = "" then
               --  Rollback to single character switch if not found
               Switch := Prefix & Element (Item.Switch, From);
            end if;

            From := From + Length (Switch) - Length (Prefix);  --  Skip switch
            Arg := Find_Parameter (Switch, Prefix, From);
            From := From + Length (Arg);  --  Skip argument

            if Arg /= "" then
               Process_Switch
                 ((Switch,
                  Parameter => (Is_Set    => True,
                                Separator => (Is_Set => False),
                                Value     => Arg)));
            elsif Switch /= Item.Switch then  --  Avoid infinite recursion
               Process_Switch
                 ((Switch, Parameter => (Is_Set => False)));
            end if;
         end loop;
      end if;
   end Parse_One_Switch;

   ------------
   -- Append --
   ------------

   procedure Append
     (Cmd        : in out Command_Line;
      Item       : Switch;
      Section    : Unbounded_String;
      Add_Before : Boolean := False)
   is
      procedure Append_To_Section
        (Section : in out Command_Lines.Section;
         Prefix  : Unbounded_String;
         Item    : Switch);
      --  Append Item to Section taking switch Prefix into account

      procedure Append_Recursive (Item : Switch);
      --  Call Append for given Item. Set Appended to True

      Appended : Boolean := False;

      -----------------------
      -- Append_To_Section --
      -----------------------

      procedure Append_To_Section
        (Section : in out Command_Lines.Section;
         Prefix  : Unbounded_String;
         Item    : Switch) is
      begin
         if Prefix /= "" then
            if not Section.Prefixes.Contains (Prefix) then
               Section.Prefixes.Insert
                 (Prefix, Argument_Maps.Empty_Map);
            end if;

            Section.Prefixes (Prefix).Include
              (Item.Switch, Item.Parameter);

         elsif Add_Before then
            Section.Switches.Prepend (Item);
         else
            Section.Switches.Append (Item);
         end if;
      end Append_To_Section;

      ----------------------
      -- Append_Recursive --
      ----------------------

      procedure Append_Recursive (Item : Switch) is
      begin
         Append (Cmd, Item, Section, Add_Before);
         Appended := True;
      end Append_Recursive;

      procedure Parse_Switch is new Parse_One_Switch (Append_Recursive);

      Prefix   : Unbounded_String;
      Sections : Section_Maps.Map renames Cmd.Sections.Unchecked_Get.all;
   begin
      if not Sections.Contains (Section) then
         --  Create section if don't have it yet
         Sections.Insert (Section, Empty_Section);
      end if;

      Parse_Switch (Cmd.Configuration, Item, Section, Prefix);

      if not Appended then
         --  If we hadn't appended it before, do it now
         Append_To_Section (Sections (Section), Prefix, Item);
      end if;
   end Append;

   -------------------
   -- Append_Switch --
   -------------------

   procedure Append_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False)
   is
      Success : Boolean;
   begin
      Append_Switch
        (Cmd, Switch, Parameter, Separator, Section, Add_Before, Success);
   end Append_Switch;

   -------------------
   -- Append_Switch --
   -------------------

   procedure Append_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False;
      Success    : out Boolean)
   is
      Item : Command_Lines.Switch;
   begin
      Check_Initialized (Cmd);
      Item.Switch := To_Unbounded_String (Switch);

      if Parameter /= "" then
         Item.Parameter :=
           (Is_Set    => True,
            Separator => (Is_Set => False),
            Value     => To_Unbounded_String (Parameter));

         if Separator /= ASCII.NUL then
            Item.Parameter.Separator := (Is_Set => True, Value => Separator);
         end if;
      end if;

      Append (Cmd, Item, To_Unbounded_String (Section), Add_Before);
      Success := True;
   end Append_Switch;

   -------------------
   -- Remove_Switch --
   -------------------

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Has_Parameter : Boolean := False;
      Section       : String  := "")
   is
      Success : Boolean;
   begin
      Remove_Switch (Cmd, Switch, Has_Parameter, Section, Success);
   end Remove_Switch;

   -------------------
   -- Remove_Switch --
   -------------------

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Has_Parameter : Boolean := False;
      Section       : String  := "";
      Success       : out Boolean) is
   begin
      Remove_Switch
        (Cmd           => Cmd,
         Switch        => To_Unbounded_String (Switch),
         Has_Parameter => Has_Parameter,
         Section       => To_Unbounded_String (Section),
         Success       => Success);
   end Remove_Switch;

   -------------------
   -- Remove_Switch --
   -------------------

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : Unbounded_String;
      Has_Parameter : Boolean := False;
      Section       : Unbounded_String;
      Success       : out Boolean)
   is
      procedure Remove_From_Section
        (Section : in out Command_Lines.Section;
         Prefix  : Unbounded_String);
      --  Remove Item from Section taking switch Prefix into account

      procedure Remove_Recursive (Item : Command_Lines.Switch);
      --  Call Remove_Switch for given Item. Set Removed to True

      Removed  : Boolean := False;

      procedure Remove_From_Section
        (Section : in out Command_Lines.Section;
         Prefix  : Unbounded_String)
      is
         Pos : Switch_Vectors.Cursor := Section.Switches.First;
      begin
         if Prefix = "" then
            while Switch_Vectors.Has_Element (Pos) loop
               declare
                  Next : constant Command_Lines.Switch :=
                    Switch_Vectors.Element (Pos);
               begin
                  if Next.Switch = Switch
                    and then Next.Parameter.Is_Set = Has_Parameter
                  then
                     Section.Switches.Delete (Pos);
                     Success := True;
                     return;
                  end if;

                  Switch_Vectors.Next (Pos);
               end;
            end loop;
         elsif Section.Prefixes.Contains (Prefix)
           and then Section.Prefixes (Prefix).Contains (Switch)
         then
            Section.Prefixes (Prefix).Delete (Switch);

            if Section.Prefixes (Prefix).Is_Empty then
               --  Prefix becomes empty, drop it
               Section.Prefixes.Delete (Prefix);
            end if;

            Success := True;
         end if;
      end Remove_From_Section;

      ----------------------
      -- Remove_Recursive --
      ----------------------

      procedure Remove_Recursive (Item : Command_Lines.Switch) is
      begin
         Remove_Switch
           (Cmd           => Cmd,
            Switch        => Item.Switch,
            Has_Parameter => Item.Parameter.Is_Set,
            Section       => Section,
            Success       => Success);
         Removed := True;
      end Remove_Recursive;

      procedure Parse_Switch is new Parse_One_Switch (Remove_Recursive);

      Prefix   : Unbounded_String;
      Sections : Section_Map_References.Element_Access;
   begin
      Success := False;
      Check_Initialized (Cmd);

      Sections := Cmd.Sections.Unchecked_Get;

      if not Sections.Contains (Section) then
         return;
      end if;

      if Has_Parameter then
         Parse_Switch
           (Cmd.Configuration,
            (Switch    => Switch,
             Parameter =>
               (Is_Set    => True,
                Separator => (Is_Set => False),
                Value     => Null_Unbounded_String)),
            Section,
            Prefix);
      else
         Parse_Switch
           (Cmd.Configuration,
            (Switch => Switch,
             Parameter => (Is_Set => False)),
            Section,
            Prefix);
      end if;

      if not Removed then
         --  If we hadn't appended it before, do it now
         Remove_From_Section (Sections.all (Section), Prefix);
      end if;
   end Remove_Switch;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Command_Line) return Boolean is
   begin
      if Self.Sections.Is_Null then
         return True;
      end if;

      for Section of Self.Sections.Unchecked_Get.all loop
         if not Section.Switches.Is_Empty then
            return False;
         else
            for Prefixed of Section.Prefixes loop
               if not Prefixed.Is_Empty then
                  return False;
               end if;
            end loop;
         end if;
      end loop;

      return True;
   end Is_Empty;

   ----------------
   -- Has_Switch --
   ----------------

   function Has_Switch
     (Cmd     : Command_Line;
      Switch  : String;
      Section : String  := "") return Boolean is
   begin
      return Has_Switch
        (Cmd, To_Unbounded_String (Switch), To_Unbounded_String (Section));
   end Has_Switch;

   ----------------
   -- Has_Switch --
   ----------------

   function Has_Switch
     (Cmd     : Command_Line;
      Switch  : Unbounded_String;
      Section : Unbounded_String) return Boolean
   is
      Pos : Switch_Vectors.Cursor;
      Conf     : Configuration_References.Element_Access;
      Prefix   : Unbounded_String;
      Sections : Section_Map_References.Element_Access;
   begin
      if Cmd.Sections.Is_Null then
         return False;
      end if;

      Conf := Cmd.Configuration.Unchecked_Get;
      Prefix := Find_Prefix (Conf, Switch);
      Sections := Cmd.Sections.Unchecked_Get;

      if not Sections.Contains (Section) then
         return False;
      end if;

      declare
         Sect  : Command_Lines.Section renames Sections.all (Section);
      begin
         if Prefix /= "" then
            return Sect.Prefixes.Contains (Prefix) and then
              Sect.Prefixes (Prefix).Contains (Switch);
         end if;

         Pos := Sect.Switches.First;

         while Switch_Vectors.Has_Element (Pos) loop
            declare
               Item : constant Command_Lines.Switch :=
                 Switch_Vectors.Element (Pos);
            begin
               if Item.Switch = Switch then
                  return True;
               end if;

               Switch_Vectors.Next (Pos);
            end;
         end loop;

         return False;
      end;
   end Has_Switch;

   -----------
   -- Start --
   -----------

   procedure Start
     (Cmd      : Command_Line;
      Iter     : in out Command_Line_Iterator;
      Expanded : Boolean := False)
   is
      Copy : Command_Line := Cmd;
   begin
      Check_Initialized (Copy);

      if Expanded then
         Iter :=
           (Expanded       => True,
            Line           => Copy,
            Section        => Copy.Sections.Unchecked_Get.First,
            Prefixed       => Prefixed_Switch_Maps.No_Element,
            Switch         => Switch_Vectors.No_Element,
            Argument       => Argument_Maps.No_Element,
            Is_New_Section => True);

      else
         Iter :=
           (Expanded       => False,
            Line           => Copy,
            Section        => Copy.Sections.Unchecked_Get.First,
            Prefixed       => Prefixed_Switch_Maps.No_Element,
            Switch         => Switch_Vectors.No_Element,
            Is_New_Section => True);
      end if;

      if Section_Maps.Has_Element (Iter.Section) then
         Update (Iter);
      end if;
   end Start;

   --------------------
   -- Current_Switch --
   --------------------

   function Current_Switch (Iter : Command_Line_Iterator) return Switch is
   begin
      if Switch_Vectors.Has_Element (Iter.Switch) then
         return Switch_Vectors.Element (Iter.Switch);
      elsif Iter.Expanded then
         return (Argument_Maps.Key (Iter.Argument),
                 Argument_Maps.Element (Iter.Argument));
      else
         raise Constraint_Error;
      end if;
   end Current_Switch;

   --------------------
   -- Current_Switch --
   --------------------

   function Current_Switch (Iter : Command_Line_Iterator) return String is
      Result : Unbounded_String;
   begin
      if Switch_Vectors.Has_Element (Iter.Switch) then
         Result := Switch_Vectors.Element (Iter.Switch).Switch;
      elsif Iter.Expanded then
         return To_String (Argument_Maps.Key (Iter.Argument));
      else
         --  Collect all prefixed switches into one single switch
         declare
            Map    : constant Argument_Maps.Map :=
              Prefixed_Switch_Maps.Element (Iter.Prefixed);
            Cursor : Argument_Maps.Cursor := Map.First;
            Arg    : Argument;
            Text   : Unbounded_String;
            Strip   : Natural;
         begin
            Result := Prefixed_Switch_Maps.Key (Iter.Prefixed);
            Strip := Length (Result);
            while Argument_Maps.Has_Element (Cursor) loop
               Text := Argument_Maps.Key (Cursor);
               Delete (Text, 1, Strip);  --  Drop prefix, we have it already
               Append (Result, Text);
               Arg := Argument_Maps.Element (Cursor);

               if Arg.Is_Set then
                  Append (Result, Arg.Value);
               end if;

               Argument_Maps.Next (Cursor);
            end loop;

         end;
      end if;

      if not Iter.Expanded then
         --  Try to shring aliases
         declare
            Section : Section_Configuration renames
              Iter.Line.Configuration.Unchecked_Get.Sections
                (Section_Maps.Key (Iter.Section));
         begin
            if Section.Expanded.Contains (Result) then
               Result := Section.Expanded (Result);
            end if;
         end;
      end if;

      return To_String (Result);
   end Current_Switch;

   --------------------
   -- Is_New_Section --
   --------------------

   function Is_New_Section (Iter : Command_Line_Iterator) return Boolean is
   begin
      return Iter.Is_New_Section;
   end Is_New_Section;

   ---------------------
   -- Current_Section --
   ---------------------

   function Current_Section (Iter : Command_Line_Iterator) return String is
   begin
      return To_String (Section_Maps.Key (Iter.Section));
   end Current_Section;

   -----------------------
   -- Current_Separator --
   -----------------------

   function Current_Separator (Iter : Command_Line_Iterator) return String is
      Result : Separator;
   begin
      if Switch_Vectors.Has_Element (Iter.Switch) then
         declare
            Parameter : Argument renames
              Switch_Vectors.Element (Iter.Switch).Parameter;
         begin
            if Parameter.Is_Set then
               Result := Parameter.Separator;

               if Result.Is_Set then
                  return (1 => Result.Value);
               end if;
            end if;
         end;
      end if;

      return "";
   end Current_Separator;

   -----------------------
   -- Current_Parameter --
   -----------------------

   function Current_Parameter (Iter : Command_Line_Iterator) return String is
      Result : Argument;
   begin
      if Switch_Vectors.Has_Element (Iter.Switch) then
         declare
            Parameter : Argument renames
              Switch_Vectors.Element (Iter.Switch).Parameter;
         begin
            if Parameter.Is_Set then
               return To_String (Parameter.Value);
            end if;
         end;
      elsif Iter.Expanded then
         Result := Argument_Maps.Element (Iter.Argument);

         if Result.Is_Set then
            return To_String (Result.Value);
         end if;
      end if;

      return "";
   end Current_Parameter;

   --------------
   -- Has_More --
   --------------

   function Has_More (Iter : Command_Line_Iterator) return Boolean is
   begin
      return Section_Maps.Has_Element (Iter.Section);
   end Has_More;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Command_Line_Iterator) is
   begin
      if Has_More (Iter) then
         Iter.Is_New_Section := False;

         if Iter.Expanded then
            if Argument_Maps.Has_Element (Iter.Argument) then
               Argument_Maps.Next (Iter.Argument);

               if Argument_Maps.Has_Element (Iter.Argument) then
                  return;
               end if;
            end if;
         end if;

         if Switch_Vectors.Has_Element (Iter.Switch) then
            Switch_Vectors.Next (Iter.Switch);

            if Switch_Vectors.Has_Element (Iter.Switch) then
               return;
            end if;

            declare
               Section : Command_Lines.Section renames
                 Iter.Line.Sections.Unchecked_Get.all (Iter.Section);
            begin
               Iter.Prefixed := Section.Prefixes.First;
            end;

         elsif Prefixed_Switch_Maps.Has_Element (Iter.Prefixed) then
            Prefixed_Switch_Maps.Next (Iter.Prefixed);
         end if;

         if Prefixed_Switch_Maps.Has_Element (Iter.Prefixed) then
            if Iter.Expanded then
               declare
                  Section : Command_Lines.Section renames
                    Iter.Line.Sections.Unchecked_Get.all (Iter.Section);
               begin
                  Iter.Argument := Section.Prefixes (Iter.Prefixed).First;
               end;
            end if;

            return;
         end if;

         Iter.Is_New_Section := True;
         Section_Maps.Next (Iter.Section);

         if Section_Maps.Has_Element (Iter.Section) then
            Update (Iter);
         end if;
      end if;
   end Next;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (Cmd      : Command_Line;
      Expanded : Boolean) return GNAT.Strings.String_List_Access
   is
      Result : GNAT.Strings.String_List_Access;
      Iter   : Command_Line_Iterator;
      Count  : Natural := 0;
   begin
      Start (Cmd, Iter, Expanded => Expanded);

      while Has_More (Iter) loop
         Count := Count + 1;

         if Is_New_Section (Iter) and then Current_Section (Iter) /= "" then
            Count := Count + 1;
         end if;

         if Current_Separator (Iter) = " "
           and then Current_Parameter (Iter) /= ""
         then
            Count := Count + 1;
         end if;

         Next (Iter);
      end loop;

      Result := new GNAT.Strings.String_List (1 .. Count);
      Count := Result'First;
      Start (Cmd, Iter, Expanded => Expanded);

      while Has_More (Iter) loop
         if Is_New_Section (Iter) and then Current_Section (Iter) /= "" then
            Result (Count) := new String'(Current_Section (Iter));
            Count := Count + 1;
         end if;

         if Current_Separator (Iter) /= " " then
            if Current_Parameter (Iter) /= "" then
               Result (Count) := new String'
                 (Current_Switch (Iter)
                  & Current_Separator (Iter)
                  & Current_Parameter (Iter));

            else
               Result (Count) := new String'(Current_Switch (Iter));
            end if;

            Count := Count + 1;

         else
            Result (Count) := new String'(Current_Switch (Iter));
            Count := Count + 1;

            if Current_Parameter (Iter) /= "" then
               Result (Count) := new String'(Current_Parameter (Iter));
               Count := Count + 1;
            end if;
         end if;

         Next (Iter);
      end loop;

      return Result;
   end To_String_List;

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Command_Line_Iterator) is
      Section : Command_Lines.Section renames
        Self.Line.Sections.Unchecked_Get.all (Self.Section);
   begin
      Self.Prefixed := Prefixed_Switch_Maps.No_Element;
      Self.Switch := Section.Switches.First;

      if Switch_Vectors.Has_Element (Self.Switch) then
         return;
      end if;

      Self.Prefixed := Section.Prefixes.First;

      while Prefixed_Switch_Maps.Has_Element (Self.Prefixed) loop
         if not Section.Prefixes (Self.Prefixed).Is_Empty then
            if Self.Expanded then
               Self.Argument := Section.Prefixes (Self.Prefixed).First;
            end if;

            return;
         end if;

         Prefixed_Switch_Maps.Next (Self.Prefixed);
      end loop;

      Section_Maps.Next (Self.Section);

      if Section_Maps.Has_Element (Self.Section) then
         Update (Self);
      end if;
   end Update;

   ---------
   -- Map --
   ---------

   function Map
     (Cmd    : Command_Line;
      Update : access procedure
        (Switch    : in out Unbounded_String;
         Section   : in out Unbounded_String;
         Parameter : in out Argument)) return Command_Line
   is
      Result : Command_Line;
      Iter   : Command_Line_Iterator;
   begin
      Result.Set_Configuration (Cmd.Get_Configuration);
      Check_Initialized (Result);
      Cmd.Start (Iter, Expanded => True);

      while Has_More (Iter) loop
         declare
            Item    : Switch := Current_Switch (Iter);
            Section : Unbounded_String :=
              To_Unbounded_String (Current_Section (Iter));
         begin
            Update (Item.Switch, Section, Item.Parameter);
            Append (Result, Item, Section);
            Next (Iter);
         end;
      end loop;

      return Result;
   end Map;

   ------------
   -- Filter --
   ------------

   function Filter
     (Cmd    : Command_Line;
      Delete : access function
        (Switch    : String;
         Section   : String;
         Parameter : Argument) return Boolean)
      return Command_Line
   is
      Result : Command_Line;
      Iter   : Command_Line_Iterator;
   begin
      Result.Set_Configuration (Cmd.Get_Configuration);
      Check_Initialized (Result);
      Cmd.Start (Iter, Expanded => True);

      while Has_More (Iter) loop
         declare
            Item    : constant Switch := Current_Switch (Iter);
            Section : constant String := Current_Section (Iter);
         begin
            if not Delete (To_String (Item.Switch),
                           Section,
                           Item.Parameter)
            then
               Append (Result, Item, To_Unbounded_String (Section));
            end if;

            Next (Iter);
         end;
      end loop;

      return Result;
   end Filter;

end Command_Lines;
