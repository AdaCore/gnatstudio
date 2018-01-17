------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Arg_Lists;         use GNATCOLL.Arg_Lists;

package body Switches_Chooser is
   use Switch_Description_Vectors, Combo_Switch_Vectors;
   use Frame_Description_Vectors;
   use Command_Lines;

   procedure Add_To_Getopt
     (Config    : Switches_Editor_Config;
      Switch    : String;
      Separator : Character;
      Section   : String);
   --  Add Switch to the automatically constructed getopt string.
   --  If Separator is ASCII.NUL, then the switches takes a parameter, but
   --  might have no separator.
   --  If it is ASCII.LF, the switch takes no parameter.
   --  If it is ASCII.CR, the switch takes an optional parameter

   procedure Free_List (Deps : in out Dependency_Description_Access);
   --  Free the whole list of dependencies

   procedure Free (Dep : in out Default_Value_Dependency);
   --  Free memory occupied by Dep

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (Deps : in out Dependency_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Dependency_Description, Dependency_Description_Access);
      N : Dependency_Description_Access;
   begin
      while Deps /= null loop
         N := Deps.Next;
         Free (Deps.Slave_Tool);
         Free (Deps.Master_Switch);
         Free (Deps.Slave_Switch);
         Free (Deps.Master_Section);
         Free (Deps.Slave_Section);
         Unchecked_Free (Deps);
         Deps := N;
      end loop;
   end Free_List;

   ----------
   -- Free --
   ----------

   procedure Free (Dep : in out Default_Value_Dependency) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Default_Value_Dependency_Record, Default_Value_Dependency);
      Tmp : Default_Value_Dependency;
   begin
      while Dep /= null loop
         Tmp := Dep.Next;
         Unchecked_Free (Dep);
         Dep := Tmp;
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Config : in out Switches_Editor_Config) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Switches_Editor_Config_Record'Class, Switches_Editor_Config);
      C : Switch_Description_Vectors.Cursor;
      Dep : Default_Value_Dependency;
   begin
      if Config /= null then
         C := First (Config.Switches);
         while Has_Element (C) loop
            if Element (C).Typ = Switch_Check then
               Dep := Element (C).Dependencies;
               Free (Dep);
            end if;
            Next (C);
         end loop;

         Free (Config.Config);
         Free_List (Config.Dependencies);
         Unchecked_Free (Config);
      end if;
   end Free;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Self      : not null access Switches_Editor_Config_Record'Class;
      Lines     : Integer;
      Columns   : Integer;
      For_Popup : Popup_Index := Main_Window)
   is
      C : Switch_Description_Vectors.Cursor;
   begin
      if For_Popup = Main_Window then
         Self.Lines := Lines;
         Self.Columns := Columns;
      else
         C := First (Self.Switches);
         while Has_Element (C) loop
            if Element (C).Typ = Switch_Popup
              and then Element (C).To_Popup = For_Popup
            then
               Self.Switches.Reference (C).Lines := Lines;
               Self.Switches.Reference (C).Columns := Columns;
               exit;
            end if;
            Next (C);
         end loop;
      end if;
   end Set_Size;

   ------------
   -- Create --
   ------------

   function Create
     (Default_Separator : String;
      Switch_Char       : Character := '-';
      Scrolled_Window   : Boolean := False;
      Show_Command_Line : Boolean := True;
      Sections          : String := "") return Switches_Editor_Config
   is
      Config : Switches_Editor_Config;
      Start, Stop : Natural;
   begin
      Config := new Switches_Editor_Config_Record'
        (Lines             => 1,
         Columns           => 1,
         Default_Separator => To_Unbounded_String (Default_Separator),
         Scrolled_Window   => Scrolled_Window,
         Switch_Char       => Switch_Char,
         Config            => <>,
         Max_Radio         => 0,
         Max_Popup         => Main_Window,
         Show_Command_Line => Show_Command_Line,
         Sections          => To_Unbounded_String (Sections),
         Switches          => <>,
         Frames            => <>,
         Filters           => <>,
         Dependencies      => null);

      --  Add star to getopt switches
      Define_Switch (Config.Config, "*");

      --  Add sections to getopt switches
      Start := Sections'First;
      Stop  := Start + 1;

      while Stop <= Sections'Last loop
         if Sections (Stop) = ' ' then
            Define_Section (Config.Config, Sections (Start .. Stop - 1));
            Start := Stop + 1;
            Stop := Start;

         elsif Stop = Sections'Last then
            Define_Section (Config.Config, Sections (Start .. Stop));
         end if;

         Stop := Stop + 1;
      end loop;

      return Config;
   end Create;

   -------------------
   -- Define_Prefix --
   -------------------

   procedure Define_Prefix
     (Config : Switches_Editor_Config;
      Prefix : String) is
   begin
      Define_Prefix (Config.Config, Prefix);
   end Define_Prefix;

   ------------------
   -- Define_Alias --
   ------------------

   procedure Define_Alias
     (Config   : Switches_Editor_Config;
      Switch   : String;
      Expanded : String) is
   begin
      Define_Alias (Config.Config, Switch, Expanded);
   end Define_Alias;

   ---------------------
   -- Set_Frame_Title --
   ---------------------

   procedure Set_Frame_Title
     (Config    : Switches_Editor_Config;
      Title     : String;
      Line      : Positive := 1;
      Column    : Positive := 1;
      Line_Span : Natural := 1;
      Col_Span  : Natural := 1;
      Popup     : Popup_Index := Main_Window)
   is
   begin
      Append
        (Config.Frames,
         Frame_Description'
           (Title     => To_Unbounded_String (Title),
            Line      => Line,
            Column    => Column,
            Popup     => Popup,
            Line_Span => Line_Span,
            Col_Span  => Col_Span));
   end Set_Frame_Title;

   ------------------------
   -- Empty_Command_Line --
   ------------------------

   function Empty_Command_Line
     (Switches : access Switches_Editor_Config_Record'Class)
      return Command_Lines.Command_Line is
   begin
      if Switches = null then
         return Result : Command_Lines.Command_Line;
      end if;

      return Result : Command_Lines.Command_Line do
         Result.Set_Configuration (Switches.Config);
      end return;
   end Empty_Command_Line;

   -------------------
   -- Add_To_Getopt --
   -------------------

   procedure Add_To_Getopt
     (Config    : Switches_Editor_Config;
      Switch    : String;
      Separator : Character;
      Section   : String)
   is
   begin
      if Separator = ASCII.LF then
         --  No parameter
         Define_Switch (Config.Config, Switch, Section);
      elsif Separator = ASCII.NUL then
         Define_Switch_With_Parameter (Config.Config, Switch, Section);
      elsif Separator = ASCII.CR then
         Define_Switch_With_Parameter
           (Config.Config, Switch, Section, Optional => True);
      else
         Define_Switch_With_Parameter
           (Config.Config, Switch, Section, Separator => Separator);
      end if;
   end Add_To_Getopt;

   ---------------
   -- Add_Check --
   ---------------

   procedure Add_Check
     (Config        : Switches_Editor_Config;
      Label         : String;
      Switch_Set    : String;
      Switch_Unset  : String;
      Default_State : Boolean;
      Section       : String := "";
      Tip           : String := "";
      Line          : Positive := 1;
      Column        : Positive := 1;
      Add_Before    : Boolean := False;
      Popup         : Popup_Index := Main_Window;
      Filter        : String := "")
   is
   begin
      Append
        (Config.Switches,
         Switch_Description'
           (Typ           => Switch_Check,
            Switch        => To_Unbounded_String (Switch_Set),
            Switch_Unset  => To_Unbounded_String (Switch_Unset),
            Default_State => Default_State,
            Initial_State => Default_State,
            Dependencies  => null,
            Label         => To_Unbounded_String (Label),
            Tip           => To_Unbounded_String (Tip),
            Section       => To_Unbounded_String (Section),
            Separator     => ASCII.NUL,
            Popup         => Popup,
            Line          => Line,
            Column        => Column,
            Add_First     => Add_Before,
            Active        => True));

      if Switch_Set /= "" then
         Add_To_Getopt
           (Config,
            Switch    => Switch_Set,
            Separator => ASCII.LF,
            Section   => Section);
      end if;

      if Switch_Unset /= "" then
         Add_To_Getopt
           (Config,
            Switch    => Switch_Unset,
            Separator => ASCII.LF,
            Section   => Section);
      end if;

      if Filter /= "" then
         Config.Filters.Append
           (new Switch_Filter_Description_Record'
              (Name         => To_Unbounded_String (Filter),
               Switch       => Config.Switches.Last_Index,
               Add_On_Match => False));
      end if;
   end Add_Check;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Config       : Switches_Editor_Config;
      Label        : String;
      Switch       : String;
      Separator    : String := ""; --  no separator
      Section      : String := "";
      Tip          : String := "";
      As_Directory : Boolean := False;
      As_File      : Boolean := False;
      Line         : Positive := 1;
      Column       : Positive := 1;
      Add_Before   : Boolean := False;
      Popup        : Popup_Index := Main_Window;
      Filter       : String := "")
   is
      Sep : Character := ASCII.NUL;
   begin
      if Separator /= "" then
         Sep := Separator (Separator'First);
      end if;

      Append
        (Config.Switches,
         Switch_Description'
           (Typ          => Switch_Field,
            Switch       => To_Unbounded_String (Switch),
            Label        => To_Unbounded_String (Label),
            Tip          => To_Unbounded_String (Tip),
            Section      => To_Unbounded_String (Section),
            Separator    => Sep,
            As_Directory => As_Directory,
            As_File      => As_File,
            Line         => Line,
            Column       => Column,
            Add_First    => Add_Before,
            Popup        => Popup,
            Active       => True));
      Add_To_Getopt
        (Config,
         Switch    => Switch,
         Separator => Sep,
         Section   => Section);

      if Filter /= "" then
         Config.Filters.Append
           (new Switch_Filter_Description_Record'
              (Name         => To_Unbounded_String (Filter),
               Switch       => Config.Switches.Last_Index,
               Add_On_Match => False));
      end if;
   end Add_Field;

   ----------------
   -- Add_Hidden --
   ----------------

   procedure Add_Hidden
     (Config    : Switches_Editor_Config;
      Switch    : String;
      Separator : String := "")
   is
      Sep : Character := ASCII.NUL;

   begin
      if Separator /= "" then
         Sep := Separator (Separator'First);
      end if;

      Add_To_Getopt (Config, Switch, Sep, Section => "");
   end Add_Hidden;

   --------------
   -- Add_Spin --
   --------------

   procedure Add_Spin
     (Config     : Switches_Editor_Config;
      Label      : String;
      Switch     : String;
      Separator  : String := ""; --  no separator
      Min        : Integer;
      Max        : Integer;
      Default    : Integer;
      Section    : String := "";
      Tip        : String := "";
      Line       : Positive := 1;
      Column     : Positive := 1;
      Add_Before : Boolean := False;
      Popup      : Popup_Index := Main_Window;
      Filter     : String := "")
   is
      Sep : Character := ASCII.NUL;
   begin
      if Separator /= "" then
         Sep := Separator (Separator'First);
      end if;
      Append
        (Config.Switches,
         Switch_Description'
           (Typ       => Switch_Spin,
            Switch    => To_Unbounded_String (Switch),
            Label     => To_Unbounded_String (Label),
            Tip       => To_Unbounded_String (Tip),
            Section   => To_Unbounded_String (Section),
            Separator => Sep,
            Min       => Min,
            Max       => Max,
            Default   => Default,
            Line      => Line,
            Column    => Column,
            Add_First => Add_Before,
            Popup     => Popup,
            Active    => True));
      Add_To_Getopt
        (Config,
         Switch    => Switch,
         Separator => Sep,
         Section   => Section);

      if Filter /= "" then
         Config.Filters.Append
           (new Switch_Filter_Description_Record'
              (Name         => To_Unbounded_String (Filter),
               Switch       => Config.Switches.Last_Index,
               Add_On_Match => False));
      end if;
   end Add_Spin;

   ---------------
   -- Add_Combo --
   ---------------

   procedure Add_Combo
     (Config     : Switches_Editor_Config;
      Label      : String;
      Switch     : String;
      Separator  : String := ""; --  no separator
      No_Switch  : String;
      No_Digit   : String;
      Entries    : Combo_Switch_Array;
      Section    : String := "";
      Tip        : String := "";
      Line       : Positive := 1;
      Column     : Positive := 1;
      Add_Before : Boolean := False;
      Popup      : Popup_Index := Main_Window;
      Filter     : String := "")
   is
      Ent : Combo_Switch_Vectors.Vector;
      S   : Character := ASCII.NUL;
   begin
      for E in Entries'Range loop
         Append (Ent, Entries (E));
      end loop;

      if Separator /= "" then
         S := Separator (Separator'First);
      end if;

      Append
        (Config.Switches,
         Switch_Description'
           (Typ       => Switch_Combo,
            Switch    => To_Unbounded_String (Switch),
            Label     => To_Unbounded_String (Label),
            Tip       => To_Unbounded_String (Tip),
            Section   => To_Unbounded_String (Section),
            Separator => S,
            No_Switch => To_Unbounded_String (No_Switch),
            No_Digit  => To_Unbounded_String (No_Digit),
            Entries   => Ent,
            Line      => Line,
            Column    => Column,
            Add_First => Add_Before,
            Popup     => Popup,
            Active    => True));

      if Separator = "" then
         --  optional parameter
         Add_To_Getopt
           (Config,
            Switch    => Switch,
            Separator => ASCII.CR,
            Section   => Section);
      else
         Add_To_Getopt
           (Config,
            Switch    => Switch,
            Separator => Separator (Separator'First),
            Section   => Section);
      end if;

      if Filter /= "" then
         Config.Filters.Append
              (new Switch_Filter_Description_Record'
                   (Name         => To_Unbounded_String (Filter),
                    Switch       => Config.Switches.Last_Index,
                    Add_On_Match => False));
      end if;
   end Add_Combo;

   ---------------
   -- Add_Popup --
   ---------------

   function Add_Popup
     (Config  : Switches_Editor_Config;
      Label   : String;
      Line    : Positive := 1;
      Column  : Positive := 1;
      Popup   : Popup_Index := Main_Window) return Popup_Index
   is
   begin
      Config.Max_Popup := Config.Max_Popup + 1;
      Append
        (Config.Switches,
         Switch_Description'
           (Typ       => Switch_Popup,
            Switch    => Null_Unbounded_String,
            Label     => To_Unbounded_String (Label),
            Tip       => Null_Unbounded_String,
            Section   => Null_Unbounded_String,
            Separator => ASCII.NUL,
            Line      => Line,
            Column    => Column,
            Lines     => 1,
            Columns   => 1,
            Add_First => False,
            Popup     => Popup,
            To_Popup  => Config.Max_Popup,
            Active    => True));
      return Config.Max_Popup;
   end Add_Popup;

   ---------------
   -- Add_Radio --
   ---------------

   function Add_Radio
     (Config  : Switches_Editor_Config;
      Label   : String;
      Tip     : String;
      Line    : Positive := 1;
      Column  : Positive := 1;
      Popup   : Popup_Index := Main_Window) return Radio_Switch
   is
   begin
      Config.Max_Radio := Config.Max_Radio + 1;
      Append
        (Config.Switches,
         Switch_Description'
           (Typ       => Switch_Radio,
            Is_Entry  => False,
            Switch    => Null_Unbounded_String,
            Label     => To_Unbounded_String (Label),
            Tip       => To_Unbounded_String (Tip),
            Section   => Null_Unbounded_String,
            Separator => ASCII.NUL,
            Group     => Config.Max_Radio,
            Line      => Line,
            Column    => Column,
            Add_First => False,
            Popup     => Popup,
            Active    => True));
      return Config.Max_Radio;
   end Add_Radio;

   ---------------------
   -- Add_Radio_Entry --
   ---------------------

   procedure Add_Radio_Entry
     (Config     : Switches_Editor_Config;
      Radio      : Radio_Switch;
      Label      : String;
      Switch     : String;
      Section    : String := "";
      Tip        : String := "";
      Add_Before : Boolean := False;
      Filter     : String := "")
   is
   begin
      Append
        (Config.Switches,
         Switch_Description'
           (Typ       => Switch_Radio,
            Is_Entry  => True,
            Switch    => To_Unbounded_String (Switch),
            Label     => To_Unbounded_String (Label),
            Tip       => To_Unbounded_String (Tip),
            Section   => To_Unbounded_String (Section),
            Separator => ASCII.NUL,
            Group     => Radio,
            Line      => 1,
            Column    => 1,
            Add_First => Add_Before,
            Popup     => Main_Window,
            Active    => True));

      if Switch /= "" then
         Add_To_Getopt
           (Config,
            Switch    => Switch,
            Separator => ASCII.LF,
            Section   => Section);
      end if;

      if Filter /= "" then
         Config.Filters.Append
              (new Switch_Filter_Description_Record'
                   (Name         => To_Unbounded_String (Filter),
                    Switch       => Config.Switches.Last_Index,
                    Add_On_Match => False));
      end if;
   end Add_Radio_Entry;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (Config         : Switches_Editor_Config;
      Switch         : String;
      Section        : String;
      Status         : Boolean;
      Slave_Tool     : String;
      Slave_Switch   : String;
      Slave_Section  : String;
      Slave_Activate : Boolean := True) is
   begin
      Config.Dependencies := new Dependency_Description'
        (Next           => Config.Dependencies,
         Master_Switch  => new String'(Switch),
         Master_Section => new String'(Section),
         Master_Status  => Status,
         Slave_Tool     => new String'(Slave_Tool),
         Slave_Section  => new String'(Slave_Section),
         Slave_Switch   => new String'(Slave_Switch),
         Slave_Status   => Slave_Activate);
   end Add_Dependency;

   ----------------------------------
   -- Add_Default_Value_Dependency --
   ----------------------------------

   procedure Add_Default_Value_Dependency
     (Config         : Switches_Editor_Config;
      Switch         : String;
      Section        : String;
      Slave_Switch   : String;
      Slave_Section  : String;
      Slave_Status   : Boolean := True)
   is
      Cursor        : Switch_Description_Vectors.Cursor :=
                        Config.Switches.First;
      Slave_Cursor  : Switch_Description_Vectors.Cursor :=
                       Switch_Description_Vectors.No_Element;
      Master_Cursor : Switch_Description_Vectors.Cursor :=
                       Switch_Description_Vectors.No_Element;

   begin
      while Has_Element (Cursor) loop
         declare
            Sw : Switch_Description renames Element (Cursor);
         begin
            if Sw.Typ = Switch_Check then
               if To_String (Sw.Section) = Slave_Section
                 and then
                   (To_String (Sw.Switch) = Slave_Switch
                    or else To_String (Sw.Switch_Unset) = Slave_Switch)
               then
                  Slave_Cursor := Cursor;

               elsif To_String (Sw.Section) = Section
                 and then To_String (Sw.Switch) = Switch
               then
                  Master_Cursor := Cursor;
               end if;

               exit when Has_Element (Master_Cursor)
                 and then Has_Element (Slave_Cursor);
            end if;
         end;

         Next (Cursor);
      end loop;

      if Has_Element (Master_Cursor)
        and then Has_Element (Slave_Cursor)
      then
         declare
            Slave  : Switch_Description :=
                       Element (Slave_Cursor);
            Enable : Boolean;
         begin
            if To_String (Slave.Switch) = Slave_Switch then
               Enable := Slave_Status;
            else
               Enable := False;
            end if;

            Slave.Dependencies := new Default_Value_Dependency_Record'
              (Enable        => Enable,
               Master_Switch => To_Index (Master_Cursor),
               Master_State  => False,
               Next          => Slave.Dependencies);
            Config.Switches.Replace_Element (Slave_Cursor, Slave);
         end;
      end if;
   end Add_Default_Value_Dependency;

   ---------------------------
   -- Root_Switches_Editors --
   ---------------------------

   package body Switches_Editors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Editor    : in out Root_Switches_Editor;
         Config    : Switches_Editor_Config)
      is
      begin
         Set_Configuration (Editor.Cmd_Line, Config.Config);
         Editor.Config := Config;
         Editor.Widgets := new Widget_Array
           (0 .. Integer (Length (Config.Switches)));
      end Initialize;

      ----------------------
      -- Get_Command_Line --
      ----------------------

      function Get_Command_Line
        (Editor   : access Root_Switches_Editor;
         Expanded : Boolean) return GNAT.Strings.String_List_Access is
      begin
         return Editor.Cmd_Line.To_String_List (Expanded);
      end Get_Command_Line;

      ----------------
      -- Set_Widget --
      ----------------

      procedure Set_Widget
        (Editor       : in out Root_Switches_Editor;
         Switch_Index : Integer;
         Widget       : access Root_Widget_Record'Class)
      is
      begin
         Editor.Widgets (Switch_Index) := Root_Widget (Widget);
      end Set_Widget;

      procedure Handle_Dependencies
        (Editor  : in out Root_Switches_Editor'Class;
         Switch  : String;
         Section : String;
         Status  : Boolean);
      --  If necessary, toggle other switches in other tools to reflect
      --  the change of status of Switch

      --------------------------
      -- Handle_Dependencies --
      --------------------------

      procedure Handle_Dependencies
        (Editor  : in out Root_Switches_Editor'Class;
         Switch  : String;
         Section : String;
         Status  : Boolean)
      is
         Deps     : Dependency_Description_Access :=
                      Editor.Config.Dependencies;
         Tool     : Root_Switches_Editor_Access;
         Changed  : Boolean;
         Success  : Boolean;

      begin
         Changed := False;

         while Deps /= null loop
            if Deps.Master_Switch.all = Switch
              and then Deps.Master_Section.all = Section
              and then Deps.Master_Status = Status
            then
               --  Find the slave tool
               Tool := Get_Tool_By_Name
                 (Editor,
                  Deps.Slave_Tool.all);

               if Tool /= null then
                  --  We give just a hint to the user that the switch
                  --  should be added, by preselecting it. The user is
                  --  still free to force another value for the slave
                  --  switch. Even if we were setting the widget as
                  --  insensitive, the command line would still be
                  --  editable anyway.

                  if not Deps.Slave_Status then
                     Remove_Switch
                       (Tool.Cmd_Line,
                        Section => Deps.Slave_Section.all,
                        Switch  => Deps.Slave_Switch.all,
                        Success => Success);
                  elsif not Tool.Cmd_Line.Has_Switch
                    (Section => Deps.Slave_Section.all,
                     Switch  => Deps.Slave_Switch.all)
                  then
                     Append_Switch
                       (Tool.Cmd_Line,
                        Section => Deps.Slave_Section.all,
                        Switch  => Deps.Slave_Switch.all,
                        Success => Success);
                  end if;

                  if Tool.all /= Editor and then Success then
                     On_Command_Line_Changed (Tool.all);
                     Update_Graphical_Command_Line (Tool.all);

                  else
                     Changed := Changed or Success;

                     if Success then

                        --  Recursive call to handle dependencies on slave
                        --  switch change of state.
                        --
                        --  We need to do it now to prevent being overridden by
                        --  another switch in the final call to
                        --  On_Command_Line_Changed:
                        --  adding "-gnatp" to "-gnatVa" is triggering
                        --  "-gnatVn" which should remove "-gnatVa". If we
                        --  don't call Handle_Dependencies
                        --  here, then what will happend is
                        --
                        --  "-gnatVa" => "-gnatVa -gnatp" =>
                        --  "-gnatVa -gnatp -gnatVn" (the Add_Switch call
                        --  above) ... then upon call to
                        --  On_Command_Line_Changed below, -gnatVa will be
                        --  analyzed first, thus removing "-gnatVn" ! Then
                        --  -gnatp gets analysed again thus adding "-gnatVn"
                        --  => infinite loop occurs.
                        --
                        --  The recursive call here will prevent this by
                        --  directly handling dependencies on "-gnatVn" leading
                        --  to "-gnatp -gnatVn" (-gnatVa removed) which is what
                        --  we want.

                        Handle_Dependencies
                          (Editor,
                           Deps.Slave_Switch.all,
                           Deps.Slave_Section.all,
                           Deps.Slave_Status);
                     end if;
                  end if;
               end if;
            end if;

            Deps := Deps.Next;
         end loop;

         if Changed then
            On_Command_Line_Changed (Editor);
            Update_Graphical_Command_Line (Editor);
         end if;

         --  Now check default values for all switches
         for J in Editor.Config.Switches.First_Index
                   .. Editor.Config.Switches.Last_Index
         loop
            declare
               Sw    : Switch_Description :=
                         Editor.Config.Switches.Element (J);
               Dep   : Default_Value_Dependency;
               State : Boolean;

            begin
               if Sw.Typ = Switch_Check then
                  --  State is always initialized using the initial_state.
                  --  We will modify it afterwards, depending on the states of
                  --  the modifiers.
                  State := Sw.Initial_State;
                  Dep   := Sw.Dependencies;

                  while Dep /= null loop
                     declare
                        Master : constant Switch_Description :=
                                   Editor.Config.Switches.Element
                                     (Dep.Master_Switch);
                     begin
                        if To_String (Master.Switch) = Switch
                          and then To_String (Master.Section) = Section
                        then
                           Dep.Master_State := Status;
                        end if;
                     end;

                     if Dep.Master_State then
                        State := Dep.Enable;
                     end if;

                     Dep := Dep.Next;
                  end loop;

                  if State /= Sw.Default_State then
                     Sw.Default_State := State;

                     if not State then
                        --  We are deactivating a switch, let's
                        --  remove unnecessary deactivation switch
                        Remove_Switch
                          (Editor.Cmd_Line,
                           Section => To_String (Sw.Section),
                           Switch  => To_String (Sw.Switch_Unset));
                     end if;

                     Editor.Config.Switches.Replace_Element (J, Sw);

                     if Editor.Widgets (J) /= null then
                        Set_Graphical_Widget
                          (Editor,
                           Editor.Widgets (J),
                           Sw.Typ,
                           Boolean'Image (State),
                           Is_Default => True);
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end Handle_Dependencies;

      -------------------
      -- Change_Switch --
      -------------------

      procedure Change_Switch
        (Editor    : in out Root_Switches_Editor;
         Widget    : access Root_Widget_Record'Class;
         Parameter : String)
      is

         Combo : Combo_Switch_Vectors.Cursor;
         Val   : Boolean;

      begin
         if not Editor.Block then
            for W in Editor.Widgets'Range loop
               if Editor.Widgets (W) = Root_Widget (Widget) then
                  declare
                     S : constant Switch_Description :=
                       Element (Editor.Config.Switches, W);
                  begin
                     --  We first remove the switch from the command line,
                     --  and add it later on if the corresponding widget is
                     --  checked
                     Remove_Switch
                       (Editor.Cmd_Line,
                        Section       => To_String (S.Section),
                        Switch        => To_String (S.Switch),
                        Has_Parameter =>
                          S.Typ in Switch_Field | Switch_Spin | Switch_Combo);

                     if S.Typ = Switch_Check
                       and then S.Switch_Unset /= Null_Unbounded_String
                     then
                        Remove_Switch
                          (Editor.Cmd_Line,
                           Section => To_String (S.Section),
                           Switch  => To_String (S.Switch_Unset));
                     end if;

                     case S.Typ is
                        when Switch_Check =>
                           if Parameter = "Checked" then
                              Append_Switch
                                (Editor.Cmd_Line,
                                 Section    => To_String (S.Section),
                                 Switch     => To_String (S.Switch),
                                 Add_Before => S.Add_First);
                              Val := True;

                           elsif Parameter = "Unchecked" then
                              if S.Default_State then
                                 --  If 'unchecked' while the default state
                                 --  is 'checked', explicitely add the
                                 --  deactivation switch
                                 Append_Switch
                                   (Editor.Cmd_Line,
                                    Section    => To_String (S.Section),
                                    Switch     => To_String (S.Switch_Unset),
                                    Add_Before => S.Add_First);
                              end if;

                              Val := False;

                           else
                              --  Checked_Default state
                              Val := True;
                           end if;

                           Handle_Dependencies
                             (Editor,
                              To_String (S.Switch),
                              To_String (S.Section),
                              Val);

                           if S.Switch_Unset /= Null_Unbounded_String then
                              Handle_Dependencies
                                (Editor,
                                 To_String (S.Switch_Unset),
                                 To_String (S.Section),
                                 not Val);
                           end if;

                        when Switch_Radio =>
                           if Boolean'Value (Parameter) then
                              Append_Switch
                                (Editor.Cmd_Line,
                                 Section    => To_String (S.Section),
                                 Switch     => To_String (S.Switch),
                                 Add_Before => S.Add_First);
                           end if;
                           Handle_Dependencies
                             (Editor,
                              To_String (S.Switch),
                              To_String (S.Section),
                              Boolean'Value (Parameter));

                        when Switch_Field =>
                           if Parameter /= "" then

                              --  ??? Note: Remove Separator switch once
                              --  GNAT GPL 2011 is out, see other calls to
                              --  Add_Switch in this package

                              Append_Switch
                                (Editor.Cmd_Line,
                                 Section    => To_String (S.Section),
                                 Switch     => To_String (S.Switch),
                                 Parameter  => Parameter,
                                 Separator  => S.Separator,
                                 Add_Before => S.Add_First);
                           end if;
                           Handle_Dependencies
                             (Editor,
                              To_String (S.Switch),
                              To_String (S.Section),
                              Parameter /= "");

                        when Switch_Spin =>
                           if Integer'Value (Parameter) /= S.Default then
                              Append_Switch
                                (Editor.Cmd_Line,
                                 Section    => To_String (S.Section),
                                 Switch     => To_String (S.Switch),
                                 Parameter  => Parameter,
                                 Separator  => S.Separator,
                                 Add_Before => S.Add_First);
                           end if;
                           Handle_Dependencies
                             (Editor,
                              To_String (S.Switch),
                              To_String (S.Section),
                              Integer'Value (Parameter) /= S.Default);

                        when Switch_Combo =>
                           Combo := First (S.Entries);
                           while Has_Element (Combo) loop
                              if Element (Combo).Label = Parameter then
                                 if Element (Combo).Value = S.No_Switch then
                                    Handle_Dependencies
                                      (Editor,
                                       To_String (S.Switch),
                                       To_String (S.Section),
                                       False);
                                 elsif Element (Combo).Value = S.No_Digit then
                                    Append_Switch
                                      (Editor.Cmd_Line,
                                       Section    => To_String (S.Section),
                                       Switch     => To_String (S.Switch),
                                       Add_Before => S.Add_First);
                                    Handle_Dependencies
                                      (Editor,
                                       To_String (S.Switch),
                                       To_String (S.Section),
                                       True);
                                 else
                                    Append_Switch
                                      (Editor.Cmd_Line,
                                       Section    => To_String (S.Section),
                                       Switch     => To_String (S.Switch),
                                       Parameter  =>
                                         To_String (Element (Combo).Value),
                                       Separator  => S.Separator,
                                       Add_Before => S.Add_First);
                                    Handle_Dependencies
                                      (Editor,
                                       To_String (S.Switch),
                                       To_String (S.Section),
                                       True);
                                 end if;

                              end if;
                              Next (Combo);
                           end loop;

                        when Switch_Popup =>
                           null;
                     end case;

                     Update_Graphical_Command_Line
                       (Root_Switches_Editor'Class (Editor));
                     return;
                  end;
               end if;
            end loop;
         end if;
      end Change_Switch;

      -----------------------------------
      -- Update_Graphical_Command_Line --
      -----------------------------------

      procedure Update_Graphical_Command_Line
        (Editor : in out Root_Switches_Editor)
      is
         List     : String_List_Access :=
                      Editor.Cmd_Line.To_String_List (Expanded => False);
         Cmd_Line : constant String :=
                      (if List /= null then
                          Argument_List_To_String (List.all)
                       else
                          "");
      begin
         Editor.Block := True;
         Set_Graphical_Command_Line
           (Root_Switches_Editor'Class (Editor), Cmd_Line);
         Free (List);
         Editor.Block := False;
      end Update_Graphical_Command_Line;

      ---------
      -- "=" --
      ---------

      function "="
        (Editor : access Root_Switches_Editor;
         Args   : GNAT.Strings.String_List) return Boolean
      is
         Cmd2         : Command_Line;
         Iter1, Iter2 : Command_Line_Iterator;
      begin
         --  ??? Not efficient to go back to a string

         Set_Configuration (Cmd2, Get_Configuration (Editor.Cmd_Line));
         Set_Command_Line (Cmd2, Argument_List_To_String (Args));

         --  The two command lines are equal if the switches are exactly in the
         --  same order. This is needed for instance when the user has typed
         --  some libraries to link with, and their order should be preserved.
         --  That means, however, that if the user unchecks and then rechecks
         --  a check button, then the command line will appear as modified.
         --  (See G315-031)

         Start (Editor.Cmd_Line, Iter1, Expanded => True);
         Start (Cmd2,            Iter2, Expanded => True);
         while Has_More (Iter1) loop
            if not Has_More (Iter2) then
               return False;
            end if;

            if Current_Switch (Iter1) /= Current_Switch (Iter2)
              or else Current_Separator (Iter1) /= Current_Separator (Iter2)
              or else Current_Parameter (Iter1) /= Current_Parameter (Iter2)
              or else Current_Section (Iter1) /= Current_Section (Iter2)
            then
               return False;
            end if;

            Next (Iter1);
            Next (Iter2);
         end loop;

         return not Has_More (Iter2);
      end "=";

      -----------------------------
      -- On_Command_Line_Changed --
      -----------------------------

      procedure On_Command_Line_Changed
        (Editor   : in out Root_Switches_Editor;
         Cmd_Line : String)
      is
      begin
         if Editor.Block then
            return;
         end if;

         Editor.Block := True;
         Set_Configuration (Editor.Cmd_Line, Editor.Config.Config);
         Set_Command_Line (Editor.Cmd_Line, Cmd_Line);
         Editor.Block := False;
         On_Command_Line_Changed (Editor);
      end On_Command_Line_Changed;

      -----------------------------
      -- On_Command_Line_Changed --
      -----------------------------

      procedure On_Command_Line_Changed
        (Editor   : in out Root_Switches_Editor'Class)
      is
         Iter                : Command_Line_Iterator;
         Switch              : Switch_Description_Vectors.Cursor :=
                                 First (Editor.Config.Switches);
         Current_Radio_Group : Radio_Switch := -1;

         function Get_Param (S : Switch_Description) return String;
         --  Returns the current parameter pointed to by Iter, after removing
         --  the separator for S if needed

         function Get_Param (S : Switch_Description) return String is
         begin
            if not Has_More (Iter) then
               return "";
            end if;

            declare
               Param : constant String := Current_Parameter (Iter);
            begin
               case S.Separator is
                  when ASCII.NUL | ASCII.LF | '=' | ASCII.CR | ' ' =>
                     return Param;
                  when others =>
                     if Param'Length > 0
                       and then Param (Param'First) = S.Separator
                     then
                        return Param (Param'First + 1 .. Param'Last);
                     else
                        return Param;
                     end if;
               end case;
            end;
         end Get_Param;

      begin
         if Editor.Block then
            return;
         end if;

         Editor.Block := True;

         while Has_Element (Switch) loop
            declare
               S : constant Switch_Description := Element (Switch);
            begin
               Start (Editor.Cmd_Line, Iter, Expanded => True);
               while Has_More (Iter) loop
                  exit when To_String (S.Switch) = Current_Switch (Iter)
                    and then To_String (S.Section) = Current_Section (Iter);
                  exit when S.Typ = Switch_Check
                    and then To_String (S.Switch_Unset) = Current_Switch (Iter)
                    and then To_String (S.Section) = Current_Section (Iter);

                  Next (Iter);
               end loop;

               case S.Typ is
                  when Switch_Check =>
                     declare
                        State      : Boolean;
                        Is_Default : Boolean;
                     begin
                        if not Has_More (Iter) then
                           Is_Default := True;
                        else
                           Is_Default := False;
                        end if;

                        if not Is_Default then
                           State := To_String (S.Switch) =
                             Current_Switch (Iter);
                        else
                           State := S.Default_State;

                           if not State then
                              Is_Default := False;
                           end if;
                        end if;

                        if Editor.Widgets (To_Index (Switch)) /= null then

                           --  ??? click on -gnatws => removes -gnatwa leads
                           --  to Is_Default => False
                           Set_Graphical_Widget
                             (Editor,
                              Editor.Widgets (To_Index (Switch)),
                              S.Typ,
                              Boolean'Image (State),
                              Is_Default);
                        end if;

                        Handle_Dependencies
                          (Editor,
                           To_String (S.Switch),
                           To_String (S.Section),
                           State);
                     end;

                  when Switch_Spin =>
                     if Editor.Widgets (To_Index (Switch)) /= null then
                        if Get_Param (S) = "" then
                           Set_Graphical_Widget
                             (Editor,
                              Editor.Widgets (To_Index (Switch)),
                              S.Typ,
                              Integer'Image (S.Default));

                        else
                           Set_Graphical_Widget
                             (Editor,
                              Editor.Widgets (To_Index (Switch)),
                              S.Typ,
                              Get_Param (S));
                        end if;
                     end if;

                  when Switch_Field =>
                     if Editor.Widgets (To_Index (Switch)) /= null then
                        Set_Graphical_Widget
                          (Editor,
                           Editor.Widgets (To_Index (Switch)),
                           S.Typ,
                           Get_Param (S));
                     end if;

                  when Switch_Radio =>
                     --  If we are starting a new radio group, pre-select
                     --  the first in the group, which is the default. It
                     --  will automatically get unselected if some other
                     --  element in the group is selected

                     if Editor.Widgets (To_Index (Switch)) /= null then
                        Set_Graphical_Widget
                          (Editor,
                           Editor.Widgets (To_Index (Switch)),
                           S.Typ,
                           Boolean'Image
                             (S.Group /= Current_Radio_Group
                              or else Has_More (Iter)));
                        Current_Radio_Group := S.Group;
                     end if;

                  when Switch_Combo =>
                     if Editor.Widgets (To_Index (Switch)) /= null then
                        declare
                           Combo : Combo_Switch_Vectors.Cursor
                             := First (S.Entries);
                           Param : constant String := Get_Param (S);
                        begin
                           while Has_Element (Combo) loop
                              if not Has_More (Iter) then
                                 exit when Element (Combo).Value = S.No_Switch;
                              else
                                 exit when
                                   (Param = ""
                                    and then
                                       Element (Combo).Value = S.No_Digit)
                                   or else Element (Combo).Value = Param;
                              end if;
                              Next (Combo);
                           end loop;

                           if Has_Element (Combo) then
                              Set_Graphical_Widget
                                (Editor,
                                 Editor.Widgets (To_Index (Switch)),
                                 S.Typ,
                                 To_String (Element (Combo).Label));
                           end if;
                        end;
                     end if;

                  when Switch_Popup =>
                     null;
               end case;
            end;

            Next (Switch);
         end loop;

         Editor.Block := False;
      end On_Command_Line_Changed;

      ----------------------
      -- Set_Command_Line --
      ----------------------

      procedure Set_Command_Line
        (Editor   : access Root_Switches_Editor;
         Cmd_Line : String) is
      begin
         Set_Graphical_Command_Line
           (Root_Switches_Editor'Class (Editor.all), Cmd_Line);
         On_Command_Line_Changed
           (Root_Switches_Editor'Class (Editor.all), Cmd_Line);
      end Set_Command_Line;

      ----------------------
      -- Set_Command_Line --
      ----------------------

      procedure Set_Command_Line
        (Editor         : access Root_Switches_Editor;
         Cmd_Line       : GNAT.Strings.String_List;
         Protect_Quotes : Boolean := True) is
      begin
         --  ??? Not very efficient to go through a string
         Set_Command_Line
           (Root_Switches_Editor'Class (Editor.all)'Access,
            Argument_List_To_String (Cmd_Line, Protect_Quotes));
      end Set_Command_Line;

      ----------------
      -- Get_Config --
      ----------------

      function Get_Config
        (Editor : access Root_Switches_Editor)
         return Switches_Editor_Config is
      begin
         return Editor.Config;
      end Get_Config;

   end Switches_Editors;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.Switch);
   end Get_Switch;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.Label);
   end Get_Label;

   -------------
   -- Get_Tip --
   -------------

   function Get_Tip (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.Tip);
   end Get_Tip;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.Section);
   end Get_Section;

   ------------------
   -- Is_Add_First --
   ------------------

   function Is_Add_First (Switch : Switch_Description) return Boolean is
   begin
      return Switch.Add_First;
   end Is_Add_First;

   ------------------
   -- Get_Spin_Min --
   ------------------

   function Get_Spin_Min (Switch : Switch_Description) return Integer is
   begin
      return Switch.Min;
   end Get_Spin_Min;

   ------------------
   -- Get_Spin_Max --
   ------------------

   function Get_Spin_Max (Switch : Switch_Description) return Integer is
   begin
      return Switch.Max;
   end Get_Spin_Max;

   ----------------------
   -- Get_Spin_Default --
   ----------------------

   function Get_Spin_Default (Switch : Switch_Description) return Integer is
   begin
      return Switch.Default;
   end Get_Spin_Default;

   -------------------------
   -- Get_Combo_No_Switch --
   -------------------------

   function Get_Combo_No_Switch
     (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.No_Switch);
   end Get_Combo_No_Switch;

   ------------------------
   -- Get_Combo_No_Digit --
   ------------------------

   function Get_Combo_No_Digit
     (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.No_Digit);
   end Get_Combo_No_Digit;

   -----------------------
   -- Get_Combo_Entries --
   -----------------------

   function Get_Combo_Entries
     (Switch : Switch_Description) return Combo_Switch_Vectors.Vector is
   begin
      return Switch.Entries;
   end Get_Combo_Entries;

   ----------------------
   -- Get_Switch_Unset --
   ----------------------

   function Get_Switch_Unset
     (Switch : Switch_Description) return String is
   begin
      return To_String (Switch.Switch_Unset);
   end Get_Switch_Unset;

   -----------------------
   -- Get_Default_State --
   -----------------------

   function Get_Default_State
     (Switch : Switch_Description) return Boolean is
   begin
      return Switch.Default_State;
   end Get_Default_State;

   -----------------------
   -- Get_Initial_State --
   -----------------------

   function Get_Initial_State
     (Switch : Switch_Description) return Boolean is
   begin
      return Switch.Initial_State;
   end Get_Initial_State;

   ---------------------------
   -- Is_Field_As_Directory --
   ---------------------------

   function Is_Field_As_Directory
     (Switch : Switch_Description) return Boolean is
   begin
      return Switch.As_Directory;
   end Is_Field_As_Directory;

   ----------------------
   -- Is_Field_As_File --
   ----------------------

   function Is_Field_As_File
     (Switch : Switch_Description) return Boolean is
   begin
      return Switch.As_File;
   end Is_Field_As_File;

   ---------------
   -- Get_Lines --
   ---------------

   function Get_Lines
     (Switches : Switches_Editor_Config)
      return Positive is
   begin
      return Switches.Lines;
   end Get_Lines;

   -----------------
   -- Get_Columns --
   -----------------

   function Get_Columns
     (Switches : Switches_Editor_Config)
      return Positive is
   begin
      return Switches.Columns;
   end Get_Columns;

   --------------------------
   -- Is_Show_Command_Line --
   --------------------------

   function Is_Show_Command_Line
     (Switches : Switches_Editor_Config)
      return Boolean is
   begin
      return Switches.Show_Command_Line;
   end Is_Show_Command_Line;

   ---------------------------
   -- Get_Default_Separator --
   ---------------------------

   function Get_Default_Separator
     (Switches : Switches_Editor_Config)
      return String is
   begin
      return To_String (Switches.Default_Separator);
   end Get_Default_Separator;

   ------------------
   -- Get_Sections --
   ------------------

   function Get_Sections
     (Switches : Switches_Editor_Config)
      return String is
   begin
      return To_String (Switches.Sections);
   end Get_Sections;

   ------------------------
   -- Is_Scrolled_Window --
   ------------------------

   function Is_Scrolled_Window
     (Switches : Switches_Editor_Config)
      return Boolean is
   begin
      return Switches.Scrolled_Window;
   end Is_Scrolled_Window;

   ---------------------
   -- Get_Switch_Char --
   ---------------------

   function Get_Switch_Char
     (Switches : Switches_Editor_Config)
      return Character is
   begin
      return Switches.Switch_Char;
   end Get_Switch_Char;

   -----------------------
   -- Get_Frames_Length --
   -----------------------

   function Get_Frames_Length
     (Switches : Switches_Editor_Config)
      return Ada.Containers.Count_Type is
   begin
      return Length (Switches.Frames);
   end Get_Frames_Length;

   ------------------------
   -- Get_Frames_Element --
   ------------------------

   function Get_Frames_Element
     (Switches : Switches_Editor_Config;
      Index : Natural)
      return Frame_Description is
   begin
      return Element (Switches.Frames, Index + First_Index (Switches.Frames));
   end Get_Frames_Element;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Frame : Frame_Description) return String is
   begin
      return To_String (Frame.Title);
   end Get_Title;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Frame : Frame_Description) return Positive is
   begin
      return Frame.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (Frame : Frame_Description) return Positive is
   begin
      return Frame.Column;
   end Get_Column;

   -------------------------
   -- Get_Switches_Length --
   -------------------------

   function Get_Switches_Length
     (Switches : Switches_Editor_Config)
      return Ada.Containers.Count_Type is
   begin
      return Length (Switches.Switches);
   end Get_Switches_Length;

   --------------------------
   -- Get_Switches_Element --
   --------------------------

   function Get_Switches_Element
     (Switches : Switches_Editor_Config;
      Index : Natural)
      return Switch_Description is
   begin
      return Element (Switches.Switches,
                      Index + First_Index (Switches.Switches));
   end Get_Switches_Element;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Switch : Switch_Description) return Positive is
   begin
      return Switch.Line;
   end Get_Line;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (Switch : Switch_Description) return Positive is
   begin
      return Switch.Column;
   end Get_Column;

   -------------------
   -- Get_Separator --
   -------------------

   function Get_Separator (Switch : Switch_Description) return Character is
   begin
      return Switch.Separator;
   end Get_Separator;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Switch : Switch_Description) return Switch_Type is
   begin
      return Switch.Typ;
   end Get_Type;

   ------_--------
   -- Get_Label --
   -------_-------

   function Get_Label (Value : Combo_Switch) return String is
   begin
      return To_String (Value.Label);
   end Get_Label;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Value : Combo_Switch) return String is
   begin
      return To_String (Value.Value);
   end Get_Value;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Filter : not null access Switch_Filter_Description_Record) return String
   is
     (To_String (Filter.Name));

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch
     (Config : not null access Switches_Editor_Config_Record'Class;
      Filter : not null Switch_Filter_Description) return Switch_Description
   is
      (Config.Switches (Filter.Switch));

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Config       : not null access Switches_Editor_Config_Record'Class;
      Filter       : not null Switch_Filter_Description;
      Matches      : Boolean;
      Before_Save  : Boolean;
      Default_Line : GNAT.Strings.String_List;
      Command_Line : in out GNAT.Strings.String_List_Access)
   is

      procedure Apply_Filter_On_Command_Line;
      --  Apply the filter's result on the target's command line

      ----------------------------------
      -- Apply_Filter_On_Command_Line --
      ----------------------------------

      procedure Apply_Filter_On_Command_Line is
         Switches_Config  : constant Switches_Editor_Config
           := Switches_Editor_Config (Config);
         Default_Cmd_Line : Command_Lines.Command_Line;
         Cmd_Line         : Command_Lines.Command_Line;
         Config           : constant Command_Line_Configuration :=
                              Switches_Config.Config;
         Switch           : constant Switch_Description :=
                              Get_Switch (Switches_Config, Filter);
         Success          : Boolean := False;
      begin
         --  Set the command line configuration
         Set_Configuration (Cmd_Line, Config);

         --  Build the command line
         Set_Command_Line
           (Cmd_Line, Argument_List_To_String (Command_Line.all));

         if not Before_Save and then not Matches then
            --  If we are not saving the target and if the filter does not
            --  match, remove its associated switch if present in the target's
            --  command line.

            Remove_Switch
              (Cmd_Line,
               Switch    => Get_Switch (Switch),
               Section   => Get_Section (Switch),
               Success   => Success);
         elsif not Has_Switch
           (Cmd_Line,
            Switch    => Get_Switch (Switch),
            Section   => Get_Section (Switch))
           and then not (Before_Save and then Matches)
         then
            --  It it matches and if the target is not going to be saved, add
            --  the switch back if it's present in the default command line.
            --  Add it back too if the target is goind to be saved, even if
            --  the filter does not match: we still want it present by default.

            declare
               Iter : Command_Line_Iterator;
            begin
               Set_Configuration (Default_Cmd_Line, Config);

               Set_Command_Line
                 (Default_Cmd_Line, Argument_List_To_String (Default_Line));

               Start (Default_Cmd_Line, Iter, Expanded => True);

               while Has_More (Iter) loop
                  exit when Get_Switch (Switch) = Current_Switch (Iter)
                    and then
                      Get_Section (Switch) = Current_Section (Iter);

                  Next (Iter);
               end loop;

               if Has_More (Iter) then
                  Append_Switch
                    (Cmd_Line,
                     Switch     => Current_Switch (Iter),
                     Parameter  => Current_Parameter (Iter),
                     Separator  => Get_Separator (Switch),
                     Section    => Get_Section (Switch),
                     Add_Before => Is_Add_First (Switch),
                     Success    => Success);
               end if;
            end;
         end if;

         --  Update the command line if the filter has affected it
         if Success then
            Command_Line := Cmd_Line.To_String_List (Expanded => False);
         end if;
      end Apply_Filter_On_Command_Line;

      Switch : Switch_Description := Config.Switches (Filter.Switch);
   begin
      Switch.Active := Matches;
      Config.Switches.Replace_Element (Filter.Switch, Switch);
      Apply_Filter_On_Command_Line;
   end Apply;

   -----------
   -- First --
   -----------

   function First
     (Config : Switches_Editor_Config) return Switch_Filter_Cursor
   is
      (Switch_Filter_Cursor'(C => Config.Filters.First));

   ----------
   -- Next --
   ----------

   procedure Next (Cursor : in out Switch_Filter_Cursor) is
   begin
      Switch_Filter_Description_Vectors.Next (Cursor.C);
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Cursor : Switch_Filter_Cursor) return Boolean
   is
     (Switch_Filter_Description_Vectors.Has_Element (Cursor.C));

   -------------
   -- Element --
   -------------

   function Element
     (Cursor : Switch_Filter_Cursor) return Switch_Filter_Description
   is
      (Switch_Filter_Description_Vectors.Element (Cursor.C));

end Switches_Chooser;
