------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with XML_Utils; use XML_Utils;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with String_Utils; use String_Utils;

with GNATCOLL.Traces;   use GNATCOLL.Traces;

package body Switches_Parser is
   Me : constant Trace_Handle := Create ("GPS.COMMON.SWITCHES");

   function "-" (Msg : String) return String;
   --  Convenient shortcut to the Gettext function.

   ---------
   -- "-" --
   ---------

   function "-" (Msg : String) return String is
   begin
      --  ??? Provide implementation
      return Msg;
   end "-";

   -------------------------
   -- Parse_Switches_Node --
   -------------------------

   procedure Parse_Switches_Node
     (Current_Tool_Name   : Unbounded_String;
      Current_Tool_Config : out Switches_Editor_Config;
      Error_Message       : out Unbounded_String;
      Finder              : Other_Config_Finder;
      Node                : XML_Utils.Node_Ptr)
   is
      Lines   : Integer := 1;
      Columns : Integer := 1;
      --  Size of the grid for this tool.

      Char        : constant String :=
                      Get_Attribute (Node, "switch_char", "-");
      Default_Sep : constant String :=
                      Get_Attribute (Node, "separator", "");

      Scrolled_Window   : Boolean;
      Show_Command_Line : Boolean;

      procedure Coordinates_From_Node (N : Node_Ptr; Line, Col : out Natural);
      --  Get the line and column from N

      function Check_Space_In_Switch (Switch : String) return Boolean;
      --  Return True if Switch contains a space

      function Get_Tip_Value (N : Node_Ptr) return String;
      --  Return the tip attribute if it exists, or the value of the tip
      --  child, or an empty string.

      procedure Process_Title_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Check_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Spin_Node       (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Field_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Radio_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Combo_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Popup_Node      (N : Node_Ptr; Popup : Popup_Index);
      procedure Process_Dependency_Node (N : Node_Ptr);
      procedure Process_Default_Value_Dependency_Node (N : Node_Ptr);
      procedure Process_Expansion_Node  (N : Node_Ptr);
      procedure Process_Hidden_Node (N : Node_Ptr);
      --  Process a child node (resp. <title>, <check>, <spin>, <radio>,
      --  <combo>, <popup>, <dependency>, <expansion>, <hidden>)

      procedure Process_Radio_Entry_Nodes
        (Parent : Node_Ptr; Radio : Radio_Switch);
      function Process_Combo_Entry_Nodes
        (Parent : Node_Ptr) return Combo_Switch_Array;
      --  Return the contents of all the <radio-entry> and
      --  <combo-entry> nodes of Parent.

      procedure Parse_Popup_Or_Main
        (N     : Node_Ptr;
         Popup : Popup_Index);
      --  Parse the subnodes of <switches>

      procedure Log_Error (S : String);
      --  Append S to the error log

      procedure Log_Error (S : String) is
      begin
         Error_Message := Error_Message & S & ASCII.LF;
      end Log_Error;

      ---------------------------
      -- Check_Space_In_Switch --
      ---------------------------

      function Check_Space_In_Switch (Switch : String) return Boolean is
      begin
         for S in Switch'Range loop
            if Is_Blank (Switch (S)) then
               Log_Error
                 (-("Attribute switch cannot contain spaces. Use the separator"
                  & " attribute if you need to separate the switch and its"
                  & " argument"));
               return True;
            end if;
         end loop;
         return False;
      end Check_Space_In_Switch;

      ---------------------------
      -- Coordinates_From_Node --
      ---------------------------

      procedure Coordinates_From_Node
        (N : Node_Ptr; Line, Col : out Natural)
      is
      begin
         Line := Safe_Value (Get_Attribute (N, "line", "1"));
         Lines := Integer'Max (Lines, Line);

         Col  := Safe_Value (Get_Attribute (N, "column", "1"));
         Columns := Integer'Max (Columns, Col);
      end Coordinates_From_Node;

      ------------------------
      -- Process_Title_Node --
      ------------------------

      procedure Process_Title_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Line_Span : constant Integer :=
                       Safe_Value (Get_Attribute (N, "line-span", "1"));
         Col_Span  : constant Integer :=
                       Safe_Value (Get_Attribute (N, "column-span", "1"));
      begin
         Coordinates_From_Node (N, Line, Col);
         Set_Frame_Title
           (Current_Tool_Config,
            Title     => N.Value.all,
            Line      => Line,
            Column    => Col,
            Line_Span => Line_Span,
            Col_Span  => Col_Span,
            Popup     => Popup);
      end Process_Title_Node;

      -------------------------------------------
      -- Process_Default_Value_Dependency_Node --
      -------------------------------------------

      procedure Process_Default_Value_Dependency_Node (N : Node_Ptr) is
         Master_Switch : constant String := Get_Attribute (N, "master-switch");
         Slave_Switch  : constant String := Get_Attribute (N, "slave-switch");
         Slave_Status  : constant String :=
                          Get_Attribute (N, "slave-status", "on");
      begin
         if Master_Switch = "" or else Slave_Switch = "" then
            Log_Error
              (-("Invalid <default-value-dependency> node in custom file," &
                     " attributes master-switch and slave-switch must be " &
                     "specified."));
            return;
         end if;

         Add_Default_Value_Dependency
           (Current_Tool_Config,
            Master_Switch,
            Get_Attribute (N, "master-section"),
            Slave_Switch,
            Get_Attribute (N, "slave-section"),
            Slave_Status = "on" or else Slave_Status = "true");
      end Process_Default_Value_Dependency_Node;

      -----------------------------
      -- Process_Dependency_Node --
      -----------------------------

      procedure Process_Dependency_Node (N : Node_Ptr) is
         Master_Page   : constant String := Get_Attribute (N, "master-page");
         Master_Switch : constant String := Get_Attribute (N, "master-switch");
         Slave_Page    : constant String := Get_Attribute (N, "slave-page");
         Slave_Switch  : constant String := Get_Attribute (N, "slave-switch");
         Master_Status : constant String :=
                           Get_Attribute (N, "master-status", "true");
         Slave_Status  : constant String :=
                           Get_Attribute (N, "slave-status", "true");
         Config        : Switches_Editor_Config;
      begin
         if Master_Page = ""
           or else Master_Switch = ""
           or else Slave_Page = ""
           or else Slave_Switch = ""
         then
            Log_Error
              (-("Invalid <dependency> node in custom file,"
               & " all attributes must be specified."));
            return;
         end if;

         if Master_Page = Current_Tool_Name then
            Config := Current_Tool_Config;

         else
            if Finder = null then
               Log_Error (-("<dependency> node not supported."));
            else
               Config := Finder (Master_Page);
            end if;

            if Config = null then
               Log_Error
                 (-("<dependency> node in custom file references"
                  & " unknown tool: ")
                  & Master_Page);
               return;
            end if;
         end if;

         Add_Dependency
           (Config,
            Master_Switch,
            Get_Attribute (N, "master-section"),
            Master_Status = "true" or else Master_Status = "on",
            Slave_Page,
            Slave_Switch,
            Get_Attribute (N, "slave-section"),
            Slave_Status = "true" or else Slave_Status = "on");
      end Process_Dependency_Node;

      -------------------------------
      -- Process_Radio_Entry_Nodes --
      -------------------------------

      procedure Process_Radio_Entry_Nodes
        (Parent : Node_Ptr;
         Radio  : Radio_Switch)
      is
         N : Node_Ptr := Parent.Child;
      begin
         while N /= null loop
            if N.Tag.all = "radio-entry" then
               declare
                  Label  : constant String := Get_Attribute (N, "label");
                  Switch : constant String := Get_Attribute (N, "switch");
                  Filter : constant String := Get_Attribute (N, "filter");
               begin
                  if Label = "" then
                     Log_Error
                       (-("Invalid <radio-entry> node in custom file,"
                        & " requires a label and a switch attributes"));
                     return;
                  end if;

                  if Check_Space_In_Switch (Switch) then
                     return;
                  end if;

                  Add_Radio_Entry
                    (Config     => Current_Tool_Config,
                     Radio      => Radio,
                     Label      => Label,
                     Switch     => Switch,
                     Section    => Get_Attribute (N, "section"),
                     Tip        => Get_Tip_Value (N),
                     Add_Before => Get_Attribute (N, "before") = "true",
                     Filter     => Filter);
               end;
            end if;
            N := N.Next;
         end loop;
      end Process_Radio_Entry_Nodes;

      -------------------------------
      -- Process_Combo_Entry_Nodes --
      -------------------------------

      function Process_Combo_Entry_Nodes
        (Parent : Node_Ptr) return Combo_Switch_Array
      is
         N            : Node_Ptr := Parent.Child;
         Num_Children : Natural := 0;
      begin
         while N /= null loop
            if N.Tag.all = "combo-entry" then
               Num_Children := Num_Children + 1;
            end if;
            N := N.Next;
         end loop;

         declare
            Buttons : Combo_Switch_Array (1 .. Num_Children);
         begin
            N := Parent.Child;
            for B in Buttons'Range loop
               while N.Tag.all /= "combo-entry" loop
                  N := N.Next;
               end loop;

               declare
                  Label : constant String := Get_Attribute (N, "label");
                  Value : constant String := Get_Attribute (N, "value");
               begin
                  if Label = "" or else Value = "" then
                     Log_Error
                       (-("Invalid <combo-entry> node in custom file,"
                        & " requires a label and a switch attributes"));
                     return Buttons (1 .. 0);
                  end if;

                  Buttons (B) :=
                    (Label => To_Unbounded_String (Label),
                     Value => To_Unbounded_String (Value));
               end;

               N := N.Next;
            end loop;

            return Buttons;
         end;
      end Process_Combo_Entry_Nodes;

      ------------------------
      -- Process_Radio_Node --
      ------------------------

      procedure Process_Radio_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         R         : Radio_Switch;
         Label     : constant String := Get_Attribute (N, "label");
         Tip       : constant String := Get_Tip_Value (N);
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" then
            Log_Error
              (-("Invalid <radio> node in custom file,"
               & " requires a label attribute"));
         end if;

         R := Add_Radio
           (Config => Current_Tool_Config,
            Label  => Label,
            Tip    => Tip,
            Line   => Line,
            Column => Col,
            Popup  => Popup);
         Process_Radio_Entry_Nodes (N, R);
      end Process_Radio_Node;

      ------------------------
      -- Process_Popup_Node --
      ------------------------

      procedure Process_Popup_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Pop       : Popup_Index;
         Saved_Lines : constant Integer := Lines;
         Saved_Columns : constant Integer := Columns;
      begin
         Coordinates_From_Node (N, Line, Col);
         if Label = "" then
            Log_Error
              (-("Invalid <popup> node in custom file,"
               & " requires a label attributes"));
            return;
         end if;

         Lines := 1;
         Columns := 1;

         Pop := Add_Popup
           (Config  => Current_Tool_Config,
            Label   => Label,
            Line    => Line,
            Column  => Col,
            Popup   => Popup);

         Parse_Popup_Or_Main
           (N     => N,
            Popup => Pop);

         Current_Tool_Config.Set_Size (Lines, Columns, For_Popup => Pop);
         Lines := Saved_Lines;
         Columns := Saved_Columns;
      end Process_Popup_Node;

      ------------------------
      -- Process_Combo_Node --
      ------------------------

      procedure Process_Combo_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
         Filter    : constant String := Get_Attribute (N, "filter");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Switch = "" then
            Log_Error
              (-("Invalid <combo> node in custom file, requires"
               & " a switch attributes"));
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Combo
           (Config     => Current_Tool_Config,
            Label      => Label,
            Switch     => Switch,
            Separator  => Get_Attribute (N, "separator", ""),
            No_Switch  => Get_Attribute (N, "noswitch"),
            No_Digit   => Get_Attribute (N, "nodigit"),
            Entries    => Process_Combo_Entry_Nodes (N),
            Section    => Get_Attribute (N, "section"),
            Tip        => Get_Tip_Value (N),
            Line       => Line,
            Column     => Col,
            Add_Before => Get_Attribute (N, "before") = "true",
            Popup      => Popup,
            Filter     => Filter);
      end Process_Combo_Node;

      -------------------------
      -- Process_Hidden_Node --
      -------------------------

      procedure Process_Hidden_Node (N : Node_Ptr) is
         Switch : constant String := Get_Attribute (N, "switch");

      begin
         if Switch = "" then
            Log_Error (-("Invalid <hidden> node in custom file,"
                       & " 'switch' attribute is required"));
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Hidden
           (Config    => Current_Tool_Config,
            Switch    => Switch,
            Separator => Get_Attribute (N, "separator", Default_Sep));
      end Process_Hidden_Node;

      ------------------------
      -- Process_Field_Node --
      ------------------------

      procedure Process_Field_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
         Filter    : constant String := Get_Attribute (N, "filter");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Log_Error
              (-("Invalid <field> node in custom file, requires"
               & " a label and a switch attributes"));
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Field
           (Current_Tool_Config,
            Label        => Label,
            Switch       => Switch,
            Separator    => Get_Attribute (N, "separator", Default_Sep),
            Section      => Get_Attribute (N, "section"),
            Tip          => Get_Tip_Value (N),
            As_Directory =>
              Get_Attribute (N, "as-directory", "false") = "true",
            As_File      => Get_Attribute (N, "as-file", "false") = "true",
            Line         => Line,
            Column       => Col,
            Add_Before   => Get_Attribute (N, "before") = "true",
            Popup        => Popup,
            Filter       => Filter);
      end Process_Field_Node;

      -----------------------
      -- Process_Spin_Node --
      -----------------------

      procedure Process_Spin_Node  (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col : Natural;
         Label     : constant String := Get_Attribute (N, "label");
         Switch    : constant String := Get_Attribute (N, "switch");
         Filter    : constant String := Get_Attribute (N, "filter");
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Log_Error
              (-("Invalid <spin> node in custom file, requires"
               & " a label and a switch attributes"));
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         Add_Spin
           (Config     => Current_Tool_Config,
            Label      => Label,
            Switch     => Switch,
            Section    => Get_Attribute (N, "section"),
            Tip        => Get_Tip_Value (N),
            Separator  => Get_Attribute (N, "separator", Default_Sep),
            Min        => Safe_Value (Get_Attribute (N, "min", "1")),
            Max        => Safe_Value (Get_Attribute (N, "max", "1")),
            Default    => Safe_Value (Get_Attribute (N, "default", "1")),
            Line       => Line,
            Column     => Col,
            Add_Before => Get_Attribute (N, "before") = "true",
            Popup      => Popup,
            Filter     => Filter);
      end Process_Spin_Node;

      ------------------------
      -- Process_Check_Node --
      ------------------------

      procedure Process_Check_Node (N : Node_Ptr; Popup : Popup_Index) is
         Line, Col     : Natural;
         Label         : constant String := Get_Attribute (N, "label");
         Switch        : constant String := Get_Attribute (N, "switch");
         Filter        : constant String := Get_Attribute (N, "filter");
         Switch_Unset  : constant String :=
                           Get_Attribute (N, "switch-off");
         Default       : constant String :=
                           To_Lower (Get_Attribute (N, "default", "off"));
         Default_State : Boolean;
      begin
         Coordinates_From_Node (N, Line, Col);

         if Label = "" or else Switch = "" then
            Log_Error
              (-("Invalid <check> node in custom file, requires"
               & " a label and a switch attributes"));
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         if Default = "off"
           or else Default = "false"
         then
            Default_State := False;
         elsif Default = "on"
           or else Default = "true"
         then
            Default_State := True;
         else
            Log_Error
              (-("Invalid <switch> node in custom file: the " &
               """default"" attribute can only take the values " &
               "'on', 'true', 'off' or 'false'. " &
               "The value found is: ") & Default);
            return;
         end if;

         Add_Check
           (Config        => Current_Tool_Config,
            Label         => Label,
            Switch_Set    => Switch,
            Switch_Unset  => Switch_Unset,
            Default_State => Default_State,
            Section       => Get_Attribute (N, "section"),
            Tip           => Get_Tip_Value (N),
            Line          => Line,
            Column        => Col,
            Add_Before    => Get_Attribute (N, "before") = "true",
            Popup         => Popup,
            Filter        => Filter);
      end Process_Check_Node;

      -------------------
      -- Get_Tip_Value --
      -------------------

      function Get_Tip_Value (N : Node_Ptr) return String is
         Tip_Attr : constant String := Get_Attribute (N, "tip");
         C        : Node_Ptr;
      begin
         if Tip_Attr /= "" then
            return Tip_Attr;
         end if;

         if N.Child /= null then
            C := Find_Tag (N.Child, "tip");
            if C /= null then
               return C.Value.all;
            end if;
         end if;

         return "";
      end Get_Tip_Value;

      ----------------------------
      -- Process_Expansion_Node --
      ----------------------------

      procedure Process_Expansion_Node (N : Node_Ptr) is
         Switch : constant String := Get_Attribute (N, "switch");
         Alias  : constant String := Get_Attribute (N, "alias");
      begin
         if Switch = "" then
            Log_Error
              (-("Invalid <expansion> node in custom file, requires"
               & " a switch attributes"));
            return;
         end if;

         if Check_Space_In_Switch (Switch) then
            return;
         end if;

         if Alias = "" then
            Define_Prefix (Current_Tool_Config, Prefix => Switch);
         else
            Define_Alias  (Current_Tool_Config, Switch, Alias);
         end if;
      end Process_Expansion_Node;

      -------------------------
      -- Parse_Popup_Or_Main --
      -------------------------

      procedure Parse_Popup_Or_Main
        (N     : Node_Ptr;
         Popup : Popup_Index)
      is
         N2 : Node_Ptr := N.Child;
      begin
         while N2 /= null loop
            if N2.Tag.all = "title" then
               Process_Title_Node (N2, Popup);
            elsif N2.Tag.all = "check" then
               Process_Check_Node (N2, Popup);
            elsif N2.Tag.all = "spin" then
               Process_Spin_Node (N2, Popup);
            elsif N2.Tag.all = "radio" then
               Process_Radio_Node (N2, Popup);
            elsif N2.Tag.all = "field" then
               Process_Field_Node (N2, Popup);
            elsif N2.Tag.all = "combo" then
               Process_Combo_Node (N2, Popup);
            elsif N2.Tag.all = "popup" then
               Process_Popup_Node (N2, Popup);
            elsif N2.Tag.all = "dependency" then
               Process_Dependency_Node (N2);
            elsif N2.Tag.all = "default-value-dependency" then
               --  Process this node after all other nodes have been parsed
               null;
            elsif N2.Tag.all = "expansion" then
               Process_Expansion_Node (N2);
            elsif N2.Tag.all = "hidden" then
               Process_Hidden_Node (N2);
            else
               Log_Error
                 (-"Invalid xml tag child for <switches>: "
                  & N2.Tag.all);
            end if;

            N2 := N2.Next;
         end loop;

         N2 := N.Child;

         while N2 /= null loop
            if N2.Tag.all = "default-value-dependency" then
               Process_Default_Value_Dependency_Node (N2);
            end if;

            N2 := N2.Next;
         end loop;

      exception
         when E : others => Trace (Me, E);
      end Parse_Popup_Or_Main;

   begin
      begin
         Scrolled_Window := Boolean'Value
           (Get_Attribute (Node, "use_scrolled_window", "false"));
      exception
         when Constraint_Error =>
            Log_Error
              (-("Invalid value specified for use_scrolled_windows: ")
               & Get_Attribute (Node, "use_scrolled_window"));
            Scrolled_Window := False;
      end;

      begin
         Show_Command_Line := Boolean'Value
           (Get_Attribute (Node, "show_command_line", "true"));
      exception
         when Constraint_Error =>
            Log_Error
              (-("Invalid value specified for show_command_line: ")
               & Get_Attribute (Node, "show_command_line"));
            Show_Command_Line := True;
      end;

      Current_Tool_Config := Create
        (Default_Separator => Default_Sep,
         Switch_Char       => Char (Char'First),
         Scrolled_Window   => Scrolled_Window,
         Show_Command_Line => Show_Command_Line,
         Sections          => Get_Attribute (Node, "sections"));

      Parse_Popup_Or_Main (Node, Main_Window);

      Current_Tool_Config.Set_Size (Lines, Columns);
   end Parse_Switches_Node;

end Switches_Parser;
