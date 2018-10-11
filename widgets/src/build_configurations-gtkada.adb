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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

with GNAT.Strings;
with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;

with Glib;                     use Glib;
with Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gtk.Button;               use Gtk.Button;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Image;                use Gtk.Image;
with Gtk.Editable;             use Gtk.Editable;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.Table;                use Gtk.Table;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Row_Reference;   use Gtk.Tree_Row_Reference;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Widget;               use Gtk.Widget;

with Gtkada.Dialogs;           use Gtkada.Dialogs;

with GUI_Utils;                use GUI_Utils;
with String_Utils;             use String_Utils;

with Build_Configurations.Gtkada.Dialogs;
use Build_Configurations.Gtkada.Dialogs;

package body Build_Configurations.Gtkada is

   use GNAT.OS_Lib;

   --  ??? Add facility to rename a target

   Icons_List : constant array (Natural range <>) of Unbounded_String :=
                  (To_Unbounded_String ("gps-build-all-symbolic"),
                   To_Unbounded_String ("gps-build-main-symbolic"),
                   To_Unbounded_String ("gps-clean-symbolic"),
                   To_Unbounded_String ("gps-compile-symbolic"),
                   To_Unbounded_String ("gps-compute-xref-symbolic"),
                   To_Unbounded_String ("gps-custom-build-symbolic"),
                   To_Unbounded_String ("gps-semantic-check-symbolic"),
                   To_Unbounded_String ("gps-syntax-check-symbolic"));

   ---------------
   -- Constants --
   ---------------

   --  Tree view constants

   Icon_Column     : constant := 0; --  Contains the icon
   Name_Column     : constant := 1; --  Contains the displayed name, as markup
   Num_Column      : constant := 2;
   --  Contains the number of the corresponding page in the main notebook
   Editable_Column : constant := 3; -- Whether the target is editable

   Column_Types : constant GType_Array :=
     (Icon_Column     => GType_String,
      Name_Column     => GType_String,
      Num_Column      => GType_Int,
      Editable_Column => GType_Boolean);

   procedure Set_Columns
     (Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Icon     : String;
      Name     : String;
      Num      : Gint;
      Editable : Boolean);
   --  Set model's values.

   -----------------
   -- Local types --
   -----------------

   type Mode_UI_Record is new Gtk_Hbox_Record with record
      Registry : Build_Config_Registry_Access;

      Notebook : Gtk_Notebook;
      --  The main notebook

      View     : Tree_View;
      --  The tree
   end record;
   type Mode_UI_Access is access all Mode_UI_Record'Class;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Gtk_New
     (Target_UI  : out Target_UI_Access;
      Registry   : Build_Config_Registry_Access;
      Fixed_Font : Pango_Font_Description);
   --  Create a new target_UI

   procedure On_Icon_Selected
     (Button : access Gtkada_Combo_Tool_Button_Record'Class;
      UI     : Target_UI_Access);
   --  Used to control whether the icon_entry should be editable

   function "-" (Msg : String) return String;
   --  Convenient shortcut to the Gettext function

   function Switches_For_Target
     (UI         : access Build_UI_Record'Class;
      Target     : Target_Access;
      Single     : Boolean;
      History    : Histories.History;
      Fixed_Font : Pango_Font_Description) return Target_UI_Access;
   --  Return the widget controlling the switches for Target
   --  If Single is True, do not display the options, the models combo, etc.

   function Get_Selected_Target
     (UI : access Build_UI_Record'Class)
      return Target_Access;
   --  Return the currently selected target, or null

   procedure Set_Switches (UI : Target_UI_Access);
   --  Set the graphical elements in UI that represent

   procedure On_Selection_Changed (UI : access Build_UI_Record'Class);
   procedure On_Selection_Changed (UI : access Mode_UI_Record'Class);
   --  Called when the selection has changed in the tree view

   procedure On_Add_Target (UI : access Build_UI_Record'Class);
   --  Launch the "add target" dialog

   procedure On_Add_Mode (UI : access Mode_UI_Record'Class);
   --  Launch the "add mode" dialog

   procedure On_Remove_Target (UI : access Build_UI_Record'Class);
   --  Remove currently selected target from UI

   procedure On_Duplicate_Target (UI : access Build_UI_Record'Class);
   --  Launch the "duplicate target" dialog

   procedure On_Target_Model_Changed (UI : access Build_UI_Record'Class);
   --  The target model has changed

   procedure On_Revert_Target (UI : access Build_UI_Record'Class);
   --  Revert current target to default command line

   procedure Save_Targets (UI : access Build_UI_Record'Class);
   --  Saves the command lines and options of all targets into the registry

   procedure Refresh
     (UI            : access Build_UI_Record'Class;
      Select_Target : String);
   procedure Refresh (UI : access Mode_UI_Record'Class);
   --  Clear the variant areas of the UI (notebook and tree view) and fill
   --  them with the information contained in the Registry.
   --  Select_Target indicates which target should be selected after the
   --  refresh. It can be empty, in which case no target is selected.

   procedure On_Entry_Changed (UI : access Build_UI_Record'Class);
   --  "changed" callback for target UI entry

   function Beautify (S : String) return String;
   --  Take a string coming from a 'Image and make it fit for GUI display

   function Uglify (S : String) return String;
   pragma Unreferenced (Uglify);
   --  Inverse operation to Beautify

   function Target_To_Key (T : Target_Access) return History_Key;
   --  Return a History_Key for storing command line for T

   procedure On_Target_Renamed
     (UI     : access Build_UI_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a target was renamed

   -------------------
   -- Target_To_Key --
   -------------------

   function Target_To_Key (T : Target_Access) return History_Key is
   begin
      return History_Key (To_String (T.Name));
   end Target_To_Key;

   --------------
   -- Beautify --
   --------------

   function Beautify (S : String) return String is
      R : String := S;
   begin
      for J in R'First + 1 .. R'Last loop
         if R (J) = '_' then
            R (J) := ' ';
         else
            R (J) := To_Lower (R (J));
         end if;
      end loop;

      return R;
   end Beautify;

   ------------
   -- Uglify --
   ------------

   function Uglify (S : String) return String is
      R : String := S;
   begin
      for J in R'First + 1 .. R'Last loop
         if R (J) = ' ' then
            R (J) := '_';
         else
            R (J) := To_Upper (R (J));
         end if;
      end loop;

      return R;
   end Uglify;

   --------------------
   -- Local Packages --
   --------------------

   package Build_UI_Callback is new Callback (Build_UI_Record);
   use Build_UI_Callback;

   package Mode_UI_Callback is new Callback (Mode_UI_Record);
   use Mode_UI_Callback;

   package Icon_Callback is new User_Callback
     (Gtkada_Combo_Tool_Button_Record, Target_UI_Access);

   ---------
   -- "-" --
   ---------

   function "-" (Msg : String) return String is
   begin
      --  ??? Provide implementation
      return Msg;
   end "-";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Target_UI  : out Target_UI_Access;
      Registry   : Build_Config_Registry_Access;
      Fixed_Font : Pango_Font_Description) is
   begin
      Target_UI := new Target_UI_Record;
      Target_UI.Registry := Registry;
      Target_UI.Fixed_Font := Fixed_Font;
      Gtk.Scrolled_Window.Initialize (Target_UI);
      Target_UI.Set_Policy (Policy_Never, Policy_Never);
   end Gtk_New;

   ------------------
   -- Save_Targets --
   ------------------

   procedure Save_Targets (UI : access Build_UI_Record'Class) is
      T  : Target_UI_Access;
      CL : GNAT.OS_Lib.Argument_List_Access;

   begin
      for J in 1 .. UI.Registry.Targets.Length loop
         T := Target_UI_Access (Get_Nth_Page (UI.Notebook, Gint (J)));

         --  Save the command line
         if T.Editor /= null then
            CL := Get_Command_Line (T.Editor, False);
            Set_Command_Line (T.Target, CL.all);
            Unchecked_Free (CL);
         end if;

         --  Save the options
         T.Target.Properties.Launch_Mode :=
           Launch_Mode_Type'Val (Get_Active (T.Launch_Combo));

         if T.Icon_Entry /= null then
            T.Target.Properties.Icon_Name :=
              To_Unbounded_String (Get_Text (T.Icon_Entry));
         else
            T.Target.Properties.Icon_Name :=
              To_Unbounded_String (Get_Selected_Item (T.Icon_Button));
         end if;

         T.Target.Properties.In_Toolbar := Get_Active (T.Icon_Check);
         T.Target.Properties.In_Menu    := Get_Active (T.Menu_Check);
         T.Target.Properties.In_Contextual_Menu_For_Projects :=
           Get_Active (T.Project_Contextual_Menu_Check);
         T.Target.Properties.In_Contextual_Menu_For_Files :=
           Get_Active (T.File_Contextual_Menu_Check);

         T.Target.Properties.Target_Type :=
           To_Unbounded_String (To_Lower (Get_Text (T.Multiple_Targets)));
      end loop;
   end Save_Targets;

   -------------------------
   -- Get_Selected_Target --
   -------------------------

   function Get_Selected_Target
     (UI : access Build_UI_Record'Class)
      return Target_Access
   is
      T : Target_UI_Access;
      N : Gint;
   begin
      N := Get_Current_Page (UI.Notebook);

      if N = 0 then
         return null;
      end if;

      T := Target_UI_Access (Get_Nth_Page (UI.Notebook, N));

      return T.Target;
   end Get_Selected_Target;

   ----------------------
   -- On_Entry_Changed --
   ----------------------

   procedure On_Entry_Changed (UI : access Build_UI_Record'Class) is
      T : constant Target_UI_Access := UI.Target_UI;
   begin
      if UI.Expand_Cmd_Line = null then
         Set_Text
           (Get_Buffer (T.Expanded_Entry),
            Get_Text (Get_Entry (T.Editor)));
      else
         Set_Text
           (Get_Buffer (T.Expanded_Entry),
            UI.Expand_Cmd_Line (Get_Text (Get_Entry (T.Editor))));
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Entry_Changed;

   ----------------------
   -- On_Revert_Target --
   ----------------------

   procedure On_Revert_Target (UI : access Build_UI_Record'Class) is
      T : Target_UI_Access;
   begin
      if not Yes_No_Dialog
        (UI, -"Revert to original settings for this target?")
      then
         return;
      end if;

      --  Find the current target UI

      T := Target_UI_Access
        (Get_Nth_Page (UI.Notebook, Get_Current_Page (UI.Notebook)));

      declare
         Target_Name : constant String := To_String (T.Target.Name);
      begin
         --  Revert to the original target
         Revert_Target (UI.Registry, Target_Name);

         --  Reset dangling pointer. Should be done in the following call to
         --  Refresh, but do it for safety.
         T.Target := null;

         Refresh (UI, Target_Name);
      end;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Revert_Target;

   -----------------------------
   -- On_Target_Model_Changed --
   -----------------------------

   procedure On_Target_Model_Changed (UI : access Build_UI_Record'Class) is
      T  : Target_UI_Access;
      It : Gtk_Tree_Iter;
      M  : Gtk_Tree_Model;
   begin
      --  Find the current target UI

      T := Target_UI_Access
        (Get_Nth_Page (UI.Notebook, Get_Current_Page (UI.Notebook)));

      --  If the selection is empty, reset it and do nothing

      if Get_Text (T.Model_Entry) = "" then
         Set_Text (T.Model_Entry, To_String (T.Target.Model.Name));
         return;
      end if;

      --  If the selection already matches the model, do nothing

      if Get_Text (T.Model_Entry) = T.Target.Model.Name then
         return;
      end if;

      --  Change the model
      Change_Model
        (UI.Registry, To_String (T.Target.Name), Get_Text (T.Model_Entry));

      --  Refresh the icon in the tree view

      if T.Target.Properties.Icon_Name = "" then
         Get_Selected (Get_Selection (UI.View), M, It);

         if It /= Null_Iter then --  It should not be null, but test for safety
            UI.View.Model.Set
              (It, Icon_Column, To_String (T.Target.Model.Icon));
         end if;
      end if;

      --  Change the widgets
      Remove (T.Frame, Get_Child (T.Frame));

      --  Set the new widgets
      Set_Switches (T);

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Target_Model_Changed;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
     (Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Icon     : String;
      Name     : String;
      Num      : Gint;
      Editable : Boolean) is
   begin
      Set_And_Clear
        (Model, Iter,
         (Icon_Column, Name_Column, Num_Column, Editable_Column),
         (As_String  (Icon),
          As_String  (Name),
          As_Int     (Num),
          As_Boolean (Editable)));
   end Set_Columns;

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches (UI : Target_UI_Access) is
      Help_Msg      : Unbounded_String := Null_Unbounded_String;
   begin
      --  Get the appropriate help if available
      if UI.Target.Properties.Help /= Null_Unbounded_String then
         Help_Msg := UI.Target.Properties.Help;
      elsif UI.Target.Model.Help /= Null_Unbounded_String then
         Help_Msg := UI.Target.Model.Help;
      end if;

      --  Create the switches editor
      Gtk_New
        (UI.Editor,
         UI.Target.Model.Switches,
         Use_Native_Dialogs => False,
         Read_Only          => False,
         History            => UI.History,
         Key                => Target_To_Key (UI.Target),
         Cmd_Line_Tooltip   => Command_Line_Editor_Tooltip_Text,
         Help_Msg           => To_String (Help_Msg),
         Fixed_Font         => UI.Fixed_Font);

      --  Create the "current command" entry

      --  Set initial values of command line and switches entries

      if not UI.Target.Command_Line.Is_Empty then
         Set_Command_Line
           (UI.Editor,
            UI.Target.Command_Line.To_String_List (Expanded => False).all,
            False);
      end if;

      Add (UI.Frame, UI.Editor);
      Show_All (UI.Frame);
   end Set_Switches;

   ----------------------
   -- On_Icon_Selected --
   ----------------------

   procedure On_Icon_Selected
     (Button : access Gtkada_Combo_Tool_Button_Record'Class;
      UI     : Target_UI_Access)
   is
      Hbox : constant Gtk_Hbox := Gtk_Hbox (Button.Get_Parent);
   begin
      Set_Tooltip_Text (Button, Get_Selected_Item (Button));

      if UI.Icon_Entry /= null then
         Remove (Hbox, UI.Icon_Entry);
         UI.Icon_Entry := null;
      end if;

      if Get_Selected_Item (Button) = "custom" then
         Gtk_New (UI.Icon_Entry);
         Pack_Start (Hbox, UI.Icon_Entry, False, False, 0);
         Show_All (UI.Icon_Entry);
      end if;
   end On_Icon_Selected;

   -------------------------
   -- Switches_For_Target --
   -------------------------

   function Switches_For_Target
     (UI         : access Build_UI_Record'Class;
      Target     : Target_Access;
      Single     : Boolean;
      History    : Histories.History;
      Fixed_Font : Pango_Font_Description) return Target_UI_Access
   is
      Table         : Gtk_Table;
      Hbox          : Gtk_Hbox;
      Label         : Gtk_Label;
      Main_Hbox     : Gtk_Hbox;
      Box           : Gtk_Box;
      Combo         : Gtk_Combo_Box_Text;
      Top_Box       : Gtk_Hbox;
      Options_Frame : Gtk_Frame;
      Locations_Frame : Gtk_Frame;
      Button        : Gtk_Button;
      Buttons_Vbox  : Gtk_Vbox;
      Scrolled      : Target_UI_Access;
   begin
      Gtk_New (Scrolled, UI.Registry, Fixed_Font);

      --  Global box

      Gtk_New_Vbox (Box);
      Scrolled.Add (Box);

      Scrolled.Target := Target;
      Scrolled.History := History;

      if not Single then
         Gtk_New_Hbox (Top_Box);
         Set_Spacing (Top_Box, 3);

         --  Create the "revert" button.
         --  ??? We should only put a revert button when there is an original
         --  target

         Gtk_New_From_Name_And_Label
           (Button, "gps-refresh-symbolic", " Revert ");
         Pack_End (Top_Box, Button, False, False, 0);

         Object_Connect
           (Widget      => Button,
            Name        => Gtk.Button.Signal_Clicked,
            Cb          => On_Revert_Target'Access,
            Slot_Object => UI,
            After       => True);

         --  Create the model combo

         Combo := Models_Combo (UI);

         Gtk_New (Label, -"Target model");
         Pack_Start (Top_Box, Label, False, False, 2);
         Pack_Start (Top_Box, Combo, True, True, 0);

         Pack_Start (Box, Top_Box, False, False, 0);
         Scrolled.Model_Entry := Gtk_Entry (Get_Child (Combo));
         Set_Editable (Scrolled.Model_Entry, False);
         Set_Text (Scrolled.Model_Entry, To_String (Target.Model.Name));

         --  Connect to a change in the model combo

         Object_Connect
           (Widget      => Combo,
            Name        => Gtk.Combo_Box.Signal_Changed,
            Cb          => On_Target_Model_Changed'Access,
            Slot_Object => UI,
            After       => True);

         --  Add the options frame

         Gtk_New_Hbox (Main_Hbox, Spacing => 3);
         Gtk_New (Options_Frame);
         Pack_Start (Box, Main_Hbox, False, False, 3);
         Pack_Start (Main_Hbox, Options_Frame, False, False, 0);

         Gtk_New (Label);
         Set_Use_Markup (Label, True);
         Set_Markup (Label, "Options");
         Set_Label_Widget (Options_Frame, Label);

         Gtk_New (Table, 3, 3, False);
         Set_Border_Width (Table, 5);
         Add (Options_Frame, Table);

         Gtk_New_Hbox (Hbox);
         Set_Spacing (Hbox, 3);
         Gtk_New (Label, "Launch mode");
         Pack_Start (Hbox, Label, False, False, 0);
         Attach (Table,
                 Child         => Hbox,
                 Left_Attach   => 0,
                 Right_Attach  => 1,
                 Top_Attach    => 0,
                 Bottom_Attach => 1,
                 Xoptions      => Expand or Fill);

         Gtk_New (Scrolled.Launch_Combo);
         for J in Launch_Mode_Type loop
            Append_Text (Scrolled.Launch_Combo, Beautify (J'Img));
         end loop;

         Set_Tooltip_Text
           (Scrolled.Launch_Combo,
            -("Specify the launch mode for this target:" & ASCII.LF &
              "    Manually: target launched explicitly by the user, with" &
              ASCII.LF &
              "        an extra dialog showing command line if run via a menu,"
              & ASCII.LF &
              "        with no dialog if run via a button" & ASCII.LF &
              "    Manually with dialog: Ditto, always using an extra dialog" &
              ASCII.LF &
              "    Manually with no dialog: Ditto, never using an extra dialog"
              & ASCII.LF &
              "    On file save: target launched automatically when a file is"
              & ASCII.LF &
              "        saved. Can be very useful for e.g. checking file syntax"
             ));

         Gtk_New_Hbox (Hbox);
         Pack_Start (Hbox, Scrolled.Launch_Combo, False, False, 0);

         Attach (Table,
                 Child         => Hbox,
                 Left_Attach   => 1,
                 Right_Attach  => 2,
                 Top_Attach    => 0,
                 Bottom_Attach => 1,
                 Xoptions      => Expand or Fill);

         declare
            Descr : constant String :=
              -("If set, GPS will create one menu/button per subtarget" &
                " as defined by the given name. If the value is set to" &
                " ""main"", one entry per main defined in your project" &
                " hierarchy will be created. See also corresponding" &
                " macros %T and %TT on command line, and" &
                " compute_build_targets hook for advanced usage of this" &
                " field");

         begin
            Gtk_New_Hbox (Hbox);
            Set_Spacing (Hbox, 3);
            Gtk_New (Label, "Target type");
            Set_Tooltip_Text (Label, Descr);
            Pack_Start (Hbox, Label, False, False, 0);
            Attach (Table,
                    Child         => Hbox,
                    Left_Attach   => 0,
                    Right_Attach  => 1,
                    Top_Attach    => 2,
                    Bottom_Attach => 3,
                    Xoptions      => Expand or Fill);

            Gtk_New (Scrolled.Multiple_Targets);
            Set_Tooltip_Text (Scrolled.Multiple_Targets, Descr);

            Gtk_New_Hbox (Hbox);
            Pack_Start (Hbox, Scrolled.Multiple_Targets, False, False, 0);

            Attach (Table,
                    Child         => Hbox,
                    Left_Attach   => 1,
                    Right_Attach  => 2,
                    Top_Attach    => 2,
                    Bottom_Attach => 3,
                    Xoptions      => Expand or Fill);
         end;

         Gtk_New_Vbox (Buttons_Vbox);

         Gtk_New (Locations_Frame);
         Pack_Start (Main_Hbox, Locations_Frame, True, True, 0);
         Add (Locations_Frame, Buttons_Vbox);

         Gtk_New (Label, "Display target");
         Set_Label_Widget (Locations_Frame, Label);

         Gtk_New (Scrolled.Icon_Check, "in the toolbar");
         Pack_Start (Buttons_Vbox, Scrolled.Icon_Check, False, False, 3);

         Gtk_New (Scrolled.Menu_Check, "in the main menu");
         Pack_Start (Buttons_Vbox, Scrolled.Menu_Check, False, False, 3);

         Gtk_New (Scrolled.Project_Contextual_Menu_Check,
                  "in contextual menus for projects");
         Pack_Start (Buttons_Vbox, Scrolled.Project_Contextual_Menu_Check,
                     False, False, 3);

         Gtk_New (Scrolled.File_Contextual_Menu_Check,
                  "in contextual menus for files");
         Pack_Start (Buttons_Vbox, Scrolled.File_Contextual_Menu_Check,
                     False, False, 3);

         Gtk_New_Hbox (Hbox);
         Set_Spacing (Hbox, 3);

         Gtk_New (Label, "Icon");
         Pack_Start (Hbox, Label, False, False, 0);
         Attach (Table,
                 Child         => Hbox,
                 Left_Attach   => 0,
                 Right_Attach  => 1,
                 Top_Attach    => 1,
                 Bottom_Attach => 2,
                 Xoptions      => Expand or Fill);

         Gtk_New_Hbox (Hbox);
         Gtk_New
           (Scrolled.Icon_Button,
            Icon_Name => To_String (Icons_List (Icons_List'First)));
         Pack_Start (Hbox, Scrolled.Icon_Button, False, False, 0);

         Icon_Callback.Connect
           (Scrolled.Icon_Button, Signal_Selection_Changed,
            On_Icon_Selected'Access, Scrolled);
         for J in Icons_List'Range loop
            Add_Item
              (Scrolled.Icon_Button,
               To_String (Icons_List (J)), To_String (Icons_List (J)));
         end loop;
         Add_Item (Scrolled.Icon_Button, "custom",
                   Icon_Name => "invalid-symbolic");  --  fallback

         Attach (Table,
                 Child         => Hbox,
                 Left_Attach   => 1,
                 Right_Attach  => 2,
                 Top_Attach    => 1,
                 Bottom_Attach => 2,
                 Xoptions      => Expand or Fill);

         --  Initialize the options

         Set_Active (Scrolled.Launch_Combo,
                     Launch_Mode_Type'Pos (Target.Properties.Launch_Mode));

         declare
            Icon : Unbounded_String;
         begin
            if Target.Properties.Icon_Name = "" then
               Icon := Target.Model.Icon;
            else
               Icon := Target.Properties.Icon_Name;
            end if;

            --  Try to select the icon
            Select_Item (Scrolled.Icon_Button, To_String (Icon));

            --  If unsuccessful, then select the custom icon, and set the
            --  text in the entry.
            if Get_Selected_Item (Scrolled.Icon_Button) /=
              To_String (Icon)
            then
               --  Selecting the "custom" item will create the Icon_Entry
               --  widget.
               Select_Item (Scrolled.Icon_Button, "custom");
               Scrolled.Icon_Entry.Set_Text (To_String (Icon));
            end if;
         end;

         Set_Active (Scrolled.Icon_Check, Target.Properties.In_Toolbar);
         Set_Active (Scrolled.Menu_Check, Target.Properties.In_Menu);
         Set_Active (Scrolled.Project_Contextual_Menu_Check,
                     Target.Properties.In_Contextual_Menu_For_Projects);
         Set_Active (Scrolled.File_Contextual_Menu_Check,
                     Target.Properties.In_Contextual_Menu_For_Files);
         Scrolled.Multiple_Targets.Set_Text
           (To_String (Target.Properties.Target_Type));
      end if;

      --  Add the switches frame
      if Scrolled.Target.Model.Switches /= null then
         Gtk_New (Scrolled.Frame);

         if Single then
            Set_Shadow_Type (Scrolled.Frame, Shadow_None);
         else
            Gtk_New (Label);
            Set_Use_Markup (Label, True);
            Set_Markup (Label, "Command line");
            Set_Label_Widget (Scrolled.Frame, Label);
         end if;

         Pack_Start (Box, Scrolled.Frame, True, True, 0);

         Set_Switches (Scrolled);

         if Single then
            Object_Connect
              (Widget      => Get_Entry (Scrolled.Editor),
               Name        => Gtk.Editable.Signal_Changed,
               Cb          => On_Entry_Changed'Access,
               Slot_Object => UI,
               After       => True);

            Gtk_New (Scrolled.Expanded_Entry);

            if UI.Expand_Cmd_Line = null then
               Set_Text
                 (Get_Buffer (Scrolled.Expanded_Entry),
                  Get_Text (Get_Entry (Scrolled.Editor)));
            else
               Set_Text
                 (Get_Buffer (Scrolled.Expanded_Entry),
                  UI.Expand_Cmd_Line (Get_Text (Get_Entry (Scrolled.Editor))));
            end if;

            Set_Editable (Scrolled.Expanded_Entry, False);
            Set_Wrap_Mode (Scrolled.Expanded_Entry, Wrap_Word);
            Get_Style_Context (Scrolled.Expanded_Entry).Add_Class
              ("command_line_preview");
            Gtk_New (Options_Frame);
            Set_Shadow_Type (Options_Frame, Shadow_None);
            Add (Options_Frame, Scrolled.Expanded_Entry);
            Pack_Start (Box, Options_Frame, False, False, 3);
         end if;
      end if;

      return Scrolled;
   end Switches_For_Target;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (UI : access Build_UI_Record'Class) is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Num   : Gint;
   begin
      Get_Selected (Get_Selection (UI.View), Model, Iter);

      if Iter /= Null_Iter then
         Num := Get_Int (UI.View.Model, Iter, Num_Column);
         Set_Current_Page (UI.Notebook, Num);
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Selection_Changed;

   procedure On_Selection_Changed (UI : access Mode_UI_Record'Class) is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Num   : Gint;
   begin
      Get_Selected (Get_Selection (UI.View), Model, Iter);

      if Iter /= Null_Iter then
         Num := Get_Int (UI.View.Model, Iter, Num_Column);
         Set_Current_Page (UI.Notebook, Num);
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Selection_Changed;

   -----------------------
   -- On_Target_Renamed --
   -----------------------

   procedure On_Target_Renamed
     (UI     : access Build_UI_Record'Class;
      Params : Glib.Values.GValues)
   is
      Text        : constant String := Get_String (Nth (Params, 2));
      Ignored     : Message_Dialog_Buttons;
      Old_Target  : constant Target_Access := Get_Selected_Target (UI);
   begin
      --  If the new name is the same as the old name, nothing to do
      if To_String (Old_Target.Name) = Text then
         return;
      end if;

      --  Validate that we are not giving the name of a target that already
      --  exists.
      if Get_Target_From_Name (UI.Registry, Text) /= null then
         Ignored := Message_Dialog
           (Msg            => -"A target with this name already exists",
            Buttons        => Button_OK,
            Title          => -"Name conflict",
            Parent         => Gtk_Window (Get_Toplevel (UI)));
         return;
      end if;

      Duplicate_Target
        (UI.Registry,
         To_String (Old_Target.Name),
         Text,
         To_String (Old_Target.Properties.Category));
      Remove_Target (UI.Registry, To_String (Old_Target.Name));

      Refresh (UI, Text);
   end On_Target_Renamed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Config_UI : out Configuration_UI_Access;
      Registry  : Build_Config_Registry_Access;
      Fixed_Font : Pango_Font_Description)
   is
      Vbox          : Gtk_Vbox;

      Col           : Gtk_Tree_View_Column;
      Text_Renderer : Gtk_Cell_Renderer_Text;
      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;

      Buttons       : Gtk_Hbox;
      Button        : Gtk_Button;
      Ignore        : Gtk_Button;
      Image         : Gtk_Image;

      Scrolled      : Gtk_Scrolled_Window;

      Dummy         : Gint;
      pragma Unreferenced (Dummy, Ignore);
   begin
      Config_UI := new Configuration_UI_Record;
      Initialize_Vbox (Config_UI, Homogeneous => False);
      Config_UI.Set_Name ("Build Targets Editor");

      Config_UI.Build_UI := new Build_UI_Record;
      Initialize_Hbox (Config_UI.Build_UI);

      Config_UI.Build_UI.Registry := Registry;

      --  Create the tree view
      Gtk_New (Config_UI.Build_UI.View, Column_Types);
      Set_Search_Column (Config_UI.Build_UI.View, Name_Column);
      Config_UI.Build_UI.View.Set_Headers_Visible (False);

      Gtk_New (Col);

      Gtk_New (Icon_Renderer);
      Gtk_New (Text_Renderer);

      Col.Pack_Start (Icon_Renderer, False);
      Col.Pack_Start (Text_Renderer, False);
      Col.Add_Attribute (Icon_Renderer, "icon-name", Icon_Column);
      Col.Add_Attribute (Text_Renderer, "markup", Name_Column);
      Col.Add_Attribute (Text_Renderer, "editable", Editable_Column);

      Build_UI_Callback.Object_Connect
        (Text_Renderer,
         Signal_Edited,
         On_Target_Renamed'Access,
         Config_UI.Build_UI);

      Dummy := Config_UI.Build_UI.View.Append_Column (Col);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Scrolled.Set_Shadow_Type (Shadow_In);
      Scrolled.Add (Config_UI.Build_UI.View);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Vbox, Scrolled, Expand => True, Fill => True);

      --  Create the Add/Remove/Duplicate buttons
      Gtk_New_Hbox (Buttons, Spacing => 3);

      Gtk_New (Button);
      Gtk_New_From_Icon_Name (Image, "gps-add-symbolic", Icon_Size_Menu);
      Button.Set_Image (Image);
      Button.Set_Relief (Relief_None);
      Button.Set_Tooltip_Text (-"Add new target");
      Buttons.Pack_Start (Button, Expand => False);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Add_Target'Access,
         Slot_Object => Config_UI.Build_UI,
         After       => True);

      Gtk_New (Button);
      Gtk_New_From_Icon_Name (Image, "gps-remove-symbolic", Icon_Size_Menu);
      Button.Set_Image (Image);
      Button.Set_Relief (Relief_None);
      Button.Set_Tooltip_Text (-"Remove selected target");
      Buttons.Pack_Start (Button, Expand => False);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Remove_Target'Access,
         Slot_Object => Config_UI.Build_UI,
         After       => True);

      Gtk_New (Button);
      Gtk_New_From_Icon_Name
        (Image, "gps-new-document-symbolic", Icon_Size_Menu);
      Button.Set_Image (Image);
      Button.Set_Relief (Relief_None);
      Button.Set_Tooltip_Text (-"Clone selected target");
      Buttons.Pack_Start (Button, Expand => False);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Duplicate_Target'Access,
         Slot_Object => Config_UI.Build_UI,
         After       => True);

      Config_UI.Build_UI.Pack_Start (Vbox, Expand => False);

      Gtk_New_Vbox (Vbox);
      Config_UI.Build_UI.Pack_Start
        (Vbox,
         Expand => True,
         Fill   => True);

      Config_UI.Pack_Start (Buttons, Expand => False);

      --  Create the main notebook

      Gtk_New (Config_UI.Build_UI.Notebook);
      Config_UI.Build_UI.Notebook.Set_Show_Tabs (False);
      Config_UI.Build_UI.Notebook.Set_Show_Border (False);
      Vbox.Pack_Start
        (Config_UI.Build_UI.Notebook,
         Expand => True,
         Fill   => True);

      --  Create page 0 in the notebook
      declare
         Label : Gtk_Label;
      begin
         Gtk_New (Label);
         Label.Set_Use_Markup (True);
         Label.Set_Markup (-"Select a target to configure.");
         Config_UI.Build_UI.Notebook.Append_Page (Label);
      end;

      Object_Connect
        (Widget      => Get_Selection (Config_UI.Build_UI.View),
         Name        => Gtk.Tree_Selection.Signal_Changed,
         Cb          => On_Selection_Changed'Access,
         Slot_Object => Config_UI.Build_UI,
         After       => True);

      --  Add everything to the dialog/window

      Config_UI.Pack_Start
        (Config_UI.Build_UI,
         Expand => True,
         Fill   => True);

      Config_UI.Build_UI.Fixed_Font := Fixed_Font;
      Refresh (Config_UI.Build_UI, "");

      --  Select the first target of the first category, initially

      Set_Current_Page (Config_UI.Build_UI.Notebook, 1);

      declare
         Path : Gtk_Tree_Path;
      begin
         Gtk_New (Path, "0:0");
         Select_Path (Get_Selection (Config_UI.Build_UI.View), Path);
         Path_Free (Path);
      end;
   end Gtk_New;

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self : not null access Configuration_UI_Record'Class) is
   begin
      Save_Targets (Self.Build_UI);
   end Apply_Changes;

   ------------------
   -- Modes_Dialog --
   ------------------

   procedure Modes_Dialog
     (Registry     : Build_Config_Registry_Access;
      Parent       : Gtk_Window   := null;
      Set_Default_Size_From_History : not null access procedure
         (Win : not null access Gtk_Window_Record'Class);
      Changes_Made : out Boolean)
   is
      UI     : Mode_UI_Access;
      Dialog : Gtk_Dialog;
      Vbox   : Gtk_Vbox;

      Col           : Gtk_Tree_View_Column;
      Text_Renderer : Gtk_Cell_Renderer_Text;
      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;

      Buttons       : Gtk_Hbox;
      Button, Temp  : Gtk_Button;
      pragma Unreferenced (Temp);
      Image         : Gtk_Image;

      Scrolled : Gtk_Scrolled_Window;

      Dummy : Gint;
      pragma Unreferenced (Dummy);
   begin
      Changes_Made := False;

      Gtk_New (Dialog => Dialog,
               Title  => -"Mode Configuration",
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent
                  or Use_Header_Bar_From_Settings (Parent));
      Set_Default_Size_From_History (Dialog);

      UI := new Mode_UI_Record;
      Initialize_Hbox (UI);

      UI.Registry := Registry;

      --  Create the tree view
      Gtk_New (UI.View, Column_Types);
      Set_Headers_Visible (UI.View, False);

      Gtk_New (Col);

      Gtk_New (Icon_Renderer);
      Gtk_New (Text_Renderer);

      Pack_Start (Col, Icon_Renderer, False);
      Pack_Start (Col, Text_Renderer, False);
      Add_Attribute (Col, Icon_Renderer, "icon-name", Icon_Column);
      Add_Attribute (Col, Text_Renderer, "markup", Name_Column);
      Dummy := Append_Column (UI.View, Col);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Add (Scrolled, UI.View);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Vbox, Scrolled, True, True, 0);

      --  Create the Add/Remove buttons
      Gtk_New_Hbox (Buttons, Spacing => 3);

      Gtk_New (Button);
      Gtk_New_From_Icon_Name (Image, "gps-add-symbolic", Icon_Size_Menu);
      Set_Image (Button, Image);
      Set_Relief (Button, Relief_None);
      Set_Tooltip_Text (Widget => Button,
                        Text   => -"Add new mode");
      Pack_Start (Buttons, Button, False, False, 0);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Add_Mode'Access,
         Slot_Object => UI,
         After       => True);

      Gtk_New (Button);
      Gtk_New_From_Icon_Name (Image, "gps-remove-symbolic", Icon_Size_Menu);
      Set_Image (Button, Image);
      Set_Relief (Button, Relief_None);
      Set_Tooltip_Text (Widget => Button,
                        Text   => -"Remove selected mode");
      Pack_Start (Buttons, Button, False, False, 0);
--        Object_Connect
--          (Widget      => Button,
--           Name        => Gtk.Button.Signal_Clicked,
--           Cb          => On_Remove_Target'Access,
--           Slot_Object => UI,
--           After       => True);

      Pack_Start (UI, Vbox, False, True, 0);

      Gtk_New_Vbox (Vbox);
      Pack_Start (UI, Vbox, True, True, 3);

      Pack_Start (Get_Content_Area (Dialog), Buttons, False, False, 3);

      --  Create the main notebook

      Gtk_New (UI.Notebook);
      Set_Show_Tabs (UI.Notebook, False);
      Set_Show_Border (UI.Notebook, False);
      Pack_Start (Vbox, UI.Notebook, True, True, 0);

      --  Create page 0 in the notebook
      declare
         Label : Gtk_Label;
      begin
         Gtk_New (Label);
         Set_Use_Markup (Label, True);
         Set_Markup (Label, -"Select a mode to configure.");
         Append_Page (UI.Notebook, Label);
      end;

      Object_Connect
        (Widget      => Get_Selection (UI.View),
         Name        => Gtk.Tree_Selection.Signal_Changed,
         Cb          => On_Selection_Changed'Access,
         Slot_Object => UI,
         After       => True);

      --  Create the dialog buttons

      Temp := Gtk_Button (Add_Button (Dialog, -"OK", Gtk_Response_OK));
      Temp := Gtk_Button
        (Add_Button (Dialog, -"Apply", Gtk_Response_Apply));
      Temp := Gtk_Button
        (Add_Button (Dialog, -"Cancel", Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);

      --  Add everything to the dialog/window

      Pack_Start (Get_Content_Area (Dialog), UI, True, True, 3);

      Refresh (UI);
      Show_All (Dialog);

      --  Select the first target of the first category, initially

      Set_Current_Page (UI.Notebook, 1);

      declare
         Path : Gtk_Tree_Path;
      begin
         Gtk_New (Path, "0");
         Select_Path (Get_Selection (UI.View), Path);
         Path_Free (Path);
      end;

      --  Run the dialog

      loop
         case Run (Dialog) is
            when Gtk_Response_Apply =>
--                 Save_Modes (UI);
               Changes_Made := True;

            when Gtk_Response_OK =>
--                 Save_Modes (UI);
               Destroy (Dialog);
               Changes_Made := True;
               exit;

            when others =>
               Destroy (Dialog);
               exit;
         end case;
      end loop;
   end Modes_Dialog;

   -------------------
   -- On_Add_Target --
   -------------------

   procedure On_Add_Target (UI : access Build_UI_Record'Class) is
      Name, Model, Cat : Unbounded_String;
      Cancelled        : Boolean;
   begin
      Add_Target_Dialog (UI, Model, Name, Cat, Cancelled);

      if not Cancelled then
         Create_Target
           (Registry => UI.Registry,
            Name     => To_String (Name),
            Category => To_String (Cat),
            Model    => To_String (Model));
         Refresh
           (UI, Strip_Single_And_Unescape_Underscores (To_String (Name)));
      end if;
   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Add_Target;

   ----------------------
   -- On_Remove_Target --
   ----------------------

   procedure On_Remove_Target (UI : access Build_UI_Record'Class) is
      Target    : Target_Access;
      Cancelled : Boolean;

      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Name      : Unbounded_String;
   begin
      Target := Get_Selected_Target (UI);
      if Target = null then
         return;
      end if;

      Delete_Target_Dialog (UI, Target, Cancelled);

      if not Cancelled then
         --  Attempt to get the name of the target immediately following the
         --  current target in the tree, so that we can highlight it after
         --  the target is removed.

         Get_Selected (Get_Selection (UI.View), Model, Iter);

         if Iter /= Null_Iter then
            Next (Model, Iter);

            if Iter /= Null_Iter then
               Name := To_Unbounded_String
                 (Get_String (Model, Iter, Name_Column));
            end if;
         end if;

         Remove_Target (UI.Registry, To_String (Target.Name));
         Refresh (UI, To_String (Name));
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Remove_Target;

   -----------------
   -- On_Add_Mode --
   -----------------

   procedure On_Add_Mode (UI : access Mode_UI_Record'Class) is
      Name : Unbounded_String;
      Mode : Mode_Record;
   begin
      Set_Unbounded_String
        (Name,
         Query_User (Parent        => Gtk_Window (Get_Toplevel (UI)),
                     Prompt        => -"Mode Name",
                     Password_Mode => False,
                     Urgent        => False));

      if Length (Name) > 0 then
         Mode.Name := Name;
         Insert_Mode
           (Registry => UI.Registry,
            Name     => Name,
            Mode     => Mode);
         Refresh (UI);
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Add_Mode;

   -------------------------
   -- On_Duplicate_Target --
   -------------------------

   procedure On_Duplicate_Target (UI : access Build_UI_Record'Class) is
      Target    : Target_Access;
      Name, Cat : Unbounded_String;
      Cancelled : Boolean;
   begin
      Target := Get_Selected_Target (UI);
      if Target = null then
         return;
      end if;

      Clone_Target_Dialog (UI, Target, Name, Cat, Cancelled);

      if not Cancelled then
         Duplicate_Target
           (UI.Registry,
            To_String (Target.Name),
            To_String (Name),
            To_String (Cat));
         Refresh (UI, To_String (Name));
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Duplicate_Target;

   --------------------------
   -- Single_Target_Dialog --
   --------------------------

   procedure Single_Target_Dialog
     (Registry        : Build_Config_Registry_Access;
      Parent          : Gtk_Window   := null;
      Target          : String;
      History         : Histories.History;
      Expand_Cmd_Line : Cmd_Line_Expander;
      Set_Default_Size_From_History : not null access procedure
         (Win : not null access Gtk_Window_Record'Class);
      Result          : out GNAT.OS_Lib.Argument_List_Access;
      Fixed_Font      : Pango_Font_Description)
   is
      UI     : Build_UI_Access;
      Dialog : Gtk_Dialog;
      Ignore : Gtk_Button;
      pragma Unreferenced (Ignore);

      function Exists
        (List : GNAT.Strings.String_List_Access;
         Item : String) return Boolean;
      --  Return true if Item is found in List

      function Exists
        (List : GNAT.Strings.String_List_Access;
         Item : String) return Boolean is
      begin
         if List = null then
            return False;
         end if;

         for J in List'Range loop
            if List (J).all = Item then
               return True;
            end if;
         end loop;

         return False;
      end Exists;

      Ent : Gtk_Entry;

      Dummy : Gint;
      pragma Unreferenced (Dummy);

   begin
      --  Return immediately if the target does not exist

      if not Contains (Registry.Targets, To_Unbounded_String (Target)) then
         return;
      end if;

      Gtk_New (Dialog => Dialog,
               Title  => Target,
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent
                  or Use_Header_Bar_From_Settings (Parent));
      Set_Default_Size_From_History (Dialog);

      UI := new Build_UI_Record;
      Initialize_Hbox (UI);

      UI.Expand_Cmd_Line := Expand_Cmd_Line;
      UI.Registry := Registry;
      UI.History  := History;

      --  Add everything to the dialog/window

      Pack_Start (Get_Content_Area (Dialog), UI, True, True, 3);

      --  Create the target UI itself

      UI.Target_UI := Switches_For_Target
        (UI         => UI,
         History    => UI.History,
         Target     => Get_Target_From_Name (Registry, Target),
         Single     => True,
         Fixed_Font => Fixed_Font);

      Pack_Start (UI, UI.Target_UI, True, True, 3);

      --  Create the dialog buttons

      Ignore := Gtk_Button
        (Add_Button (Dialog, -"Execute", Gtk_Response_OK));
      Ignore := Gtk_Button
        (Add_Button (Dialog, -"Cancel", Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);

      Ent := Get_Entry (UI.Target_UI.Editor);

      --  Set the entry to the latest history

      if History /= null then
         declare
            List    : constant GNAT.Strings.String_List_Access :=
              Get_History (History.all, Target_To_Key (UI.Target_UI.Target));
            Default : constant String := Argument_List_To_String
              (UI.Target_UI.Target.Command_Line.To_String_List
                 (Expanded => False).all);

         begin
            if List /= null
              and then List'Length /= 0
              and then List (List'First) /= null
              and then Exists (List, Default)
            then
               Set_Text (Ent, List (List'First).all);
               Set_Command_Line (UI.Target_UI.Editor, List (List'First).all);
            else
               --  If not already done, add the contents of the entry to the
               --  history. That way, the user can always find the original
               --  command line through the history.
               --  Note that we always look for the entry in the history,
               --  since the default command may have changed since last time.

               Add_To_History
                 (History.all,
                  Target_To_Key (UI.Target_UI.Target),
                  Default);
            end if;
         end;
      end if;

      --  Show the dialog

      Show_All (Dialog);

      --  Grab the focus on the entry, select the text, and make it activate
      --  the default, so that the user only has to press Enter if he is
      --  happy with the selection.
      Grab_Focus (Ent);
      Select_Region (Ent, 0);
      Set_Activates_Default (Ent, True);

      --  Run the dialog

      if Run (Dialog) = Gtk_Response_OK then
         Result := Get_Command_Line (UI.Target_UI.Editor, False);
         if History /= null then
            Add_To_History
              (History.all,
               Target_To_Key (UI.Target_UI.Target),
               Get_Text (Get_Entry (UI.Target_UI.Editor)));

            Set_Persistent
              (History.all,
               Target_To_Key (UI.Target_UI.Target),
               UI.Target_UI.Target.Model.Persistent_History);
         end if;
      end if;

      Destroy (Dialog);
   end Single_Target_Dialog;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (UI            : access Build_UI_Record'Class;
      Select_Target : String)
   is
      Count : Gint := 1;
      --  Indicates the number of the target that we are currently adding

      Target_Iter : Gtk_Tree_Row_Reference;

      procedure Add_Target
        (View   : Tree_View;
         Target : Target_Access);
      --  Add Target to View

      ----------------
      -- Add_Target --
      ----------------

      procedure Add_Target
        (View   : Tree_View;
         Target : Target_Access)
      is
         function Get_Or_Create_Category
           (C : Unbounded_String) return Gtk_Tree_Iter;
         --  Return iter corresponding to category C, creating it if necessary

         ----------------------------
         -- Get_Or_Create_Category --
         ----------------------------

         function Get_Or_Create_Category
           (C : Unbounded_String) return Gtk_Tree_Iter
         is
            function Get_Category_Name (S : Unbounded_String) return String;
            --  Return the string to store in model for category S

            function Strip_Underscores (S : String) return String;
            --  Strip key shortcut underscores from S

            function Strip_Underscores (S : String) return String is
               Result : String (S'Range);
               Index  : Natural := Result'First;
            begin
               for J in S'Range loop
                  if S (J) /= '_'
                    or else (J > S'First and then S (J - 1) = '_')
                  then
                     Result (Index) := S (J);
                     Index := Index + 1;
                  end if;
               end loop;

               return Result (Result'First .. Index - 1);
            end Strip_Underscores;

            -----------------------
            -- Get_Category_Name --
            -----------------------

            function Get_Category_Name (S : Unbounded_String) return String is
            begin
               return "<b>"
                 & Glib.Convert.Escape_Text (Strip_Underscores (To_String (S)))
                 & "</b>";
            end Get_Category_Name;

            Iter     : Gtk_Tree_Iter;
            Cat_Name : constant String := Get_Category_Name (C);
         begin
            Iter := Get_Iter_First (View.Model);

            --  Look for existing top-level iter with the right name
            while Iter /= Null_Iter loop
               if Get_String (View.Model, Iter, Name_Column) = Cat_Name then
                  return Iter;
               end if;

               Next (View.Model, Iter);
            end loop;

            --  We have not found our iter, create it now
            Append (View.Model, Iter, Null_Iter);
            Set_Columns
              (View.Model, Iter,
               Icon     => "gps-emblem-directory-open",
               Name     => Cat_Name,
               Num      => 0, --  Category iters correspond to
                              --  page 0 in the main notebook
               Editable => False);

            return Iter;
         end Get_Or_Create_Category;

         Category : Gtk_Tree_Iter;
         Iter     : Gtk_Tree_Iter;
         Path     : Gtk_Tree_Path;

         Icon_Str : Unbounded_String;
      begin
         Category := Get_Or_Create_Category (Target.Properties.Category);

         Append (View.Model, Iter, Category);

         if Target.Properties.Icon_Name /= "" then
            Icon_Str := Target.Properties.Icon_Name;
         elsif Target.Model.Icon /= "" then
            Icon_Str := Target.Model.Icon;
         end if;

         if Icon_Str /= "" then
            Set_Columns
              (View.Model, Iter,
               Icon     => To_String (Icon_Str),
               Name     => Glib.Convert.Escape_Text (To_String (Target.Name)),
               Num      => Count,
               Editable => not Get_Properties (Target).Read_Only);
         else
            Set_And_Clear
              (View.Model, Iter,
               (Name_Column, Num_Column, Editable_Column),
               (As_String (Glib.Convert.Escape_Text (To_String (Target.Name))),
                As_Int     (Count),
                As_Boolean (not Get_Properties (Target).Read_Only)));
         end if;

         if Select_Target = To_String (Target.Name) then
            Path := Get_Path (View.Model, Iter);
            Gtk_New (Target_Iter, +View.Model, Path);
            Path_Free (Path);
         end if;
      end Add_Target;

      use Target_List;
      C : Cursor;

   begin
      --  Empty the tree

      Clear (UI.View.Model);

      --  Empty the notebook

      --  Note that we keep page 0 in the notebook: this is the empty page
      for J in reverse 1 .. Get_N_Pages (UI.Notebook) - 1 loop
         Remove_Page (UI.Notebook, J);
      end loop;

      --  Fill the tree and the notebook

      C := UI.Registry.Targets.First;

      --  Iterate over all targets
      while Has_Element (C) loop
         --  Add the target in the tree_view
         Add_Target (UI.View, Element (C));

         --  Add the page in the notebook
         Append_Page
           (UI.Notebook,
            Switches_For_Target
              (UI, Element (C), False, UI.History, UI.Fixed_Font));

         Count := Count + 1;
         Next (C);
      end loop;

      Show_All (UI.Notebook);

      Expand_All (UI.View);

      --  Select the target

      if Target_Iter /= Null_Gtk_Tree_Row_Reference then
         if Valid (Target_Iter) then
            declare
               Path : Gtk_Tree_Path;
               Iter : Gtk_Tree_Iter;
            begin
               Path := Get_Path (Target_Iter);
               Iter := Get_Iter (UI.View.Model, Path);
               Select_Iter (Get_Selection (UI.View), Iter);
               Path_Free (Path);
            end;
         end if;

         Free (Target_Iter);
      end if;
   end Refresh;

   procedure Refresh (UI : access Mode_UI_Record'Class) is
      Count : Gint := 1;
      --  Indicates the number of the target that we are currently adding

      procedure Add_Mode
        (View   : Tree_View;
         Mode   : Mode_Record);
      --  Add Mode to View

      --------------
      -- Add_Mode --
      --------------

      procedure Add_Mode
        (View  : Tree_View;
         Mode  : Mode_Record)
      is
         function Get_Mode_Name (S : Unbounded_String) return String;
         --  Return the string to store in model for mode S

         -------------------
         -- Get_Mode_Name --
         -------------------

         function Get_Mode_Name (S : Unbounded_String) return String is
         begin
            return "<b>"
              & Glib.Convert.Escape_Text (To_String (S))
              & "</b>";
         end Get_Mode_Name;

         Iter      : Gtk_Tree_Iter;
         Mode_Name : constant String := Get_Mode_Name (Mode.Name);
         Table     : Gtk_Table;
         Ent       : Gtk_GEntry;
         Label     : Gtk_Label;
         Check     : Gtk_Check_Button;

         use Model_List;
         C : Cursor;
         S : Unbounded_String;

      begin
         Iter := Get_Iter_First (View.Model);

         --  Look for existing top-level iter with the right name
         while Iter /= Null_Iter loop
            if Get_String (View.Model, Iter, Name_Column) = Mode_Name then
               return;
            end if;

            Next (View.Model, Iter);
         end loop;

         --  We have not found our iter, create it now
         Append (View.Model, Iter, Null_Iter);
         Set_Columns
           (View.Model, Iter,
            Icon     => "gps-emblem-directory-open",
            Name     => Mode_Name,
            Num      => Count, --  Set the corresponding page in the notebook
            Editable => False);

         Gtk_New (Table, 5, 2, False);

         Gtk_New (Label, -"Description");
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, 0, 1, Fill, 0,
                 Xpadding => 2, Ypadding => 2);
         Gtk_New (Ent);
         Set_Text (Ent, To_String (Mode.Description));
         Attach (Table, Ent, 1, 2, 0, 1, Yoptions => 0);

         Gtk_New (Label, -"Models");
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, 1, 2, Fill, 0,
                 Xpadding => 2, Ypadding => 2);
         Gtk_New (Ent);

         C := First (Mode.Models);

         if Has_Element (C) then
            loop
               Append (S, Element (C).Model);
               Next (C);

               exit when not Has_Element (C);

               Append (S, ",");
            end loop;
         end if;

         Set_Text (Ent, To_String (S));
         Attach (Table, Ent, 1, 2, 1, 2, Ypadding => 2, Yoptions => 0);

         Gtk_New (Label, -"Arguments");
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, 2, 3, Fill, 0,
                 Xpadding => 2, Ypadding => 2);
         Gtk_New (Ent);

         declare
            List : String_List_Access :=
              Mode.Args.To_String_List (Expanded => False);
         begin
            Set_Text (Ent, Argument_List_To_String (List.all));
            Free (List);
         end;

         Attach (Table, Ent, 1, 2, 2, 3, Ypadding => 2, Yoptions => 0);

         Gtk_New (Label, -"Subdir");
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, 3, 4, Fill, 0,
                 Xpadding => 2, Ypadding => 2);
         Gtk_New (Ent);
         Set_Text (Ent, To_String (Mode.Subdir));
         Attach (Table, Ent, 1, 2, 3, 4, Ypadding => 2, Yoptions => 0);

         Gtk_New (Label, -"Shadow");
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, 4, 5, Fill, 0,
                 Xpadding => 2, Ypadding => 2);
         Gtk_New (Check);
         Set_Active (Check, Mode.Shadow);
         Attach (Table, Check, 1, 2, 4, 5, Ypadding => 2, Yoptions => 0);

         --  Add the page in the notebook
         Append_Page (UI.Notebook, Table);
      end Add_Mode;

      use Mode_Map;
      C : Cursor;

   begin
      --  Empty the tree

      Clear (UI.View.Model);

      --  Empty the notebook

      --  Note that we keep page 0 in the notebook: this is the empty page
      for J in reverse 1 .. Get_N_Pages (UI.Notebook) - 1 loop
         Remove_Page (UI.Notebook, J);
      end loop;

      --  Fill the tree and the notebook

      C := UI.Registry.Modes.First;

      --  Iterate over all targets
      while Has_Element (C) loop
         --  Add the target in the tree_view
         Add_Mode (UI.View, Element (C));
         Count := Count + 1;
         Next (C);
      end loop;

      Show_All (UI.Notebook);
      Expand_All (UI.View);
   end Refresh;

end Build_Configurations.Gtkada;
