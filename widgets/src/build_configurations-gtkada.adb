-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Glib;        use Glib;
with Glib.Object; use Glib.Object;

with Gtk.Button;               use Gtk.Button;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Image;                use Gtk.Image;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo_Box;            use Gtk.Combo_Box;
with Gtk.Combo_Box_Entry;      use Gtk.Combo_Box_Entry;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Table;                use Gtk.Table;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;               use Gtk.Widget;

with Switches_Chooser.Gtkada;  use Switches_Chooser.Gtkada;
with GUI_Utils;                use GUI_Utils;

with Build_Configurations.Gtkada.Dialogs;
use Build_Configurations.Gtkada.Dialogs;
with GNAT.Strings;

package body Build_Configurations.Gtkada is

   use GNAT.OS_Lib;

   --  ??? Add facility to rename a target

   ---------------
   -- Constants --
   ---------------

   --  Tree view constants

   Icon_Column : constant := 0; --  Contains the icon
   Name_Column : constant := 1; --  Contains the displayed name, as markup
   Num_Column  : constant := 2;
   --  Contains the number of the corresponding page in the main notebook

   -----------------
   -- Local types --
   -----------------

   type Target_UI_Record is new Gtk_Hbox_Record with record
      Registry    : Build_Config_Registry_Access;
      Target      : Target_Access;

      Tooltips    : Gtk_Tooltips;

      Frame        : Gtk_Frame;
      --  The frame that contains the elements to describe the switches

      Editor       : Switches_Editor := null;
      --  The one switch editor for the target, if there is only one command

      Model_Entry  : Gtk_Entry;
      --  The entry containing the model

      History      : Histories.History;

      Icon_Entry   : Gtk_Entry;
      Icon_Check   : Gtk_Check_Button;
      Launch_Combo : Gtk_Combo_Box;
   end record;
   type Target_UI_Access is access all Target_UI_Record'Class;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Gtk_New
     (Target_UI : out Target_UI_Access;
      Registry  : Build_Config_Registry_Access);
   --  Create a new target_UI

   function "-" (Msg : String) return String;
   --  Convenient shortcut to the Gettext function

   function Switches_For_Target
     (UI       : access Build_UI_Record'Class;
      Target   : Target_Access;
      Single   : Boolean;
      History  : Histories.History) return Target_UI_Access;
   --  Return the widget controlling the switches for Target
   --  If Single is True, do not display the options, the models combo, etc.

   function Get_Selected_Target
     (UI : access Build_UI_Record'Class)
      return Target_Access;
   --  Return the currently selected target, or null

   procedure Set_Switches (UI : Target_UI_Access);
   --  Set the graphical elements in UI that represent

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   procedure On_Selection_Changed (UI : access Build_UI_Record'Class);
   --  Called when the selection has changed in the tree view

   procedure On_Add_Target (UI : access Build_UI_Record'Class);
   --  Launch the "add target" dialog

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

   procedure Refresh (UI : access Build_UI_Record'Class);
   --  Clear the variant areas of the UI (notebook and tree view) and fill
   --  them with the information contained in the Registry.

   function Beautify (S : String) return String;
   --  Take a string coming from a 'Image and make it fit for GUI display

   function Uglify (S : String) return String;
   pragma Unreferenced (Uglify);
   --  Inverse operation to Beautify

   function Target_To_Key (T : Target_Access) return History_Key;
   --  Return a History_Key for storing command line for T.

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

   package Target_UI_Callback is new Callback (Target_UI_Record);
   use Target_UI_Callback;

   ---------
   -- "-" --
   ---------

   function "-" (Msg : String) return String is
   begin
      --  ??? Provide implementation
      return Msg;
   end "-";

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Icon_Column => GType_String,
         Name_Column => GType_String,
         Num_Column  => GType_Int);
   end Columns_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Target_UI : out Target_UI_Access;
      Registry  : Build_Config_Registry_Access) is
   begin
      Target_UI := new Target_UI_Record;
      Target_UI.Registry := Registry;
      Initialize_Vbox (Target_UI);
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
         CL := Get_Command_Line (T.Editor, False);
         Set_Command_Line (UI.Registry, T.Target, CL.all);
         Unchecked_Free (CL);

         --  Save the options
         T.Target.Properties.Launch_Mode :=
           Launch_Mode_Type'Val (Get_Active (T.Launch_Combo));

         T.Target.Icon := To_Unbounded_String (Get_Text (T.Icon_Entry));
         T.Target.Properties.Icon_In_Toolbar := Get_Active (T.Icon_Check);
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
   -- On_Revert_Target --
   ----------------------

   procedure On_Revert_Target (UI : access Build_UI_Record'Class) is
      T  : Target_UI_Access;
   begin
      if not Yes_No_Dialog (UI, "Revert to original command line?") then
         return;
      end if;

      --  Find the current target UI

      T := Target_UI_Access
        (Get_Nth_Page (UI.Notebook, Get_Current_Page (UI.Notebook)));

      if T.Target.Default_Command_Line /= null then
         Set_Command_Line (T.Editor, T.Target.Default_Command_Line.all);
      end if;

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

      if T.Target.Icon = "" then
         Get_Selected (Get_Selection (UI.View), M, It);

         if It /= Null_Iter then --  It should not be null, but test for safety
            Set (UI.View.Model, It, Icon_Column,
                 To_String (T.Target.Model.Icon));
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

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches (UI : Target_UI_Access) is
   begin
      --  Create the switches editor

      Gtk_New
        (UI.Editor,
         UI.Target.Model.Switches,
         UI.Tooltips,
         False,
         UI.History,
         Target_To_Key (UI.Target));

      --  Create the "current command" entry

      --  Set initial values of command line and switches entries

      if UI.Target.Command_Line /= null
        and then UI.Target.Command_Line'Length > 0
      then
         Set_Command_Line (UI.Editor, UI.Target.Command_Line.all);
      end if;

      Add (UI.Frame, UI.Editor);
      Show_All (UI.Frame);
   end Set_Switches;

   -------------------------
   -- Switches_For_Target --
   -------------------------

   function Switches_For_Target
     (UI       : access Build_UI_Record'Class;
      Target   : Target_Access;
      Single   : Boolean;
      History  : Histories.History) return Target_UI_Access
   is
      Table         : Gtk_Table;
      Hbox          : Gtk_Hbox;
      Label         : Gtk_Label;
      Box           : Target_UI_Access;
      Combo         : Gtk_Combo_Box_Entry;
      Top_Box       : Gtk_Hbox;
      Options_Frame : Gtk_Frame;
      Button        : Gtk_Button;

   begin
      --  Global box

      Gtk_New (Box, UI.Registry);
      Box.Target := Target;
      Box.History := History;
      Box.Tooltips := UI.Tooltips;

      if not Single then
         Gtk_New_Hbox (Top_Box);
         Set_Spacing (Top_Box, 3);

         --  Create the "revert" button for targets that have a default command
         --  line

         if Target.Default_Command_Line /= null then
            Gtk_New_From_Stock_And_Label (Button, "gtk-refresh", " Revert ");
            Pack_End (Top_Box, Button, False, False, 0);

            Object_Connect
              (Widget      => Button,
               Name        => Gtk.Button.Signal_Clicked,
               Cb          => On_Revert_Target'Access,
               Slot_Object => UI,
               After       => True);
         end if;

         --  Create the model combo

         Combo := Models_Combo (UI);

         Gtk_New (Label, -"Target type ");
         Pack_Start (Top_Box, Label, False, False, 0);
         Pack_Start (Top_Box, Combo, False, False, 0);

         Pack_Start (Box, Top_Box, False, False, 0);
         Box.Model_Entry := Gtk_Entry (Get_Child (Combo));
         Set_Editable (Box.Model_Entry, False);
         Set_Text (Box.Model_Entry, To_String (Target.Model.Name));

         --  Connect to a change in the model combo

         Object_Connect
           (Widget      => Combo,
            Name        => Gtk.Combo_Box.Signal_Changed,
            Cb          => On_Target_Model_Changed'Access,
            Slot_Object => UI,
            After       => True);

         --  Add the options frame

         Gtk_New (Options_Frame);
         Gtk_New (Label);
         Set_Use_Markup (Label, True);
         Set_Markup (Label, "Options");
         Set_Label_Widget (Options_Frame, Label);

         Gtk_New (Table, 2, 3, False);
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

         Gtk_New_Text (Box.Launch_Combo);
         for J in Launch_Mode_Type loop
            Append_Text (Box.Launch_Combo, Beautify (J'Img));
         end loop;
         Gtk_New_Hbox (Hbox);
         Pack_Start (Hbox, Box.Launch_Combo, False, False, 0);

         Attach (Table,
                 Child         => Hbox,
                 Left_Attach   => 1,
                 Right_Attach  => 2,
                 Top_Attach    => 0,
                 Bottom_Attach => 1,
                 Xoptions      => Expand or Fill);

         Gtk_New (Box.Icon_Check, "Display button in toolbar");

         Attach (Table,
                 Child         => Box.Icon_Check,
                 Left_Attach   => 2,
                 Right_Attach  => 3,
                 Top_Attach    => 0,
                 Bottom_Attach => 1);

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

         Gtk_New (Box.Icon_Entry);
         Gtk_New_Hbox (Hbox);
         Pack_Start (Hbox, Box.Icon_Entry, False, False, 0);

         Attach (Table,
                 Child         => Hbox,
                 Left_Attach   => 1,
                 Right_Attach  => 2,
                 Top_Attach    => 1,
                 Bottom_Attach => 2,
                 Xoptions      => Expand or Fill);

         Pack_Start (Box, Options_Frame, False, False, 3);

         --  Initialize the options

         Set_Active (Box.Launch_Combo,
                     Launch_Mode_Type'Pos (Target.Properties.Launch_Mode));

         if Target.Icon = "" then
            Set_Text (Box.Icon_Entry, To_String (Target.Model.Icon));
         else
            Set_Text (Box.Icon_Entry, To_String (Target.Icon));
         end if;

         Set_Active (Box.Icon_Check, Target.Properties.Icon_In_Toolbar);
      end if;

      --  Add the switches frame

      Gtk_New (Box.Frame);

      if Single then
         Set_Shadow_Type (Box.Frame, Shadow_None);
      else
         Gtk_New (Label);
         Set_Use_Markup (Label, True);
         Set_Markup (Label, "Switches");
         Set_Label_Widget (Box.Frame, Label);
      end if;

      Pack_Start (Box, Box.Frame, True, True, 0);

      Set_Switches (Box);
      return Box;
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

   --------------------------
   -- Configuration_Dialog --
   --------------------------

   procedure Configuration_Dialog
     (Registry     : Build_Config_Registry_Access;
      Parent       : Gtk_Window   := null;
      Tooltips     : Gtk_Tooltips := null;
      Changes_Made : out Boolean)
   is
      UI     : Build_UI_Access;
      Dialog : Gtk_Dialog;
      Vbox   : Gtk_Vbox;

      Col           : Gtk_Tree_View_Column;
      Text_Renderer : Gtk_Cell_Renderer_Text;
      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;

      Buttons       : Gtk_Hbox;
      Button        : Gtk_Button;
      Image         : Gtk_Image;

      Scrolled : Gtk_Scrolled_Window;

      Dummy : Gint;
      pragma Unreferenced (Dummy);
   begin
      Changes_Made := False;

      Gtk_New (Dialog => Dialog,
               Title  => -"Build Configuration",
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent or No_Separator);

      if Parent /= null then
         Set_Transient_For (Dialog, Parent);
      end if;

      Set_Default_Size (Dialog, 750, 550);

      UI := new Build_UI_Record;
      Initialize_Hbox (UI);

      UI.Registry := Registry;

      if Tooltips = null then
         Gtk_New (UI.Tooltips);
      else
         UI.Tooltips := Tooltips;
      end if;

      --  Create the tree view
      Gtk_New (UI.View, Columns_Types);
      Set_Headers_Visible (UI.View, False);

      Gtk_New (Col);

      Gtk_New (Icon_Renderer);
      Gtk_New (Text_Renderer);

      Pack_Start (Col, Icon_Renderer, False);
      Pack_Start (Col, Text_Renderer, False);
      Add_Attribute (Col, Icon_Renderer, "stock-id", Icon_Column);
      Add_Attribute (Col, Text_Renderer, "markup", Name_Column);
      Dummy := Append_Column (UI.View, Col);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Set_Shadow_Type (Scrolled, Shadow_In);
      Add (Scrolled, UI.View);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Vbox, Scrolled, True, True, 0);

      --  Create the Add/Remove/Duplicate buttons
      Gtk_New_Hbox (Buttons, Spacing => 3);

      Gtk_New (Button);
      Gtk_New (Image, Stock_Add, Icon_Size_Menu);
      Set_Image (Button, Image);
      Set_Relief (Button, Relief_None);
      Pack_Start (Buttons, Button, False, False, 0);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Add_Target'Access,
         Slot_Object => UI,
         After       => True);

      Gtk_New (Button);
      Gtk_New (Image, Stock_Remove, Icon_Size_Menu);
      Set_Image (Button, Image);
      Set_Relief (Button, Relief_None);
      Pack_Start (Buttons, Button, False, False, 0);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Remove_Target'Access,
         Slot_Object => UI,
         After       => True);

      Gtk_New (Button);
      Gtk_New (Image, Stock_New, Icon_Size_Menu);
      Set_Image (Button, Image);
      Set_Relief (Button, Relief_None);
      Pack_Start (Buttons, Button, False, False, 0);
      Object_Connect
        (Widget      => Button,
         Name        => Gtk.Button.Signal_Clicked,
         Cb          => On_Duplicate_Target'Access,
         Slot_Object => UI,
         After       => True);

      Pack_Start (UI, Vbox, False, True, 0);

      Gtk_New_Vbox (Vbox);
      Pack_Start (UI, Vbox, True, True, 3);

      Pack_Start (Get_Vbox (Dialog), Buttons, False, False, 3);

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
         Set_Markup (Label, -"Select a target to configure.");
         Append_Page (UI.Notebook, Label);
      end;

      Object_Connect
        (Widget      => Get_Selection (UI.View),
         Name        => Gtk.Tree_Selection.Signal_Changed,
         Cb          => On_Selection_Changed'Access,
         Slot_Object => UI,
         After       => True);

      --  Create the dialog buttons

      Button := Gtk_Button (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Apply, Gtk_Response_Apply));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);

      --  Add everything to the dialog/window

      Pack_Start (Get_Vbox (Dialog), UI, True, True, 3);
      Set_Has_Separator (Dialog, False);

      Refresh (UI);

      Show_All (Dialog);

      --  Select the first target of the first category, initially

      Set_Current_Page (UI.Notebook, 1);

      declare
         Path : Gtk_Tree_Path;
      begin
         Path := Gtk_New ("0:0");
         Select_Path (Get_Selection (UI.View), Path);
         Path_Free (Path);
      end;

      --  Run the dialog

      loop
         case Run (Dialog) is
            when Gtk_Response_Apply =>
               Save_Targets (UI);
               Changes_Made := True;

            when Gtk_Response_OK =>
               Save_Targets (UI);
               Destroy (Dialog);
               Changes_Made := True;
               exit;

            when others =>
               Destroy (Dialog);
               exit;
         end case;
      end loop;
   end Configuration_Dialog;

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
         Refresh (UI);
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
   begin
      Target := Get_Selected_Target (UI);
      if Target = null then
         return;
      end if;

      Delete_Target_Dialog (UI, Target, Cancelled);

      if not Cancelled then
         Remove_Target (UI.Registry, To_String (Target.Name));
         Refresh (UI);
      end if;

   exception
      when E : others =>
         Log
           (UI.Registry, "Unexpected exception " & Exception_Information (E));
   end On_Remove_Target;

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
         Refresh (UI);
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
     (Registry : Build_Config_Registry_Access;
      Parent   : Gtk_Window   := null;
      Tooltips : Gtk_Tooltips := null;
      Target   : String;
      History  : Histories.History;
      Result   : out GNAT.OS_Lib.Argument_List_Access)
   is
      UI     : Build_UI_Access;
      Dialog : Gtk_Dialog;
      Button : Gtk_Button;
      pragma Unreferenced (Button);

      Ent    : Gtk_Entry;

      Dummy : Gint;
      pragma Unreferenced (Dummy);

      Target_UI : Target_UI_Access;
   begin
      --  Return immediately if the target does not exist.

      if not Registry.Targets.Contains (To_Unbounded_String (Target)) then
         return;
      end if;

      Gtk_New (Dialog => Dialog,
               Title  => (-"Build target '") & Target & "'",
               Parent => Parent,
               Flags  => Modal or Destroy_With_Parent or No_Separator);

      if Parent /= null then
         Set_Transient_For (Dialog, Parent);
      end if;

      UI := new Build_UI_Record;
      Initialize_Hbox (UI);

      UI.Registry := Registry;
      UI.History  := History;

      if Tooltips = null then
         Gtk_New (UI.Tooltips);
      else
         UI.Tooltips := Tooltips;
      end if;

      --  Add everything to the dialog/window

      Pack_Start (Get_Vbox (Dialog), UI, True, True, 3);
      Set_Has_Separator (Dialog, False);

      --  Create the target UI itself

      Target_UI := Switches_For_Target
        (UI      => UI,
         History => UI.History,
         Target  => Registry.Targets.Element (To_Unbounded_String (Target)),
         Single  => True);

      Pack_Start (UI, Target_UI, True, True, 3);

      --  Create the dialog buttons

      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Execute, Gtk_Response_OK));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));

      Set_Default_Response (Dialog, Gtk_Response_OK);

      Ent := Get_Entry (Target_UI.Editor);

      --  Set the entry to the latest history.

      if History /= null then
         declare
            List : constant GNAT.Strings.String_List_Access :=
              Get_History (History.all, Target_To_Key (Target_UI.Target));
         begin
            if List /= null
              and then List'Length /= 0
              and then List (List'First) /= null
            then
               Set_Text (Ent, List (List'First).all);
            end if;
         end;
      end if;

      --  Grab the focus on the entry, select the text, and make it activate
      --  the default, so that the user only has to press Enter if he is
      --  happy with the selection.
      Grab_Focus (Ent);
      Select_Region (Ent, 0);
      Set_Activates_Default (Ent, True);

      --  Show the dialog

      Show_All (Dialog);

      --  Run the dialog

      if Run (Dialog) = Gtk_Response_OK then
         Result := Get_Command_Line (Target_UI.Editor, False);
         if History /= null then
            Add_To_History
              (History.all,
               Target_To_Key (Target_UI.Target),
               Get_Text (Get_Entry (Target_UI.Editor)));
         end if;
      end if;

      Destroy (Dialog);
   end Single_Target_Dialog;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (UI : access Build_UI_Record'Class) is
      Count : Gint := 1;
      --  Indicates the number of the target that we are currently adding

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

            -----------------------
            -- Get_Category_Name --
            -----------------------

            function Get_Category_Name (S : Unbounded_String) return String is
            begin
               return "<b>" & To_String (S) & "</b>";
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
            Set (View.Model, Iter, Name_Column, Cat_Name);
            Set (View.Model, Iter, Icon_Column, "gps-folder-open");

            --  Category iters correspond to page 0 in the main notebook
            Set (View.Model, Iter, Num_Column, 0);
            return Iter;
         end Get_Or_Create_Category;

         Category : Gtk_Tree_Iter;
         Iter     : Gtk_Tree_Iter;

         Icon_Str : Unbounded_String;
      begin
         Category := Get_Or_Create_Category (Target.Category);

         Append (View.Model, Iter, Category);
         Set (View.Model, Iter, Name_Column, To_String (Target.Name));
         Set (View.Model, Iter, Num_Column, Count);

         if Target.Icon /= "" then
            Icon_Str := Target.Icon;
         elsif Target.Model.Icon /= "" then
            Icon_Str := Target.Model.Icon;
         end if;

         if Icon_Str /= "" then
            Set (View.Model, Iter, Icon_Column, To_String (Icon_Str));
         end if;
      end Add_Target;

      use Target_Map;
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
            Switches_For_Target (UI, Element (C), False, UI.History));

         Count := Count + 1;
         Next (C);
      end loop;

      Show_All (UI.Notebook);

      Expand_All (UI.View);
   end Refresh;

end Build_Configurations.Gtkada;
