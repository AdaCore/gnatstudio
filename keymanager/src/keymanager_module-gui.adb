------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with System;                   use System;

with GNAT.Case_Util;           use GNAT.Case_Util;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with Glib.Main;                use Glib.Main;
with Glib.Object;              use Glib, Glib.Object;

with Gdk.Cursor;               use Gdk.Cursor;
with Gdk.Device;               use Gdk.Device;
with Gdk.Event;                use Gdk.Event;
with Gdk.Types;                use Gdk.Types;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Window;               use Gdk, Gdk.Window;

with Gtkada.Dialogs;           use Gtkada.Dialogs;
with Gtk.Accel_Group;          use Gtk.Accel_Group;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Button_Box;           use Gtk.Button_Box;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Flow_Box;             use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;       use Gtk.Flow_Box_Child;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Radio_Button;         use Gtk.Radio_Button;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Separator_Menu_Item;  use Gtk.Separator_Menu_Item;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Toolbar;              use Gtk.Toolbar;
with Gtk.Tool_Button;          use Gtk.Tool_Button;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;    use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;      use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;
with Pango.Enums;              use Pango.Enums;
with Pango.Layout;             use Pango.Layout;

with Config;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Default_Preferences;      use Default_Preferences;
with Default_Preferences.GUI;  use Default_Preferences.GUI;
with Dialog_Utils;             use Dialog_Utils;
with Generic_Views;            use Generic_Views;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Intl;                 use GPS.Intl;
with GPS.Main_Window;          use GPS.Main_Window;
with GPS.Search;               use GPS.Search;
with GUI_Utils;                use GUI_Utils;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with Histories;                use Histories;
with GPS.Dialogs;              use GPS.Dialogs;

package body KeyManager_Module.GUI is

   Me : constant Trace_Handle := Create ("GPS,KEY_MANAGER.KEYMGR_GUI");

   Action_Column     : constant := 0;
   Key_Column        : constant := 1;
   Weight_Column     : constant := 2;
   Icon_Name_Column  : constant := 3;

   Shortcuts_Only      : Boolean_Preference;
   Categories_Pref     : Boolean_Preference;
   Show_Empty_Cat      : Boolean_Preference;

   Key_Shortcuts_Page_Name       : constant String := "General/Key Shortcuts";
   --  Name of the key shortcuts editor preferences page

   Key_Theme_Assistant_Page_Name : constant String := "Key shortcuts theme";
   --  Name of the preferences assistant page used to choose a key theme

   Action_Column_Min_Width       : constant := 250;
   --  Minimum width of the 'Actions' tree view column

   type Keys_Editor_Preferences_Page_Record is new Preferences_Page_Record with
      record
         Kernel : Kernel_Handle;
      end record;
   type Keys_Editor_Preferences_Page is
     access all Keys_Editor_Preferences_Page_Record'Class;
   --  Type used to represent the key shortcuts editor preferences page model.

   overriding function Get_Widget
     (Self    : not null access Keys_Editor_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;

   type Key_Themes_Assistant_Page_Record is
     new Keys_Editor_Preferences_Page_Record with null record;
   --  Type reprensenting the key themes preferences assistant page model.

   type Key_Theme_Widget_Record is new Gtk_Vbox_Record with record
      Kernel       : Kernel_Handle;
      Radio_Button : Gtk_Radio_Button;
   end record;
   type Key_Theme_Widget is access all Key_Theme_Widget_Record'Class;
   --  Type representing the key themes widgets displayed in the key themes
   --  preferences assistant page.

   overriding function Get_Widget
     (Self    : not null access Key_Themes_Assistant_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;

   procedure On_Child_Activated
     (Self  : access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   --  Called when a key theme is selected using the Gtk_Flow_Box selection
   --  mechanism.

   type On_Key_Theme_Selected_User_Data is record
      Child    : Gtk_Flow_Box_Child;
      Flow_Box : Gtk_Flow_Box;
      Kernel   : Kernel_Handle;
   end record;
   package On_Key_Theme_Selected_Handlers is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Radio_Button_Record,
      User_Type   => On_Key_Theme_Selected_User_Data);

   procedure On_Key_Theme_Selected
     (Self      : access Gtk_Radio_Button_Record'Class;
      User_Data : On_Key_Theme_Selected_User_Data);
   --  Called when a key theme radio button is selected in the key themes
   --  preferences assistant page.

   type Keys_Editor_Record is new Generic_Views.View_Record with record
      View               : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Help               : Gtk_Label;
      Remove_Button      : Gtk_Button;
      Grab_Button        : Gtk_Toggle_Button;
      Disable_Filtering  : Boolean := False;
      Themes             : Gtk_Combo_Box_Text;

      Filter_Grab        : Gtk_Tool_Button;

      In_Grab : Boolean := False;

      Filter_Pattern     : Search_Pattern_Access;
      --  ??? Should be freed when the view is destroyed
   end record;
   function Initialize
     (Editor : access Keys_Editor_Record'Class) return Gtk_Widget;
   overriding procedure Create_Toolbar
     (View    : not null access Keys_Editor_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Keys_Editor_Record;
      Pattern : in out Search_Pattern_Access);

   package Keys_Editor_Views is new Simple_Views
     (Module_Name        => "Keyshortcuts_editor",
      View_Name          => "Key Shortcuts",
      Formal_View_Record => Keys_Editor_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => False,
      Group              => Group_Default,
      Areas              => Gtkada.MDI.Both,
      Default_Width      => 700,
      Default_Height     => 700,
      Commands_Category  => -"Views",
      Add_Close_Button_On_Float => True,
      MDI_Flags          =>
         All_Buttons or Float_To_Main or Always_Destroy_Float,
      Position           => Position_Float,
      Initialize         => Initialize);
   use Keys_Editor_Views;
   subtype Keys_Editor_View is Keys_Editor_Views.View_Access;

   type Keys_Editor_Preferences_Page_View_Record is
     new Preferences_Page_View_Record with record
      Editor : Keys_Editor_View;
   end record;
   type Keys_Editor_Preferences_Page_View is
     access all Keys_Editor_Preferences_Page_View_Record'Class;
   --  Type used to represent the preferences page view for the keys editor
   --  shortcuts.

   overriding procedure Create_Menu
     (Self : not null access Keys_Editor_Preferences_Page_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   package Keys_Timeout is new Glib.Main.Generic_Sources (Keys_Editor_View);

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class);
   procedure Refill_Editor (View : access GObject_Record'Class);
   --  Fill the contents of the editor
   --  The second version is suitable for gtk+ callbacks.

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class);
   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class);
   procedure On_Reset (Editor : access Gtk_Widget_Record'Class);
   procedure On_Create (Editor : access Gtk_Widget_Record'Class);
   --  Handle the "Grab", "Remove", "Reset" and "Create" buttons

   function On_Delete
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Standard event handlers

   procedure On_Grab_For_Filter (View : access GObject_Record'Class);
   --  Called when the user wants to grab a key for the filter

   function Grab_Multiple_Key
     (View        : not null access Keys_Editor_Record'Class;
      For_Filter  : Boolean;
      For_Display : Boolean := False) return String;
   --  Grab a key binding, with support for multiple keymaps. Returns the
   --  empty string if no key could be grabbed.
   --  If For_Display is true, the returned string is suitable for displaying
   --  the shortcut to the user, but not to parse it into its components.
   --  For_Filter indicates the context of the grab, to change the label of
   --  the button.

   type Event_Info is record
      Key   : Gdk_Key_Type;
      State : Gdk_Modifier_Type;
   end record;
   type Event_Info_Access is access all Event_Info;
   package Event_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Event_Info_Access);

   procedure Key_Grab
     (Self      : not null access Keys_Editor_Record'Class;
      Key       : out Gdk.Types.Gdk_Key_Type;
      Mods      : out Gdk.Types.Gdk_Modifier_Type);
   --  Temporarily grab the pointer and keyboards for In_Widget, and returns
   --  the first fully defined key that the user has pressed. (Key, Mods) is
   --  set to (0, 0) if no key could be grabbed.
   --  Nothing is done in In_Widget, it is only used as a target for the grab
   --  operations.
   --  In_Widget must be realized.
   --
   --  In_Widget mustn't be a modal dialog, since otherwise the handling of
   --  grabs will interfer with the dialog.

   function Key_Press_In_Grab
     (In_Widget : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event;
      Output    : Event_Info_Access) return Boolean;
   --  Temporary event filter set when grabing the key for a key preference

   function Cancel_Grab (Self : Keys_Editor_View) return Boolean;
   --  Exit the current nest main loop, if any

   procedure On_Load_Key_Theme (Editor : access GObject_Record'Class);
   --  Called when the user selects an alternative key theme to load

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class);
   --  Refresh the list of key bindings in editor. Better use this one than
   --  Fill_Editor when possible, since this will preserve expanded/closed
   --  nodes.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences change

   package Keys_Editor_Visible_Funcs is new
     Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data (Keys_Editor_View);
   function Action_Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor_View) return Boolean;
   --  Selects whether a given row should be visible in the key shortcuts
   --  editor.

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection has changed

   function Find_Parent
     (Model  : Gtk_Tree_Store;
      Action : Action_Record_Access) return Gtk_Tree_Iter;
   --  Find the parent node for Action.
   --  Create the parent node if needed

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Icon   : String := "";
      Key    : String := "";
      Weight : Pango.Enums.Weight := Pango_Weight_Normal)
      return Gtk_Tree_Iter;
   --  Add a new line into the model

   type Expand_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Expand_All_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Expand all files within the current category

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Keys_Editor_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      Page_View     : Keys_Editor_Preferences_Page_View;
      Editor        : access Keys_Editor_Record;
      Editor_View   : Gtk_Widget;
      Focus_Widget  : Gtk_Widget;
      pragma Unreferenced (Manager, Focus_Widget);
   begin
      Page_View := new Keys_Editor_Preferences_Page_View_Record;
      Dialog_Utils.Initialize (Page_View);

      Editor := new Keys_Editor_Record;
      Editor.Set_Kernel (Self.Kernel);
      Focus_Widget := Initialize (Editor);
      Editor_View := Create_Finalized_View (Editor);

      Page_View.Append (Editor_View, Expand => True, Fill => True);
      Page_View.Editor := Editor;

      return Gtk_Widget (Page_View);
   end Get_Widget;

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Key_Themes_Assistant_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Manager);
      Key_Themes        : constant Key_Theme_Type_List :=
                            List_Key_Themes (Self.Kernel);
      Key_Theme         : Key_Theme_Type;
      Label             : Gtk_Label;
      Label_Size_Group  : Gtk_Size_Group;
      Theme_Widget      : Key_Theme_Widget;
      Hbox              : Gtk_Hbox;
      Flow_Box          : Gtk_Flow_Box;
      Radio_Group       : Gtk_Radio_Button;
      Padding           : constant Guint := 5;
      Default_Key_Theme : constant String := (if Config.Darwin_Target then
                                                 "mac_os"
                                              else
                                                 "default");

      procedure Create_Key_Theme_Widget
        (Key_Theme_Name : String;
         Description    : String);
      --  Create a key theme widget for the key theme denoted by
      --  Key_Theme_Name, if any, and append it to the page's flow box.

      procedure Add_Key_Shortcut_Example (Action : String);
      --  Add a key shortcut example in the key theme's widget 'Example'
      --  section, showing which key shortcut is binded to the given Action for
      --  this key theme.

      -----------------------------
      -- Create_Key_Theme_Widget --
      -----------------------------

      procedure Create_Key_Theme_Widget
        (Key_Theme_Name : String;
         Description    : String) is
      begin
         Key_Theme :=
           Get_Key_Theme (Find_By_Name (Key_Themes, Key_Theme_Name));

         if Key_Theme /= Null_Key_Theme then
            declare
               Key_Theme_Name : String := Key_Theme.Get_Name;
            begin
               Theme_Widget := new Key_Theme_Widget_Record;
               Theme_Widget.Kernel := Self.Kernel;

               Initialize_Vbox (Theme_Widget, Homogeneous => False);
               Flow_Box.Add (Theme_Widget);

               Gtk_New_Hbox (Hbox);
               Theme_Widget.Pack_Start (Hbox, Expand => False);

               --  Create the key theme's radio button

               To_Mixed (Key_Theme_Name);
               Gtk_New
                 (Theme_Widget.Radio_Button,
                  Group => Radio_Group,
                  Label => Key_Theme_Name);
               On_Key_Theme_Selected_Handlers.Connect
                 (Widget    => Theme_Widget.Radio_Button,
                  Name      => Gtk.Toggle_Button.Signal_Toggled,
                  Cb        => On_Key_Theme_Selected'Access,
                  User_Data => On_Key_Theme_Selected_User_Data'
                    (Child    => Gtk_Flow_Box_Child (Theme_Widget.Get_Parent),
                     Flow_Box => Flow_Box,
                     Kernel   => Self.Kernel));
               Hbox.Pack_Start (Theme_Widget.Radio_Button, Expand => False);

               if Radio_Group = null then
                  Radio_Group := Theme_Widget.Radio_Button;
               end if;

               --  Create the description label

               Gtk_New (Label, Description);
               Label.Set_Alignment (0.0, 0.5);
               Theme_Widget.Pack_Start
                 (Label,
                  Expand  => False,
                  Padding => Padding);

               --  Create the 'Examples' label

               Gtk_New (Label);
               Label.Set_Markup ("<b>Examples</b>");
               Label.Set_Alignment (0.0, 0.5);
               Theme_Widget.Pack_Start
                 (Label,
                  Expand  => False,
                  Padding => Padding);

               --  Load the key theme we want to display and retrieve some
               --  key shortcuts examples once it's loaded.

               Remove_Shortcuts (Self.Kernel, Mode => Standard_Shortcuts);
               Load_Key_Theme (Self.Kernel, Key_Theme.Get_Name);
               Set_Key_Theme (Self.Kernel, Key_Theme.Get_Name);

               Add_Key_Shortcut_Example ("open file");
               Add_Key_Shortcut_Example ("save");
               Add_Key_Shortcut_Example ("paste from clipboard");
            end;
         end if;
      end Create_Key_Theme_Widget;

      ------------------------------
      -- Add_Key_Shortcut_Example --
      ------------------------------

      procedure Add_Key_Shortcut_Example (Action : String)
      is
         User_Changed : aliased Boolean;
         Key          : constant String :=
                          Lookup_Key_From_Action
                            (Get_Shortcuts (Self.Kernel),
                             Action          => Action,
                             Use_Markup      => False,
                             Is_User_Changed => User_Changed'Unchecked_Access,
                             Default         => -Disabled_String);
      begin
         Gtk_New_Hbox (Hbox, Homogeneous => False);
         Theme_Widget.Pack_Start (Hbox, Expand => False, Padding => Padding);

         Gtk_New (Label, Action);
         Label.Set_Alignment (0.0, 0.5);
         Label_Size_Group.Add_Widget (Label);
         Hbox.Pack_Start (Label, Expand => False);

         Gtk_New (Label, Key);
         Label.Set_Alignment (0.0, 0.5);
         Hbox.Pack_Start (Label, Expand => False, Padding => Padding);
      end Add_Key_Shortcut_Example;

   begin
      --  Create the flow box that will contain the key themes widgets

      Gtk_New (Flow_Box);
      Flow_Box.Set_Min_Children_Per_Line (2);
      Flow_Box.Set_Orientation (Orientation_Horizontal);
      Flow_Box.Set_Homogeneous (True);
      Flow_Box.Set_Selection_Mode (Selection_Single);

      Gtk_New (Label_Size_Group);

      --  Create a key theme widget for the 'default' key theme and select it
      --  in the flow box.

      Create_Key_Theme_Widget
        (Key_Theme_Name => Default_Key_Theme,
         Description    => "The default key shortcuts theme.");
      Flow_Box.Select_Child (Gtk_Flow_Box_Child (Theme_Widget.Get_Parent));

      --  Create a key theme widget for the 'emacs' key theme

      Create_Key_Theme_Widget
        (Key_Theme_Name => "emacs",
         Description    => "A key shortcuts theme based on the emacs default "
         & "key shortcuts.");

      --  Reset to the 'defaut' key theme so that it's the default one if the
      --  user does not explicitly choose one in the UI.

      Remove_Shortcuts (Self.Kernel, Mode => Standard_Shortcuts);
      Load_Key_Theme (Self.Kernel, Default_Key_Theme);
      Set_Key_Theme (Self.Kernel, Default_Key_Theme);

      Flow_Box.On_Child_Activated (On_Child_Activated'Access);

      return Gtk_Widget (Flow_Box);
   end Get_Widget;

   ------------------------
   -- On_Child_Activated --
   ------------------------

   procedure On_Child_Activated
     (Self  : access Gtk_Flow_Box_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class)
   is
      pragma Unreferenced (Self);
      Theme_Widget : constant Key_Theme_Widget :=
                       Key_Theme_Widget (Child.Get_Child);
   begin
      Theme_Widget.Radio_Button.Set_Active (True);
   end On_Child_Activated;

   ---------------------------
   -- On_Key_Theme_Selected --
   ---------------------------

   procedure On_Key_Theme_Selected
     (Self      : access Gtk_Radio_Button_Record'Class;
      User_Data : On_Key_Theme_Selected_User_Data)
   is
      Theme_Name : String := Self.Get_Label;
   begin
      User_Data.Flow_Box.Select_Child (User_Data.Child);

      To_Lower (Theme_Name);
      Remove_Shortcuts (User_Data.Kernel, Mode => Standard_Shortcuts);
      Load_Key_Theme (User_Data.Kernel, Theme_Name);
      Set_Key_Theme (User_Data.Kernel, Theme_Name);
   end On_Key_Theme_Selected;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Expand_All_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Context.Context);
      Editor    : constant Preferences_Editor :=
                    Kernel.Get_Preferences.Get_Editor;
      Page_View : Keys_Editor_Preferences_Page_View;
      Path      : Gtk_Tree_Path;
      pragma Unreferenced (Self);
   begin
      if Editor /= null then
         Page_View := Keys_Editor_Preferences_Page_View
           (Editor.Get_Page_View (Key_Shortcuts_Page_Name));
      end if;

      if Page_View /= null then
         declare
            Tree_View : constant Gtk_Tree_View := Page_View.Editor.View;
         begin
            Path := Gtk_Tree_Path_New_First;

            if Tree_View.Row_Expanded (Path) then
               Tree_View.Collapse_All;
            else
               Tree_View.Expand_All;
            end if;

            Path_Free (Path);
         end;
      end if;

      return Commands.Success;
   end Execute;

   ---------
   -- Set --
   ---------

   function Set
     (Model  : Gtk_Tree_Store;
      Parent : Gtk_Tree_Iter;
      Descr  : String;
      Icon   : String := "";
      Key    : String := "";
      Weight : Pango.Enums.Weight := Pango_Weight_Normal)
      return Gtk_Tree_Iter
   is
      procedure Set
        (Tree, Iter : System.Address;
         Col1       : Gint; Value1 : String;
         Col2       : Gint; Value2 : String;
         Col3       : Gint; Value3 : String;
         Col4       : Gint; Value4 : Pango.Enums.Weight);
      pragma Import (C, Set, "ada_gtk_tree_store_set_ptr_ptr_ptr_weight");

      Iter : Gtk_Tree_Iter;

   begin
      Append (Model, Iter, Parent);
      Set
        (Get_Object (Model), Iter'Address,
         Col1 => Action_Column,     Value1 => Descr & ASCII.NUL,
         Col2 => Key_Column,        Value2 => Key & ASCII.NUL,
         Col3 => Icon_Name_Column,  Value3 => Icon & ASCII.NUL,
         Col4 => Weight_Column,     Value4 => Weight);
      return Iter;
   end Set;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent
     (Model  : Gtk_Tree_Store;
      Action : Action_Record_Access) return Gtk_Tree_Iter
   is
      Parent : Gtk_Tree_Iter;
      Base_Cat : constant String := Get_Category (Action);
      Cat : constant String :=
        (if Base_Cat = "" then "<no category" else Base_Cat);
   begin
      Parent := Find_Node (Model, Cat, Action_Column);
      if Parent = Null_Iter then
         Parent := Set (Model, Null_Iter, Descr => Cat);
      end if;
      return Parent;
   end Find_Parent;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor (Editor : access Keys_Editor_Record'Class) is
      Categories : constant Boolean := Categories_Pref.Get_Pref;
      Shortcuts  : constant Boolean := Shortcuts_Only.Get_Pref;
      Empty_Cat  : constant Boolean := Show_Empty_Cat.Get_Pref;

      Parent       : Gtk_Tree_Iter;
      Action       : Action_Record_Access;
      Action_Iter  : Action_Iterator := Start (Editor.Kernel);
      User_Changed : aliased Boolean;
   begin
      --  Disable tree filtering while refreshing the contents of the tree.
      --  This works around a bug in gtk+.
      Editor.Disable_Filtering := True;

      Clear (Editor.Model);

      --  Add all known actions in the table.
      loop
         Action := Get (Action_Iter);
         exit when Action = null;

         declare
            Name  : constant String := Get_Name (Action);
            Label : constant String := Get_Label (Action);
            Key   : constant String :=
                      Lookup_Key_From_Action
                        (Get_Shortcuts (Editor.Kernel),
                         Name,
                         Use_Markup      => False,
                         Is_User_Changed => User_Changed'Unchecked_Access,
                         Default         => -Disabled_String);
            Show  : Boolean;
         begin
            --  Do not show actions with no category, by default
            Show := Empty_Cat
              or else Get_Category (Action) /= ""
              or else Key /= "";

            if Show then
               Show := not Shortcuts or else Key /= "";
            end if;

            if Show then
               if Categories then
                  --  Create category node only when needed, which ensures
                  --  we do not show empty categories
                  Parent := Find_Parent (Editor.Model, Action);
               else
                  Parent := Null_Iter;
               end if;

               Parent := Set
                 (Model   => Editor.Model,
                  Parent  => Parent,
                  Descr   => Label,
                  Icon    => Get_Icon_Name (Action),
                  Key     => Key
                    & (if User_Changed then " (modified)" else ""),
                  Weight  => (if User_Changed then Pango_Weight_Bold
                              else Pango_Weight_Normal));
            end if;
         end;

         Next (Editor.Kernel, Action_Iter);
      end loop;

      Editor.Disable_Filtering := False;

      Refilter (Editor.Filter);
   end Fill_Editor;

   ---------------------------
   -- Add_Selection_Changed --
   ---------------------------

   procedure Add_Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed        : constant Keys_Editor_View := Keys_Editor_View (Editor);
      Selection : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Action    : Action_Record_Access;
   begin
      Get_Selected (Selection, Model, Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)
      if Iter /= Null_Iter
        and then Children (Model, Iter) = Null_Iter
      then
         Set_Sensitive (Ed.Remove_Button, True);
         Set_Sensitive (Ed.Grab_Button, True);

         Action := Lookup_Action (Ed.Kernel, Get_String (Model, Iter, 0));

         --  Action could be null if we chose to display only lines with
         --  shortcuts and the user clicks on a line for a category
         if Action /= null then
            Ed.Help.Set_Markup
              (Get_Full_Description (Action, Ed.Kernel, Use_Markup => True));
         end if;
      else
         Set_Sensitive (Ed.Remove_Button, False);
         Set_Sensitive (Ed.Grab_Button, False);
         Ed.Help.Set_Markup ("");
      end if;

   exception
      when E : others => Trace (Me, E);
   end Add_Selection_Changed;

   -----------------------
   -- Action_Is_Visible --
   -----------------------

   function Action_Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Keys_Editor_View) return Boolean
   is
      Row_Visible : Boolean := True;
      Child       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Action      : Action_Record_Access;
   begin
      if Data.Disable_Filtering then
         return True;
      end if;

      --  Compute the row itself should be visible (not withstanding its
      --  children.

      if Data.Filter_Pattern /= null then
         Row_Visible :=
           Data.Filter_Pattern.Start (Get_String (Model, Iter, 0)) /= No_Match
           or else
           Data.Filter_Pattern.Start (Get_String (Model, Iter, 1)) /= No_Match;

         if not Row_Visible then
            Action := Lookup_Action (Data.Kernel, Get_String (Model, Iter, 0));
            if Action /= null then
               Row_Visible :=
                 Data.Filter_Pattern.Start
                   (Get_Full_Description
                      (Action, Kernel => null, Use_Markup => True))
                 /= No_Match;
            end if;
         end if;
      end if;

      --  If the row should be invisible, but any of its children is visible,
      --  we display it anyway.

      if not Row_Visible then
         Child := Children (Model, Iter);
         while Child /= Null_Iter loop
            if Action_Is_Visible (Model, Child, Data) then
               return True;
            end if;
            Next (Model, Child);
         end loop;
      end if;

      return Row_Visible;

   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Action_Is_Visible;

   -------------------
   -- Refill_Editor --
   -------------------

   procedure Refill_Editor (View : access GObject_Record'Class) is
   begin
      Fill_Editor (Keys_Editor_View (View));
   end Refill_Editor;

   --------------------
   -- Refresh_Editor --
   --------------------

   procedure Refresh_Editor (Editor : access Keys_Editor_Record'Class) is

      procedure Set
        (Tree, Iter : System.Address; Col1 : Gint; Val1 : Pango.Enums.Weight);
      pragma Import (C, Set, "ada_gtk_tree_store_set_weight");

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter);
      --  Refresh for Iter and its sibling

      ------------------
      -- Refresh_Iter --
      ------------------

      procedure Refresh_Iter (Iter : Gtk_Tree_Iter) is
         It           : Gtk_Tree_Iter;
      begin
         It := Iter;
         while It /= Null_Iter loop
            if Children (Editor.Model, It) /= Null_Iter then
               Refresh_Iter (Children (Editor.Model, It));
            else
               declare
                  User_Changed : aliased Boolean;
                  Key : constant String :=
                    Lookup_Key_From_Action
                      (Get_Shortcuts (Editor.Kernel),
                       Action => Get_String (Editor.Model, It, Action_Column),
                       Default => "",
                       Use_Markup => False,
                       Is_User_Changed => User_Changed'Unchecked_Access);
                  W : Weight;
               begin
                  Set
                    (Editor.Model, It, Key_Column,
                     Key & (if User_Changed then " (modified)" else ""));

                  if User_Changed then
                     W := Pango_Weight_Bold;
                  else
                     W := Pango_Weight_Normal;
                  end if;

                  Set
                    (Get_Object (Editor.Model), It'Address, Weight_Column, W);
               end;
            end if;

            Next (Editor.Model, It);
         end loop;
      end Refresh_Iter;

   begin
      Refresh_Iter (Get_Iter_First (Editor.Model));
      Add_Selection_Changed (Editor);
   end Refresh_Editor;

   -----------------------
   -- Key_Press_In_Grab --
   -----------------------

   function Key_Press_In_Grab
     (In_Widget : access Gtk_Widget_Record'Class;
      Event     : Gdk_Event;
      Output    : Event_Info_Access) return Boolean
   is
      pragma Unreferenced (In_Widget);
      Text  : constant String :=
        Image (Get_Key_Val (Event), Get_State (Event));
   begin
      if Text /= Special_Key_Binding then
         Output.Key := Get_Key_Val (Event);
         Output.State := Get_State (Event) and Get_Default_Mod_Mask;
         Main_Quit;
      end if;
      return True;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Key_Press_In_Grab;

   --------------
   -- Key_Grab --
   --------------

   procedure Key_Grab
     (Self      : not null access Keys_Editor_Record'Class;
      Key       : out Gdk.Types.Gdk_Key_Type;
      Mods      : out Gdk.Types.Gdk_Modifier_Type)
   is
      Top    : constant Gtk_Widget := Self.Get_Toplevel;
      Device : Gdk_Device := null;
      Id     : Handler_Id;
      Output : aliased Event_Info := (0, 0);
      Cursor : Gdk.Gdk_Cursor;
   begin
      Grab_Focus (Top);

      --  We could enable a grab of the device with the following code.
      --  This seems to be very system-specific though, since setting for
      --  instance a Key_Press_Mask on a mouse device works fine on OSX, but
      --  is rejected with X11 servers. For now, we are leaving this code
      --  disabled and using the simpler Grab_Add.
      --      Device := Gtk.Main.Get_Current_Event_Device;

      if Device /= null then   --  might be null in testsuite
         if Device.Get_Source /= Source_Keyboard then
            Device := Device.Get_Associated_Device;
         end if;

         if Device = null
           or else Device.Get_Source /= Source_Keyboard
           or else Device.Grab
             (Window         => Top.Get_Window,
              Grab_Ownership => Ownership_Application,
              Owner_Events   => True,
              Event_Mask     => Key_Press_Mask,
              Cursor         => null,
              Time           => Gdk.Types.Current_Time) /= Grab_Success
         then
            Key := 0;
            Mods := 0;
            return;
         end if;
      else
         Top.Grab_Add;
      end if;

      Self.In_Grab := True;

      Id := Event_Callback.Connect
        (Top, Signal_Key_Press_Event,
         Event_Callback.To_Marshaller (Key_Press_In_Grab'Access),
         User_Data => Output'Unchecked_Access);

      Gdk_New (Cursor, Watch);
      Set_Cursor (Top.Get_Window, Cursor);

      Ref (Top);  --  in case the user closes the dialog.
      Gtk.Main.Main;

      if Top.Get_Window /= null then
         Set_Cursor (Top.Get_Window, null);
         Unref (Cursor);
         Gtk.Handlers.Disconnect (Top, Id);
         Key  := Output.Key;
         Mods := Output.State;
      else
         Key  := GDK_Escape;
         Mods := 0;
      end if;

      if Device /= null then
         Device.Ungrab (Gdk.Types.Current_Time);
      elsif Top.Get_Window /= null then
         Top.Grab_Remove;
      end if;

      Self.In_Grab := False;

      Unref (Top);
   end Key_Grab;

   -----------------
   -- Cancel_Grab --
   -----------------

   function Cancel_Grab (Self : Keys_Editor_View) return Boolean is
   begin
      --  If there is a grab pending

      if Self.In_Grab then
         Main_Quit;
         Self.In_Grab := False;
      end if;

      return True;  --  so that we can remove it later without an error
   end Cancel_Grab;

   -----------------------
   -- Grab_Multiple_Key --
   -----------------------

   function Grab_Multiple_Key
     (View        : not null access Keys_Editor_Record'Class;
      For_Filter  : Boolean;
      For_Display : Boolean := False) return String
   is
      Grabbed, Tmp : GNAT.Strings.String_Access;
      Key          : Gdk_Key_Type;
      Modif        : Gdk_Modifier_Type;
      Id           : Glib.Main.G_Source_Id;

      procedure Reset;
      procedure Reset is
         Dummy : Boolean;
         pragma Unreferenced (Dummy);
      begin
         if For_Filter then
            View.Filter_Grab.Set_Label ("Grab");
         else
            View.Grab_Button.Set_Label ("Add");
         end if;

         Unblock_Key_Shortcuts (View.Kernel);
         Dummy := Cancel_Grab (View);
      end Reset;

   begin
      Block_Key_Shortcuts (View.Kernel);

      if For_Filter then
         View.Filter_Grab.Set_Label ("press key");
      else
         View.Grab_Button.Set_Label ("press key");
      end if;

      Key_Grab (View, Key, Modif);

      if View.Get_Toplevel.In_Destruction then
         Reset;
         return "";
      elsif Key /= GDK_Escape or else Modif /= 0 then
         if For_Display then
            Grabbed := new String'
              (Gtk.Accel_Group.Accelerator_Get_Label (Key, Modif));
         else
            Grabbed := new String'(Image (Key, Modif));
         end if;
      else
         Reset;
         return "";
      end if;

      --  Are we grabbing multiple keymaps ?

      loop
         Id := Keys_Timeout.Timeout_Add
           (500, Cancel_Grab'Access, Keys_Editor_View (View));
         Key_Grab (View, Key, Modif);
         Glib.Main.Remove (Id);

         exit when Key = 0 and then Modif = 0;

         if Key = GDK_Escape and then Modif = 0 then
            Free (Grabbed);
            return "";
         end if;

         Tmp := Grabbed;
         if For_Display then
            Grabbed := new String'
              (Grabbed.all & ' '
               & Gtk.Accel_Group.Accelerator_Get_Label (Key, Modif));
         else
            Grabbed := new String'(Grabbed.all & ' ' & Image (Key, Modif));
         end if;

         Free (Tmp);
      end loop;

      Reset;

      return K : constant String := Grabbed.all do
         Free (Grabbed);
      end return;

   exception
      when others =>
         Reset;
         raise;
   end Grab_Multiple_Key;

   -----------------
   -- On_Grab_Key --
   -----------------

   procedure On_Grab_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed         : constant Keys_Editor_View := Keys_Editor_View (Editor);
      Selection  : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Sort_Model : Gtk_Tree_Model;
      Sort_Iter, Filter_Iter, Iter : Gtk_Tree_Iter;
   begin
      if Get_Active (Ed.Grab_Button) then
         Get_Selected (Selection, Sort_Model, Sort_Iter);
         if Sort_Iter /= Null_Iter then
            Convert_Iter_To_Child_Iter (Ed.Sort, Filter_Iter, Sort_Iter);
            Convert_Iter_To_Child_Iter (Ed.Filter, Iter, Filter_Iter);
         else
            Iter := Null_Iter;
         end if;

         --  Only edit for leaf nodes (otherwise these are contexts)

         if Iter /= Null_Iter
           and then Children (Ed.Model, Iter) = Null_Iter
         then
            declare
               Key          : constant String :=
                                Grab_Multiple_Key (Ed, For_Filter => False);
               Old_Action   : constant String := Lookup_Action_From_Key
                 (Key, Get_Shortcuts (Ed.Kernel));
               Old_Prefix   : constant String := Actions_With_Key_Prefix
                 (Key, Get_Shortcuts (Ed.Kernel));
               User_Changed : aliased Boolean := False;
               Count_Prefix : constant Natural :=
                                Count
                                  (Old_Prefix,
                                   To_Set (String'(1 => ASCII.LF)));
               New_Action   : constant String :=
                                Get_String (Ed.Model, Iter, Action_Column);

               type Key_Binding_Action_Type is
                 (Unbind_Old_Actions, Keep_Old_Bindings, Nothing);

               Binding_Action : Key_Binding_Action_Type := Keep_Old_Bindings;
            begin
               if Key /= "" and then Key /= "Escape" then
                  --  Do we already have an action with such a binding ?

                  if Old_Action /= ""
                    and then
                      Equal (Old_Action, New_Action, Case_Sensitive => False)
                    --  And there is a single action for this key
                    and then Count_Prefix = 1
                    --  We also check for the key binding, this is the tricky
                    --  case where the action is mapped to ctrl-x for example
                    --  and we want to map it to ctrl-x+b. So we do nothing
                    --  only if the if keys are fully equivelent.
                    and then Key = Lookup_Key_From_Action
                      (Get_Shortcuts (Ed.Kernel),
                       Old_Action,
                       Is_User_Changed => User_Changed'Access)
                  then
                     --  key already bound to Old_Action and no clash for the
                     --  prefix, nothing to do.
                     Binding_Action := Nothing;

                  elsif Count_Prefix > 1
                    or else (Count_Prefix = 1
                             and then Index (Old_Prefix, New_Action) = 0)
                  then
                     if Active (Testsuite_Handle) then
                        --  When running the testsuite, we cannot display the
                        --  dialog, since there is apparently no way to control
                        --  is from python otherwise (probably because it is
                        --  running in its own gtk+ loop).

                        --  Perform the replacement
                        Binding_Action := Unbind_Old_Actions;

                        Trace
                          (Testsuite_Handle,
                           "Dialog for already assigned key would have"
                           & " been displayed, old_action='"
                           & Old_Action & "' action='" & New_Action & "'");

                     else
                        declare
                           Dialog : Gtk_Dialog;
                           Button : Gtk_Widget;
                        begin
                           Dialog := Create_Gtk_Dialog
                             (Msg           =>  Key
                              & (-" (or prefix) is already assigned to: ")
                              & ASCII.LF & ASCII.LF
                              & Old_Prefix & ASCII.LF
                              & (-"Do you want to remove other assignments?"),
                              Dialog_Type   => Warning,
                              Title         =>
                                -"Key shortcuts already exist",
                              Parent        => Get_Main_Window (Ed.Kernel));
                           Button := Dialog.Add_Button
                             (Text        => "Leave",
                              Response_Id => Gtk_Response_Yes);
                           Button.Grab_Default;

                           Button := Dialog.Add_Button
                             (Text        => Stock_Remove,
                              Response_Id => Gtk_Response_OK);

                           Button := Dialog.Add_Button
                             (Text        => Stock_Cancel,
                              Response_Id => Gtk_Response_Cancel);

                           Dialog.Show_All;

                           case Dialog.Run is
                              when Gtk_Response_Yes =>
                                 Binding_Action := Keep_Old_Bindings;
                              when Gtk_Response_OK =>
                                 Binding_Action := Unbind_Old_Actions;
                              when others =>
                                 Binding_Action := Nothing;
                           end case;

                           Dialog.Destroy;
                        end;
                     end if;
                  end if;

                  if Binding_Action = Nothing then
                     Set_Active (Ed.Grab_Button, False);
                     return;
                  end if;

                  Bind_Default_Key_Internal
                    (Kernel           => Ed.Kernel,
                     Table            => Get_Shortcuts (Ed.Kernel).all,
                     Action           => New_Action,
                     Key              => Key,
                     Save_In_Keys_XML => True,
                     Remove_Existing_Actions_For_Shortcut =>
                       (Binding_Action = Unbind_Old_Actions),
                     Remove_Existing_Shortcuts_For_Action => False);
                  Save_Custom_Keys (Ed.Kernel);
                  Refresh_Editor (Ed);
               end if;
            end;
         end if;

         Set_Active (Ed.Grab_Button, False);
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Grab_Key;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Editor : access Gtk_Widget_Record'Class) is
      Self   : constant Keys_Editor_View := Keys_Editor_View (Editor);
      Dialog : GPS_Dialog;
      Label  : Gtk_Label;
      Ent    : Gtk_Entry;
      W      : Gtk_Widget;
      pragma Unreferenced (W);
   begin
      Gtk_New (Dialog,
               Title  => -"Select key theme name",
               Kernel => Self.Kernel,
               Flags  => Modal);

      Gtk_New (Label, -"Enter theme name:");
      Label.Set_Alignment (0.0, 0.5);
      Dialog.Get_Content_Area.Pack_Start (Label, Expand => False);

      Gtk_New (Ent);
      Dialog.Get_Content_Area.Pack_Start (Ent, Expand => False);

      W := Dialog.Add_Button (-"OK", Gtk_Response_OK);
      W := Dialog.Add_Button (-"Cancel", Gtk_Response_Cancel);
      Dialog.Set_Default_Response (Gtk_Response_Cancel);

      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         declare
            Name : constant String := Ent.Get_Text;
         begin
            Save_Keys
              (Self.Kernel, Save_All => True,
               Filename => Create_From_Dir
                 (User_Key_Theme_Directory (Self.Kernel), +Name & ".xml"));

            --  Discard all user-specific shortcuts
            Remove_Shortcuts (Self.Kernel, User_Shortcuts);
            Save_Custom_Keys (Self.Kernel);

            Self.Themes.Insert_Text (0, Text => Name);
            Self.Themes.Set_Active (0);
         end;
      end if;

      Dialog.Destroy;
   end On_Create;

   --------------
   -- On_Reset --
   --------------

   procedure On_Reset (Editor : access Gtk_Widget_Record'Class) is
      Self : constant Keys_Editor_View := Keys_Editor_View (Editor);
   begin
      if Message_Dialog
        (Dialog_Type    => Confirmation,
         Buttons        => Button_Yes or Button_No,
         Default_Button => Button_Yes,
         Title          => -"Reset custom shortcuts",
         Parent         => Gtk_Window (Editor.Get_Toplevel),
         Msg            =>
           -("This operation will remove all the custom shortcuts you have"
             & ASCII.LF
             & "added (set the filter to 'modified' to see them)."
             & ASCII.LF & ASCII.LF
             & "Remove all custom shortcuts?"))
        = Button_Yes
      then
         Remove_Shortcuts (Self.Kernel, User_Shortcuts);
         Save_Custom_Keys (Self.Kernel);
         Refresh_Editor (Self);
      end if;
   end On_Reset;

   -------------------
   -- On_Remove_Key --
   -------------------

   procedure On_Remove_Key (Editor : access Gtk_Widget_Record'Class) is
      Ed         : constant Keys_Editor_View := Keys_Editor_View (Editor);
      Selection  : constant Gtk_Tree_Selection := Get_Selection (Ed.View);
      Sort_Model : Gtk_Tree_Model;
      Iter, Filter_Iter, Sort_Iter  : Gtk_Tree_Iter;

      function Run_Remove_Dialog
        (Keys : Unbounded_String_Array) return Unbounded_String_Array;
      --  Run a dialog asking which key bindings the user wants to remove.
      --  Return the list of the key bindings to remove.

      function Contains
        (List : Unbounded_String_Array;
         S    : Unbounded_String) return Boolean
      is
        (for some Val of List => S = Val);
      --  Return True if List contains S

      -----------------------
      -- Run_Remove_Dialog --
      -----------------------

      function Run_Remove_Dialog
        (Keys : Unbounded_String_Array) return Unbounded_String_Array
      is
         Dialog       : Gtk_Dialog;
         View         : Dialog_View;
         Group_Widget : Dialog_Group_Widget;
         Label        : Gtk_Label;
         Tree         : Gtk_Tree_View;
         Tree_Model   : Gtk_Tree_Store;
         Button       : Gtk_Widget with Unreferenced;
         Response     : Gtk_Response_Type;
         Iter         : Gtk_Tree_Iter;
         Col_Name     : aliased String := "Key Binding";
      begin
         --  Create the dialog

         Gtk_New (Dialog,
                  Title  => -"Remove key bindings",
                  Parent => Get_Main_Window (Ed.Kernel),
                  Flags  => Modal or Destroy_With_Parent);
         Set_Default_Size_From_History
           (Dialog,
            Name   => Dialog.Get_Title,
            Kernel => Ed.Kernel,
            Width  => 300,
            Height => 200);

         --  Create the help label

         View := new Dialog_View_Record;
         Dialog_Utils.Initialize (View);
         Dialog.Get_Content_Area.Pack_Start (View);

         Group_Widget := new Dialog_Group_Widget_Record;
         Initialize (Group_Widget, Parent_View => View);
         Gtk_New
           (Label,
            "Select the key bindings that you want ro remove for this "
            & "action.");
         Apply_Doc_Style (Label);
         Group_Widget.Append_Child (Label, Expand => False);

         --  Create the tree view listing the key bindings

         Group_Widget := new Dialog_Group_Widget_Record;
         Initialize
           (Group_Widget,
            Parent_View         => View,
            Allow_Multi_Columns => False);

         Tree := Create_Tree_View
           (Column_Types     => (0 => GType_String),
            Column_Names     => (1 => Col_Name'Unchecked_Access),
            Sortable_Columns => False);

         Tree.Get_Selection.Set_Mode (Selection_Multiple);
         Tree_Model := -Tree.Get_Model;

         for Key of Keys loop
            Tree_Model.Append (Iter, Null_Iter);
            Tree_Model.Set (Iter, 0, To_String (Key));
         end loop;

         Group_Widget.Append_Child (Tree);

         --  Create the reponse buttons

         Button := Dialog.Add_Button ("Remove", Gtk_Response_OK);
         Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);

         Dialog.Show_All;
         Response := Dialog.Run;

         case Response is
            when Gtk_Response_OK =>
               declare
                  use Gtk_Tree_Path_List;
                  Count  : constant Integer := Integer
                    (Tree.Get_Selection.Count_Selected_Rows);
                  Model  : Gtk_Tree_Model;
                  List   : Gtk_Tree_Path_List.Glist;
                  G_Iter : Gtk_Tree_Path_List.Glist;
                  Path   : Gtk_Tree_Path;
                  Result : Unbounded_String_Array (1 .. Count);
                  J      : Integer := Result'First;
               begin
                  Tree.Get_Selection.Get_Selected_Rows (Model, List);
                  Tree_Model := -Model;

                  if Model /= Null_Gtk_Tree_Model
                    and then List /= Gtk_Tree_Path_List.Null_List
                  then
                     G_Iter := Gtk_Tree_Path_List.First (List);

                     while G_Iter /= Gtk_Tree_Path_List.Null_List loop
                        Path := Gtk_Tree_Path
                          (Gtk_Tree_Path_List.Get_Data (G_Iter));
                        Iter := Get_Iter (Model, Path);
                        Result (J) := To_Unbounded_String
                          (Tree_Model.Get_String (Iter, 0));
                        J := J + 1;
                        G_Iter := Gtk_Tree_Path_List.Next (G_Iter);
                     end loop;

                  end if;

                  Free (List);
                  Dialog.Destroy;

                  return Result;
               end;
            when others =>
               declare
                  Result : constant Unbounded_String_Array (1 .. 0) :=
                             (others => <>);
               begin
                  Dialog.Destroy;

                  return Result;
               end;
         end case;
      end Run_Remove_Dialog;

   begin
      Get_Selected (Selection, Sort_Model, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Sort, Filter_Iter, Sort_Iter);
      Convert_Iter_To_Child_Iter (Ed.Filter, Iter, Filter_Iter);

      --  Only edit for leaf nodes (otherwise these are contexts)

      if Iter /= Null_Iter
        and then Children (Ed.Model, Iter) = Null_Iter
      then
         declare
            Action     : constant String :=
                           Get_String (Ed.Model, Iter, Action_Column);
            Table      : constant HTable_Access :=
                           Get_Shortcuts (Ed.Kernel);
            Keys       : constant Unbounded_String_Array :=
                           Lookup_Keys_From_Action
              (Table       => Table,
               Action      => Action,
               For_Display => False);
            Nb_Bindings : constant Integer := Keys'Length;
         begin
            case Nb_Bindings is
               when 0 =>
                  return;
               when 1 =>
                  Bind_Default_Key_Internal
                    (Table             => Table.all,
                     Kernel            => Ed.Kernel,
                     Action            =>
                        Get_String (Ed.Model, Iter, Action_Column),
                     Key               => "",
                     Save_In_Keys_XML  => True,
                     Remove_Existing_Shortcuts_For_Action => True,
                     Remove_Existing_Actions_For_Shortcut => False);
               when others =>
                  declare
                     Keys_To_Remove : constant Unbounded_String_Array :=
                                        Run_Remove_Dialog (Keys);
                  begin
                     Remove_In_Keymap (Table.all, Action);

                     for Key of Keys loop
                        if not Contains (Keys_To_Remove, Key) then
                           Bind_Default_Key_Internal
                             (Table                                =>
                                Table.all,
                              Kernel                               =>
                                Ed.Kernel,
                              Action                               =>
                                Get_String (Ed.Model, Iter, Action_Column),
                              Key                                  =>
                                To_String (Key),
                              Save_In_Keys_XML                     => True,
                              Remove_Existing_Shortcuts_For_Action => False,
                              Remove_Existing_Actions_For_Shortcut => False);
                        end if;
                     end loop;
                  end;
            end case;

            Save_Custom_Keys (Ed.Kernel);
            Refresh_Editor (Ed);
         end;
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Remove_Key;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access Keys_Editor_Preferences_Page_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K   : constant Kernel_Handle := Self.Editor.Kernel;
      Sep : Gtk_Separator_Menu_Item;
   begin
      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, K, Shortcuts_Only);
      Append_Menu (Menu, K, Categories_Pref);
      Append_Menu (Menu, K, Show_Empty_Cat);
   end Create_Menu;

   ------------------------
   -- On_Grab_For_Filter --
   ------------------------

   procedure On_Grab_For_Filter (View : access GObject_Record'Class) is
      V : constant Keys_Editor_View := Keys_Editor_View (View);
      Key : constant String := Grab_Multiple_Key
        (V, For_Filter => True, For_Display => True);
   begin
      V.Set_Filter (Key);
   end On_Grab_For_Filter;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Keys_Editor_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "keyshortcuts",
         Tooltip     => -"Filter the contents of the shortcuts list",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy
         or Has_Approximate);

      Gtk_New (View.Filter_Grab, Label => -"Grab");
      View.Filter_Grab.Set_Tooltip_Text (-"Grab a key sequence to search for");
      View.Filter_Grab.On_Clicked (On_Grab_For_Filter'Access, View);
      View.Append_Toolbar
        (Toolbar, View.Filter_Grab, Right_Align => True, Homogeneous => False);
   end Create_Toolbar;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Keys_Editor_Record;
      Pattern : in out Search_Pattern_Access)
   is
   begin
      Free (Self.Filter_Pattern);
      Self.Filter_Pattern := Pattern;
      Self.Filter.Refilter;

      if Pattern /= null then
         Self.View.Expand_All;  --  show all results more conveniently
      end if;
   end Filter_Changed;

   -----------------------
   -- On_Load_Key_Theme --
   -----------------------

   procedure On_Load_Key_Theme (Editor : access GObject_Record'Class) is
      Self : constant Keys_Editor_View := Keys_Editor_View (Editor);
   begin
      Remove_Shortcuts (Self.Kernel, Mode => Standard_Shortcuts);
      Load_Key_Theme (Self.Kernel, Self.Themes.Get_Active_Text);
      Fill_Editor (Self);
      Set_Key_Theme (Self.Kernel, Self.Themes.Get_Active_Text);
   end On_Load_Key_Theme;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Cancel_Grab (Keys_Editor_View (Widget));

      --  ??? Should we also reset the handling of key shortcuts ?
   end On_Destroy;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      return Keys_Editor_View (Widget).In_Grab;
   end On_Delete;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Editor : access Keys_Editor_Record'Class) return Gtk_Widget
   is
      Scrolled           : Gtk_Scrolled_Window;
      Hbox               : Gtk_Box;
      Bbox               : Gtk_Button_Box;
      Button             : Gtk_Button;
      Col                : Gtk_Tree_View_Column;
      Render             : Gtk_Cell_Renderer_Text;
      Pixbuf             : Gtk_Cell_Renderer_Pixbuf;
      Frame              : Gtk_Frame;
      Pane               : Gtk_Paned;
      Ignore             : Gint;
      Sep                : Gtk_Separator;
      Selected           : Gint := 0;
      Key_Themes         : constant Key_Theme_Type_List :=
                             List_Key_Themes (Editor.Kernel);
      Key_Theme_Cursor   : Key_Theme_Type_Cursor :=
                             Get_First_Reference (Key_Themes);
      Current_Theme_Name : constant String := Get_Key_Theme (Editor.Kernel);
      Nb_Key_Themes      : Gint := 0;
   begin
      Initialize_Vbox (Editor);
      Editor.Set_Name ("Key shortcuts");  --  for testsuite

      Editor.On_Destroy (On_Destroy'Access);
      Editor.On_Delete_Event (On_Delete'Access);

      --  The model we will modify, wrapped in a filter and sort model

      Gtk_New
        (Editor.Model,
         (Action_Column     => GType_String,
          Key_Column        => GType_String,
          Weight_Column     => GType_Int,
          Icon_Name_Column  => GType_String));

      Gtk_New (Editor.Filter, +Editor.Model);
      Keys_Editor_Visible_Funcs.Set_Visible_Func
        (Editor.Filter, Action_Is_Visible'Access, Editor);

      Gtk_New_With_Model (Editor.Sort, +Editor.Filter);

      --  A hbox: on the left, the list of actions and help, on the left some
      --  buttons to modify key shortcuts

      Gtk_New_Hbox (Hbox);
      Editor.Pack_Start (Hbox, Expand => True, Fill => True);

      Gtk_New_Vpaned (Pane);
      Hbox.Pack_Start (Pane, Expand => True, Fill => True);

      --  List of actions

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Pane.Pack1 (Scrolled, Resize => True, Shrink => True);
      Pane.Set_Position (500);

      Gtk_New (Editor.View, Editor.Sort);
      Editor.View.Set_Name ("Key shortcuts tree"); --  for testsuite
      Scrolled.Add (Editor.View);
      Widget_Callback.Object_Connect
        (Get_Selection (Editor.View), Gtk.Tree_Selection.Signal_Changed,
         Add_Selection_Changed'Access, Editor);

      --  Action buttons

      Gtk_New (Bbox, Orientation_Vertical);
      Bbox.Set_Layout (Buttonbox_Start);
      Bbox.Set_Spacing (5);
      Hbox.Pack_Start (Bbox, Expand => False, Fill => True);

      Gtk_New (Editor.Themes);
      Bbox.Add (Editor.Themes);
      Editor.Themes.Set_Tooltip_Text
        (-("Select an alternate list of shortcuts. User-overridden shortcuts"
           & " are preserved, but all others are reset and reloaded from the"
           & " new theme"));
      while Key_Theme_Cursor /= Null_Key_Theme_Type_Cursor loop
         declare
            Key_Theme_Name : constant String :=
                               Get_Key_Theme (Key_Theme_Cursor).Get_Name;
         begin
            Editor.Themes.Append_Text (Key_Theme_Name);

            if Key_Theme_Name = Current_Theme_Name then
               Selected := Nb_Key_Themes;
            end if;

            Nb_Key_Themes := Nb_Key_Themes + 1;

            Next (Key_Theme_Cursor);
         end;
      end loop;

      Editor.Themes.Set_Active (Selected);

      --  Set the callback after setting the active item.
      Editor.Themes.On_Changed (On_Load_Key_Theme'Access, Editor);

      Gtk_New (Button, -"Reset");
      Button.Set_Tooltip_Text
        (-"Remove all custom key bindings, and revert to the theme's default");
      Bbox.Add (Button);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, On_Reset'Access, Editor);

      Gtk_New (Button, -"Create");
      Button.Set_Tooltip_Text
        (-("Creates a new key theme. This theme includes all the current"
         & " key bindings."
         & ASCII.LF
         & "Once the theme has been created, the manual changes will be part"
         & " of the theme, and thus no longer marked as 'modified'."));
      Bbox.Add (Button);
      Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, On_Create'Access, Editor);

      Gtk_New (Sep, Orientation_Horizontal);
      Bbox.Add (Sep);

      Gtk_New (Editor.Remove_Button, -"Remove");
      Editor.Remove_Button.Set_Tooltip_Text (-"Remove selected key binding");
      Editor.Remove_Button.Set_Sensitive (False);
      Bbox.Add (Editor.Remove_Button);
      Widget_Callback.Object_Connect
        (Editor.Remove_Button,
         Gtk.Button.Signal_Clicked, On_Remove_Key'Access, Editor);

      Gtk_New (Editor.Grab_Button, -"Add");
      Editor.Grab_Button.Set_Tooltip_Text
        (-"Add a new key binding to the selected action");
      Editor.Grab_Button.Set_Sensitive (False);
      Bbox.Add (Editor.Grab_Button);
      Widget_Callback.Object_Connect
        (Editor.Grab_Button,
         Gtk.Toggle_Button.Signal_Toggled, On_Grab_Key'Access, Editor);

      --  Help on selected action

      Gtk_New (Frame);
      Pane.Pack2 (Frame, Resize => True, Shrink => True);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Editor.Help);
      Editor.Help.Set_Alignment (0.0, 0.0);
      Editor.Help.Set_Use_Markup (True);
      Editor.Help.Set_Line_Wrap (True);
      Editor.Help.Set_Justify (Justify_Fill);
      Scrolled.Add (Editor.Help);

      --  The tree

      Gtk_New (Render);
      Set_Property
        (Render, Gtk.Cell_Renderer_Text.Ellipsize_Property, Ellipsize_Middle);

      Gtk_New (Pixbuf);
      Pixbuf.Set_Alignment (Xalign => 0.0, Yalign => 0.5);

      Gtk_New (Col);

      --  We set a minimum width for the 'Actions' column to ensure that most
      --  of the action names are fully visible by default, even if ellipsizing
      --  is enabled for the column's Gtk_Cell_Renrerer_Text.
      Col.Set_Min_Width (Action_Column_Min_Width);

      Ignore := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Action");
      Pack_Start (Col, Pixbuf, False);
      Add_Attribute (Col, Pixbuf, "icon-name", Icon_Name_Column);
      Pack_Start (Col, Render, True);
      Add_Attribute (Col, Render, "text", Action_Column);
      Add_Attribute (Col, Render, "weight", Weight_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Action_Column);

      Clicked (Col);

      Gtk_New (Col);
      Ignore := Append_Column (Editor.View, Col);
      Set_Title (Col, -"Shortcut");
      Pack_Start (Col, Render, False);
      Add_Attribute (Col, Render, "text", Key_Column);
      Add_Attribute (Col, Render, "weight", Weight_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Key_Column);

      Fill_Editor (Editor);

      return Gtk_Widget (Editor);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      Editor    : constant Preferences_Editor :=
                    Kernel.Get_Preferences.Get_Editor;
      Page_View : Keys_Editor_Preferences_Page_View;
      pragma Unreferenced (Self);
   begin

      if Editor /= null then
         Page_View := Keys_Editor_Preferences_Page_View
           (Editor.Get_Page_View (Key_Shortcuts_Page_Name));
      end if;

      if Page_View /= null then
         if Pref = null
           or else Pref = Preference (Shortcuts_Only)
           or else Pref = Preference (Categories_Pref)
           or else Pref = Preference (Show_Empty_Cat)
         then
            Refill_Editor (Page_View.Editor);
         end if;
      end if;
   end Execute;

   -----------------------
   -- Register_Key_Menu --
   -----------------------

   procedure Register_Key_Menu
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Manager          : constant Preferences_Manager :=
                           Kernel.Get_Preferences;
      Keys_Editor_Page : constant Keys_Editor_Preferences_Page :=
                           new Keys_Editor_Preferences_Page_Record;
      Keys_Theme_Page  : constant Keys_Editor_Preferences_Page :=
                           new Key_Themes_Assistant_Page_Record'
                             (Preferences_Page_Record with
                              Kernel => Kernel_Handle (Kernel));
   begin
      Shortcuts_Only := Manager.Create_Invisible_Pref
        ("shortcuts-only", False,
         Label => -"Shortcuts only",
         Doc   => -"Display only actions with a key shortcut.");
      Categories_Pref := Manager.Create_Invisible_Pref
        ("shortcuts-categories", True,
         Label => -"Show categories",
         Doc => -"Group actions by categories.");
      Show_Empty_Cat := Manager.Create_Invisible_Pref
        ("shortcuts-show-empty-cat", False,
         Label => -"Show all categories",
         Doc => -("Show actions with no category."
           & ASCII.LF
           & "These actions are typically internal to GPS, and are generally"
           & " not bound to a key shortcut. However, it might occasionally be"
           & " useful to see them."));

      Register_Action
        (Kernel, "key shortcuts expand all",
         new Expand_All_Command,
         -"Expand or collapse all nodes in the shortcuts editor",
         Icon_Name => "gps-expand-all-symbolic",
         Category => -"Key Shortcuts");

      Keys_Editor_Page.Kernel := Kernel_Handle (Kernel);
      Manager.Register_Page
         (Name             => Key_Shortcuts_Page_Name,
          Page             => Preferences_Page (Keys_Editor_Page),
          Priority         => -1,
          Replace_If_Exist => False);
      Manager.Register_Page
         (Name             => Key_Theme_Assistant_Page_Name,
          Page             => Preferences_Page (Keys_Theme_Page),
          Priority         => -1,
          Page_Type        => Integrated_Page,
          Replace_If_Exist => False);

      Preferences_Changed_Hook.Add (new On_Pref_Changed);
   end Register_Key_Menu;

end KeyManager_Module.GUI;
