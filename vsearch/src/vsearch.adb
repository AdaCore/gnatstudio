------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Templates;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Window;                use Gdk.Window;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Types;                use Glib.Types;

with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Cell_Layout;           use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Clipboard;             use Gtk.Clipboard;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Editable;              use Gtk.Editable;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Selection_Data;        use Gtk.Selection_Data;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Toggle_Tool_Button;    use Gtk.Toggle_Tool_Button;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Window;                use Gtk.Window;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Combo_Box;             use Gtk.Combo_Box;
with Gtk.Combo_Box_Text;        use Gtk.Combo_Box_Text;
with Gtk.Label;                 use Gtk.Label;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;              use Gtkada.Types;

with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GPS.Editors;               use GPS.Editors;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Search;                use GPS.Search;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Dialog_Utils;              use Dialog_Utils;
with Generic_Views;             use Generic_Views;
with GUI_Utils;                 use GUI_Utils;

with Histories;                 use Histories;
with Projects;                  use Projects;
with XML_Utils;                 use XML_Utils;

package body Vsearch is
   Me : constant Trace_Handle := Create ("Vsearch");

   Search_Module_Name : constant String := "Search";

   Mode_Hist_Key           : constant History_Key := "search_mode";
   Pattern_Hist_Key        : constant History_Key := "search_patterns";
   Replace_Hist_Key        : constant History_Key := "search_replace";
   Case_Sensitive_Hist_Key : constant History_Key := "case_sensitive_search";
   Whole_Word_Hist_Key     : constant History_Key := "whole_word_search";
   Regexp_Search_Hist_Key  : constant History_Key := "regexp_search";
   --  The key for the histories.

   Max_Nb_History_Entries  : constant Positive := 5;
   --  Maximum number of entries in history for search/replace patterns

   Pattern_Child_Key : constant String := "pattern_child";
   Replace_Child_Key  : constant String := "replace_child";
   --  Keys used to identify the pattern/replace entry widgets

   Last_Function_In_Module_Key : constant History_Key := "search_function_";
   --  The prefix used to store the name of the last search function used for
   --  each module. The name of the module is appended to form the real key.

   Incremental_Search               : Boolean_Preference;
   Select_On_Match                  : Boolean_Preference;
   Close_On_Match                   : Boolean_Preference;
   Ask_Confirmation_For_Replace_All : Boolean_Preference;
   Keep_Previous_Search_Context     : Boolean_Preference;
   --  The preferences

   H_Padding : constant Guint := 5;
   --  The horizontal padding used between widgets of a same row

   type Search_Regexp is record
      Name           : GNAT.Strings.String_Access;
      Regexp         : GNAT.Strings.String_Access;
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
   end record;
   type Search_Regexps_Array is array (Natural range <>) of Search_Regexp;
   type Search_Regexps_Array_Access is access Search_Regexps_Array;

   type Vsearch_Mode is (Unknown, Find_Only, Find_And_Replace);
   --  Type used to represent the different search view modes.
   --  Here is a short description of each mode:
   --
   --    . Unknown: default mode. Used to know whether we should set the mode
   --      of the search view from the history.
   --
   --    . Find_Only: the search view can only be used to find occurences
   --      (no replacing).
   --
   --    . Find_And_Replace: the search view can also be used to replace
   --      occurences.

   type Vsearch_Record is new View_Record with record
      Mode                    : Vsearch_Mode := Unknown;
      Main_View               : Dialog_View_With_Button_Box;
      Replace_Combo           : Gtk_Combo_Box;
      Context_Combo           : Gtk_Combo_Box_Text;
      Pattern_Combo           : Gtk_Combo_Box;
      Scope_Selector_Box      : Gtk_Box;
      Scope_Optional_Box      : Gtk_Box;
      Scope_Selector_Combo    : Gtk_Combo_Box_Text;
      Scope_Separator_Label   : Gtk_Label;
      Scope_Selector_Optional : Gtk_Widget;
      Case_Toggle             : Gtk_Toggle_Tool_Button;
      Whole_Word_Toggle       : Gtk_Toggle_Tool_Button;
      Regexp_Toggle           : Gtk_Toggle_Tool_Button;
      Search_Next_Button      : Gtk.Button.Gtk_Button;
      Replace_Button          : Gtk.Button.Gtk_Button;
      Replace_Search_Button   : Gtk.Button.Gtk_Button;
      Replace_All_Button      : Gtk.Button.Gtk_Button;
      Search_Previous_Button  : Gtk.Button.Gtk_Button;
      Search_All_Button       : Gtk.Button.Gtk_Button;
      Replace_Only_Button     : Gtk.Button.Gtk_Button;
      Selector                : Scope_Selector;

      Find_Next               : Boolean := False;
      Selection_From          : Gtk_Text_Mark;
      Selection_To            : Gtk_Text_Mark;

      Projects                : Standard.Projects.Project_Type_Array_Access;
      --  Restrict the search to these projects

      Interactive_Context     : Root_Search_Context_Access;
      --  The search context for interactieve search and replace.
      --  It is owned by this widget.
      --  Unused for "Search All" and "Replace All", so that we can run
      --  multiple such commands in parallel, and these commands can outlive
      --  the widget.

      Locked                  : Boolean := False;
      --  Used to lock the search view when the search pattern changes in
      --  incremental mode.

      Search_Has_Failed       : Boolean := False;
      --  Used to know if the last search operation has failed.
      --  This is needed for the 'backspace' feature of the incremental mode.

      Focused : Gtk_Widget;
      --  The widget wich has had focus
   end record;

   overriding procedure Create_Toolbar
     (View    : not null access Vsearch_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Vsearch_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure On_Create
     (Self  : not null access Vsearch_Record;
      Child : not null access GPS_MDI_Child_Record'Class);

   function Initialize
     (Self : access Vsearch_Record'Class) return Gtk_Widget;
   --  Create a new search window and returns the focus widget

   procedure Set_Vsearch_Mode
     (Self : not null access Vsearch_Record'Class;
      Mode : Vsearch_Mode);
   --  Set the mode of the given search mode, hiding or showing the widgets
   --  related with replacing depending on With_Replace.

   function Is_In_Incremental_Mode
     (Self : not null access Vsearch_Record'Class) return Boolean;
   --  Return True if the incremental mode preference is enabled and the
   --  currently selected search module supports it. Return False otherwise.

   package Search_Views is new Generic_Views.Simple_Views
     (Module_Name            => Search_Module_Name,
      View_Name              => -"Search",
      Formal_View_Record     => Vsearch_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Hide_Rather_Than_Close => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Float,
      Group                  => Group_Consoles,
      Commands_Category      => "",  --  no automatic command
      MDI_Flags        => All_Buttons or Float_To_Main or Always_Destroy_Float,
      Areas                  => Sides_Only,
      Default_Width          => -1,
      Default_Height         => -1,
      Add_Close_Button_On_Float => True);
   use Search_Views;
   subtype Vsearch_Access is Search_Views.View_Access;

   procedure Register_Preferences (Kernel : access Kernel_Handle_Record'Class);
   --  Register the preferences associated to the search functions

   package Implements_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, GObject_Record, GObject);
   function "+"
     (Widget : access GObject_Record'Class)
      return Gtk.Editable.Gtk_Editable
      renames Implements_Editable.To_Interface;

   procedure Set_Search_Module
     (Self   : not null access Vsearch_Record'Class;
      Module : Search_Module);
   --  Set the the current search module used by the Search view

   procedure Set_Last_Of_Module
     (Handle : access Kernel_Handle_Record'Class;
      Module : Search_Module);
   --  The Module given in parameter is set as being the last one selected
   --  by the user, and will be the next one shown for the corresponding
   --  module.

   function Get_Search_Module_From_Context
     (Vsearch      : not null access Vsearch_Record'Class;
      Context      : Selection_Context;
      In_Selection : Boolean := False) return Search_Module;
   --  Return the first search module that matches the given context.
   --  If the context does not macth with any of registered search module,
   --  return the currently used search module, if any, or the default one
   --  otherwise.

   procedure Free (Module : Search_Module);
   --  Free the memory associated with Module

   package Search_Modules_List is new Ada.Containers.Doubly_Linked_Lists
     (Search_Module);
   use Search_Modules_List;

   type Vsearch_Module_Record is new Module_ID_Record with record
      Search_Modules        : Search_Modules_List.List;
      --  Global variable that contains the list of all registered search
      --  functions.

      Default_Search_Module : Search_Module;
      --  The default search module to use when no one matches with the current
      --  context.

      Search_Started        : Boolean := False;
      --  Whether the user has started a search (Next and Previous should work)

      Search_Regexps        : Search_Regexps_Array_Access;
      --  The list of predefined regexps for the search module.

      Has_Focus_On_Click    : Boolean := False;
      --  If Patern/Replace combo has focus on mouse click
   end record;
   type Vsearch_Module is access all Vsearch_Module_Record'Class;

   overriding procedure Customize
     (Module : access Vsearch_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   overriding procedure Destroy (Module : in out Vsearch_Module_Record);
   --  See inherited documentation

   Vsearch_Module_Id : Vsearch_Module;
   --  ??? Register in the kernel, shouldn't be a global variable

   Column_Text           : constant Gint := 0;
   Column_Pattern        : constant Gint := 1;
   Column_Case_Sensitive : constant Gint := 2;
   Column_Is_Regexp      : constant Gint := 3;
   Column_Whole_Word     : constant Gint := 4;
   Column_Is_Separator   : constant Gint := 5;

   type Search_Specific_Context is new Interactive_Command with record
      Context     : GNAT.Strings.String_Access;
      Incremental : Boolean := False;
   end record;
   overriding procedure Primitive_Free
     (Action : in out Search_Specific_Context);
   overriding function Execute
     (Action  : access Search_Specific_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  A command that opens the search view and presets the Look In field to
   --  a specific function. If Context is null, then the previous context is
   --  preserve if the preference Keep_Previous_Search_Context is set,
   --  otherwise the context is reset depending on the current module.

   type Replace_Specific_Context is new Search_Specific_Context with
     null record;
   overriding function Execute
     (Action  : access Replace_Specific_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Same as Search_Specific_Context commands but opens the search view in
   --  the replace mode (i.e: showing the replace-related widgets).

   type Abstract_Search_Command is abstract new Interactive_Command with record
      Kernel                 : access Kernel_Handle_Record'Class;
      Search_Backward        : Boolean;

      Context                : Root_Search_Context_Access;
      Context_Is_Owned       : Boolean;
      --  This context is either:
      --  * owned and freed when the command is destroyed, when using
      --    "search all" or "replace all".
      --  * owned by the search widget for other interactive uses.

      Found                  : Boolean := False;
      Select_Editor_On_Match : Boolean;
   end record;
   overriding procedure Primitive_Free (Self : in out Abstract_Search_Command);

   type Search_Command is new Abstract_Search_Command with null record;
   overriding function Execute
     (Self    : access Search_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   overriding function Name
     (Self    : access Search_Command) return String is ("search");

   type Replace_Command is new Abstract_Search_Command with record
      Replace_With           : GNAT.Strings.String_Access;
   end record;
   overriding procedure Primitive_Free (Self : in out Replace_Command);
   overriding function Execute
     (Self    : access Replace_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   overriding function Name
     (Self    : access Replace_Command) return String is ("replace");

   function Create_Replace
     (Vsearch         : not null access Vsearch_Record'Class;
      All_Occurrences : Boolean)
      return access Replace_Command'Class;
   --  Create a new replace command from the settings in the dialog.
   --  Result must be freed by the caller.

   function Substitute_Label
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Label  : String) return String;
   --  Substitute macros in the label of the search contexts.

   function Is_Separator_Row_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
   --  Used to know if the given row should be displayed as a separator in the
   --  search pattern combo box.

   function On_Button_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Remember is an entry has focus at the moment of mouse click.

   function On_Button_Release
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Select the full text of an entry when it is clicked by left mouse
   --  button and doesn't have focus, to help users clear the entry.

   procedure On_Vsearch_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the dialog is destroyed

   procedure Set_Selected_Project
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context);
   --  Set the current project, from the information in Context.
   --  This also refreshes the 'context' combo box.

   procedure Refresh_Context_Combo
     (Kernel : access Kernel_Handle_Record'Class);
   --  Update the context_combo, so that missing contexts are added, and the
   --  label of existing ones is updated to show the next context (macro
   --  substitution,...)

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Search_Regexps_Array, Search_Regexps_Array_Access);

   procedure Set_First_Next_Mode
     (Vsearch   : access Vsearch_Record'Class;
      Find_Next : Boolean);
   --  If Find_Next is False, a new search will be started, otherwise the next
   --  occurence of the current search will be searched.

   type Set_First_Next_Mode_Cb is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : Set_First_Next_Mode_Cb;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Aborts the current search pattern

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project view has changed.
   --  In such a case, we cancel any pending search, since the list of
   --  source files might have changed and thus we need to restart from
   --  scratch.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed.
   --  Used to update the font used by the search/replace entries if necessary.

   function Key_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the pattern field.

   function Key_Press_Replace
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the replacement field.

   function Get_Nth_Search_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Num    : Positive) return Search_Module;
   --  Return the Num-th registered module, or No_Search if there is no such
   --  module.

   function Find_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Label  : String) return Search_Module;
   --  Search the list of registered search functions for a matching module.
   --  No_Search is returned if no such module was found.

   function Get_Or_Create_Vsearch
     (Kernel        : access Kernel_Handle_Record'Class;
      Raise_Widget  : Boolean := False;
      Reset_Entries : Boolean := False;
      Context       : GNAT.Strings.String_Access := null;
      Mode          : Vsearch_Mode := Find_Only)
      return Vsearch_Access;
   --  Return a valid vsearch widget, creating one if necessary.
   --  If Reset_Entries is True, the fields in the dialog are reset depending
   --  on the current module. Context indicates the value that should be
   --  set for "Look In". If null, this will be set based on the current module
   --  The replace-related widgets are shown/hidden depending on Mode.

   function Create_Context
     (Vsearch         : not null access Vsearch_Record'Class;
      All_Occurrences : Boolean)
     return Root_Search_Context_Access;
   --  Create the search context based on the current contents of the GUI.
   --  Result must be freed by the caller.

   procedure Reset_Interactive_Context
     (Vsearch : not null access Vsearch_Record'Class);
   --  Free the interactive search context and reset the state of buttons

   procedure Internal_Search
     (Vsearch         : Vsearch_Access;
      All_Occurrences : Boolean := False;
      Is_Incremental  : Boolean := False;
      Replace         : Boolean := False);
   --  Internal implementation of search capability, used by On_Search and
   --  On_Search_Replace.

   procedure Receive_Text
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Text      : Glib.UTF8_String);
   --  Used to paste the contents of the clibboard in the search pattern entry,
   --  when no selection can be found

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Search (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Find/Next" is clicked.

   procedure On_Replace (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Replace" is clicked.

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Previous" is clicked.

   procedure On_Search_All (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Search all" is clicked.

   procedure On_Replace_Search (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Replace_Find" is clicked.

   procedure On_Replace_All (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Replace_All" is clicked.

   procedure On_Pattern_Combo_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the pattern combo has changed.

   procedure On_Context_Combo_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the entry "Look in" has changed.

   type Can_Fill_With_Current_Word_Filter is new Action_Filter_Record with
     null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Can_Fill_With_Current_Word_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Whether the search view's search entry can be filled with the current
   --  editor's buffer word.
   --  Return False when there is no current editor or when the search view's
   --  search entry does not have the focus.

   type Fill_With_Current_Word_Command is new Interactive_Command with
     null record;
   overriding function Execute
     (Command : access Fill_With_Current_Word_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to fill the search view's search entry with the focused
   --  editor current word.

   type Has_Search_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Search_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  Whether a search is in progress

   type Find_Next_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_Next_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to find the next occurrence of the search pattern

   type Find_Previous_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to find the previous occurrence of the search pattern

   type Find_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to find all the occurrence of the search pattern

   type Replace_Current_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Replace_Current_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to replace the current occurence of the search pattern by
   --  the replace pattern

   type Replace_And_Find_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Replace_And_Find_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback used replace the next occurrence of the search pattern by
   --  the replace pattern and find the next occurrence right after.

   type Replace_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Replace_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Command used to replace all the occurrences of the search pattern by the
   --  replace pattern.

   type New_Predefined_Regexp is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : New_Predefined_Regexp;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when a new predefined regexp has been added to the kernel.

   procedure Selection_Changed (Vsearch : access Gtk_Widget_Record'Class);
   --  Called when the selected pattern has changed, to reflect the settings
   --  for the predefined patterns

   type Search_Functions_Changed is new Simple_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : Search_Functions_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the list of registered search functions has changed.

   procedure Add_To_History_And_Combo
     (Vsearch        : not null access Vsearch_Record'Class;
      Pattern        : String;
      Whole_Word     : Boolean;
      Case_Sensitive : Boolean;
      Regexp         : Boolean);
   --  Add the current settings to the history, and updates the combo box

   procedure On_Float (Search_Child : access Gtk_Widget_Record'Class);
   --  Called when the floating state of the search widget has changed.
   --  Used to disable teh search view scrolled window when floating.

   procedure Add_History_To_Combo
     (Vsearch : not null access Vsearch_Record'Class;
      Value   : String);
   --  Add a history entry to the combo box.

   procedure On_Destroy_Child
     (Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when dialog box is destroing

   procedure On_Destroy_Focused (Widget : access Gtk_Widget_Record'Class);
   --  Called when widget which had focus is destroing

   --------------
   -- On_Float --
   --------------

   procedure On_Float (Search_Child : access Gtk_Widget_Record'Class) is
      Child   : constant MDI_Child := MDI_Child (Search_Child);
      Vsearch : constant Vsearch_Access :=
                  Search_Views.View_From_Child (Child);
   begin
      if Is_Floating (Child) then
         Vsearch.Main_View.Set_Policy (Policy_Never, Policy_Never);
      else
         Vsearch.Main_View.Set_Policy (Policy_Automatic, Policy_Automatic);
      end if;
   end On_Float;

   ----------
   -- Free --
   ----------

   procedure Free (Module : Search_Module)
   is
      Selector : constant Scope_Selector := Module.Get_Scope_Selector;
   begin
      if Selector /= null then
         Unref (Gtk_Widget (Selector.Get_Scope_Combo));

         if Selector.Get_Optional_Widget /= null then
            Unref (Selector.Get_Optional_Widget);
         end if;
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Vsearch_Module_Record) is
   begin
      if Module.Search_Regexps /= null then
         for S in Module.Search_Regexps'Range loop
            Free (Module.Search_Regexps (S).Name);
            Free (Module.Search_Regexps (S).Regexp);
         end loop;

         Unchecked_Free (Module.Search_Regexps);
      end if;

      for M of Module.Search_Modules loop
         Free (M);
      end loop;
   end Destroy;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Search_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Dummy    : Message_Dialog_Buttons;
      Found    : Boolean;
      Continue : Boolean;
   begin
      Self.Context.Search
        (Self.Kernel,
         Search_Backward      => Self.Search_Backward,
         From_Selection_Start => False,
         Give_Focus           => Self.Select_Editor_On_Match,
         Found                => Found,
         Continue             => Continue);

      Self.Found := Self.Found or else Found;

      if Continue then
         Self.Set_Progress
           ((Running,
             Get_Current_Progress (Self.Context),
             Get_Total_Progress (Self.Context)));
         return Execute_Again;
      end if;

      if not Self.Found then
         Dummy := Message_Dialog
           (Msg     => "No occurrences of '" &
            Context_Look_For (Self.Context) & "' found."
            & ASCII.LF & "in "
            & Context_Look_In (Self.Context.all),
            Title   => -"Search",
            Buttons => Button_OK,
            Parent  => Self.Kernel.Get_Main_Window);
      end if;

      return Success;
   end Execute;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (Self : in out Replace_Command) is
   begin
      Free (Self.Replace_With);
      Primitive_Free (Abstract_Search_Command (Self));  -- inherited
   end Primitive_Free;

   overriding procedure Primitive_Free
     (Self : in out Abstract_Search_Command) is
   begin
      if Self.Context_Is_Owned then
         Free (Self.Context);
      end if;
      Primitive_Free (Interactive_Command (Self));  --  inherited
   end Primitive_Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Replace_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
   begin
      if Replace
        (Context         => Self.Context,
         Kernel          => Self.Kernel,
         Replace_String  => Self.Replace_With.all,
         Case_Preserving => True,
         Search_Backward => Self.Search_Backward,
         Give_Focus      => Self.Select_Editor_On_Match)
      then
         Self.Set_Progress
           ((Running,
             Get_Current_Progress (Self.Context),
             Get_Total_Progress (Self.Context)));
         return Execute_Again;
      end if;

      Insert (Self.Kernel, Get_Terminate_Message (Self.Context, Replace));
      return Success;
   end Execute;

   --------------------------
   -- Add_History_To_Combo --
   --------------------------

   procedure Add_History_To_Combo
     (Vsearch : not null access Vsearch_Record'Class;
      Value   : String)
   is
      Model : constant Gtk_List_Store := -Get_Model (Vsearch.Pattern_Combo);
      Iter  : Gtk_Tree_Iter;
   begin
      if Value /= "" then
         if Value (Value'Last) = Character'Val (127)
           and then Value'Length > 4
         then
            Iter := Add_Unique_List_Entry
              (Model, Value (Value'First .. Value'Last - 4),
               Prepend => True,
               Col     => Column_Text);
            Model.Set
              (Iter, Column_Pattern, Value (Value'First .. Value'Last - 4));
            Model.Set
              (Iter, Column_Whole_Word, Value (Value'Last - 3) = '*');
            Model.Set
              (Iter, Column_Is_Regexp, Value (Value'Last - 2) = '*');
            Model.Set
              (Iter, Column_Case_Sensitive, Value (Value'Last - 1) = '*');
         else
            Iter := Add_Unique_List_Entry
              (Model, Value, Prepend => True, Col => Column_Text);
            Model.Set (Iter, Column_Pattern, Value);
            Model.Set (Iter, Column_Case_Sensitive, False);
            Model.Set (Iter, Column_Is_Regexp, False);
            Model.Set (Iter, Column_Whole_Word, False);
         end if;

         Model.Set
           (Iter, Column_Is_Separator, False);
      end if;
   end Add_History_To_Combo;

   ------------------------------
   -- Add_To_History_And_Combo --
   ------------------------------

   procedure Add_To_History_And_Combo
     (Vsearch : not null access Vsearch_Record'Class;
      Pattern : String;
      Whole_Word     : Boolean;
      Case_Sensitive : Boolean;
      Regexp         : Boolean)
   is
      V : constant String :=
        Pattern
        & (if Whole_Word then '*' else ' ')
        & (if Regexp then '*' else ' ')
        & (if Case_Sensitive then '*' else ' ')
        & Character'Val (127);
   begin
      Add_To_History (Get_History (Vsearch.Kernel).all, Pattern_Hist_Key, V);
      Add_History_To_Combo (Vsearch, V);
   end Add_To_History_And_Combo;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Vsearch         : not null access Vsearch_Record'Class;
      All_Occurrences : Boolean)
      return Find_Utils.Root_Search_Context_Access
   is
      use Widget_List;
      Module         : constant Search_Module :=
                         Find_Module
                           (Vsearch.Kernel,
                            Get_Active_Id (Vsearch.Context_Combo));
      History        : constant History_Record :=
                         Get_History (Vsearch.Kernel).all;
      Pattern        : constant String :=
                         Get_Active_Text (Vsearch.Pattern_Combo);
      Replace_Text   : constant String :=
                         Get_Active_Text (Vsearch.Replace_Combo);
      Whole_Word     : constant Boolean :=
                         Get_History (History, Key => Whole_Word_Hist_Key);
      Case_Sensitive : constant Boolean :=
                         Get_History (History, Key => Case_Sensitive_Hist_Key);
      Kind           : constant GPS.Search.Search_Kind :=
                         (if Get_History
                            (History, Key => Regexp_Search_Hist_Key)
                          then
                             Regexp
                          else
                             Full_Text);
      Ctxt           : Root_Search_Context_Access;
   begin
      if Module /= null and then Pattern /= "" then
         Ctxt := Module.Create_Context
           (Vsearch.Kernel,
            All_Occurrences,
            Vsearch.Selector);
      end if;

      if Ctxt = null then
         return null;
      end if;

      Ctxt.Set_Pattern
        (Pattern        => Pattern,
         Case_Sensitive => Case_Sensitive,
         Whole_Word     => Whole_Word,
         Kind           => Kind);
      Reset (Ctxt, Vsearch.Kernel);

      --  Update the contents of the combo boxes
      Add_To_History_And_Combo
        (Vsearch, Pattern,
         Whole_Word     => Whole_Word,
         Case_Sensitive => Case_Sensitive,
         Regexp         => Kind = Regexp);

      Add_Unique_Combo_Entry
        (Vsearch.Replace_Combo, Replace_Text, Prepend => True);
      Add_To_History
        (Get_History (Vsearch.Kernel).all, Replace_Hist_Key, Replace_Text);

      return Ctxt;
   end Create_Context;

   -------------------------------
   -- Reset_Interactive_Context --
   -------------------------------

   procedure Reset_Interactive_Context
     (Vsearch : not null access Vsearch_Record'Class)
   is
   begin
      Free (Vsearch.Interactive_Context);

      --  Disable the replace buttons, since we haven't made sure the current
      --  position matches the pattern
      Vsearch.Replace_Button.Set_Sensitive (False);
      Vsearch.Replace_Search_Button.Set_Sensitive (False);
   end Reset_Interactive_Context;

   ---------------------
   -- Internal_Search --
   ---------------------

   procedure Internal_Search
     (Vsearch         : Vsearch_Access;
      All_Occurrences : Boolean := False;
      Is_Incremental  : Boolean := False;
      Replace         : Boolean := False)
   is
      Module              : constant Search_Module :=
                              Find_Module
                                (Vsearch.Kernel,
                                 Get_Active_Id (Vsearch.Context_Combo));
      Occurrence          : Search_Occurrence;
      Found               : Boolean;
      Has_Next            : Boolean;
      Dummy               : Message_Dialog_Buttons;
      Ctxt                : Root_Search_Context_Access;
      Pattern             : constant String :=
                              Get_Active_Text (Vsearch.Pattern_Combo);
      C                   : access Search_Command;
      Has_Select_On_Match : constant Boolean :=
                              (not Vsearch.Is_In_Incremental_Mode
                               and then Select_On_Match.Get_Pref);
      Search_Category     : constant String :=
                -"Search for: " & Vsearch.Pattern_Combo.Get_Active_Text;

   begin
      if All_Occurrences then
         --  If there is already a search going on for this category, do not
         --  launch a new one.
         if Has_Queue (Vsearch.Kernel, Search_Category) then
            return;
         end if;

         Vsearch.Find_Next := False;
         Ctxt := Create_Context (Vsearch, All_Occurrences);

      else
         --  Reuse the current context if there is one (to continue from where
         --  we were)

         if Vsearch.Interactive_Context = null then
            Reset_Interactive_Context (Vsearch);
            Ctxt := Create_Context (Vsearch, All_Occurrences);
            Vsearch.Interactive_Context := Ctxt;
         else
            Ctxt := Vsearch.Interactive_Context;
         end if;
      end if;

      if Ctxt /= null then
         if All_Occurrences then
            C := new Search_Command'
              (Interactive_Command with
               Select_Editor_On_Match => Has_Select_On_Match,
               Kernel                 => Vsearch.Kernel,
               Search_Backward        => False,
               Context                => Ctxt,
               Context_Is_Owned       => True,  --  command will free context
               Found                  => False);
            Launch_Background_Command
              (Vsearch.Kernel, C, True, True, Search_Category);

         else
            Occurrence := Ctxt.Search
              (Kernel               => Vsearch.Kernel,
               Search_Backward      => False,
               From_Selection_Start => Is_Incremental,
               Give_Focus           => Has_Select_On_Match,
               Found                => Found,
               Continue             => Has_Next);

            --  Push the occurrence in the module's search occurrences stack
            --  if it supports the incremental search mode. Free it otherwise.
            if Occurrence /= null then
               if Vsearch.Is_In_Incremental_Mode then
                  Module.Push_Occurrence (Occurrence);
               else
                  Free (Occurrence);
               end if;

               --  Remove any displayed information since a match has been
               --  found.
               Remove_Information_On_Child
                 (Vsearch.Main_View,
                  Child_Key => Pattern_Child_Key);
            end if;

            Vsearch.Replace_Button.Set_Sensitive
              (Found and then Module.Is_Option_Supported (Supports_Replace));
            Vsearch.Replace_Search_Button.Set_Sensitive
              (Found and then Module.Is_Option_Supported (Supports_Replace));

            --  Give a visual feedback that the search is terminated.
            if not Found
              and then not Has_Next
              and then not Get_End_Notif_Done (Ctxt.all)
            then
               Stop_Macro_Action_Hook.Run (Vsearch.Kernel);
               Display_Information_On_Child
                 (Vsearch.Main_View,
                  Child_Key => Pattern_Child_Key,
                  Message   => "No occurrences of '" & Pattern & "' found in "
                  & Context_Look_In (Ctxt.all),
                  Is_Error  => True);
               Ctxt.Set_End_Notif_Done (True);
               Vsearch.Search_Has_Failed := True;
            end if;

            --  We keep the "Next" mode until a new context is created by
            --  the user, so that even after reaching the end of the search,
            --  we can restart from the beginning simply by pressing Ctrl-N
            Set_First_Next_Mode (Vsearch, Find_Next => True);
         end if;
      end if;

      --  If the dialog is not docked and the option to auto-close the dialog
      --  is set, close the dialog.

      if not Replace
        and then Vsearch.Get_Realized
        and then Is_Floating (Search_Views.Child_From_View (Vsearch))
        and then Close_On_Match.Get_Pref
      then
         Search_Views.Close (Vsearch.Kernel);
      end if;
   end Internal_Search;

   ---------------
   -- On_Search --
   ---------------

   procedure On_Search (Object : access Gtk_Widget_Record'Class) is
   begin
      Internal_Search (Vsearch_Access (Object));
   end On_Search;

   ------------------------
   -- On_Search_Previous --
   ------------------------

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class) is
      Vsearch        : constant Vsearch_Access := Vsearch_Access (Object);
      All_Occurences : constant Boolean := False;
      Found          : Boolean;
      Has_Next       : Boolean;
      Ctxt           : Root_Search_Context_Access;

   begin
      Reset_Interactive_Context (Vsearch);
      Ctxt := Create_Context (Vsearch, All_Occurences);

      if Ctxt /= null then
         Vsearch.Interactive_Context := Ctxt;  --  owned by vsearch

         Search
           (Ctxt,
            Vsearch.Kernel,
            Search_Backward      => True,
            From_Selection_Start => False,
            Give_Focus           => (not Vsearch.Is_In_Incremental_Mode
                                     and then Select_On_Match.Get_Pref),
            Found                => Found,
            Continue             => Has_Next);

         if not Found then
            Stop_Macro_Action_Hook.Run (Vsearch.Kernel);
         end if;

         Vsearch.Replace_Search_Button.Set_Sensitive (Found);
         Vsearch.Replace_Button.Set_Sensitive (Found);
         Set_First_Next_Mode (Vsearch, Find_Next => True);
      end if;
   end On_Search_Previous;

   -------------------
   -- On_Search_All --
   -------------------

   procedure On_Search_All (Object : access Gtk_Widget_Record'Class) is
   begin
      Internal_Search (Vsearch_Access (Object), True);
   end On_Search_All;

   ----------------
   -- On_Replace --
   ----------------

   procedure On_Replace (Object : access Gtk_Widget_Record'Class) is
      C        : access Replace_Command;
      Result   : Command_Return_Type with Unreferenced;
   begin
      C := Create_Replace (Vsearch_Access (Object), All_Occurrences => False);
      if C /= null then
         Result := C.Execute;
         Unref (Command_Access (C));
      end if;
   end On_Replace;

   -----------------------
   -- On_Replace_Search --
   -----------------------

   procedure On_Replace_Search (Object : access Gtk_Widget_Record'Class) is
   begin
      On_Replace (Object);
      On_Search (Object);
   end On_Replace_Search;

   --------------------
   -- On_Replace_All --
   --------------------

   procedure On_Replace_All (Object : access Gtk_Widget_Record'Class) is
      Vsearch     : constant Vsearch_Access := Vsearch_Access (Object);
      Has_Next    : Boolean;
      pragma Unreferenced (Has_Next);

      Dialog : constant Gtk_Dialog := Create_Gtk_Dialog
        (Msg      => (-"You are about to replace all occurrences of """)
         & Get_Active_Text (Vsearch.Pattern_Combo) & """."
         & ASCII.LF & (-"Continue?"),
         Dialog_Type => Warning,
         Title       => -"Replacing all occurrences",
         Parent      => Gtk_Window (Get_Toplevel (Vsearch)));

      Dummy       : Gtk_Widget;
      Do_Not_Ask  : Gtk_Check_Button;
      Box         : Gtk_Hbox;
      Response    : Gtk_Response_Type;

   begin
      if Ask_Confirmation_For_Replace_All.Get_Pref then
         Gtk_New (Do_Not_Ask, -"Do not ask this question again");
         Gtk_New_Hbox (Box);
         Pack_Start (Get_Content_Area (Dialog), Box, True, True, 3);
         Pack_Start (Box, Do_Not_Ask, True, False, 3);

         Dummy := Add_Button
           (Dialog,
            Text => "Yes",
            Response_Id => Gtk_Response_Yes);

         Dummy := Add_Button
           (Dialog,
            Text => "No",
            Response_Id => Gtk_Response_No);

         Show_All (Dialog);
         Response := Run (Dialog);

         if Get_Active (Do_Not_Ask) then
            Set_Pref
              (Ask_Confirmation_For_Replace_All, Vsearch.Kernel, False);
         end if;

         Destroy (Dialog);

         if Response /= Gtk_Response_Yes then
            return;
         end if;
      end if;

      Launch_Background_Command
        (Vsearch.Kernel, Create_Replace (Vsearch, All_Occurrences => True),
         True, True, -"Search and replace");
   end On_Replace_All;

   --------------------
   -- Create_Replace --
   --------------------

   function Create_Replace
     (Vsearch         : not null access Vsearch_Record'Class;
      All_Occurrences : Boolean)
     return access Replace_Command'Class
   is
      Ctxt : Root_Search_Context_Access;
   begin
      if All_Occurrences then
         --  Create a new context
         Ctxt := Create_Context (Vsearch, All_Occurrences => All_Occurrences);

      else
         --  Reuse the context we already have, since we need to know the
         --  location of the last search

         Ctxt := Vsearch.Interactive_Context;
         Vsearch.Replace_Button.Set_Sensitive (False);
         Vsearch.Replace_Search_Button.Set_Sensitive (False);
      end if;

      if Ctxt = null then
         return null;
      end if;

      return new Replace_Command'
        (Interactive_Command with
         Search_Backward        => False,
         Context                => Ctxt,
         Context_Is_Owned       => All_Occurrences,  --  command will free ctxt
         Kernel                 => Vsearch.Kernel,
         Select_Editor_On_Match => (not Vsearch.Is_In_Incremental_Mode
                                    and then Select_On_Match.Get_Pref),
         Found                  => False,
         Replace_With           =>
            new String'(Get_Active_Text (Vsearch.Replace_Combo)));
   end Create_Replace;

   ------------------------------
   -- On_Pattern_Combo_Changed --
   ------------------------------

   procedure On_Pattern_Combo_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Object);
   begin
      if not Vsearch.Locked and then Vsearch.Is_In_Incremental_Mode then
         Reset_Interactive_Context (Vsearch);
         Internal_Search
           (Vsearch,
            All_Occurrences => False,
            Is_Incremental  => True,
            Replace         => False);
      end if;
   end On_Pattern_Combo_Changed;

   ------------------------------
   -- On_Context_Combo_Changed --
   ------------------------------

   procedure On_Context_Combo_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      Vsearch             : constant Vsearch_Access := Vsearch_Access (Object);
      Module              : constant Search_Module :=
                              Find_Module
                                (Vsearch.Kernel,
                                 Get_Active_Id (Vsearch.Context_Combo));
      Selector            : constant Scope_Selector :=
                              (if Module /= null then
                                  Module.Get_Scope_Selector
                               else
                                  null);
      Has_Replace         : Boolean;
      Has_Case_Sensitive  : Boolean;
      Has_All_Occurrences : Boolean;
      Has_Whole_Word      : Boolean;
      Has_Backward        : Boolean;
      Child               : MDI_Child;
   begin
      if Module /= null then
         Set_Last_Of_Module (Vsearch.Kernel, Module);

         --  Set the widgets' sensitivity/sate according to the options
         --  supported by the newly selected search module.
         Has_Replace := Module.Is_Option_Supported (Supports_Replace);
         Has_Case_Sensitive := Module.Is_Option_Supported (Case_Sensitive);
         Has_All_Occurrences := Module.Is_Option_Supported (All_Occurrences);
         Has_Whole_Word := Module.Is_Option_Supported (Whole_Word);
         Has_Backward := Module.Is_Option_Supported (Search_Backward);

         Vsearch.Replace_Combo.Set_Sensitive (Has_Replace);
         Vsearch.Replace_All_Button.Set_Sensitive
           (Has_Replace and Has_All_Occurrences);
         Vsearch.Search_All_Button.Set_Sensitive (Has_All_Occurrences);
         Vsearch.Case_Toggle.Set_Sensitive (Has_Case_Sensitive);
         Vsearch.Whole_Word_Toggle.Set_Sensitive (Has_Whole_Word);
         Vsearch.Search_Previous_Button.Set_Sensitive (Has_Backward);

         Vsearch.Case_Toggle.Set_Active
           (Vsearch.Case_Toggle.Get_Active and then Has_Case_Sensitive);
         Vsearch.Whole_Word_Toggle.Set_Active
           (Vsearch.Whole_Word_Toggle.Get_Active and then Has_Whole_Word);

         --  We remove the scope selector widgets, but there is still one
         --  reference hold by the module, so they don't get destroyed.
         if Vsearch.Scope_Selector_Combo /= null then
            Vsearch.Scope_Selector_Box.Remove
              (Vsearch.Scope_Separator_Label);
            Vsearch.Scope_Selector_Box.Remove
              (Vsearch.Scope_Selector_Combo);
         end if;

         if Vsearch.Scope_Selector_Optional /= null then
            Vsearch.Scope_Optional_Box.Remove
              (Vsearch.Scope_Selector_Optional);
         end if;

         Vsearch.Selector := null;
         Vsearch.Scope_Selector_Combo := null;
         Vsearch.Scope_Selector_Optional := null;

         if Selector /= null then
            Vsearch.Selector := Selector;
            Vsearch.Scope_Selector_Combo := Selector.Get_Scope_Combo;
            Vsearch.Scope_Selector_Optional := Selector.Get_Optional_Widget;

            Gtk_New (Vsearch.Scope_Separator_Label, "In");
            Vsearch.Scope_Selector_Box.Pack_End
              (Vsearch.Scope_Separator_Label,
               Expand  => False,
               Padding => H_Padding);

            Vsearch.Scope_Selector_Box.Pack_End
              (Vsearch.Scope_Selector_Combo,
               Expand => True,
               Fill   => True);

            Vsearch.Scope_Selector_Box.Show_All;

            if Vsearch.Scope_Selector_Optional /= null then
               Vsearch.Scope_Optional_Box.Pack_Start
                 (Vsearch.Scope_Selector_Optional,
                  Expand => True,
                  Fill   => True);
               Vsearch.Scope_Optional_Box.Show_All;
            end if;
         end if;

         Child := Search_Views.Child_From_View (Vsearch);
         if Child /= null and then Is_Floating (Child) then
            --  Reset if any size was set previously
            Vsearch.Set_Size_Request (-1, -1);
            Vsearch.Queue_Resize;
         end if;

         --  Clear the module's search occurrences stack
         Module.Clear_Occurrences;
      end if;
   end On_Context_Combo_Changed;

   -------------------------
   -- Set_First_Next_Mode --
   -------------------------

   procedure Set_First_Next_Mode
     (Vsearch   : access Vsearch_Record'Class;
      Find_Next : Boolean)
   is
      Label : Gtk_Label;
      Align : Gtk_Alignment;
      Req   : Gtk_Requisition;

   begin
      Vsearch.Find_Next := Find_Next;

      if Find_Next then
         --  Make sure the Search_Next_Button only grows in size. This makes
         --  sure that a user won't click on "Find" and see the button shrink
         --  so that next time he clicks (without moving the mouse) is on
         --  "Replace".

         Size_Request (Vsearch.Search_Next_Button, Req);
         Set_Size_Request (Vsearch.Search_Next_Button, Req.Width, Req.Height);

         Remove (Vsearch.Search_Next_Button,
                 Get_Child (Vsearch.Search_Next_Button));

         Gtk_New_With_Mnemonic (Label, -"_Next");
         Gtk_New (Align, 0.5, 0.5, 0.0, 0.0);
         Add (Vsearch.Search_Next_Button, Align);
         Add (Align, Label);
         Show_All (Align);

         Vsearch_Module_Id.Search_Started := True;

         --  Always activate the "Next" button, so that we can still do
         --  selective replace. Otherwise, the button is greyed out when we
         --  have a replace string, and would stay as such.
         Set_Sensitive (Vsearch.Search_Next_Button, True);

      else
         --  Remove size constraints on Search_Next_Button.
         Set_Size_Request (Vsearch.Search_Next_Button, -1, -1);
         Remove (Vsearch.Search_Next_Button,
                 Get_Child (Vsearch.Search_Next_Button));

         Gtk_New_With_Mnemonic (Label, -"_Find");
         Gtk_New (Align, 0.5, 0.5, 0.0, 0.0);
         Add (Vsearch.Search_Next_Button, Align);
         Add (Align, Label);
         Show_All (Align);

         Vsearch_Module_Id.Search_Started := False;
      end if;
   end Set_First_Next_Mode;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : Set_First_Next_Mode_Cb;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      View : constant Vsearch_Access := Search_Views.Retrieve_View (Kernel);
   begin
      --  We might be in the process of destroying GPS (for instance, the
      --  current search context detects that the current MDI_Child was
      --  destroyed, and resets the context).

      if View /= null then
         Set_First_Next_Mode (View, Find_Next => False);

         --  Do not reset the interactive context when we are in incremental
         --  mode and if the currently selected search module supports it
         --  since the interactive search context is systematically reset in
         --  this case (see On_Patter_Combo_Changed).

         if not View.Is_In_Incremental_Mode then
            Reset_Interactive_Context (View);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Reset_Search (null, Kernel_Handle (Kernel));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      Vsearch : constant Vsearch_Access := Search_Views.Retrieve_View (Kernel);
   begin
      if Vsearch /= null then
         Set_Font_And_Colors
           (Widget     => Vsearch.Pattern_Combo,
            Fixed_Font => True,
            Pref       => Pref);
         Set_Font_And_Colors
           (Widget     => Vsearch.Replace_Combo,
            Fixed_Font => True,
            Pref       => Pref);
      end if;
   end Execute;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Vsearch             : constant Vsearch_Access := Vsearch_Access (Widget);
      Module              : constant Search_Module :=
                              Find_Module
                                (Vsearch.Kernel,
                                 Get_Active_Id (Vsearch.Context_Combo));
      In_Incremental_Mode : constant Boolean := Vsearch.Is_In_Incremental_Mode;
      Key                 : constant Gdk_Key_Type := Get_Key_Val (Event);
   begin
      if Key = GDK_Return or else Key = GDK_KP_Enter then
         if Is_Sensitive (Vsearch.Search_Next_Button) then

            --  Don't give the focus to the search/next button when we are in
            --  the incremental mode so that the user can still modify the
            --  entry after pressing the Enter/Return key.
            if not In_Incremental_Mode then
               Grab_Focus (Vsearch.Search_Next_Button);
            end if;

            On_Search (Vsearch);
         else
            Grab_Focus (Vsearch.Replace_Search_Button);
            On_Replace (Vsearch);
         end if;

         return True;
      elsif In_Incremental_Mode and then Key = GDK_BackSpace then
         declare
            Occurrence    : Search_Occurrence;
            Start_Pos     : Gint;
            End_Pos       : Gint;
            Has_Selection : Boolean;
         begin
            Get_Selection_Bounds (Gtk_Entry (Vsearch.Pattern_Combo.Get_Child),
                                  Start_Pos     => Start_Pos,
                                  End_Pos       => End_Pos,
                                  Has_Selection => Has_Selection);

            --  If some text is selected in the search pattern entry, return
            --  False directly so that the text gets deleted.
            if Has_Selection then
               return False;
            end if;

            --  Pop the last saved occurence and use the one just after, so
            --  that we don't go to the same occurrence every time.
            if not Vsearch.Search_Has_Failed then
               Occurrence := Module.Pop_Occurrence;
               Free (Occurrence);
            else
               Vsearch.Search_Has_Failed := False;
            end if;

            Occurrence := Module.Get_Last_Occurrence;

            if Occurrence /= null then

               --  Clear the module's occurrences stack if the last
               --  occurrence's pattern contains only one character. This
               --  avoids going back to previous searches.
               if Occurrence.Get_Pattern'Length = 1 then
                  Module.Clear_Occurrences;
                  return False;
               end if;

               Vsearch.Locked := True;

               --  Remove any displayed information since and set the
               --  sensitivity of the 'Replace' and 'Replace and Find'
               --  buttons since a match has been found.
               Remove_Information_On_Child
                 (Vsearch.Main_View,
                  Child_Key => Pattern_Child_Key);
               Vsearch.Replace_Button.Set_Sensitive (True);
               Vsearch.Replace_Search_Button.Set_Sensitive (True);

               Set_Active_Text (Vsearch.Pattern_Combo, Occurrence.Get_Pattern);
               Gtk_Entry
                 (Vsearch.Pattern_Combo.Get_Child).Select_Region (0, -1);
               Module.Highlight_Occurrence (Occurrence);

               Vsearch.Locked := False;

               return True;
            end if;
         end;
      elsif Key = GDK_Escape then
         declare
            Occurrence : constant Search_Occurrence :=
                           Module.Get_Last_Occurrence;
         begin
            if Occurrence /= null then
               Module.Give_Focus_To_Occurrence (Occurrence);

               return True;
            end if;
         end;
      end if;

      return False;
   end Key_Press;

   -----------------------
   -- Key_Press_Replace --
   -----------------------

   function Key_Press_Replace
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Widget);
   begin
      if Get_Key_Val (Event) = GDK_Return
        or else Get_Key_Val (Event) = GDK_KP_Enter
      then
         Grab_Focus (Vsearch.Replace_Search_Button);
         On_Replace_Search (Vsearch);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Key_Press_Replace;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
     (Vsearch : access Gtk_Widget_Record'Class)
   is
      use Widget_List;
      Search : constant Vsearch_Access := Vsearch_Access (Vsearch);
      Iter           : Gtk_Tree_Iter;
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
      Whole_Word     : Boolean;

   begin
      if Get_Active_Iter (Search.Pattern_Combo) /= Null_Iter then
         Iter := Get_Active_Iter (Search.Pattern_Combo);
         Case_Sensitive := Get_Boolean
           (Search.Pattern_Combo.Get_Model, Iter, Column_Case_Sensitive);
         Is_Regexp := Get_Boolean
           (Search.Pattern_Combo.Get_Model, Iter, Column_Is_Regexp);
         Whole_Word := Get_Boolean
           (Search.Pattern_Combo.Get_Model, Iter, Column_Whole_Word);

         Set_Active (Search.Case_Toggle, Case_Sensitive);
         Set_Active (Search.Regexp_Toggle, Is_Regexp);
         Set_Active (Search.Whole_Word_Toggle, Whole_Word);
      end if;

   exception
      when Gtkada.Types.Data_Error =>
         null;
   end Selection_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : New_Predefined_Regexp;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Vsearch    : constant Vsearch_Access :=
                     Search_Views.Retrieve_View (Kernel);
      Model      : constant Gtk_List_Store :=
                     -Get_Model (Vsearch.Pattern_Combo);
      Item       : Gtk_Tree_Iter;
      Casing     : constant Boolean := Get_Active (Vsearch.Case_Toggle);
      Whole_Word : constant Boolean := Get_Active (Vsearch.Whole_Word_Toggle);
      Regexp     : constant Boolean := Get_Active (Vsearch.Regexp_Toggle);

   begin
      --  Add a separator row to distinguish prefefined regexps from the
      --  patterns typed by the user.

      Item := Add_Unique_List_Entry (Model, Text => "", Col => Column_Text);
      Model.Set (Item, Column_Is_Separator, True);

      --  Add an entry for each registered prefedined regexp

      for R of Vsearch_Module_Id.Search_Regexps.all loop
         Item := Add_Unique_Combo_Entry (Vsearch.Pattern_Combo, R.Name.all);
         Model.Set (Item, Column_Pattern,        R.Regexp.all);
         Model.Set (Item, Column_Case_Sensitive, R.Case_Sensitive);
         Model.Set (Item, Column_Is_Regexp,      R.Is_Regexp);
         Model.Set (Item, Column_Whole_Word,     False);
         Model.Set (Item, Column_Is_Separator,   False);
      end loop;

      --  Restore the options as before (they might have changed depending
      --  on the last predefined regexp we inserted)

      Set_Active (Vsearch.Case_Toggle, Casing);
      Set_Active (Vsearch.Whole_Word_Toggle, Whole_Word);
      Set_Active (Vsearch.Regexp_Toggle, Regexp);
   end Execute;

   ---------------------------
   -- Refresh_Context_Combo --
   ---------------------------

   procedure Refresh_Context_Combo
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Vsearch : constant Vsearch_Access := Search_Views.Retrieve_View (Kernel);
      Num     : Positive := 1;
      Store   : constant Gtk_List_Store :=
                  Gtk_List_Store'(-Vsearch.Context_Combo.Get_Model);
      Id_Col  : constant Gint := Vsearch.Context_Combo.Get_Id_Column;
      Iter    : Gtk_Tree_Iter;
      Found   : Boolean;
   begin
      loop
         declare
            Module : constant Search_Module :=
                       Get_Nth_Search_Module (Kernel, Num);
         begin
            exit when Module = null;

            Found := False;
            Iter := Store.Get_Iter_First;
            while Iter /= Null_Iter loop
               --  The id is the un-substituted label
               if Get_String (Store, Iter, Id_Col) = Module.Get_Label then
                  --  Update the text, after substituting macros.
                  Store.Set
                    (Iter, 0, Substitute_Label (Kernel, Module.Get_Label));
                  Found := True;
                  exit;
               end if;
               Store.Next (Iter);
            end loop;

            if not Found then
               Vsearch.Context_Combo.Append
                 (Id   => Module.Get_Label,
                  Text => Substitute_Label (Kernel, Module.Get_Label));
            end if;

            Num := Num + 1;
         end;
      end loop;

      --  If no active search module is selected, select the default one
      if Vsearch.Context_Combo.Get_Active = -1 then
         Vsearch.Set_Search_Module (Vsearch_Module_Id.Default_Search_Module);
      end if;
   end Refresh_Context_Combo;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : Search_Functions_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Refresh_Context_Combo (Kernel);
   end Execute;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      E : constant Gtk_Entry := Gtk_Entry (Self);
   begin
      if not Vsearch_Module_Id.Has_Focus_On_Click and Event.Button = 1 then
         E.Select_Region (0, -1);
      end if;

      return False;
   end On_Button_Release;

   ---------------------------
   -- Is_Separator_Row_Func --
   ---------------------------

   function Is_Separator_Row_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      (Get_Boolean (Model, Iter, Column_Is_Separator));

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Event);
      E : constant Gtk_Entry := Gtk_Entry (Self);
   begin
      Vsearch_Module_Id.Has_Focus_On_Click := E.Is_Focus;
      return False;
   end On_Button_Press;

   ------------------------
   -- On_Vsearch_Destroy --
   ------------------------

   procedure On_Vsearch_Destroy (Self : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Self);
   begin
      --  The widgets in Context_Specific have a longer lifecycle than the
      --  dialog itself: make sure here that they are not destroyed when the
      --  dialog is destroyed.
      if Vsearch.Scope_Selector_Combo /= null then
         Vsearch.Scope_Selector_Box.Remove
           (Vsearch.Scope_Selector_Combo);
      end if;

      if Vsearch.Scope_Selector_Optional /= null then
         Vsearch.Scope_Optional_Box.Remove
           (Vsearch.Scope_Selector_Optional);
      end if;

      Reset_Interactive_Context (Vsearch);
   end On_Vsearch_Destroy;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Vsearch_Record'Class) return Gtk_Widget
   is

      procedure Disable_Button_Focus (Combo_Box : Gtk_Combo_Box);
      --  Disable focus on internal button in given Combo_Box

      procedure Initialize_From_History;
      --  Create all the history keys needed by the search view and try to get
      --  content from it to initialize the search view (e.g: get the last
      --  saved search patterns).

      --------------------------
      -- Disable_Button_Focus --
      --------------------------

      procedure Disable_Button_Focus (Combo_Box : Gtk_Combo_Box) is
         Focus_Chain : Gtk.Widget.Widget_List.Glist;
      begin
         --  Create single element list that contains only text entry
         Gtk.Widget.Widget_List.Append (Focus_Chain, Combo_Box.Get_Child);
         --  Set it as focus chain
         Combo_Box.Set_Focus_Chain (Focus_Chain);
      end Disable_Button_Focus;

      -----------------------------
      -- Initialize_From_History --
      -----------------------------

      procedure Initialize_From_History is
         History   : History_Record := Get_History (Self.Kernel).all;
         Last_Mode : constant String_List_Access :=
                       Get_History (History, Key => Mode_Hist_Key);
         Patterns  : constant String_List_Access :=
                       Get_History (History, Pattern_Hist_Key);
      begin
         --  Create a new key in the history to save the replace patterms and
         --  fill the replace combo first, so that the selection remains in the
         --  pattern combo.

         Create_New_Key_If_Necessary
           (History,
            Key      => Replace_Hist_Key,
            Key_Type => Strings);
         Set_Max_Length
           (History,
            Num => Max_Nb_History_Entries,
            Key => Replace_Hist_Key);

         Get_History
           (History,
            Key         => Replace_Hist_Key,
            Combo       => Self.Replace_Combo,
            Clear_Combo => False,
            Prepend     => True);

         --  Create a new key in the history to save the typed search patterns
         --  and fill the search pattern combo if some values are found in
         --  history.

         Create_New_Key_If_Necessary
           (History,
            Key      => Pattern_Hist_Key,
            Key_Type => Strings);
         Set_Max_Length
           (History,
            Num => Max_Nb_History_Entries,
            Key => Pattern_Hist_Key);

         if Patterns /= null then
            for I in reverse Patterns'Range loop
               Add_History_To_Combo (Self, Patterns (I).all);
            end loop;

            Self.Pattern_Combo.Set_Active (0);
            Gtk_Entry (Self.Pattern_Combo.Get_Child).Select_Region (0, -1);
         else
            Set_Active_Text (Self.Pattern_Combo, "");
         end if;

         --  Create a key in the history to save the last used vsearch mode and
         --  retrieve it if it already exists.

         Create_New_Key_If_Necessary
           (History,
            Key       => Mode_Hist_Key,
            Key_Type  => Strings);
         Set_Max_Length
           (History,
            Num => 1,
            Key => Mode_Hist_Key);

         if Last_Mode /= null and then Self.Mode = Unknown then
            Self.Set_Vsearch_Mode
              (Mode => Vsearch_Mode'Value (Last_Mode (Last_Mode'Last).all));
         end if;
      end Initialize_From_History;

      Group_Widget : Dialog_Group_Widget;
      Replace_Row  : Gtk_Widget;
      Layout       : Gtk_Cell_Layout;
      Renderer     : Gtk_Cell_Renderer_Text;
      Model        : Gtk_List_Store;
   begin
      Gtk.Box.Initialize_Vbox (Self);

      Self.Main_View := new Dialog_View_With_Button_Box_Record;
      Dialog_Utils.Initialize
        (Self.Main_View,
         Position => Pos_Right);
      Self.Pack_Start (Self.Main_View, Expand => True, Fill => True);

      --  Find/Replace combo boxes

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View         => Self.Main_View,
         Allow_Multi_Columns => False);

      Gtk.List_Store.Gtk_New
        (Model,
         (Guint (Column_Text)           => GType_String,
          Guint (Column_Pattern)        => GType_String,
          Guint (Column_Case_Sensitive) => GType_Boolean,
          Guint (Column_Is_Regexp)      => GType_Boolean,
          Guint (Column_Whole_Word)     => GType_Boolean,
          Guint (Column_Is_Separator)   => GType_Boolean));
      Gtk_New_With_Model_And_Entry (Self.Pattern_Combo, +Model);
      Set_Font_And_Colors (Self.Pattern_Combo.Get_Child, Fixed_Font => True);
      Self.Pattern_Combo.Set_Entry_Text_Column (Column_Pattern);
      Layout := +Self.Pattern_Combo;

      Widget_Callback.Object_Connect
        (Self.Pattern_Combo, Gtk.Combo_Box.Signal_Changed,
         On_Pattern_Combo_Changed'Access, Self);
      Self.Pattern_Combo.Set_Row_Separator_Func (Is_Separator_Row_Func'Access);
      Self.Pattern_Combo.Get_Child.On_Button_Press_Event
        (On_Button_Press'Access, After => False);
      Self.Pattern_Combo.Get_Child.On_Button_Release_Event
        (On_Button_Release'Access, After => False);

      Disable_Button_Focus (Self.Pattern_Combo);

      Gtk_New (Renderer);
      Gtk.Cell_Layout.Clear (Layout);
      Gtk.Cell_Layout.Pack_Start (Layout, Renderer, True);
      Gtk.Cell_Layout.Add_Attribute (Layout, Renderer, "text", Column_Text);
      Set_Tooltip_Text (Self.Pattern_Combo,
                        -"The searched word or pattern");

      Group_Widget.Create_Child
        (Self.Pattern_Combo,
         Label     => "Find",
         Child_Key => Pattern_Child_Key,
         Expand    => True,
         Fill      => True);

      Gtk.List_Store.Gtk_New (Model, (0 .. 0 => GType_String));
      Gtk_New_With_Model_And_Entry (Self.Replace_Combo, +Model);
      Set_Font_And_Colors (Self.Replace_Combo.Get_Child, Fixed_Font => True);
      Self.Replace_Combo.Set_Entry_Text_Column (0);
      Self.Replace_Combo.Set_Tooltip_Text
        (-("The text that will replace each match. Next special patterns are" &
           " recognized in regexp mode:" & ASCII.LF &
           " * \0 - refers to the complete matched string" & ASCII.LF &
           " * \1..\9 - refer to the corresponding matching subexpression" &
           ASCII.LF &
           " * \i or \i(start,step) - refers to the sequentially " &
           "increasing number"));

      Disable_Button_Focus (Self.Replace_Combo);

      Self.Replace_Combo.Get_Child.On_Button_Press_Event
        (On_Button_Press'Access, After => False);
      Self.Replace_Combo.Get_Child.On_Button_Release_Event
        (On_Button_Release'Access, After => False);

      Replace_Row := Group_Widget.Create_Child
        (Self.Replace_Combo,
         Label     => "Replace",
         Child_Key => Replace_Child_Key);
      Replace_Row.Set_No_Show_All (True);

      --  Context specific search

      Gtk_New_Hbox (Self.Scope_Selector_Box, Homogeneous => False);

      Gtk_New (Self.Context_Combo);
      Self.Context_Combo.Set_Tooltip_Text (-"The context of the search");
      Self.Scope_Selector_Box.Pack_End
        (Self.Context_Combo,
         Expand => True,
         Fill   => True);
      Widget_Callback.Object_Connect
        (Self.Context_Combo, Gtk.Combo_Box.Signal_Changed,
         On_Context_Combo_Changed'Access, Self);
      Self.Context_Combo.Set_Name ("search scope combo");

      Group_Widget.Create_Child
        (Self.Scope_Selector_Box,
         Label     => "Where",
         Expand    => True,
         Fill      => True);

      Gtk_New_Vbox (Self.Scope_Optional_Box, Homogeneous => False);
      Group_Widget.Append_Child (Self.Scope_Optional_Box, Expand => False);

      --  The buttons

      Gtk_New_With_Mnemonic (Self.Search_Next_Button, -"_Find");
      Set_First_Next_Mode (Self, Find_Next => False);
      Self.Main_View.Append_Button (Self.Search_Next_Button);
      Self.Search_Next_Button.Set_Tooltip_Text (-"Search next occurrence");
      Widget_Callback.Object_Connect
        (Self.Search_Next_Button, Signal_Clicked,
         On_Search'Access, Self);

      Gtk_New_With_Mnemonic (Self.Search_Previous_Button, -"_Previous");
      Self.Search_Previous_Button.Set_No_Show_All (True);
      Self.Main_View.Append_Button (Self.Search_Previous_Button);
      Self.Search_Previous_Button.Set_Tooltip_Text
        (-"Search previous occurrence");
      Widget_Callback.Object_Connect
        (Self.Search_Previous_Button, Signal_Clicked,
         On_Search_Previous'Access, Self);

      Gtk_New_With_Mnemonic (Self.Search_All_Button, -"Find All");
      Self.Search_All_Button.Set_No_Show_All (True);
      Self.Main_View.Append_Button (Self.Search_All_Button);
      Self.Search_All_Button.Set_Tooltip_Text (-"Find all occurences");
      Widget_Callback.Object_Connect
        (Self.Search_All_Button, Signal_Clicked,
         On_Search_All'Access, Self);

      Gtk_New (Self.Replace_Button, -"Replace");
      Self.Replace_Button.Set_No_Show_All (True);
      Self.Main_View.Append_Button (Self.Replace_Button);
      Self.Replace_Button.Set_Tooltip_Text (-"Replace next occurrence");
      Widget_Callback.Object_Connect
        (Self.Replace_Button, Signal_Clicked,
         On_Replace'Access, Self);
      Self.Replace_Button.Set_Sensitive (False);

      Gtk_New_With_Mnemonic (Self.Replace_Search_Button, -"Replace & Find");
      Self.Replace_Search_Button.Set_No_Show_All (True);
      Self.Main_View.Append_Button (Self.Replace_Search_Button);
      Self.Replace_Search_Button.Set_Tooltip_Text
        (-"Replace, then find next occurrence");
      Widget_Callback.Object_Connect
        (Self.Replace_Search_Button, Signal_Clicked,
         On_Replace_Search'Access, Self);

      Gtk_New_With_Mnemonic (Self.Replace_All_Button, -"Replace All");
      Self.Replace_All_Button.Set_No_Show_All (True);
      Self.Main_View.Append_Button (Self.Replace_All_Button);
      Self.Replace_All_Button.Set_Tooltip_Text
        (-"Replace all occurences");
      Widget_Callback.Object_Connect
        (Self.Replace_All_Button, Signal_Clicked,
         On_Replace_All'Access, Self);

      Self.On_Destroy (On_Vsearch_Destroy'Access);

      --  Any change to the fields resets the search mode
      Return_Callback.Object_Connect
        (Self.Pattern_Combo.Get_Child, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (Key_Press'Access), Self);
      Return_Callback.Object_Connect
        (Self.Replace_Combo.Get_Child, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (Key_Press_Replace'Access), Self);
      Kernel_Callback.Connect
        (Self.Pattern_Combo, Gtk.Combo_Box.Signal_Changed,
         Reset_Search'Access, Self.Kernel);
      Kernel_Callback.Connect
        (Self.Context_Combo, Gtk.Combo_Box.Signal_Changed,
         Reset_Search'Access, Self.Kernel);

      --  Initialize the widgets that may have saved items in history
      Initialize_From_History;

      Search_Reset_Hook.Add (new Set_First_Next_Mode_Cb);
      Search_Functions_Changed_Hook.Add (new Search_Functions_Changed);
      Search_Regexps_Changed_Hook.Add (new New_Predefined_Regexp);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      --  ??? Should be changed when prefs are changed
      Set_Font_And_Colors (Self.Main_View, Fixed_Font => False);

      return Self.Pattern_Combo.Get_Child;
   end Initialize;

   ----------------------
   -- Set_Vsearch_Mode --
   ----------------------

   procedure Set_Vsearch_Mode
     (Self : not null access Vsearch_Record'Class;
      Mode : Vsearch_Mode)
   is
      Show_Replace_Widgets : constant Boolean := Mode = Find_And_Replace;
   begin
      Self.Mode := Mode;
      Add_To_History
        (Self.Kernel,
         Key       => Mode_Hist_Key,
         New_Entry => Mode'Image);

      Self.Main_View.Set_Child_Visible
        (Child_Key => Replace_Child_Key,
         Visible   => Show_Replace_Widgets);
      Self.Replace_Button.Set_Visible (Show_Replace_Widgets);
      Self.Replace_All_Button.Set_Visible (Show_Replace_Widgets);
      Self.Replace_Search_Button.Set_Visible (Show_Replace_Widgets);
      Self.Search_Previous_Button.Set_Visible (not Show_Replace_Widgets);
      Self.Search_All_Button.Set_Visible (not Show_Replace_Widgets);
   end Set_Vsearch_Mode;

   ----------------------------
   -- Is_In_Incremental_Mode --
   ----------------------------

   function Is_In_Incremental_Mode
     (Self : not null access Vsearch_Record'Class) return Boolean
   is
      Module : constant Search_Module :=
                 Find_Module
                   (Self.Kernel, Label => Get_Active_Id (Self.Context_Combo));
   begin
      return (Module /= null
              and then Module.Is_Option_Supported (Supports_Incremental)
              and then Incremental_Search.Get_Pref);
   end Is_In_Incremental_Mode;

   ------------------
   -- Receive_Text --
   ------------------

   procedure Receive_Text
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Text      : Glib.UTF8_String)
   is
      pragma Unreferenced (Clipboard);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Vsearch_Module_Id.Get_Kernel);
   begin
      if Text /= ""
        and then Text'Length < 128
      then
         Set_Active_Text (View.Pattern_Combo, Text);
      end if;
   end Receive_Text;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Vsearch_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Gtk_New (View.Regexp_Toggle);
      View.Regexp_Toggle.Set_Icon_Name ("gps-regexp-symbolic");
      View.Regexp_Toggle.Set_Tooltip_Text
        (-"The pattern is a regular expression");
      Create_New_Boolean_Key_If_Necessary
        (Get_History (View.Kernel).all, Regexp_Search_Hist_Key, False);
      Associate
        (Get_History (View.Kernel).all, Regexp_Search_Hist_Key,
         View.Regexp_Toggle,
         Default => False);
      Kernel_Callback.Connect
        (View.Regexp_Toggle,
         Gtk.Toggle_Tool_Button.Signal_Toggled,
         Reset_Search'Access,
         View.Kernel);
      Toolbar.Insert (View.Regexp_Toggle);

      Gtk_New (View.Case_Toggle);
      View.Case_Toggle.Set_Icon_Name ("gps-case-sensitive-symbolic");
      View.Case_Toggle.Set_Tooltip_Text
        (-("Select this to differenciate upper from lower casing in search"
         & " results"));
      Create_New_Boolean_Key_If_Necessary
        (Get_History (View.Kernel).all, Case_Sensitive_Hist_Key, False);
      Associate
        (Get_History (View.Kernel).all, Case_Sensitive_Hist_Key,
         View.Case_Toggle, Default => False);
      Kernel_Callback.Connect
        (View.Case_Toggle,
         Gtk.Toggle_Tool_Button.Signal_Toggled,
         Reset_Search'Access,
         View.Kernel);
      Toolbar.Insert (View.Case_Toggle);

      Gtk_New (View.Whole_Word_Toggle);
      View.Whole_Word_Toggle.Set_Icon_Name ("gps-whole-word-symbolic");
      View.Whole_Word_Toggle.Set_Tooltip_Text
        (-("Select this if the pattern should only match a whole word, never"
         & " part of a word"));
      Create_New_Boolean_Key_If_Necessary
        (Get_History (View.Kernel).all, Whole_Word_Hist_Key, False);
      Associate
        (Get_History (View.Kernel).all, Whole_Word_Hist_Key,
         View.Whole_Word_Toggle, Default => False);
      Kernel_Callback.Connect
        (View.Whole_Word_Toggle,
         Gtk.Toggle_Tool_Button.Signal_Toggled,
         Reset_Search'Access,
         View.Kernel);
      Toolbar.Insert (View.Whole_Word_Toggle);

      --  Include all the patterns that have been predefined so far, and make
      --  sure that new patterns will be automatically added.
      Widget_Callback.Object_Connect
        (View.Pattern_Combo, Gtk.Combo_Box.Signal_Changed,
         Selection_Changed'Access, View);
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Vsearch_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu
        (Menu,
         Kernel => View.Kernel,
         Pref   => Incremental_Search);
      Append_Menu
        (Menu,
         Kernel => View.Kernel,
         Pref   => Close_On_Match);
      Append_Menu
        (Menu,
         Kernel => View.Kernel,
         Pref   => Select_On_Match);
   end Create_Menu;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self  : not null access Vsearch_Record;
      Child : not null access GPS_MDI_Child_Record'Class)
   is
      P  : aliased Search_Functions_Changed;
      P2 : aliased New_Predefined_Regexp;
   begin
      P.Execute (Self.Kernel);
      P2.Execute (Self.Kernel);

      Widget_Callback.Connect (Child, Signal_Float_Child, On_Float'Access);
      Widget_Callback.Connect (Child, Signal_Unfloat_Child, On_Float'Access);

      On_Float (Child);

      Widget_Callback.Connect
        (Child, Signal_Before_Destroy_Child, On_Destroy_Child'Access);
   end On_Create;

   ----------------------
   -- On_Destroy_Child --
   ----------------------

   procedure On_Destroy_Child
     (Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Vsearch : constant Vsearch_Access :=
        Vsearch_Access (GPS_MDI_Child (Child).Get_Actual_Widget);
   begin
      if Vsearch.Focused /= null then
         if Vsearch.Focused.Get_Parent /= null then
            Vsearch.Focused.Get_Parent.Grab_Focus;
         end if;
         Vsearch.Focused.Grab_Focus;
         Vsearch.Focused := null;
      end if;
   end On_Destroy_Child;

   ------------------------
   -- On_Destroy_Focused --
   ------------------------

   procedure On_Destroy_Focused (Widget : access Gtk_Widget_Record'Class) is
   begin
      Vsearch_Access (Widget).Focused := null;
   end On_Destroy_Focused;

   ---------------------------
   -- Get_Or_Create_Vsearch --
   ---------------------------

   function Get_Or_Create_Vsearch
     (Kernel        : access Kernel_Handle_Record'Class;
      Raise_Widget  : Boolean := False;
      Reset_Entries : Boolean := False;
      Context       : GNAT.Strings.String_Access := null;
      Mode          : Vsearch_Mode := Find_Only) return Vsearch_Access
   is
      --  We must create the search dialog only after we have found the current
      --  context, otherwise it would return the context of the search widget
      --  itself
      Selected   : constant Selection_Context := Kernel.Get_Current_Context;
      W          : Gtk_Widget := Get_Current_Focus_Widget (Kernel);
      Buffer     : Gtk_Text_Buffer;
      First_Iter : Gtk_Text_Iter;
      Last_Iter  : Gtk_Text_Iter;

      Has_Selection : Boolean := False;
      --  If W has selecion saved in First_Iter, Last_Iter
      Has_Multiline_Selection : Boolean := False;
      --  If W has multiline selecion saved in First_Iter, Last_Iter

      View     : Vsearch_Access;
      Dummy    : Boolean;
      Default_Pattern         : GNAT.Strings.String_Access := null;

   begin
      declare
         Start, Stop : Gint;
         Success : Boolean := False;
      begin
         if W /= null
           and then Is_A (W.Get_Type, Gtk.Editable.Get_Type)
         then
            Get_Selection_Bounds (+W, Start, Stop, Success);

            if Success and then Start /= Stop then
               Default_Pattern := new String'(Get_Chars (+W, Start, Stop));
            end if;

         elsif W /= null and then W.all in Gtk_Text_View_Record'Class then
            Buffer := Get_Buffer (Gtk_Text_View (W));
            Get_Selection_Bounds
              (Buffer, First_Iter, Last_Iter, Has_Selection);

            if Has_Selection then
               if Get_Line (First_Iter) = Get_Line (Last_Iter) then
                  Default_Pattern := new String'
                    (Get_Slice (Buffer, First_Iter, Last_Iter));

                  Select_Range (Buffer, First_Iter, First_Iter);
               else
                  Has_Multiline_Selection := True;
               end if;
            end if;

         else
            W := null;
         end if;

         if Default_Pattern /= null
           and then
             (Default_Pattern'Length >= 128
              or else Index (Default_Pattern.all, (1 => ASCII.LF)) /= 0)
         then
            --  In this case, the selection is not suitable for being used by
            --  the search widget

            Free (Default_Pattern);
         end if;
      end;

      View := Search_Views.Get_Or_Create_View (Kernel, Focus => Raise_Widget);
      if W /= null then
         Widget_Callback.Object_Connect
           (W, Signal_Destroy, On_Destroy_Focused'Access, View);
         View.Focused := W;
      end if;

      --  Automatically fill the pattern text entry with the selection, if
      --  there is one which does not contain multiple lines.
      --  We also set the "Replace" in this case, since users will generally
      --  want to make a minor modification, rather than reuse the previous
      --  replacement text (which is still accessible through the combo).

      if Default_Pattern /= null then
         Set_Active_Text (View.Pattern_Combo, Default_Pattern.all);
         Set_Active_Text (View.Replace_Combo, Default_Pattern.all);
         Free (Default_Pattern);
      else
         Request_Text
           (Gtk.Clipboard.Get (Selection_Primary),
            Receive_Text'Access);
      end if;

      if Has_Selection then
         --  Restore multiline selection
         if Has_Multiline_Selection then
            Select_Range (Buffer, First_Iter, Last_Iter);
         end if;

         View.Selection_From := Buffer.Create_Mark
           (Mark_Name    => "search_from",
            Where        => First_Iter,
            Left_Gravity => True);

         View.Selection_To := Buffer.Create_Mark
           (Mark_Name    => "search_to",
            Where        => Last_Iter,
            Left_Gravity => False);
      end if;

      Set_Selected_Project (Kernel, Selected);

      if Reset_Entries then
         --  ??? Temporarily: reset the entry. The correct fix would be to
         --  separate the find and replace tabs, so that having a default
         --  entry in this combo doesn't look strange when the combo is
         --  insensitive.

         if Context = null then
            View.Set_Search_Module
              (Get_Search_Module_From_Context
                 (View,
                  Context      => Selected,
                  In_Selection => Has_Multiline_Selection));
         else
            Dummy := View.Context_Combo.Set_Active_Id (Context.all);
         end if;

         Grab_Toplevel_Focus (Get_MDI (Kernel), View.Pattern_Combo.Get_Child);
      end if;

      --  Set the mode of the search view
      View.Set_Vsearch_Mode (Mode => Mode);

      return View;
   end Get_Or_Create_Vsearch;

   ----------------------
   -- Substitute_Label --
   ----------------------

   function Substitute_Label
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Label  : String) return String
   is
      function Substitution (Param : String; Quoted : Boolean) return String;
      function Substitution (Param : String; Quoted : Boolean) return String is
         pragma Unreferenced (Quoted);
      begin
         if Param = "p" then
            declare
               P : constant Project_Type_Array :=
                 Get_Selected_Project (Kernel);
               Result : Unbounded_String;
            begin
               if P'Length = 0 then
                  return Get_Project (Kernel).Name;
               else
                  for P2 in P'Range loop
                     if Result /= Null_Unbounded_String then
                        Append (Result, ", ");
                     end if;
                     Append (Result, P (P2).Name);
                  end loop;
                  return To_String (Result);
               end if;
            end;
         end if;
         return "%" & Param;
      end Substitution;
   begin
      return GNATCOLL.Templates.Substitute
        (Label,
         Delimiter => '%',
         Callback  => Substitution'Unrestricted_Access,
         Recursive => False);
   end Substitute_Label;

   --------------------------
   -- Set_Selected_Project --
   --------------------------

   procedure Set_Selected_Project
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context)
   is
      View : constant Vsearch_Access := Search_Views.Retrieve_View (Kernel);
      Set : File_Info_Set;
      Idx : Integer;
   begin
      if View = null then
         return;
      end if;

      Free (View.Projects);

      if Has_Project_Information (Context) then
         View.Projects :=
           new Project_Type_Array'(1 .. 1 => Project_Information (Context));
      elsif Has_File_Information (Context) then
         Set := Get_Project_Tree (Kernel).Info_Set
           (File_Information (Context));

         if not Set.Is_Empty then
            View.Projects := new Project_Type_Array
              (1 .. Integer (Set.Length));
            Idx := 1;

            for P of Set loop
               declare
                  F_Info : constant File_Info'Class := File_Info'Class (P);
               begin
                  View.Projects (Idx) := F_Info.Project;
               end;
               Idx := Idx + 1;
            end loop;
         end if;
      end if;

      Refresh_Context_Combo (Kernel);
   end Set_Selected_Project;

   --------------------------
   -- Get_Selected_Project --
   --------------------------

   function Get_Selected_Project
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Project_Type_Array
   is
      View : constant Vsearch_Access := Search_Views.Retrieve_View (Kernel);
   begin
      if View = null or else View.Projects = null then
         return Project_Type_Array'(1 .. 0 => No_Project);
      else
         return View.Projects.all;
      end if;
   end Get_Selected_Project;

   -------------------
   -- Get_Selection --
   -------------------

   procedure Get_Selection
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      From   : out Gtk_Text_Mark;
      To     : out Gtk_Text_Mark)
   is
      View : constant Vsearch_Access := Search_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         From := View.Selection_From;
         To   := View.Selection_To;
      end if;
   end Get_Selection;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Can_Fill_With_Current_Word_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context));
   begin
      if View /= null and then View.Pattern_Combo.Get_Child.Has_Focus then
         declare
            Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                       View.Kernel.Get_Buffer_Factory.Get (Open_View => False);
         begin
            return Buffer /= GPS.Editors.Nil_Editor_Buffer;
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Search_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter, Context);
   begin
      return Vsearch_Module_Id.Search_Started;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Fill_With_Current_Word_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Vsearch : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
      Buffer  : constant GPS.Editors.Editor_Buffer'Class :=
                  Vsearch.Kernel.Get_Buffer_Factory.Get (Open_View => False);

   begin
      if Buffer /= GPS.Editors.Nil_Editor_Buffer then
         declare
            Start_Loc : constant GPS.Editors.Editor_Location'Class :=
                          Buffer.Selection_Start;
            End_Loc   : GPS.Editors.Editor_Location'Class :=
                          Buffer.Selection_End;
         begin
            End_Loc := End_Loc.Forward_Word (1);

            Buffer.Select_Text (Start_Loc, End_Loc);

            Set_Active_Text
              (Vsearch.Pattern_Combo,
               Text => Buffer.Get_Chars (Start_Loc, End_Loc));
            Gtk_Entry (Vsearch.Pattern_Combo.Get_Child).Set_Position (-1);
         end;
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_Next_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
   begin
      if View /= null then
         On_Search (View);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
   begin
      if View /= null then
         On_Search_Previous (View);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
   begin
      if View /= null and then View.Is_Visible then
         On_Search_All (View);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Replace_Current_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
   begin
      if View /= null then
         On_Replace (View);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Replace_And_Find_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
   begin
      if View /= null then
         On_Replace_Search (View);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Replace_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
   begin
      if View /= null and then View.Replace_All_Button.Get_Sensitive then
         On_Replace_Search (View);
      end if;
      return Commands.Success;
   end Execute;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
     (Action : in out Search_Specific_Context) is
   begin
      Free (Action.Context);
   end Primitive_Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Action  : access Search_Specific_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Vsearch : Vsearch_Access := Search_Views.Retrieve_View
        (Get_Kernel (Context.Context));
      Dummy  : Boolean;
   begin
      if Action.Incremental then
         Set_Pref
           (Pref    => Incremental_Search,
            Manager => Kernel.Get_Preferences,
            Value   => True);
      end if;

      --  In incremental mode, we want users to be able to search for the next
      --  occurence if the search entry has already the focus with the same
      --  action to imitate what Emacs does.

      if Vsearch /= null
        and then Vsearch.Pattern_Combo.Get_Child.Has_Focus
        and then Vsearch.Is_In_Incremental_Mode
      then
         On_Search (Vsearch);
      else
         Vsearch := Get_Or_Create_Vsearch
           (Get_Kernel (Context.Context),
            Raise_Widget  => True,
            Reset_Entries => True,
            Context       => Action.Context,
            Mode          => Find_Only);
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Action  : access Replace_Specific_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Vsearch : Vsearch_Access;
      pragma Unreferenced (Vsearch);
   begin
      Vsearch := Get_Or_Create_Vsearch
        (Get_Kernel (Context.Context),
         Raise_Widget  => True,
         Reset_Entries => True,
         Context       => Action.Context,
         Mode          => Find_And_Replace);
      return Success;
   end Execute;

   ------------------------------
   -- Register_Search_Function --
   ------------------------------

   procedure Register_Search_Function
     (Kernel        : access Kernel_Handle_Record'Class;
      Module        : not null access Search_Module_Type'Class;
      Is_Default    : Boolean := False)
   is
      Id    : constant Module_ID := Module.Get_Id;
      Label : constant String := Module.Get_Label;
   begin
      if Id /= null then
         Create_New_Key_If_Necessary
           (Get_History (Kernel).all,
            Last_Function_In_Module_Key & History_Key (Get_Name (Id)),
            Key_Type => Strings);
         Set_Max_Length
           (Get_History (Kernel).all,
            1,   --  Only the last one is interesting
            Last_Function_In_Module_Key & History_Key (Get_Name (Id)));
      end if;

      Prepend (Vsearch_Module_Id.Search_Modules, Module);

      if Is_Default then
         Vsearch_Module_Id.Default_Search_Module := Search_Module (Module);
      end if;

      if Module.Get_Scope_Selector /= null then
         declare
            Scope_Combo    : constant Gtk_Widget := Gtk_Widget
              (Module.Get_Scope_Selector.Get_Scope_Combo);
            Scope_Optional : constant Gtk_Widget :=
                               Module.Get_Scope_Selector.Get_Optional_Widget;
         begin
            --  Make sure the extra information is not destroyed for the
            --  duration of GPS, even when the search window is destroyed
            --  (since the same extra info widget will be reused for the next
            --  search window, to preserve the current values).
            Scope_Combo.Ref_Sink;

            --  Since the same widget could be shared amongst several search
            --  contexts, we should own one extra ref for each search context
            Scope_Combo.Ref;

            if Scope_Optional /= null then
               Scope_Optional.Ref_Sink;
               Scope_Optional.Ref;
            end if;
         end;
      end if;

      Register_Action
        (Kernel,
         Name        => -"Search in context: " & Label,
         Command     => new Search_Specific_Context'
           (Interactive_Command
            with
              Context     => new String'(Label),
              Incremental => False),
         Description => -("Open the search dialog, and preset the ""Look In"""
           & " field to """ & Label & """"),
         Category    => -"Search");

      Search_Functions_Changed_Hook.Run (Kernel);
   end Register_Search_Function;

   ---------------------------
   -- Get_Nth_Search_Module --
   ---------------------------

   function Get_Nth_Search_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Num    : Positive) return Search_Module
   is
      pragma Unreferenced (Kernel);
      Node : Search_Modules_List.Cursor :=
        Vsearch_Module_Id.Search_Modules.First;
   begin
      for N in 1 .. Num - 1 loop
         Node := Next (Node);
      end loop;

      if Has_Element (Node) then
         return Element (Node);
      end if;

      return null;
   end Get_Nth_Search_Module;

   -----------------
   -- Find_Module --
   -----------------

   function Find_Module
     (Kernel : access Kernel_Handle_Record'Class;
      Label  : String) return Search_Module
   is
      pragma Unreferenced (Kernel);
   begin
      for Module of Vsearch_Module_Id.Search_Modules loop
         if Module.Get_Label = Label then
            return Module;
         end if;
      end loop;

      return null;
   end Find_Module;

   ------------------------------------
   -- Get_Search_Module_From_Context --
   ------------------------------------

   function Get_Search_Module_From_Context
     (Vsearch      : not null access Vsearch_Record'Class;
      Context      : Selection_Context;
      In_Selection : Boolean := False) return Search_Module
   is
      Id                  : constant Module_ID :=
                              Module_ID (Get_Creator (Context));
      List                : Search_Modules_List.Cursor :=
                              Vsearch_Module_Id.Search_Modules.First;
      Last_Matching_Node  : Search_Modules_List.Cursor := No_Element;
      Key                 : constant History_Key :=
        Last_Function_In_Module_Key & History_Key (Get_Name (Id));
      Last_Selected       : constant String_List_Access :=
        Get_History (Get_History (Get_Kernel (Context)).all, Key);
   begin
      while Has_Element (List) loop
         if Element (List).Get_Id = Id
           and Element (List).Get_In_Selection = In_Selection
         then
            Last_Matching_Node := List;

            if not Get_Pref (Keep_Previous_Search_Context)
              or else Last_Selected = null
              or else Last_Selected (Last_Selected'First).all =
                 Element (List).Get_Label
            then
               if Active (Me) then
                  Trace (Me, "Get last search function for module "
                         & String (Key) & ": " & Element (List).Get_Label);
               end if;

               return Element (List);
            end if;
         end if;

         List := Next (List);
      end loop;

      if Has_Element (Last_Matching_Node) then
         return Element (Last_Matching_Node);
      end if;

      --  If the context does not match with any registered module, return the
      --  one currently used, if any, or the default one otherwise.
      return Module : Search_Module do
         if Id /= null and then Id.Module_Name = Search_Module_Name then
            Module := Find_Module
              (Vsearch.Kernel,
               Label => Get_Active_Text (Vsearch.Context_Combo));
         end if;

         if Module = null then
            Module := Vsearch_Module_Id.Default_Search_Module;
         end if;
      end return;
   end Get_Search_Module_From_Context;

   -----------------------
   -- Set_Search_Module --
   -----------------------

   procedure Set_Search_Module
     (Self   : not null access Vsearch_Record'Class;
      Module : Search_Module)
   is
      Dummy : Boolean;
   begin
      if Module /= null then
         Dummy := Self.Context_Combo.Set_Active_Id (Module.Get_Label);
      end if;
   end Set_Search_Module;

   ------------------------
   -- Set_Last_Of_Module --
   ------------------------

   procedure Set_Last_Of_Module
     (Handle : access Kernel_Handle_Record'Class;
      Module : Search_Module) is
   begin
      if not Get_Pref (Keep_Previous_Search_Context)
        or else Module.Get_Id = null
      then
         return;
      end if;

      if Active (Me) then
         Trace (Me, "Set last search function for module "
                & Get_Name (Module.Get_Id)
                & " to " & Module.Get_Label);
      end if;

      Add_To_History
        (Get_History (Handle).all,
         Last_Function_In_Module_Key & History_Key (Get_Name (Module.Get_Id)),
         Module.Get_Label);
   end Set_Last_Of_Module;

   -----------------------------
   -- Register_Search_Pattern --
   -----------------------------

   procedure Register_Search_Pattern
     (Kernel         : access Kernel_Handle_Record'Class;
      Name           : String;
      Regexp         : String;
      Case_Sensitive : Boolean := False;
      Is_Regexp      : Boolean := True)
   is
      Tmp : Search_Regexps_Array_Access := Vsearch_Module_Id.Search_Regexps;
   begin
      if Tmp /= null then
         Vsearch_Module_Id.Search_Regexps :=
           new Search_Regexps_Array (Tmp'First .. Tmp'Last + 1);
         Vsearch_Module_Id.Search_Regexps (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      else
         Vsearch_Module_Id.Search_Regexps := new Search_Regexps_Array (1 .. 1);
      end if;

      Tmp := Vsearch_Module_Id.Search_Regexps;
      Tmp (Tmp'Last) :=
        (Name           => new String'(Name),
         Regexp         => new String'(Regexp),
         Case_Sensitive => Case_Sensitive,
         Is_Regexp      => Is_Regexp);

      Search_Regexps_Changed_Hook.Run (Kernel);
   end Register_Search_Pattern;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Vsearch_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);
      Patt, Name : Node_Ptr;
   begin
      if Node.Tag.all = "vsearch-pattern" then
         Name := Find_Tag (Node.Child, "name");
         Patt := Find_Tag (Node.Child, "regexp");
         if Patt = null then
            Patt := Find_Tag (Node.Child, "string");
         end if;

         if Patt /= null and then Name /= null then
            Register_Search_Pattern
              (Kernel         => Get_Kernel (Module.all),
               Name           => Name.Value.all,
               Regexp         => Patt.Value.all,
               Case_Sensitive =>
                 To_Lower (Get_Attribute (Patt, "case-sensitive")) = "true",
               Is_Regexp      => Patt.Tag.all = "regexp");
         end if;
      end if;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter  : Action_Filter;
   begin
      Vsearch_Module_Id := new Vsearch_Module_Record;
      Search_Views.Register_Module (Kernel, Module_ID (Vsearch_Module_Id));

      Register_Action
        (Kernel, "Search",
         new Search_Specific_Context'
           (Interactive_Command
            with
              Context     => null,
              Incremental => False),
         Description => -("Open the search dialog. If you have selected the"
           & " preference Search/Preserve Search Context, the same context"
           & " will be selected, otherwise the context is reset depending on"
           & " the active window"),
         Icon_Name   => "gps-search-symbolic",
         Category    => -"Search");

      Register_Action
        (Kernel, "incremental search",
         new Search_Specific_Context'
           (Interactive_Command
            with
              Context     => null,
              Incremental => True),
         Description => -("Open the search dialog in incremental mode."),
         Icon_Name   => "gps-search-symbolic",
         Category    => -"Search");

      Register_Action
        (Kernel, "Replace",
         new Replace_Specific_Context'
           (Interactive_Command
            with
              Context     => null,
              Incremental => False),
         Description => -("Open the search dialog in the replace mode."
           & " If you have selected the"
           & " preference Search/Preserve Search Context, the same context"
           & " will be selected, otherwise the context is reset depending on"
           & " the active window"),
         Icon_Name   => "gps-search-symbolic",
         Category    => -"Search");

      Filter := new Can_Fill_With_Current_Word_Filter;
      Register_Action
        (Kernel, "fill search with current word",
         new Fill_With_Current_Word_Command,
         Description => -("Fill the Search view's search entry with the "
           & "focused editor's current word"),
         Filter      => Filter,
         Category    => -"Search");

      Filter  := new Has_Search_Filter;
      Register_Action
        (Kernel, "find next",
         new Find_Next_Command,
         Description => -"Find the next occurrence of the search pattern",
         Filter      => Filter,
         Category    => -"Search");

      Register_Action
        (Kernel, "find previous",
         new Find_Previous_Command,
         Description => -"Find the previous occurrence of the search pattern",
         Filter      => Filter,
         Category    => -"Search");

      Register_Action
        (Kernel, "find all",
         new Find_All_Command,
         Description => -"Find all the occurrences of the search pattern",
         Category    => -"Search");

      Register_Action
        (Kernel, "replace current",
         new Replace_Current_Command,
         Description => -"Replace the current matched occurrence, if any, " &
           "with the replace pattern",
         Filter      => Filter,
         Category    => -"Search");

      Register_Action
        (Kernel, "replace and find",
         new Replace_And_Find_Command,
         Description => -("Replace the current matched occurrence, if any, " &
             "with the replace pattern and find the next occurrence"),
         Category    => -"Search");

      Register_Action
        (Kernel, "replace all",
         new Replace_All_Command,
         Description => -("Replace all the occurrences of the search pattern "
           & "by the replace pattern"),
         Category    => -"Search");

      Register_Preferences (Kernel);
   end Register_Module;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Incremental_Search := Create
        (Get_Preferences (Kernel),
         Name    => "Search-Incremental",
         Label   => -"Incremental search",
         Path    => -":Search",
         Doc     =>
           -"Enable the incremental mode. In this mode, a search will be "
         & "automatically performed whenever the search pattern is modified, "
         & "starting from the current location to next occurence in the "
         & "current file.",
         Default => False);

      Select_On_Match := Create
        (Get_Preferences (Kernel),
         Name    => "Search-Select-On-Match",
         Label   => -"Select on match",
         Path    => -":Search",
         Doc     =>
           -"When a match is found, give the focus to the matching editor. If"
         & " unselected, the focus is left on the search window, which means"
         & " you can keep typing Enter to go to the next search, but can't"
         & " modify the editor directly. This option is ignored when the"
         & " incremental mode is enabled.",
         Default => False);

      Close_On_Match := Create
        (Get_Preferences (Kernel),
         Name    => "Search-Close-On-Match",
         Label   => -"Close on match",
         Path    => -":Search",
         Doc     =>
           -"If this is selected, the search dialog is closed when a match is"
         & " found. You can still search for the next occurrence by using"
         & " the appropriate shortcut (Ctrl-N by default)",
         Default => False);

      Ask_Confirmation_For_Replace_All := Create
        (Get_Preferences (Kernel),
         Name  => "Ask-Confirmation-For-Replace-All",
         Label => -"Confirmation for 'Replace all'",
         Path  => -":Search",
         Doc   =>
            -"Enable the confirmation popup before a replace all operation.",
         Default => True);

      Keep_Previous_Search_Context := Create
        (Get_Preferences (Kernel),
         Name  => "keep-previous-search-context",
         Label => -"Preserve Search Context",
         Path  => -":Search",
         Doc   => -("Preserve the contents of the ""Look in"" entry"
           & " between searches."),
         Default => False);
   end Register_Preferences;

   ------------------
   -- Reset_Search --
   ------------------

   procedure Reset_Search
     (Object : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      --  Call this to avoid dangling pointers in View.Projects
      Set_Selected_Project (Kernel, Kernel.Get_Current_Context);
      Search_Reset_Hook.Run (Kernel);
   end Reset_Search;

end Vsearch;
