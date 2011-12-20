------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with System;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Window;                use Gdk.Window;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Alignment;             use Gtk.Alignment;
with Gtk.Cell_Layout;           use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Clipboard;             use Gtk.Clipboard;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Editable;              use Gtk.Editable;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Selection;             use Gtk.Selection;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_View;             use Gtk.Text_View;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Types;              use Gtkada.Types;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Commands.Generic_Asynchronous;
with Default_Preferences;       use Default_Preferences;
with Generic_List;
with GUI_Utils;                 use GUI_Utils;
with GNATCOLL.VFS;
with Histories;                 use Histories;
with Traces;                    use Traces;
with String_Utils;              use String_Utils;
with XML_Utils;                 use XML_Utils;

package body Vsearch is
   Me : constant Debug_Handle := Create ("Vsearch");

   Pattern_Hist_Key   : constant History_Key := "search_patterns";
   Replace_Hist_Key   : constant History_Key := "search_replace";
   Window_X_Hist_Key  : constant History_Key := "search_window_x";
   Window_Y_Hist_Key  : constant History_Key := "search_window_y";
   Select_On_Match_Hist_Key    : constant History_Key := "select_on_match";
   Close_On_Match_Hist_Key     : constant History_Key := "close_on_match";
   --  The key for the histories.

   Last_Function_In_Module_Key : constant History_Key := "search_function_";
   --  The prefix used to store the name of the last search function used for
   --  each module. The name of the module is appended to form the real key.

   Ask_Confirmation_For_Replace_All : Boolean_Preference;
   Keep_Previous_Search_Context     : Boolean_Preference;
   --  The preferences

   Close_On_Match_Description : constant String :=
     -("If this is selected, the search dialog is closed when a match is"
       & " found. You can still search for the next occurrence by using"
       & " the appropriate shortcut (Ctrl-N by default)");

   Select_On_Match_Description : constant String :=
     -("When a match is found, give the focus to the matching editor. If"
       & " unselected, the focus is left on the search window, which means"
       & " you can keep typing Enter to go to the next search, but can't"
       & " modify the editor directly");

   type Search_Module_Data is record
      Mask              : Search_Options_Mask;
      Factory           : Module_Search_Context_Factory;
      Extra_Information : Gtk.Widget.Gtk_Widget;
      Id                : Module_ID;
      Label             : String_Access;
   end record;

   No_Search : constant Search_Module_Data :=
     (Label             => null,
      Mask              => 0,
      Factory           => null,
      Id                => null,
      Extra_Information => null);

   procedure Set_Last_Of_Module
     (Handle      : access Kernel_Handle_Record'Class;
      Search_Data : Search_Module_Data);
   --  The Search_Data given in parameter is set as beign the last one selected
   --  by the user, and will be the next one shown for the corresponding
   --  module.

   function Search_Context_From_Module
     (Id     : not null access Module_ID_Record'Class;
      Handle : access Kernel_Handle_Record'Class) return Search_Module_Data;
   --  Return the first search context that matches Id, or No_Search if there
   --  is none.

   procedure Free (Data : in out Search_Module_Data);
   --  Free the memory associated with Data

   package Search_Modules_List is new Generic_List (Search_Module_Data);
   use Search_Modules_List;

   Search_Module_Name : constant String := "Search";

   type Vsearch_Module_Record is new Module_ID_Record with record
      Search_Modules : Search_Modules_List.List;
      --  Global variable that contains the list of all registered search
      --  functions.

      Search : Vsearch_Access;
      --  The extended search widget, stored for the whole life of GPS, so that
      --  histories are kept

      Next_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Prev_Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;

      Search_Regexps : Search_Regexps_Array_Access;
      --  The list of predefined regexps for the search module.

      Tab_Width      : Natural;
      --  The default tab width.
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

   type Search_Specific_Context is new Interactive_Command with record
      Context : String_Access;
   end record;
   overriding procedure Free (Action : in out Search_Specific_Context);
   overriding function Execute
     (Action  : access Search_Specific_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  A command that opens the search dialog and presets the Look In field to
   --  a specific function. If Context is null, then the previous context is
   --  preserve if the preference Keep_Previous_Search_Context is set,
   --  otherwise the context is reset depending on the current module.

   type Idle_Search_Data is record
      Vsearch         : Vsearch_Access;
      Search_Backward : Boolean;
      Context         : Search_Context_Access;
      Found           : Boolean := False;
      Replace_With    : String_Access := null;
      --  Whether the search results in at least one match.
      Case_Preserving : Boolean;
   end record;

   procedure Search_Iterate
     (Data    : in out Idle_Search_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Perform an atomic search operation.

   procedure Replace_Iterate
     (Data    : in out Idle_Search_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Perform an atomic replace operation.

   procedure Free (D : in out Idle_Search_Data);
   --  Free memory associated with D.

   package Search_Commands is new Commands.Generic_Asynchronous
     (Data_Type => Idle_Search_Data,
      Free      => Free);
   --  Handle the search/replace commands.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Search_Regexps_Array, Search_Regexps_Array_Access);

   procedure Set_First_Next_Mode
     (Vsearch   : access Vsearch_Record'Class;
      Find_Next : Boolean);
   --  If Find_Next is False, a new search will be started, otherwise the next
   --  occurence of the current search will be searched.

   procedure Set_First_Next_Mode_Cb
     (Kernel : access Kernel_Handle_Record'Class);
   --  Aborts the current search pattern

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed.
   --  In such a case, we cancel any pending search, since the list of
   --  source files might have changed and thus we need to restart from
   --  scratch.

   function Key_Press
     (Vsearch : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the pattern field.

   function Key_Press_Replace
     (Vsearch : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the replacement field.

   procedure Register_Default_Search
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the default search function

   function Get_Nth_Search_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Num    : Positive) return Search_Module_Data;
   --  Return the Num-th registered module, or No_Search if there is no such
   --  module.

   function Find_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Label  : String) return Search_Module_Data;
   --  Search the list of registered search functions for a matching module.
   --  No_Search is returned if no such module was found.

   function Get_Or_Create_Vsearch
     (Kernel        : access Kernel_Handle_Record'Class;
      Raise_Widget  : Boolean := False;
      Float_Widget  : Boolean := True;
      Reset_Entries : Boolean := False;
      Context       : String_Access := null) return Vsearch_Access;
   --  Return a valid vsearch widget, creating one if necessary.
   --  If Reset_Entries is True, the fields in the dialog are reset depending
   --  on the current module. Context indicates the value that should be
   --  set for "Look In". If null, this will be set based on the current module

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure Create_Context
     (Vsearch : access Vsearch_Record'Class; All_Occurrences : Boolean);
   --  Create the search context based on the current contents of the GUI.
   --  This is stored in Vsearch.Last_Search_Context or
   --  Vsearch.Last_Search_All_Context before being stored in
   --  Idle_Search_Data.Context.

   function On_Delete (Search : access Gtk_Widget_Record'Class)
                       return Boolean;
   --  Called when the search widget is about to be destroyed.

   procedure Replace_Text_Changed
     (Vsearch : access Gtk_Widget_Record'Class);
   --  Called when the contents of the "Replace" field has changed

   procedure Resize_If_Needed (Vsearch : access Vsearch_Record'Class);
   --  Resize the vsearch window if needed.

   procedure Internal_Search
     (Vsearch         : Vsearch_Access;
      All_Occurrences : Boolean := False;
      Replace         : Boolean := False);
   --  Internal implementation of search capability, used by On_Search and
   --  On_Search_Replace.

   procedure Receive_Text
     (Clipboard : Gtk_Clipboard;
      Text      : Interfaces.C.Strings.chars_ptr;
      Data      : System.Address);
   pragma Convention (C, Receive_Text);
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

   procedure On_Context_Combo_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the entry "Look in" is changed.

   procedure Search_Next_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for menu Edit->Search Next

   procedure Search_Previous_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for menu Edit->Search Previous

   procedure New_Predefined_Regexp
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when a new predefined regexp has been added to the kernel.

   procedure Selection_Changed (Vsearch : access Gtk_Widget_Record'Class);
   --  Called when the selected pattern has changed, to reflect the settings
   --  for the predefined patterns

   procedure Search_Functions_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the list of registered search functions has changed.

   procedure Close_Vsearch (Search : access Gtk_Widget_Record'Class);
   --  Called when the search widget should be closed

   procedure Float_Vsearch (Search_Child : access Gtk_Widget_Record'Class);
   --  The floating state of the search widget has changed

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Store_Position (Vsearch : Vsearch_Access);
   procedure Restore_Position (Vsearch : Vsearch_Access);
   --  Store and restore the position of the Vsearch dialog.

   ----------------------
   -- Restore_Position --
   ----------------------

   procedure Restore_Position (Vsearch : Vsearch_Access) is
      Child   : constant MDI_Child :=
        Find_MDI_Child (Get_MDI (Vsearch.Kernel), Vsearch);
      Hist : String_List_Access;
      X, Y : Gint;
      Win  : Gtk_Widget;
   begin
      Win := Get_Toplevel (Get_Widget (Child));

      Hist := Get_History
        (Get_History (Vsearch.Kernel).all, Window_X_Hist_Key);

      if Hist = null then
         return;
      end if;

      X := Gint'Value (Hist (Hist'First).all);

      Hist := Get_History
        (Get_History (Vsearch.Kernel).all, Window_Y_Hist_Key);

      if Hist = null then
         return;
      end if;

      Y := Gint'Value (Hist (Hist'First).all);

      Set_UPosition (Win, X, Y);
   end Restore_Position;

   --------------------
   -- Store_Position --
   --------------------

   procedure Store_Position (Vsearch : Vsearch_Access) is
      Child   : constant MDI_Child :=
        Find_MDI_Child (Get_MDI (Vsearch.Kernel), Vsearch);

      Win  : Gtk_Widget;
      X, Y : Gint;
   begin
      if Is_Floating (Child) then
         --  Store the position of the floating window

         Win := Get_Toplevel (Get_Widget (Child));
         Get_Root_Origin (Get_Window (Win), X, Y);

         Add_To_History
           (Get_History (Vsearch.Kernel).all, Window_X_Hist_Key,
            Gint'Image (X));

         Add_To_History
           (Get_History (Vsearch.Kernel).all, Window_Y_Hist_Key,
            Gint'Image (Y));
      end if;
   end Store_Position;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      Extended : Vsearch_Access;
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Vsearch" then
         Extended := Get_Or_Create_Vsearch
           (User, Raise_Widget => False, Float_Widget => False);

         return Find_MDI_Child (Get_MDI (User), Extended);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      N : Node_Ptr;
      Extended : Vsearch_Access;
   begin
      if Widget.all in Vsearch_Record'Class then
         Extended := Vsearch_Access (Widget);

         --  We do not want floating search to appear automatically next time
         --  gps is started
         if Get_State (Find_MDI_Child (Get_MDI (User), Extended)) /=
           Gtkada.MDI.Floating
         then
            N := new Node;
            N.Tag := new String'("Vsearch");
            return N;
         end if;
      end if;

      return null;
   end Save_Desktop;

   -------------------
   -- Float_Vsearch --
   -------------------

   procedure Float_Vsearch (Search_Child : access Gtk_Widget_Record'Class) is
      Child   : constant MDI_Child := MDI_Child (Search_Child);
      Vsearch : constant Vsearch_Access := Vsearch_Access (Get_Widget (Child));
      Close_Button : Gtk_Button;
   begin
      if Is_Floating (Child) then
         Set_Resizable (Gtk_Dialog (Get_Toplevel (Vsearch)), True);

         if Vsearch.Scrolled /= null then
            Ref (Vsearch.Scrolled);
            Vsearch.Remove (Vsearch.Scrolled);
            Vsearch.View.Reparent (Vsearch);
            Unref (Vsearch.Scrolled);
            Vsearch.Scrolled := null;
         end if;
         Vsearch.Set_Border_Width (0);

         --  Add the "Close" button.
         Close_Button := Gtk_Button
           (Add_Button (Gtk_Dialog (Get_Toplevel (Vsearch)),
            Stock_Close,
            Gtk_Response_Cancel));

         Show_All (Vsearch.Auto_Hide_Check);
         Set_Child_Visible (Vsearch.Auto_Hide_Check, True);

         Widget_Callback.Object_Connect
           (Close_Button, Signal_Clicked, Close_Vsearch'Access, Vsearch);

         --  Set the position of the floating window
         Restore_Position (Vsearch);
         Resize_If_Needed (Vsearch);
      else
         --  Create a scrolled window and put vsearch's content in it
         if Vsearch.Scrolled = null then
            Gtk_New (Vsearch.Scrolled);
            Vsearch.Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
            Show (Vsearch.Scrolled);

            Ref (Vsearch.View);
            Vsearch.View.Reparent (Vsearch.Scrolled);
            Vsearch.Add (Vsearch.Scrolled);
            Unref (Vsearch.View);
         end if;

         Hide_All (Vsearch.Auto_Hide_Check);
         Set_Child_Visible (Vsearch.Auto_Hide_Check, False);

         --  Store the position of the floating window
         Store_Position (Vsearch);
         Set_Size_Request (Vsearch, -1, -1);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Float_Vsearch;

   -------------------
   -- Close_Vsearch --
   -------------------

   procedure Close_Vsearch (Search : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Search);
   begin
      Store_Position (Vsearch);
      Close (Get_MDI (Vsearch.Kernel), Search, Force => True);

      --  Give the focus back to the main Window, since this is not always
      --  done by the window manager (e.g. under Windows)

      Gdk_Raise (Get_Window (Get_Main_Window (Vsearch.Kernel)));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Close_Vsearch;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Search_Module_Data) is
   begin
      if Data.Extra_Information /= null then
         Unref (Data.Extra_Information);
      end if;

      Free (Data.Label);
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

      Search_Modules_List.Free (Module.Search_Modules);
      Unref (Module.Next_Menu_Item);
      Unref (Module.Prev_Menu_Item);
   end Destroy;

   --------------------
   -- Search_Iterate --
   --------------------

   procedure Search_Iterate
     (Data    : in out Idle_Search_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Vsearch  : constant Vsearch_Access := Data.Vsearch;
      Button   : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

      Found    : Boolean;
      Continue : Boolean;
   begin
      Search
        (Data.Context,
         Data.Vsearch.Kernel,
         Data.Search_Backward,
         Give_Focus => Get_Active (Data.Vsearch.Select_Editor_Check),
         Found      => Found,
         Continue   => Continue);

      Data.Found := Data.Found or else Found;

      if Continue then
         Set_Progress
           (Command,
            (Running,
             Get_Current_Progress (Data.Context),
             Get_Total_Progress (Data.Context)));
         Result := Execute_Again;
         return;
      end if;

      Set_Sensitive (Data.Vsearch.Search_Next_Button, True);
      Pop_State (Data.Vsearch.Kernel);
      Data.Vsearch.Search_Idle_Handler := 0;

      if not Data.Found then
         Button := Message_Dialog
           (Msg     => "No occurrences of '" &
            Context_Look_For (Data.Context) & "' found."
            & ASCII.LF & "in "
            & Context_Look_In (Data.Context.all),
            Title   => -"Search",
            Buttons => Button_OK,
            Parent  => Get_Main_Window (Vsearch.Kernel));
      end if;

      Free (Data.Context);
      Result := Success;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Data.Vsearch.Kernel);
         Data.Vsearch.Search_Idle_Handler := 0;

         Result := Success;
         return;
   end Search_Iterate;

   ---------------------
   -- Replace_Iterate --
   ---------------------

   procedure Replace_Iterate
     (Data    : in out Idle_Search_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type) is
   begin
      if Replace
        (Data.Context,
         Data.Vsearch.Kernel,
         Data.Replace_With.all,
         Data.Case_Preserving,
         Data.Search_Backward,
         Give_Focus => Get_Active (Data.Vsearch.Select_Editor_Check))
      then
         Set_Progress
           (Command,
            (Running,
             Get_Current_Progress (Data.Context),
             Get_Total_Progress (Data.Context)));

         Result := Execute_Again;
      else
         Insert
           (Data.Vsearch.Kernel,
            Get_Terminate_Message (Data.Context, Replace));
         Free (Data.Context);
         Free (Data.Replace_With);
         Set_Sensitive (Data.Vsearch.Search_Next_Button, True);
         Pop_State (Data.Vsearch.Kernel);
         Data.Vsearch.Search_Idle_Handler := 0;
         Result := Success;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Free (Data.Replace_With);
         Pop_State (Data.Vsearch.Kernel);
         Data.Vsearch.Search_Idle_Handler := 0;
         Result := Success;
   end Replace_Iterate;

   --------------------
   -- Create_Context --
   --------------------

   procedure Create_Context
     (Vsearch : access Vsearch_Record'Class; All_Occurrences : Boolean)
   is
      use Widget_List;
      Data    : constant Search_Module_Data :=
                  Find_Module
                    (Vsearch.Kernel, Get_Active_Text (Vsearch.Context_Combo));
      Pattern : constant String := Get_Active_Text (Vsearch.Pattern_Combo);
      Iter    : Gtk_Tree_Iter;
      List    : Gtk_List_Store;
      Options : Search_Options;
   begin
      if not All_Occurrences then
         Free (Vsearch.Last_Search_Context);
      end if;

      if Data.Factory /= null and then Pattern /= "" then
         if All_Occurrences then
            Vsearch.Last_Search_All_Context := Data.Factory
              (Vsearch.Kernel, All_Occurrences, Data.Extra_Information);
         else
            Vsearch.Last_Search_Context := Data.Factory
              (Vsearch.Kernel, All_Occurrences, Data.Extra_Information);
         end if;

         if (All_Occurrences and then Vsearch.Last_Search_All_Context /= null)
           or else Vsearch.Last_Search_Context /= null
         then
            Options :=
              (Case_Sensitive => Get_Active (Vsearch.Case_Check),
               Whole_Word     => Get_Active (Vsearch.Whole_Word_Check),
               Regexp         => Get_Active (Vsearch.Regexp_Check));

            if All_Occurrences then
               Set_Context (Vsearch.Last_Search_All_Context, Pattern, Options);
               Reset (Vsearch.Last_Search_All_Context, Vsearch.Kernel);
            else
               Set_Context (Vsearch.Last_Search_Context, Pattern, Options);
               Reset (Vsearch.Last_Search_Context, Vsearch.Kernel);
            end if;
         else
            Insert (Vsearch.Kernel, -"Invalid search context specified");
            return;
         end if;
      end if;

      --  Update the contents of the combo boxes
      if Get_Active_Iter (Vsearch.Pattern_Combo) = Null_Iter then
         Iter := Add_Unique_Combo_Entry
           (Vsearch.Pattern_Combo, Pattern, Prepend => True);
         List := Gtk_List_Store (Get_Model (Vsearch.Pattern_Combo));
         List.Set (Iter, Column_Pattern, Pattern);
         List.Set (Iter, Column_Case_Sensitive, Options.Case_Sensitive);
         List.Set (Iter, Column_Is_Regexp, Options.Regexp);
         Add_To_History
           (Get_History (Vsearch.Kernel).all, Pattern_Hist_Key, Pattern);

         --  It sometimes happens that another entry is selected, for some
         --  reason. This also resets the options to the unwanted selection,
         --  so we also need to override them.
         Set_Active_Text (Vsearch.Pattern_Combo, Pattern);
         Set_Active (Vsearch.Case_Check, Options.Case_Sensitive);
         Set_Active (Vsearch.Whole_Word_Check, Options.Whole_Word);
         Set_Active (Vsearch.Regexp_Check, Options.Regexp);
      end if;

      declare
         Replace_Text : constant String :=
                          Get_Active_Text (Vsearch.Replace_Combo);
      begin
         Add_Unique_Combo_Entry
           (Vsearch.Replace_Combo, Replace_Text, Prepend => True);
         Add_To_History
           (Get_History (Vsearch.Kernel).all, Replace_Hist_Key, Replace_Text);
         Set_Active_Text (Vsearch.Replace_Combo, Replace_Text);
      end;
   end Create_Context;

   ---------------------
   -- Internal_Search --
   ---------------------

   procedure Internal_Search
     (Vsearch         : Vsearch_Access;
      All_Occurrences : Boolean := False;
      Replace         : Boolean := False)
   is
      Data     : constant Search_Module_Data :=
                   Find_Module
                     (Vsearch.Kernel, Get_Active_Text (Vsearch.Context_Combo));
      Toplevel : Gtk_Widget := Get_Toplevel (Vsearch);
      Found    : Boolean;
      Has_Next : Boolean;
      Button   : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

      Pattern  : constant String := Get_Active_Text (Vsearch.Pattern_Combo);
      C        : Search_Commands.Generic_Asynchronous_Command_Access;

      Search_Category : constant String :=
                          -"Search for: " &
                          Vsearch.Pattern_Combo.Get_Active_Text;
   begin
      if All_Occurrences then
         --  If there is already a search going on for this category, do not
         --  launch a new one.
         if Has_Queue (Vsearch.Kernel, Search_Category) then
            return;
         end if;

         Vsearch.Find_Next := False;
      end if;

      if Gtk_Widget (Vsearch) = Toplevel then
         Toplevel := Gtk_Widget (Get_Main_Window (Vsearch.Kernel));
      end if;

      if not Vsearch.Find_Next then
         Create_Context (Vsearch, All_Occurrences);
      end if;

      if (All_Occurrences and then Vsearch.Last_Search_All_Context /= null)
        or else Vsearch.Last_Search_Context /= null
      then
         if All_Occurrences then
            --  Set up the search. Everything is automatically
            --  put back when the idle loop terminates.
            --  ??? What is that supposed to mean.

            Search_Commands.Create
              (C,
               Search_Category,
               (Vsearch         => Vsearch,
                Search_Backward => False,
                Context         => Vsearch.Last_Search_All_Context,
                Case_Preserving => Get_Active
                  (Vsearch.Case_Preserving_Replace),
                Found           => False,
                Replace_With    => null),
               Search_Iterate'Access);

            Launch_Background_Command
              (Vsearch.Kernel, Command_Access (C), True, True,
               Search_Category);

         else
            Push_State (Vsearch.Kernel, Processing);

            Search
              (Vsearch.Last_Search_Context,
               Vsearch.Kernel,
               Search_Backward => False,
               Give_Focus      => Get_Active (Vsearch.Select_Editor_Check),
               Found           => Found,
               Continue        => Has_Next);

            Pop_State (Vsearch.Kernel);

            if not Found then
               Set_Sensitive
                 (Vsearch.Replace_Search_Button, False);
               Set_Sensitive (Vsearch.Replace_Button, False);
            else
               if (Data.Mask and Supports_Replace) /= 0 then
                  Set_Sensitive
                    (Vsearch.Replace_Search_Button, True);
                  Set_Sensitive (Vsearch.Replace_Button, True);
               end if;
            end if;

            --  Give a visual feedback that the search is terminated.
            if not Found
              and then not Has_Next
              and then not Get_End_Notif_Done (Vsearch.Last_Search_Context.all)
            then
               Stop_Macro (Vsearch.Kernel);
               Button := Message_Dialog
                 (Msg     => (-"No occurrences of '") & Pattern &
                  (-"' found in") & ASCII.LF
                  & Context_Look_In (Vsearch.Last_Search_Context.all),
                  Title   => -"Search",
                  Buttons => Button_OK,
                  Parent  => Gtk_Window (Toplevel));
               Set_End_Notif_Done (Vsearch.Last_Search_Context.all, True);
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
        and then Realized_Is_Set (Vsearch)
        and then Get_Child_Visible (Vsearch.Auto_Hide_Check)
        and then Get_Active (Vsearch.Auto_Hide_Check)
      then
         Close_Vsearch (Vsearch);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Vsearch.Kernel);
   end Internal_Search;

   ---------------
   -- On_Search --
   ---------------

   procedure On_Search (Object : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Object);
   begin
      if Vsearch.Last_Search_Context /= null then
         Set_End_Notif_Done (Vsearch.Last_Search_Context.all, False);
      end if;

      Internal_Search (Vsearch_Access (Object));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Search;

   ------------------------
   -- On_Search_Previous --
   ------------------------

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class) is
      Vsearch        : constant Vsearch_Access := Vsearch_Access (Object);
      All_Occurences : constant Boolean := False;
      Found          : Boolean;
      Has_Next       : Boolean;

   begin
      if Vsearch.Last_Search_Context /= null then
         Set_End_Notif_Done (Vsearch.Last_Search_Context.all, False);
      end if;

      if not Vsearch.Find_Next then
         Create_Context (Vsearch, False);
      end if;

      if Vsearch.Last_Search_Context /= null then
         if All_Occurences then
            --  Is this supported ???
            return;

         else
            Push_State (Vsearch.Kernel, Processing);
            Search
              (Vsearch.Last_Search_Context,
               Vsearch.Kernel,
               Search_Backward => True,
               Give_Focus      => Get_Active (Vsearch.Select_Editor_Check),
               Found           => Found,
               Continue        => Has_Next);
            Pop_State (Vsearch.Kernel);

            if not Found then
               Stop_Macro (Vsearch.Kernel);
               Set_Sensitive
                 (Vsearch.Replace_Search_Button, False);
               Set_Sensitive (Vsearch.Replace_Button, False);
            else
               Set_Sensitive
                 (Vsearch.Replace_Search_Button, True);
               Set_Sensitive (Vsearch.Replace_Button, True);
            end if;

            Set_First_Next_Mode (Vsearch, Find_Next => True);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Search_Previous;

   -------------------
   -- On_Search_All --
   -------------------

   procedure On_Search_All (Object : access Gtk_Widget_Record'Class) is
   begin
      Internal_Search (Vsearch_Access (Object), True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Search_All;

   ----------------
   -- On_Replace --
   ----------------

   procedure On_Replace (Object : access Gtk_Widget_Record'Class) is
      Vsearch     : constant Vsearch_Access := Vsearch_Access (Object);
      Has_Next    : Boolean;
      pragma Unreferenced (Has_Next);

   begin
      if Vsearch.Last_Search_Context /= null then
         Set_End_Notif_Done (Vsearch.Last_Search_Context.all, False);
      end if;

      Push_State (Vsearch.Kernel, Processing);
      Has_Next := Replace
        (Vsearch.Last_Search_Context,
         Vsearch.Kernel,
         Get_Active_Text (Vsearch.Replace_Combo),
         Get_Active (Vsearch.Case_Preserving_Replace),
         Search_Backward => False,
         Give_Focus => Get_Active (Vsearch.Select_Editor_Check));
      Pop_State (Vsearch.Kernel);

      Set_Sensitive (Vsearch.Replace_Button, False);
      Set_Sensitive (Vsearch.Replace_Search_Button, False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Replace;

   -----------------------
   -- On_Replace_Search --
   -----------------------

   procedure On_Replace_Search (Object : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Object);
      Has_Next    : Boolean;
      pragma Unreferenced (Has_Next);
   begin
      if Vsearch.Last_Search_Context /= null then
         Set_End_Notif_Done (Vsearch.Last_Search_Context.all, False);
      end if;

      Push_State (Vsearch.Kernel, Processing);

      Has_Next := Replace
        (Vsearch.Last_Search_Context,
         Vsearch.Kernel,
         Get_Active_Text (Vsearch.Replace_Combo),
         Get_Active (Vsearch.Case_Preserving_Replace),
         Search_Backward => False,
         Give_Focus => Get_Active (Vsearch.Select_Editor_Check));
      Pop_State (Vsearch.Kernel);

      On_Search (Object);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Replace_Search;

   --------------------
   -- On_Replace_All --
   --------------------

   procedure On_Replace_All (Object : access Gtk_Widget_Record'Class) is
      Vsearch     : constant Vsearch_Access := Vsearch_Access (Object);
      Has_Next    : Boolean;
      pragma Unreferenced (Has_Next);

      C           : Search_Commands.Generic_Asynchronous_Command_Access;
      Button      : Gtk_Widget;
      pragma Unreferenced (Button);

      Dialog : constant Gtk_Dialog := Create_Gtk_Dialog
        (Msg      => (-"You are about to replace all occurrences of """)
         & Get_Active_Text (Vsearch.Pattern_Combo) & """."
         & ASCII.LF & (-"Continue?"),
         Dialog_Type => Warning,
         Title       => -"Replacing all occurrences",
         Parent      => Gtk_Window (Get_Toplevel (Vsearch)));

      Do_Not_Ask : Gtk_Check_Button;
      Box        : Gtk_Hbox;

      Response : Gtk_Response_Type;

   begin
      if Ask_Confirmation_For_Replace_All.Get_Pref then
         Gtk_New (Do_Not_Ask, -"Do not ask this question again");
         Gtk_New_Hbox (Box);
         Pack_Start (Get_Vbox (Dialog), Box, True, True, 3);
         Pack_Start (Box, Do_Not_Ask, True, False, 3);

         Button := Add_Button
           (Dialog,
            Text => "gtk-yes",
            Response_Id => Gtk_Response_Yes);

         Button := Add_Button
           (Dialog,
            Text => "gtk-no",
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

      Create_Context (Vsearch, True);

      Search_Commands.Create
        (C,
         -"Searching/Replacing",
         (Vsearch         => Vsearch,
          Search_Backward => False,
          Context         => Vsearch.Last_Search_All_Context,
          Case_Preserving => Get_Active (Vsearch.Case_Preserving_Replace),
          Found           => False,
          Replace_With    =>
            new String'(Get_Active_Text (Vsearch.Replace_Combo))),
         Replace_Iterate'Access);

      Launch_Background_Command
        (Vsearch.Kernel, Command_Access (C), True, True, "Search");

      Set_Sensitive (Vsearch.Replace_Button, False);
      Set_Sensitive (Vsearch.Replace_Search_Button, False);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Replace_All;

   ----------------------
   -- Resize_If_Needed --
   ----------------------

   procedure Resize_If_Needed
     (Vsearch : access Vsearch_Record'Class)
   is
      Child : constant MDI_Child := Find_MDI_Child
        (Get_MDI (Vsearch.Kernel), Vsearch);
      Req : Gtk_Requisition;
   begin
      if Child /= null then
         if Is_Floating (Child) then
            --  Reset if any size was set previously
            Vsearch.Set_Size_Request (-1, -1);
            Size_Request (Vsearch, Req);
            Vsearch.Set_Size_Request (Req.Width, Req.Height);
         end if;
      end if;
   end Resize_If_Needed;

   ------------------------------
   -- On_Context_Entry_Changed --
   ------------------------------

   procedure On_Context_Combo_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      Vsearch : constant Vsearch_Access := Vsearch_Access (Object);
      Data    : constant Search_Module_Data :=
                  Find_Module
                    (Vsearch.Kernel, Get_Active_Text (Vsearch.Context_Combo));
      Replace : Boolean;

   begin
      if Data /= No_Search then
         Set_Last_Of_Module (Vsearch.Kernel, Data);
         Replace := (Data.Mask and Supports_Replace) /= 0;
         Set_Sensitive (Vsearch.Replace_Label, Replace);
         Set_Sensitive (Vsearch.Replace_Combo, Replace);
         Set_Sensitive (Vsearch.Replace_All_Button, Replace);
         Set_Sensitive (Vsearch.Case_Preserving_Replace, Replace);

         if (Data.Mask and All_Occurrences) = 0 then
            Set_Sensitive (Vsearch.Replace_All_Button, False);
            Set_Sensitive (Vsearch.Search_All_Button, False);
         else
            Set_Sensitive (Vsearch.Search_All_Button, True);
         end if;

         if (Data.Mask and Case_Sensitive) = 0 then
            Set_Active (Vsearch.Case_Check, False);
         end if;

         if (Data.Mask and Whole_Word) = 0 then
            Set_Active (Vsearch.Whole_Word_Check, False);
         end if;

         if (Data.Mask and Find_Utils.Regexp) = 0 then
            Set_Active (Vsearch.Regexp_Check, False);
         end if;

         Set_Sensitive
           (Vsearch.Case_Check, (Data.Mask and Case_Sensitive) /= 0);
         Set_Sensitive
           (Vsearch.Whole_Word_Check, (Data.Mask and Whole_Word) /= 0);
         Set_Sensitive (Vsearch.Regexp_Check,
                        (Data.Mask and Find_Utils.Regexp) /= 0);

         Set_Sensitive (Vsearch.Search_Previous_Button,
                        (Data.Mask and Search_Backward) /= 0);

         if Vsearch.Extra_Information /= null then
            Remove (Vsearch.Context_Specific, Vsearch.Extra_Information);
            Vsearch.Extra_Information := null;
         end if;

         if Data.Extra_Information /= null then
            Pack_Start (Vsearch.Context_Specific,
                        Data.Extra_Information, Expand => False);
            Show_All (Vsearch.Context_Specific);
            Vsearch.Extra_Information := Data.Extra_Information;
         end if;

         Resize_If_Needed (Vsearch);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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

         Set_Sensitive (Vsearch_Module_Id.Next_Menu_Item, True);
         Set_Sensitive (Vsearch_Module_Id.Prev_Menu_Item, True);

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

         Set_Sensitive (Vsearch_Module_Id.Next_Menu_Item, False);
         Set_Sensitive (Vsearch_Module_Id.Prev_Menu_Item, False);
      end if;
   end Set_First_Next_Mode;

   ----------------------------
   -- Set_First_Next_Mode_Cb --
   ----------------------------

   procedure Set_First_Next_Mode_Cb
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      --  We might be in the process of destroying GPS (for instance, the
      --  current search context detects that the current MDI_Child was
      --  destroyed, and resets the context).
      if Vsearch_Module_Id.Search /= null then
         Set_First_Next_Mode (Vsearch_Module_Id.Search, Find_Next => False);
         Set_Sensitive (Vsearch_Module_Id.Search.Replace_Search_Button, False);
         Set_Sensitive (Vsearch_Module_Id.Search.Replace_Button, False);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Set_First_Next_Mode_Cb;

   -----------------------------
   -- On_Project_View_Changed --
   -----------------------------

   procedure On_Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Reset_Search (null, Kernel_Handle (Kernel));
   end On_Project_View_Changed;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Vsearch : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean
   is
      Ext : constant Vsearch_Access := Vsearch_Access (Vsearch);
   begin
      if Get_Key_Val (Event) = GDK_Return
        or else Get_Key_Val (Event) = GDK_KP_Enter
      then
         if Is_Sensitive (Ext.Search_Next_Button) then
            Grab_Focus (Ext.Search_Next_Button);
            On_Search (Vsearch);
         else
            Grab_Focus (Ext.Replace_Search_Button);
            On_Replace (Ext);
         end if;
         return True;
      end if;
      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Key_Press;

   --------------------------
   -- Replace_Text_Changed --
   --------------------------

   procedure Replace_Text_Changed
     (Vsearch : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Vsearch);
   begin
      null;
      --  Reset_Search (Vsearch, Vsearch_Access (Vsearch).Kernel);
   end Replace_Text_Changed;

   -----------------------
   -- Key_Press_Replace --
   -----------------------

   function Key_Press_Replace
     (Vsearch : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean
   is
      Ext : constant Vsearch_Access := Vsearch_Access (Vsearch);
   begin
      if Get_Key_Val (Event) = GDK_Return
        or else Get_Key_Val (Event) = GDK_KP_Enter
      then
         Grab_Focus (Ext.Replace_Search_Button);
         On_Replace_Search (Ext);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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

   begin
      if Get_Active_Iter (Search.Pattern_Combo) /= Null_Iter then
         Iter := Get_Active_Iter (Search.Pattern_Combo);
         Case_Sensitive := Search.Pattern_Combo.Get_Model.Get_Boolean
           (Iter, Column_Case_Sensitive);
         Is_Regexp := Search.Pattern_Combo.Get_Model.Get_Boolean
           (Iter, Column_Is_Regexp);

         Set_Active (Search.Case_Check, Case_Sensitive);
         Set_Active (Search.Regexp_Check, Is_Regexp);
      end if;

   exception
      when Gtkada.Types.Data_Error =>
         null;
      when E : others => Trace (Exception_Handle, E);
   end Selection_Changed;

   ---------------------------
   -- New_Predefined_Regexp --
   ---------------------------

   procedure New_Predefined_Regexp
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Search  : constant Vsearch_Access := Vsearch_Module_Id.Search;
      Item    : Gtk_Tree_Iter;
      List    : Gtk_List_Store;
      Is_Regexp, Case_Sensitive : Boolean;
      Options : constant Search_Options :=
        (Case_Sensitive => Get_Active (Search.Case_Check),
         Whole_Word     => Get_Active (Search.Whole_Word_Check),
         Regexp         => Get_Active (Search.Regexp_Check));

   begin
      for S in 1 .. Search_Regexps_Count (Kernel) loop
         Item := Add_Unique_Combo_Entry
           (Search.Pattern_Combo,
            Get_Nth_Search_Regexp_Name (Kernel, S));
         Get_Nth_Search_Regexp_Options
           (Kernel, S,
            Case_Sensitive => Case_Sensitive,
            Is_Regexp => Is_Regexp);

         List := Gtk_List_Store (Get_Model (Search.Pattern_Combo));
         List.Set (Item, Column_Pattern, Get_Nth_Search_Regexp (Kernel, S));
         List.Set (Item, Column_Case_Sensitive, Case_Sensitive);
         List.Set (Item, Column_Is_Regexp, Is_Regexp);
      end loop;

      --  Restore the options as before (they might have changed depending
      --  on the last predefined regexp we inserted)

      Set_Active (Search.Case_Check, Options.Case_Sensitive);
      Set_Active (Search.Whole_Word_Check, Options.Whole_Word);
      Set_Active (Search.Regexp_Check, Options.Regexp);
   end New_Predefined_Regexp;

   ------------------------------
   -- Search_Functions_Changed --
   ------------------------------

   procedure Search_Functions_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Vsearch : constant Vsearch_Access := Vsearch_Module_Id.Search;
      Num     : Positive := 1;
      Last_Id : Module_ID;
   begin
      loop
         declare
            Data : constant Search_Module_Data :=
              Get_Nth_Search_Module (Kernel, Num);
         begin
            exit when Data = No_Search;

            Add_Unique_Combo_Entry (Vsearch.Context_Combo, Data.Label.all);

            Last_Id := Data.Id;
            Num := Num + 1;
         end;
      end loop;

      Add_Unique_Combo_Entry
        (Vsearch.Context_Combo,
         Search_Context_From_Module (Last_Id, Kernel).Label.all,
         Select_Text => True);
   end Search_Functions_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (New_Vsearch : out Vsearch_Access;
      Handle      : GPS.Kernel.Kernel_Handle) is
   begin
      New_Vsearch := new Vsearch_Record;
      Vsearch.Initialize (New_Vsearch, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Vsearch : access Vsearch_Record'Class;
      Handle  : GPS.Kernel.Kernel_Handle)
   is
      Model       : Gtk_List_Store;
      Layout      : Gtk_Cell_Layout;
      Renderer    : Gtk_Cell_Renderer_Text;
      Value       : String_List_Access;
      Iter        : Gtk_Tree_Iter;
      Options_Box : Gtk_Frame;
      Scope_Box   : Gtk_Frame;
      Alignment   : Gtk_Alignment;

   begin
      Vsearch.Kernel := Handle;

      Gtk.Box.Initialize_Vbox (Vsearch);
      Vsearch.Set_Border_Width (0);

      Gtk_New (Vsearch.View);
      Vsearch.View.Set_Border_Width (0);
      Vsearch.View.Set_Shadow_Type (Shadow_None);
      Vsearch.Add (Vsearch.View);

      Gtk_New_Vbox (Vsearch.Main, False, 0);
      Vsearch.Main.Set_Border_Width (5);
      Vsearch.View.Add (Vsearch.Main);

      --  Find/Replace combo boxes

      Gtk_New (Vsearch.Table, 2, 2, False);
      Pack_Start (Vsearch.Main, Vsearch.Table, False, False, 0);

      Gtk_New (Vsearch.Search_For_Label, -("Find:"));
      Set_Alignment (Vsearch.Search_For_Label, 0.0, 0.5);
      Attach (Vsearch.Table, Vsearch.Search_For_Label, 0, 1, 0, 1, Fill);

      Gtk_New (Vsearch.Replace_Label, -("Replace:"));
      Set_Alignment (Vsearch.Replace_Label, 0.0, 0.5);
      Attach (Vsearch.Table, Vsearch.Replace_Label, 0, 1, 1, 2, Fill);

      Gtk.List_Store.Gtk_New (Model, (0 .. 0 => GType_String));

      Gtk_New_With_Model_And_Entry (Vsearch.Replace_Combo, Model);
      Vsearch.Replace_Combo.Set_Entry_Text_Column (0);
      Attach
        (Vsearch.Table, Vsearch.Replace_Combo, 1, 2, 1, 2,
         Xpadding => 0,
         Ypadding => 2);
      Set_Tooltip_Text (Vsearch.Replace_Combo,
               -"The text that will replace each match");

      Gtk.List_Store.Gtk_New
        (Model,
         (Guint (Column_Text)           => GType_String,
          Guint (Column_Pattern)        => GType_String,
          Guint (Column_Case_Sensitive) => GType_Boolean,
          Guint (Column_Is_Regexp)      => GType_Boolean));
      Gtk_New_With_Model_And_Entry (Vsearch.Pattern_Combo, Model);
      Vsearch.Pattern_Combo.Set_Entry_Text_Column (Column_Pattern);
      Attach
        (Vsearch.Table, Vsearch.Pattern_Combo, 1, 2, 0, 1,
         Xpadding => 0,
         Ypadding => 2);
      Layout := Implements_Cell_Layout.To_Interface (Vsearch.Pattern_Combo);

      Gtk_New (Renderer);
      Gtk.Cell_Layout.Clear (Layout);
      Gtk.Cell_Layout.Pack_Start (Layout, Renderer, True);
      Gtk.Cell_Layout.Add_Attribute (Layout, Renderer, "text", Column_Text);
      Set_Tooltip_Text (Vsearch.Pattern_Combo,
                        -"The searched word or pattern");

      --  The buttons

      Gtk.Alignment.Gtk_New (Alignment, 0.5, 0.0, 0.0, 0.0);
      Pack_Start (Vsearch.Main, Alignment, False, False, 5);

      Gtk_New (Vsearch.Buttons_Table, 2, 3, False);
      Set_Row_Spacings (Vsearch.Buttons_Table, 3);
      Set_Col_Spacings (Vsearch.Buttons_Table, 3);
      Alignment.Add (Vsearch.Buttons_Table);

      Gtk_New_With_Mnemonic (Vsearch.Search_Next_Button, -"_Find");
      Set_First_Next_Mode (Vsearch, Find_Next => False);
      Attach
        (Vsearch.Buttons_Table, Vsearch.Search_Next_Button, 0, 1, 0, 1, Fill);
      Set_Tooltip_Text
        (Vsearch.Search_Next_Button,
         -"Search next occurrence");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Next_Button, Signal_Clicked,
         On_Search'Access, Vsearch);

      Gtk_New_With_Mnemonic (Vsearch.Search_Previous_Button, -"_Previous");
      Attach
        (Vsearch.Buttons_Table,
         Vsearch.Search_Previous_Button, 1, 2, 0, 1, Fill);
      Set_Tooltip_Text
        (Vsearch.Search_Previous_Button,
         -"Search previous occurrence");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Previous_Button, Signal_Clicked,
         On_Search_Previous'Access, Vsearch);

      Gtk_New_With_Mnemonic (Vsearch.Search_All_Button, -"Find All");
      Attach
        (Vsearch.Buttons_Table, Vsearch.Search_All_Button, 2, 3, 0, 1, Fill);
      Set_Tooltip_Text
        (Vsearch.Search_All_Button, -"Find all occurences");
      Widget_Callback.Object_Connect
        (Vsearch.Search_All_Button, Signal_Clicked,
         On_Search_All'Access, Vsearch);

      Gtk_New (Vsearch.Replace_Button, -"Replace");
      Attach
        (Vsearch.Buttons_Table,
         Vsearch.Replace_Button, 0, 1, 1, 2, Fill);
      Set_Tooltip_Text
        (Vsearch.Replace_Button,
         -"Replace next occurrence");
      Widget_Callback.Object_Connect
        (Vsearch.Replace_Button, Signal_Clicked,
         On_Replace'Access, Vsearch);
      Set_Sensitive (Vsearch_Access (Vsearch).Replace_Button, False);

      Gtk_New_With_Mnemonic (Vsearch.Replace_Search_Button, -"Replace & Find");
      Attach
        (Vsearch.Buttons_Table,
         Vsearch.Replace_Search_Button, 1, 2, 1, 2, Fill);
      Set_Tooltip_Text
        (Vsearch.Replace_Search_Button, -"Replace, then find next occurrence");
      Widget_Callback.Object_Connect
        (Vsearch.Replace_Search_Button, Signal_Clicked,
         On_Replace_Search'Access, Vsearch);

      Gtk_New_With_Mnemonic (Vsearch.Replace_All_Button, -"Repl All");
      Attach
        (Vsearch.Buttons_Table, Vsearch.Replace_All_Button, 2, 3, 1, 2, Fill);
      Set_Tooltip_Text
        (Vsearch.Replace_All_Button, -"Replace all occurences");
      Widget_Callback.Object_Connect
        (Vsearch.Replace_All_Button, Signal_Clicked,
         On_Replace_All'Access, Vsearch);

      --  Main (fixed) options

      Gtk_New (Options_Box, -"Options");
      Pack_Start (Vsearch.Main, Options_Box, False, False, 2);

      Gtk_New_Vbox (Vsearch.Options_Frame, Homogeneous => False);
      Vsearch.Options_Frame.Set_Border_Width (4);
      Add (Options_Box, Vsearch.Options_Frame);

      Gtk_New (Vsearch.Options_Vbox, 4, 2, False);
      Pack_Start (Vsearch.Options_Frame, Vsearch.Options_Vbox);

      Gtk_New (Vsearch.Regexp_Check, -"Regexp");
      Set_Tooltip_Text
        (Vsearch.Regexp_Check,
         -"The pattern is a regular expression");
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Handle).all, "regexp_search", False);
      Associate
        (Get_History (Handle).all, "regexp_search", Vsearch.Regexp_Check);
      Attach (Vsearch.Options_Vbox, Vsearch.Regexp_Check, 0, 1, 0, 1);

      Gtk_New (Vsearch.Whole_Word_Check, -"Whole Word");
      Set_Tooltip_Text
        (Vsearch.Whole_Word_Check,
         -("Select this if the pattern should only match a whole word, never"
           & " part of a word"));
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Handle).all, "whole_word_search", False);
      Associate
        (Get_History (Handle).all, "whole_word_search",
         Vsearch.Whole_Word_Check);
      Attach (Vsearch.Options_Vbox, Vsearch.Whole_Word_Check, 0, 1, 1, 2);

      Gtk_New (Vsearch.Case_Check, -"Case Sensitive");
      Set_Tooltip_Text
        (Vsearch.Case_Check,
         -("Select this to differenciate upper from lower casing in search"
           & " results"));
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Handle).all, "case_sensitive_search", False);
      Associate
        (Get_History (Handle).all, "case_sensitive_search",
         Vsearch.Case_Check);
      Attach (Vsearch.Options_Vbox, Vsearch.Case_Check, 0, 1, 2, 3);

      Gtk_New (Vsearch.Select_Editor_Check, -"Select on Match");
      Set_Tooltip_Text
        (Vsearch.Select_Editor_Check,
         Select_On_Match_Description);
      Associate
        (Hist   => Get_History (Handle).all,
         Key    => Select_On_Match_Hist_Key,
         Button => Vsearch.Select_Editor_Check);
      Attach (Vsearch.Options_Vbox, Vsearch.Select_Editor_Check, 1, 2, 0, 1);

      Gtk_New (Vsearch.Case_Preserving_Replace, -"Preserve Casing");
      Set_Tooltip_Text
        (Vsearch.Case_Preserving_Replace,
         -"Select this to preserve original word casing when replacing");
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Handle).all, "case_preserving_replace", True);
      Associate
        (Get_History (Handle).all, "case_preserving_replace",
         Vsearch.Case_Preserving_Replace);
      Attach
        (Vsearch.Options_Vbox, Vsearch.Case_Preserving_Replace, 1, 2, 1, 2);
      Set_Sensitive (Vsearch.Case_Preserving_Replace, False);

      Gtk_New (Vsearch.Auto_Hide_Check, -"Close on Match");
      Set_Tooltip_Text
        (Vsearch.Auto_Hide_Check,
         -Close_On_Match_Description);
      Associate
        (Hist   => Get_History (Handle).all,
         Key    => Close_On_Match_Hist_Key,
         Button => Vsearch.Auto_Hide_Check);
      Attach (Vsearch.Options_Vbox, Vsearch.Auto_Hide_Check,  1, 2, 2, 3);

      --  Context specific search

      Gtk_New (Scope_Box, -"Scope");
      Pack_Start (Vsearch.Main, Scope_Box, False, False, 2);

      Gtk_New_Vbox (Vsearch.Scope_Frame, Homogeneous => False);
      Vsearch.Scope_Frame.Set_Border_Width (4);
      Add (Scope_Box, Vsearch.Scope_Frame);

      Gtk_New_Text (Vsearch.Context_Combo);
      Set_Tooltip_Text (Vsearch.Context_Combo, -"The context of the search");
      Pack_Start (Vsearch.Scope_Frame, Vsearch.Context_Combo);
      Widget_Callback.Object_Connect
        (Vsearch.Context_Combo, Gtk.Combo_Box.Signal_Changed,
         On_Context_Combo_Changed'Access, Vsearch);

      Gtk_New_Vbox (Vsearch.Context_Specific, Homogeneous => False);
      Pack_Start (Vsearch.Scope_Frame, Vsearch.Context_Specific, False);

      --  Any change to the fields resets the search mode
      Return_Callback.Object_Connect
        (Vsearch.Pattern_Combo.Get_Child, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (Key_Press'Access), Vsearch);
      Return_Callback.Object_Connect
        (Vsearch.Replace_Combo.Get_Child, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (Key_Press_Replace'Access), Vsearch);
      Kernel_Callback.Connect
        (Vsearch.Pattern_Combo, Gtk.Combo_Box.Signal_Changed,
         Reset_Search'Access, Handle);
      Widget_Callback.Object_Connect
        (Vsearch.Replace_Combo, Gtk.Combo_Box.Signal_Changed,
         Replace_Text_Changed'Access, Vsearch);
      Kernel_Callback.Connect
        (Vsearch.Context_Combo, Gtk.Combo_Box.Signal_Changed,
         Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Case_Check,
         Gtk.Toggle_Button.Signal_Toggled, Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Case_Preserving_Replace,
         Gtk.Toggle_Button.Signal_Toggled, Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Whole_Word_Check, Gtk.Toggle_Button.Signal_Toggled,
         Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Regexp_Check,
         Gtk.Toggle_Button.Signal_Toggled, Reset_Search'Access, Handle);

      --  Include all the patterns that have been predefined so far, and make
      --  sure that new patterns will be automatically added.
      Widget_Callback.Object_Connect
        (Vsearch.Pattern_Combo, Gtk.Combo_Box.Signal_Changed,
         Selection_Changed'Access, Vsearch);

      --  Fill the replace combo first, so that the selection remains in
      --  the pattern combo
      Get_History
        (Get_History (Handle).all, Replace_Hist_Key, Vsearch.Replace_Combo,
         Clear_Combo => False, Prepend => True);

      Model := Gtk_List_Store (Get_Model (Vsearch.Pattern_Combo));
      Value := Get_History (Get_History (Handle).all, Pattern_Hist_Key);

      if Value /= null then
         for V in Value'Range loop
            if Value (V).all /= "" then
               Iter := Add_Unique_List_Entry
                 (Model, Value (V).all,
                  Prepend => True,
                  Col     => Column_Text);
               Model.Set (Iter, Column_Pattern, Value (V).all);
            end if;
         end loop;

         Set_Active (Vsearch.Pattern_Combo, 0);
         Select_Region (Gtk_Entry (Vsearch.Pattern_Combo.Get_Child), 0, -1);

      else
         Set_Active_Text (Vsearch.Pattern_Combo, "");
      end if;

      Add_Hook (Handle, Search_Reset_Hook,
                Wrapper (Set_First_Next_Mode_Cb'Access),
                Name => "vsearch.search_reset");
      Add_Hook (Handle, Search_Functions_Changed_Hook,
                Wrapper (Search_Functions_Changed'Access),
                Name => "vsearch.search_functions");
      Add_Hook (Handle, Search_Regexps_Changed_Hook,
                Wrapper (New_Predefined_Regexp'Access),
                Name => "vsearch.search_regexps");
      Add_Hook (Handle, Project_View_Changed_Hook,
                Wrapper (On_Project_View_Changed'Access),
                Name => "vsearch.project_view_changed");

      --  ??? Should be changed when prefs are changed
      Set_Font_And_Colors (Vsearch.Table, Fixed_Font => False);
   end Initialize;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (Search : access Gtk_Widget_Record'Class) return Boolean
   is
      Vsearch : constant Vsearch_Access := Vsearch_Access (Search);
   begin
      --  When the user presses "Escape" in the dialog, gtk+ will emit the
      --  "on_delete" signal and call this function (the MDI also calls it when
      --  closing the MDI child, in particular when loading a new perspective).
      --  We close the MDI_Child manually, and make sure the widget itself is
      --  not destroyed since it is still referenced in the module.
      --  Close_Vsearch ensures that on_delete is not emitted to avoid infinite
      --  recursion.

      Close_Vsearch (Vsearch);
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end On_Delete;

   ------------------
   -- Receive_Text --
   ------------------

   procedure Receive_Text
     (Clipboard : Gtk_Clipboard;
      Text      : Interfaces.C.Strings.chars_ptr;
      Data      : System.Address)
   is
      pragma Unreferenced (Clipboard, Data);
   begin
      if Text /= Interfaces.C.Strings.Null_Ptr then
         declare
            Ada_Text : constant String := Interfaces.C.Strings.Value (Text);
         begin
            if Ada_Text /= ""
              and then Ada_Text'Length < 128
              and then Index (Ada_Text, (1 => ASCII.LF)) = 0
            then
               Set_Active_Text
                 (Vsearch_Module_Id.Search.Pattern_Combo, Ada_Text);
            end if;
         end;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Receive_Text;

   ---------------------------
   -- Get_Or_Create_Vsearch --
   ---------------------------

   function Get_Or_Create_Vsearch
     (Kernel        : access Kernel_Handle_Record'Class;
      Raise_Widget  : Boolean := False;
      Float_Widget  : Boolean := True;
      Reset_Entries : Boolean := False;
      Context       : String_Access := null) return Vsearch_Access
   is
      --  We must create the search dialog only after we have found the current
      --  context, otherwise it would return the context of the search widget
      --  itself
      Module   : constant Module_ID := Get_Current_Module (Kernel);

      Child   : GPS_MDI_Child;
      Success : Boolean;

      Default_Pattern : String_Access := null;

      pragma Unreferenced (Success);
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
                              (Get_MDI (Kernel), Vsearch_Record'Tag));

      declare
         W : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);
         Start, Stop : Guint;
         Success : Boolean := False;
      begin
         if W /= null and then W.all in Gtk_Editable_Record'Class then
            Get_Selection_Bounds (Gtk_Editable (W), Success, Start, Stop);

            if Success and then Start /= Stop then
               Default_Pattern := new String'
                 (Get_Chars (Gtk_Editable (W), Gint (Start), Gint (Stop)));
            end if;
         elsif W /= null and then W.all in Gtk_Text_View_Record'Class then
            declare
               Buffer     : constant Gtk_Text_Buffer := Get_Buffer
                 (Gtk_Text_View (W));
               First_Iter : Gtk_Text_Iter;
               Last_Iter  : Gtk_Text_Iter;
            begin
               Get_Selection_Bounds
                 (Buffer, First_Iter, Last_Iter, Success);

               if Success and then
                 Get_Offset (First_Iter) /= Get_Offset (Last_Iter)
               then
                  Default_Pattern := new String'
                    (Get_Slice (Buffer, First_Iter, Last_Iter));
               end if;

               Select_Range (Buffer, First_Iter, First_Iter);
            end;
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

      --  If not currently displayed
      if Child = null then
         --  Create if not done yet

         if Vsearch_Module_Id.Search = null then
            Gtk_New (Vsearch_Module_Id.Search, Kernel_Handle (Kernel));

            --  Show the already registered modules
            Search_Functions_Changed (Kernel);
            New_Predefined_Regexp (Kernel);

            --  keep a reference on it so that it isn't destroyed when the MDI
            --  child is destroyed.

            Ref (Vsearch_Module_Id.Search);

            Return_Callback.Connect
              (Vsearch_Module_Id.Search,
               Gtk.Widget.Signal_Delete_Event, On_Delete'Access);
         end if;

         Gtk_New (Child, Vsearch_Module_Id.Search,
                  Flags => All_Buttons or Float_As_Transient
                  or Always_Destroy_Float,
                  Focus_Widget =>
                    Vsearch_Module_Id.Search.Pattern_Combo.Get_Child,
                  Group    => Group_View,
                  Module => Vsearch_Module_Id,
                  Desktop_Independent => True);
         Set_Title (Child, -"Search");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Left);
         Float_Vsearch (Child);

         Widget_Callback.Connect
           (Child, Signal_Float_Child, Float_Vsearch'Access);
         Widget_Callback.Connect
           (Child, Signal_Unfloat_Child, Float_Vsearch'Access);
         Float_Child (Child, Float_Widget);
         Set_Focus_Child (Child);
      end if;

      --  Automatically fill the pattern text entry with the selection, if
      --  there is one which does not contain multiple lines.

      if Default_Pattern /= null then
         Set_Active_Text
           (Vsearch_Module_Id.Search.Pattern_Combo, Default_Pattern.all);
         Free (Default_Pattern);
      else
         Request_Text
           (Gtk.Clipboard.Get (Selection_Primary),
            Receive_Text'Access,
            System.Null_Address);
      end if;

      if Raise_Widget then
         Raise_Child (Child);
      end if;

      if Reset_Entries then
         --  ??? Temporarily: reset the entry. The correct fix would be to
         --  separate the find and replace tabs, so that having a default
         --  entry in this combo doesn't look strange when the combo is
         --  insensitive.

         declare
            Vsearch  : constant Vsearch_Access := Vsearch_Module_Id.Search;
         begin
            if Context = null then
               if Module /= null then
                  declare
                     Search : constant Search_Module_Data :=
                       Search_Context_From_Module (Module, Vsearch.Kernel);
                  begin
                     if Search /= No_Search then
                        Add_Unique_Combo_Entry
                          (Vsearch.Context_Combo, Search.Label.all, True);
                     end if;
                  end;
               end if;
            else
               Add_Unique_Combo_Entry
                 (Vsearch.Context_Combo, Context.all, True);
            end if;

            Grab_Focus (Vsearch.Pattern_Combo.Get_Child);
         end;
      end if;

      return Vsearch_Module_Id.Search;
   end Get_Or_Create_Vsearch;

   --------------------
   -- Search_Next_Cb --
   --------------------

   procedure Search_Next_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      if Vsearch_Module_Id.Search /= null then
         On_Search (Vsearch_Module_Id.Search);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Search_Next_Cb;

   ------------------------
   -- Search_Previous_Cb --
   ------------------------

   procedure Search_Previous_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      if Vsearch_Module_Id.Search /= null then
         On_Search_Previous (Vsearch_Module_Id.Search);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Search_Previous_Cb;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Action : in out Search_Specific_Context) is
   begin
      Free (Action.Context);
   end Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Action  : access Search_Specific_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Vsearch : Vsearch_Access;
      pragma Unreferenced (Vsearch);
   begin
      if Action.Context = null then
         null;
      else
         null;
      end if;

      Vsearch := Get_Or_Create_Vsearch
        (Get_Kernel (Context.Context),
         Raise_Widget  => True,
         Reset_Entries => True,
         Context       => Action.Context);

      return Success;
   end Execute;

   ------------------------------
   -- Register_Search_Function --
   ------------------------------

   procedure Register_Search_Function
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Label             : String;
      Factory           : Find_Utils.Module_Search_Context_Factory;
      Extra_Information : access Gtk.Widget.Gtk_Widget_Record'Class := null;
      Id          : access GPS.Kernel.Abstract_Module_ID_Record'Class := null;
      Mask              : Search_Options_Mask := All_Options)
   is
      Data : constant Search_Module_Data :=
        Search_Module_Data'
          (Label             => new String'(Label),
           Factory           => Factory,
           Extra_Information => Gtk_Widget (Extra_Information),
           Id                => Module_ID (Id),
           Mask              => Mask);
      Command : Interactive_Command_Access;
   begin
      if Id /= null then
         Create_New_Key_If_Necessary
           (Get_History (Kernel).all,
            Last_Function_In_Module_Key & History_Key (Get_Name (Data.Id)),
            Key_Type => Strings);
         Set_Max_Length
           (Get_History (Kernel).all,
            1,   --  Only the last one is interesting
            Last_Function_In_Module_Key & History_Key (Get_Name (Data.Id)));
      end if;

      Prepend (Vsearch_Module_Id.Search_Modules, Data);

      if Data.Extra_Information /= null then
         Ref (Data.Extra_Information);
         Ref_Sink (Data.Extra_Information);
      end if;

      Command := new Search_Specific_Context'
        (Interactive_Command with Context => new String'(Label));

      Register_Action
        (Kernel,
         Name        => -"Search in context: " & Label,
         Command     => Command,
         Description => -("Open the search dialog, and preset the ""Look In"""
           & " field to """ & Label & """"),
         Category    => -"Search");

      Run_Hook (Kernel, Search_Functions_Changed_Hook);
   end Register_Search_Function;

   ---------------------------
   -- Get_Nth_Search_Module --
   ---------------------------

   function Get_Nth_Search_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Num    : Positive) return Search_Module_Data
   is
      pragma Unreferenced (Kernel);
      Node : Search_Modules_List.List_Node := First
        (Vsearch_Module_Id.Search_Modules);
   begin
      for N in 1 .. Num - 1 loop
         Node := Next (Node);
      end loop;

      if Node = Null_Node then
         return No_Search;
      else
         return Data (Node);
      end if;
   end Get_Nth_Search_Module;

   -----------------
   -- Find_Module --
   -----------------

   function Find_Module
     (Kernel : access Kernel_Handle_Record'Class;
      Label  : String) return Search_Module_Data
   is
      pragma Unreferenced (Kernel);

      List : List_Node := First (Vsearch_Module_Id.Search_Modules);
   begin
      while List /= Null_Node loop
         if Data (List).Label.all = Label then
            return Data (List);
         end if;

         List := Next (List);
      end loop;

      return No_Search;
   end Find_Module;

   --------------------------------
   -- Search_Context_From_Module --
   --------------------------------

   function Search_Context_From_Module
     (Id     : not null access Module_ID_Record'Class;
      Handle : access Kernel_Handle_Record'Class)
      return Search_Module_Data
   is
      List : List_Node := First (Vsearch_Module_Id.Search_Modules);
      Last_Matching_Node : List_Node := Null_Node;
      Key : constant History_Key :=
        Last_Function_In_Module_Key & History_Key (Get_Name (Module_ID (Id)));
      Last_Selected : constant String_List_Access :=
        Get_History (Get_History (Handle).all, Key);

   begin
      while List /= Null_Node loop
         if Data (List).Id = Module_ID (Id) then
            Last_Matching_Node := List;

            if not Get_Pref (Keep_Previous_Search_Context)
              or else Last_Selected = null
              or else Last_Selected (Last_Selected'First).all =
                 Data (List).Label.all
            then
               if Active (Me) then
                  Trace (Me, "Get last search function for module "
                         & String (Key) & ": " & Data (List).Label.all);
               end if;

               return Data (List);
            end if;
         end if;

         List := Next (List);
      end loop;

      if Last_Matching_Node /= Null_Node then
         return Data (Last_Matching_Node);
      end if;

      return No_Search;
   end Search_Context_From_Module;

   ------------------------
   -- Set_Last_Of_Module --
   ------------------------

   procedure Set_Last_Of_Module
     (Handle      : access Kernel_Handle_Record'Class;
      Search_Data : Search_Module_Data) is
   begin
      if not Get_Pref (Keep_Previous_Search_Context)
        or else Search_Data.Id = null
      then
         return;
      end if;

      if Active (Me) then
         Trace (Me, "Set last search function for module "
                & Get_Name (Search_Data.Id)
                & " to " & Search_Data.Label.all);
      end if;

      Add_To_History
        (Get_History (Handle).all,
         Last_Function_In_Module_Key & History_Key (Get_Name (Search_Data.Id)),
         Search_Data.Label.all);
   end Set_Last_Of_Module;

   -----------------------------
   -- Register_Default_Search --
   -----------------------------

   procedure Register_Default_Search
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Search_Pattern
        (Kernel,
         Name           => -"Simple string",
         Regexp         => "",
         Case_Sensitive => False,
         Is_Regexp      => False);
      Register_Search_Pattern
        (Kernel,
         Name           => -"Regular expression",
         Regexp         => "",
         Case_Sensitive => False,
         Is_Regexp      => True);
   end Register_Default_Search;

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

      Run_Hook (Kernel, Search_Regexps_Changed_Hook);
   end Register_Search_Pattern;

   --------------------------
   -- Search_Regexps_Count --
   --------------------------

   function Search_Regexps_Count
     (Kernel : access Kernel_Handle_Record'Class) return Natural
   is
      pragma Unreferenced (Kernel);
   begin
      return Vsearch_Module_Id.Search_Regexps'Length;
   end Search_Regexps_Count;

   -----------------------------------
   -- Get_Nth_Search_Regexp_Options --
   -----------------------------------

   procedure Get_Nth_Search_Regexp_Options
     (Kernel         : access Kernel_Handle_Record'Class;
      Num            : Natural;
      Case_Sensitive : out Boolean;
      Is_Regexp      : out Boolean)
   is
      pragma Unreferenced (Kernel);
   begin
      Case_Sensitive := Vsearch_Module_Id.Search_Regexps (Num).Case_Sensitive;
      Is_Regexp      := Vsearch_Module_Id.Search_Regexps (Num).Is_Regexp;
   end Get_Nth_Search_Regexp_Options;

   --------------------------------
   -- Get_Nth_Search_Regexp_Name --
   --------------------------------

   function Get_Nth_Search_Regexp_Name
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return String
   is
      pragma Unreferenced (Kernel);
   begin
      return Vsearch_Module_Id.Search_Regexps (Num).Name.all;
   end Get_Nth_Search_Regexp_Name;

   ----------------------------------
   -- Get_Nth_Search_Regexp_Regexp --
   ----------------------------------

   function Get_Nth_Search_Regexp
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return String
   is
      pragma Unreferenced (Kernel);
   begin
      return Vsearch_Module_Id.Search_Regexps (Num).Regexp.all;
   end Get_Nth_Search_Regexp;

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
      Navigate : constant String := "/_" & (-"Navigate");
      Find_All : constant String := -"Find All References";
      Mitem    : Gtk_Separator_Menu_Item;

      open_options_xpm : aliased Gtkada.Types.Chars_Ptr_Array (0 .. 0);
      pragma Import (C, open_options_xpm, "unfold_block_xpm");
      close_options_xpm  : aliased Gtkada.Types.Chars_Ptr_Array (0 .. 0);
      pragma Import (C, close_options_xpm, "fold_block_xpm");

      Command : Interactive_Command_Access;
   begin
      Vsearch_Module_Id := new Vsearch_Module_Record;
      Register_Module
        (Module      => Module_ID (Vsearch_Module_Id),
         Kernel      => Kernel,
         Module_Name => Search_Module_Name,
         Priority    => Default_Priority);
      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);

      Vsearch_Module_Id.Tab_Width := Tab_Width;

      Close_Options_Pixbuf := Gdk.Pixbuf.Gdk_New_From_Xpm_Data
        (close_options_xpm);
      Open_Options_Pixbuf := Gdk.Pixbuf.Gdk_New_From_Xpm_Data
        (open_options_xpm);

      --  Register the menus
      Register_Menu
        (Kernel, Navigate, null, Ref_Item => -"Edit", Add_Before => False);

      Command := new Search_Specific_Context'
        (Interactive_Command with Context => null);
      Register_Action
        (Kernel,
         Name        => -"Search",
         Command     => Command,
         Description => -("Open the search dialog. If you have selected the"
           & " preference Search/Preserve Search Context, the same context"
           & " will be selected, otherwise the context is reset depending on"
           & " the active window"),
         Category    => -"Search");
      Register_Menu
        (Kernel,
         Parent_Path => Navigate,
         Text        => -"_Find or Replace...",
         Stock_Image => Stock_Find,
         Callback    => null,
         Command     => Command,
         Ref_Item    => Find_All,
         Accel_Key   => GDK_F, Accel_Mods => Control_Mask);

      Vsearch_Module_Id.Next_Menu_Item := Register_Menu
        (Kernel, Navigate, -"Find _Next",
         "", Search_Next_Cb'Access,
         Accel_Key => GDK_N, Accel_Mods => Control_Mask);
      Ref (Vsearch_Module_Id.Next_Menu_Item);
      Set_Sensitive (Vsearch_Module_Id.Next_Menu_Item, False);

      Vsearch_Module_Id.Prev_Menu_Item := Register_Menu
        (Kernel, Navigate, -"Find _Previous",
         "", Search_Previous_Cb'Access,
         Accel_Key => GDK_P, Accel_Mods => Control_Mask);
      Ref (Vsearch_Module_Id.Prev_Menu_Item);
      Set_Sensitive (Vsearch_Module_Id.Prev_Menu_Item, False);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Navigate, Mitem);

      --  Register the default search functions

      Register_Default_Search (Kernel);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "vsearch.preferences_changed");

      Register_Preferences (Kernel);
   end Register_Module;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Ask_Confirmation_For_Replace_All := Create
        (Get_Preferences (Kernel),
         Name  => "Ask-Confirmation-For-Replace-All",
         Label => -"Confirmation for 'Replace all'",
         Page  => -"Search",
         Doc   => -("Enable the confirmation popup before doing a" &
           " replace all from the search window."),
         Default => True);

      Keep_Previous_Search_Context := Create
        (Get_Preferences (Kernel),
         Name  => "keep-previous-search-context",
         Label => -"Preserve Search Context",
         Page  => -"Search",
         Doc   => -("Preserve the contents of the ""Look in"" entry"
           & " between consecutive searches."),
         Default => False);
   end Register_Preferences;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Idle_Search_Data) is
      pragma Unreferenced (D);
   begin
      null;
   end Free;

   -------------------
   -- Get_Tab_Width --
   -------------------

   function Get_Tab_Width return Natural is
   begin
      --  This is needed in the context of the automatic testsuite
      if Vsearch_Module_Id = null then
         return 8;
      else
         return Vsearch_Module_Id.Tab_Width;
      end if;
   end Get_Tab_Width;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Vsearch_Module_Id.Tab_Width := Tab_Width;
   end Preferences_Changed;

   ------------------
   -- Reset_Search --
   ------------------

   procedure Reset_Search
     (Object : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      Run_Hook (Kernel, Search_Reset_Hook);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Reset_Search;

end Vsearch;
