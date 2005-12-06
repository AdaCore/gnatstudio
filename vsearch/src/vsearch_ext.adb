-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2005                      --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Event;               use Gdk.Event;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gdk.Window;              use Gdk.Window;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Xml_Int;            use Glib.Xml_Int;
with Gtk.Alignment;           use Gtk.Alignment;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Clipboard;           use Gtk.Clipboard;
with Gtk.Hbutton_Box;         use Gtk.Hbutton_Box;
with Gtk.Check_Button;        use Gtk.Check_Button;
with Gtk.Combo;               use Gtk.Combo;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Image;               use Gtk.Image;
with Gtk.Label;               use Gtk.Label;
with Gtk.List;                use Gtk.List;
with Gtk.List_Item;           use Gtk.List_Item;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Selection;           use Gtk.Selection;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with Gtk.Tooltips;            use Gtk.Tooltips;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.MDI;              use Gtkada.MDI;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.Types;            use Gtkada.Types;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Console;      use GPS.Kernel.Console;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Find_Utils;              use Find_Utils;
with GUI_Utils;               use GUI_Utils;
with Generic_List;
with Histories;               use Histories;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Traces; use Traces;
with VFS;

with Commands;                use Commands;
with Commands.Generic_Asynchronous;

package body Vsearch_Ext is

   Pattern_Hist_Key   : constant History_Key := "search_patterns";
   Replace_Hist_Key   : constant History_Key := "search_replace";
   Auto_Hide_Hist_Key : constant History_Key := "search_autohide";
   Select_Window_Hist_Key : constant History_Key := "search_select_window";
   Window_X_Hist_Key  : constant History_Key := "search_window_x";
   Window_Y_Hist_Key  : constant History_Key := "search_window_y";
   --  The key for the histories.

   procedure Free (Data : in out Search_Module_Data);
   --  Free the memory associated with Data

   package Search_Modules_List is new Generic_List
     (Find_Utils.Search_Module_Data);

   use Search_Modules_List;

   Search_Module_Name : constant String := "Search";

   type Vsearch_Module_Record is new Module_ID_Record with record
      Search_Modules : Search_Modules_List.List;
      --  Global variable that contains the list of all registered search
      --  functions.

      Search : Vsearch_Extended;
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

   procedure Customize
     (Module : access Vsearch_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   procedure Destroy (Module : in out Vsearch_Module_Record);
   --  See inherited documentation

   Vsearch_Module_Id : Vsearch_Module;
   --  ??? Register in the kernel, shouldn't be a global variable

   type Search_User_Data_Record is record
      Case_Sensitive : Boolean;
      Is_Regexp      : Boolean;
   end record;

   package Search_User_Data is new User_Data (Search_User_Data_Record);

   Search_User_Data_Quark : constant String := "gps-search_user";

   type Idle_Search_Data is record
      Vsearch         : Vsearch_Extended;
      Search_Backward : Boolean;
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
     (Vsearch   : access Vsearch_Extended_Record'Class;
      Find_Next : Boolean);
   --  If Find_Next is False, a new search will be started, otherwise the next
   --  occurence of the current search will be searched.

   procedure Set_First_Next_Mode_Cb
     (Kernel : access Kernel_Handle_Record'Class);
   --  Aborts the current search pattern

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
     (Kernel       : access Kernel_Handle_Record'Class;
      Raise_Widget : Boolean := False;
      Float_Widget : Boolean := True) return Vsearch_Extended;
   --  Return a valid vsearch widget, creating one if necessary.

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

   procedure Create_Context (Vsearch : access Vsearch_Extended_Record'Class);
   --  Create the search context based on the current contents of the GUI.
   --  This is stored in Vsearch.Last_Search_Context.

   function On_Delete (Search : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Called when the search widget is about to be destroyed.

   procedure Replace_Text_Changed
     (Vsearch : access Gtk_Widget_Record'Class);
   --  Called when the contents of the "Replace" field has changed

   procedure Resize_If_Needed (Vsearch : access Vsearch_Extended_Record'Class);
   --  Resize the vsearch window if needed.

   procedure Internal_Search
     (Vsearch : Vsearch_Extended;
      Replace : Boolean := False);
   --  Internal implementation of search capability, used by On_Search and
   --  On_Search_Replace.

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Search (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Find/Next" is clicked.

   procedure On_Search_Replace (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Replace" is clicked.

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Previous" is clicked.

   procedure On_Options_Toggled (Object : access Gtk_Widget_Record'Class);
   --  Called when button "Options" is toggled.

   procedure On_Context_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);
   --  Called when the entry "Look in" is changed.

   procedure Search_Menu_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the menu Edit->Search

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

   procedure Store_Position (Vsearch : Vsearch_Extended);
   procedure Restore_Position (Vsearch : Vsearch_Extended);
   --  Store and restore the position of the Vsearc dialog.

   ----------------------
   -- Restore_Position --
   ----------------------

   procedure Restore_Position (Vsearch : Vsearch_Extended) is
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

   procedure Store_Position (Vsearch : Vsearch_Extended) is
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
      Extended : Vsearch_Extended;
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
      Extended : Vsearch_Extended;
   begin
      if Widget.all in Vsearch_Extended_Record'Class then
         Extended := Vsearch_Extended (Widget);

         --  We do not want floating search to appear automatically next time
         --  gps is started
         if Get_State (Find_MDI_Child (Get_MDI (User), Extended)) /=
           Floating
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
      Vsearch : constant Vsearch_Extended :=
        Vsearch_Extended (Get_Widget (Child));
      Close_Button : Gtk_Button;
   begin
      if Is_Floating (Child) then
         Set_Resizable (Gtk_Dialog (Get_Toplevel (Vsearch)), True);

         --  Add the "Close" button.
         Close_Button := Gtk_Button
           (Add_Button (Gtk_Dialog (Get_Toplevel (Vsearch)),
                        Stock_Close,
                        Gtk_Response_Cancel));

         Show_All (Vsearch.Auto_Hide_Check);
         Set_Child_Visible (Vsearch.Auto_Hide_Check, True);

         Widget_Callback.Object_Connect
           (Close_Button, "clicked", Close_Vsearch'Access, Vsearch);

         --  Set the position of the floating window
         Restore_Position (Vsearch);
      else
         Hide_All (Vsearch.Auto_Hide_Check);
         Set_Child_Visible (Vsearch.Auto_Hide_Check, False);

         --  Store the position of the floating window
         Store_Position (Vsearch);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Float_Vsearch;

   -------------------
   -- Close_Vsearch --
   -------------------

   procedure Close_Vsearch (Search : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Search);
   begin
      Store_Position (Vsearch);
      Close (Get_MDI (Vsearch.Kernel), Search, Force => True);

      --  Give the focus back to the main Window, since this is not always
      --  done by the window manager (e.g. under Windows)

      Gdk_Raise (Get_Window (Get_Main_Window (Vsearch.Kernel)));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Close_Vsearch;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Search_Module_Data) is
   begin
      if Data.Extra_Information /= null then
         Unref (Data.Extra_Information);
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Vsearch_Module_Record) is
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
      Vsearch  : constant Vsearch_Extended := Vsearch_Extended (Data.Vsearch);
      Button   : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

      Found    : Boolean;
      Continue : Boolean;
   begin
      if Data.Vsearch.Continue then
         Search
           (Data.Vsearch.Last_Search_Context,
            Data.Vsearch.Kernel,
            Data.Search_Backward,
            Give_Focus => Get_Active (Data.Vsearch.Select_Editor_Check),
            Found      => Found,
            Continue   => Continue);

         Data.Vsearch.Found := Data.Vsearch.Found or else Found;

         if Continue then
            Set_Progress
              (Command,
               (Running,
                Get_Current_Progress (Data.Vsearch.Last_Search_Context),
                Get_Total_Progress (Data.Vsearch.Last_Search_Context)));
            Result := Execute_Again;
            return;
         end if;
      end if;

      Free (Data.Vsearch.Last_Search_Context);
      Set_Sensitive (Data.Vsearch.Search_Next_Button, True);
      Pop_State (Data.Vsearch.Kernel);
      Data.Vsearch.Search_Idle_Handler := 0;

      if not Vsearch.Found then
         Button := Message_Dialog
           (Msg     => "No occurrences of '" &
            Get_Text (Vsearch.Pattern_Entry) & "' found.",
            Title   => -"Search",
            Buttons => Button_OK,
            Parent  => Get_Main_Window (Vsearch.Kernel));
      end if;

      Result := Success;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      if Data.Vsearch.Continue
        and then Replace
           (Data.Vsearch.Last_Search_Context,
            Data.Vsearch.Kernel,
            Get_Text (Data.Vsearch.Replace_Entry),
            --  ??? The user could change this interactively in the meantime
            Data.Search_Backward,
            Give_Focus => Get_Active (Data.Vsearch.Select_Editor_Check))
      then
         Set_Progress
           (Command,
            (Running,
             Get_Current_Progress (Data.Vsearch.Last_Search_Context),
             Get_Total_Progress (Data.Vsearch.Last_Search_Context)));

         Result := Execute_Again;
      else
         Insert (Data.Vsearch.Kernel,
                 -"Finished replacing the string in all the files");
         Free (Data.Vsearch.Last_Search_Context);
         Set_Sensitive (Data.Vsearch.Search_Next_Button, True);
         Pop_State (Data.Vsearch.Kernel);
         Data.Vsearch.Search_Idle_Handler := 0;
         Result := Success;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Pop_State (Data.Vsearch.Kernel);
         Data.Vsearch.Search_Idle_Handler := 0;
         Result := Success;
   end Replace_Iterate;

   --------------------
   -- Create_Context --
   --------------------

   procedure Create_Context (Vsearch : access Vsearch_Extended_Record'Class) is
      use Widget_List;
      Data            : constant Search_Module_Data :=
                          Find_Module
                            (Vsearch.Kernel,
                             Get_Text (Get_Entry (Vsearch.Context_Combo)));
      Pattern         : constant String := Get_Text (Vsearch.Pattern_Entry);
      All_Occurrences : constant Boolean :=
                          Get_Active (Vsearch.Search_All_Check);
      Options         : Search_Options;
   begin
      Free (Vsearch.Last_Search_Context);

      if Data.Factory /= null and then Pattern /= "" then
         Vsearch.Last_Search_Context := Data.Factory
           (Vsearch.Kernel, All_Occurrences, Data.Extra_Information);

         if Vsearch.Last_Search_Context /= null then
            Options :=
              (Case_Sensitive => Get_Active (Vsearch.Case_Check),
               Whole_Word     => Get_Active (Vsearch.Whole_Word_Check),
               Regexp         => Get_Active (Vsearch.Regexp_Check));
            Set_Context (Vsearch.Last_Search_Context, Pattern, Options);
            Reset (Vsearch.Last_Search_Context, Vsearch.Kernel);
         else
            Insert (Vsearch.Kernel, -"Invalid search context specified");
            return;
         end if;
      end if;

      --  Update the contents of the combo boxes
      if Get_Selection (Get_List (Vsearch.Pattern_Combo)) =
        Widget_List.Null_List
      then
         Add_Unique_Combo_Entry
           (Vsearch.Pattern_Combo, Pattern, Prepend => True);
         Add_To_History
           (Get_History (Vsearch.Kernel).all, Pattern_Hist_Key, Pattern);

         --  It sometimes happens that another entry is selected, for some
         --  reason. This also resets the options to the unwanted selection,
         --  so we also need to override them.
         Set_Text (Vsearch.Pattern_Entry, Pattern);
         Set_Active (Vsearch.Case_Check, Options.Case_Sensitive);
         Set_Active (Vsearch.Whole_Word_Check, Options.Whole_Word);
         Set_Active (Vsearch.Regexp_Check, Options.Regexp);
      end if;

      declare
         Replace_Text : constant String := Get_Text (Vsearch.Replace_Entry);
      begin
         Add_Unique_Combo_Entry
           (Vsearch.Replace_Combo, Replace_Text, Prepend => True);
         Add_To_History
           (Get_History (Vsearch.Kernel).all, Replace_Hist_Key, Replace_Text);
         Set_Text (Vsearch.Replace_Entry, Replace_Text);
      end;
   end Create_Context;

   ---------------------
   -- Internal_Search --
   ---------------------

   procedure Internal_Search
     (Vsearch : Vsearch_Extended;
      Replace : Boolean := False)
   is
      Toplevel       : Gtk_Widget := Get_Toplevel (Vsearch);
      Found          : Boolean;
      Has_Next       : Boolean;
      Button         : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

      All_Occurences : constant Boolean :=
                         Get_Active (Vsearch.Search_All_Check);
      Pattern        : constant String := Get_Text (Vsearch.Pattern_Entry);
      C              : Search_Commands.Generic_Asynchronous_Command_Access;

   begin
      if Gtk_Widget (Vsearch) = Toplevel then
         Toplevel := Gtk_Widget (Get_Main_Window (Vsearch.Kernel));
      end if;

      if not Vsearch.Find_Next then
         Create_Context (Vsearch);
      end if;

      if Vsearch.Last_Search_Context /= null then
         Vsearch.Continue := True;

         if All_Occurences then
            --  Set up the search. Everything is automatically
            --  put back when the idle loop terminates.
            --  ??? What is that supposed to mean.

            Vsearch.Found := False;

            Search_Commands.Create
              (C,
               -"Searching",
               (Vsearch => Vsearch, Search_Backward => False),
               Search_Iterate'Access);

            Launch_Background_Command
              (Vsearch.Kernel, Command_Access (C), True, True, "Search");

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

            --  Give a visual feedback that the search is terminated.
            if not Found
              and then not Has_Next
            then
               Button := Message_Dialog
                 (Msg     => (-"No occurrences of '") & Pattern &
                    (-"' found."),
                  Title   => -"Search",
                  Buttons => Button_OK,
                  Parent  => Gtk_Window (Toplevel));
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
        and then Get_History
          (Get_History (Vsearch.Kernel).all, Auto_Hide_Hist_Key)
      then
         Close_Vsearch (Vsearch);
      end if;

   exception
      when E : others =>
         Pop_State (Vsearch.Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Internal_Search;

   ---------------
   -- On_Search --
   ---------------

   procedure On_Search (Object : access Gtk_Widget_Record'Class) is
   begin
      Internal_Search (Vsearch_Extended (Object));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Search;

   -----------------------
   -- On_Search_Replace --
   -----------------------

   procedure On_Search_Replace (Object : access Gtk_Widget_Record'Class) is
      Vsearch     : constant Vsearch_Extended := Vsearch_Extended (Object);
      Has_Next    : Boolean;
      pragma Unreferenced (Has_Next);

      All_Occurences : constant Boolean := Get_Active
        (Vsearch.Search_All_Check);
      C              : Search_Commands.Generic_Asynchronous_Command_Access;

   begin
      if not All_Occurences then
         --  First time in Interactive mode ?
         if not Vsearch.Find_Next
           or else Vsearch.Last_Search_Context = null
         then
            Internal_Search (Vsearch, Replace => True);

         else
            Push_State (Vsearch.Kernel, Processing);
            Has_Next := Replace
              (Vsearch.Last_Search_Context,
               Vsearch.Kernel,
               Get_Text (Vsearch.Replace_Entry),
               Search_Backward => False,
               Give_Focus => Get_Active (Vsearch.Select_Editor_Check));
            Pop_State (Vsearch.Kernel);
         end if;

      else
         Create_Context (Vsearch);

         Search_Commands.Create
           (C,
              -"Searching/Replacing",
            (Vsearch => Vsearch, Search_Backward => False),
            Replace_Iterate'Access);

         Launch_Background_Command
           (Vsearch.Kernel, Command_Access (C), True, True, "Search");
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Search_Replace;

   ------------------------
   -- On_Search_Previous --
   ------------------------

   procedure On_Search_Previous (Object : access Gtk_Widget_Record'Class) is
      Vsearch        : constant Vsearch_Extended := Vsearch_Extended (Object);
      All_Occurences : constant Boolean :=
                         Get_Active (Vsearch.Search_All_Check);
      Found          : Boolean;
      Has_Next       : Boolean;

   begin
      if not Vsearch.Find_Next then
         Create_Context (Vsearch);
      end if;

      if Vsearch.Last_Search_Context /= null then
         Vsearch.Continue := True;

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

            Set_First_Next_Mode (Vsearch, Find_Next => True);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Search_Previous;

   ----------------------
   -- Resize_If_Needed --
   ----------------------

   procedure Resize_If_Needed
     (Vsearch : access Vsearch_Extended_Record'Class)
   is
      Win : Gtk_Window;
      Child : constant MDI_Child := Find_MDI_Child
        (Get_MDI (Vsearch.Kernel), Vsearch);
      Req : Gtk_Requisition;
   begin
      if Child /= null then
         if Is_Floating (Child) then
            Win := Gtk_Window (Get_Toplevel (Vsearch));
            if Win /= null then
               Resize (Win, -1, -1);
            end if;

         else
            Size_Request (Child, Req);
            Set_Size
              (Get_MDI (Vsearch.Kernel),
               Child      => Child,
               Width      => Req.Width,
               Height     => Req.Height,
               Fixed_Size => True);
         end if;
      end if;
   end Resize_If_Needed;

   ------------------------
   -- On_Options_Toggled --
   ------------------------

   procedure On_Options_Toggled (Object : access Gtk_Widget_Record'Class) is
      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
   begin
      Set_Child_Visible (Vsearch.Options_Frame,
                         Get_Active (Vsearch.Options_Toggle));

      --  We need to explicitely hide the options frame, otherwise the size is
      --  incorrectly recomputed by gtk+. Looks like a bug to me...

      if Get_Active (Vsearch.Options_Toggle) then
         Show (Vsearch_Module_Id.Search.Options_Frame);
      else
         Hide (Vsearch_Module_Id.Search.Options_Frame);
      end if;

      Resize_If_Needed (Vsearch);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Options_Toggled;

   ------------------------------
   -- On_Context_Entry_Changed --
   ------------------------------

   procedure On_Context_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      use Widget_List;

      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Object);
      Data    : constant Search_Module_Data := Find_Module
        (Vsearch.Kernel, Get_Text (Get_Entry (Vsearch.Context_Combo)));
      Replace : Boolean;

   begin
      if Data /= No_Search then
         Replace := (Data.Mask and Supports_Replace) /= 0;
         Set_Sensitive (Vsearch.Replace_Label, Replace);
         Set_Sensitive (Vsearch.Replace_Combo, Replace);
         Set_Sensitive (Vsearch.Search_Replace_Button, Replace);

         if (Data.Mask and All_Occurrences) = 0 then
            Set_Active (Vsearch.Search_All_Check, False);
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
           (Vsearch.Search_All_Check, (Data.Mask and All_Occurrences) /= 0);

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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Context_Entry_Changed;

   -------------------------
   -- Set_First_Next_Mode --
   -------------------------

   procedure Set_First_Next_Mode
     (Vsearch   : access Vsearch_Extended_Record'Class;
      Find_Next : Boolean)
   is
      Box : Gtk_Box;
      Label : Gtk_Label;
      Image : Gtk_Image;
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

         Set_Use_Stock (Vsearch.Search_Next_Button, False);
         Remove (Vsearch.Search_Next_Button,
                 Get_Child (Vsearch.Search_Next_Button));

         Gtk_New_Hbox (Box, Homogeneous => False, Spacing => 2);
         Gtk_New (Image, Stock_Id => Stock_Find, Size => Icon_Size_Button);
         Pack_Start (Box, Image, Expand => False, Fill => False);
         Gtk_New_With_Mnemonic (Label, -"_Next");
         Pack_End (Box, Label, Expand => False, Fill => False);
         Gtk_New (Align, 0.5, 0.5, 0.0, 0.0);
         Add (Vsearch.Search_Next_Button, Align);
         Add (Align, Box);
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
         Set_Use_Stock (Vsearch.Search_Next_Button, True);
         Set_Label (Vsearch.Search_Next_Button, Stock_Find);
         Set_Sensitive (Vsearch_Module_Id.Next_Menu_Item, False);
         Set_Sensitive (Vsearch_Module_Id.Prev_Menu_Item, False);

         --  "Find" should not be active if we have some replacement text
         Set_Sensitive
           (Vsearch.Search_Next_Button, Get_Text (Vsearch.Replace_Entry) = "");
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
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Set_First_Next_Mode_Cb;

   ---------------
   -- Key_Press --
   ---------------

   function Key_Press
     (Vsearch : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean
   is
      Ext : constant Vsearch_Extended := Vsearch_Extended (Vsearch);
   begin
      if Get_Key_Val (Event) = GDK_Return
        or else Get_Key_Val (Event) = GDK_KP_Enter
      then
         if Is_Sensitive (Ext.Search_Next_Button) then
            Grab_Focus (Ext.Search_Next_Button);
            On_Search (Vsearch);
         else
            Grab_Focus (Ext.Search_Replace_Button);
            On_Search_Replace (Ext);
         end if;
         return True;
      end if;
      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press;

   --------------------------
   -- Replace_Text_Changed --
   --------------------------

   procedure Replace_Text_Changed
     (Vsearch : access Gtk_Widget_Record'Class)
   is
      Has_Replace : constant Boolean :=
        Get_Text (Vsearch_Extended (Vsearch).Replace_Entry) /= "";
   begin
      --  "Find" should not be active if we have some replacement text
      Set_Sensitive
        (Vsearch_Extended (Vsearch).Search_Next_Button, not Has_Replace);
      Reset_Search (Vsearch, Vsearch_Extended (Vsearch).Kernel);
   end Replace_Text_Changed;

   -----------------------
   -- Key_Press_Replace --
   -----------------------

   function Key_Press_Replace
     (Vsearch : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean
   is
      Ext : constant Vsearch_Extended := Vsearch_Extended (Vsearch);
   begin
      if Get_Key_Val (Event) = GDK_Return
        or else Get_Key_Val (Event) = GDK_KP_Enter
      then
         Grab_Focus (Ext.Search_Replace_Button);
         On_Search_Replace (Ext);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_Replace;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
     (Vsearch : access Gtk_Widget_Record'Class)
   is
      use Widget_List;
      Search : constant Vsearch_Extended := Vsearch_Extended (Vsearch);
      Data   : Search_User_Data_Record;
      Item   : Gtk_List_Item;

   begin
      if Get_Selection (Get_List (Search.Pattern_Combo)) /=
        Widget_List.Null_List
      then
         Item := Gtk_List_Item
           (Get_Data (Get_Selection (Get_List (Search.Pattern_Combo))));
         Data := Search_User_Data.Get (Item, Search_User_Data_Quark);

         Set_Active (Search.Case_Check, Data.Case_Sensitive);
         Set_Active (Search.Regexp_Check, Data.Is_Regexp);
      end if;

   exception
      when Gtkada.Types.Data_Error =>
         null;
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Selection_Changed;

   ---------------------------
   -- New_Predefined_Regexp --
   ---------------------------

   procedure New_Predefined_Regexp
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Search  : constant Vsearch_Extended := Vsearch_Module_Id.Search;
      Item    : Gtk_List_Item;
      Is_Regexp, Case_Sensitive : Boolean;
      Data    : Search_User_Data_Record;
      Options : constant Search_Options :=
        (Case_Sensitive => Get_Active (Search.Case_Check),
         Whole_Word     => Get_Active (Search.Whole_Word_Check),
         Regexp         => Get_Active (Search.Regexp_Check));

   begin
      for S in 1 .. Search_Regexps_Count (Kernel) loop
         Item := Add_Unique_Combo_Entry
           (Search.Pattern_Combo,
            Get_Nth_Search_Regexp_Name (Kernel, S),
            Get_Nth_Search_Regexp (Kernel, S),
            Use_Item_String => True);
         Get_Nth_Search_Regexp_Options
           (Kernel, S, Case_Sensitive => Case_Sensitive,
            Is_Regexp => Is_Regexp);

         Data := (Case_Sensitive => Case_Sensitive,
                  Is_Regexp      => Is_Regexp);

         Search_User_Data.Set (Item, Data, Search_User_Data_Quark);
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
      Vsearch : constant Vsearch_Extended := Vsearch_Module_Id.Search;
      Num     : Positive := 1;
   begin
      loop
         declare
            Data : constant Search_Module_Data :=
                     Get_Nth_Search_Module (Kernel, Num);
         begin
            exit when Data = No_Search;

            Add_Unique_Combo_Entry
              (Vsearch.Context_Combo, Data.Label);
            Set_Text (Get_Entry (Vsearch.Context_Combo), Data.Label);

            Num := Num + 1;
         end;
      end loop;
   end Search_Functions_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Vsearch : out Vsearch_Extended;
      Handle  : GPS.Kernel.Kernel_Handle) is
   begin
      Vsearch := new Vsearch_Extended_Record;
      Vsearch_Ext.Initialize (Vsearch, Handle);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Vsearch : access Vsearch_Extended_Record'Class;
      Handle  : GPS.Kernel.Kernel_Handle)
   is
      pragma Suppress (All_Checks);
      Bbox : Gtk_Hbutton_Box;
   begin
      Vsearch_Pkg.Initialize (Vsearch, Handle);
      Vsearch.Kernel := Handle;

      --  Create the widget

      Widget_Callback.Object_Connect
        (Vsearch.Context_Entry, "changed",
         On_Context_Entry_Changed'Access, Vsearch);

      Widget_Callback.Object_Connect
        (Vsearch, "map", On_Options_Toggled'Access, Vsearch);

      Gtk_New_From_Stock (Vsearch.Search_Next_Button, Stock_Find);
      Set_First_Next_Mode (Vsearch, Find_Next => False);
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Search_Next_Button, False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Search_Next_Button,
         -"Search next/all occurrence(s)");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Next_Button, "clicked", On_Search'Access, Vsearch);

      Gtk_New (Vsearch.Search_Replace_Button, -"Replace");
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Search_Replace_Button, False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Search_Replace_Button,
         -"Replace next/all occurrence(s)");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Replace_Button, "clicked",
         On_Search_Replace'Access, Vsearch);

      Gtk_New_With_Mnemonic (Vsearch.Search_Previous_Button, -"_Prev.");
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Search_Previous_Button,
         False, False, 0);
      Set_Tip
        (Get_Tooltips (Handle), Vsearch.Search_Previous_Button,
         -"Search previous occurrence");
      Widget_Callback.Object_Connect
        (Vsearch.Search_Previous_Button, "clicked",
         On_Search_Previous'Access, Vsearch);

      Gtk_New (Vsearch.Options_Toggle, -"Options>>");
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Handle).all, "Vsearch_Options_Button", True);
      Associate
        (Get_History (Handle).all, "Vsearch_Options_Button",
         Vsearch.Options_Toggle);
      Pack_Start
        (Vsearch.Buttons_Hbox, Vsearch.Options_Toggle, False, False, 0);
      Set_Tip (Get_Tooltips (Handle), Vsearch.Options_Toggle,
               -"Display extended options");
      Widget_Callback.Object_Connect
        (Vsearch.Options_Toggle, "toggled",
         On_Options_Toggled'Access, Vsearch);

      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Spread);
      Pack_End (Vsearch, Bbox, Expand => False, Padding => 5);

      --  Any change to the fields resets the search mode
      Return_Callback.Object_Connect
        (Vsearch.Pattern_Entry, "key_press_event",
         Return_Callback.To_Marshaller (Key_Press'Access), Vsearch);
      Return_Callback.Object_Connect
        (Vsearch.Replace_Entry, "key_press_event",
         Return_Callback.To_Marshaller (Key_Press_Replace'Access), Vsearch);
      Kernel_Callback.Connect
        (Vsearch.Pattern_Entry, "changed", Reset_Search'Access, Handle);
      Widget_Callback.Object_Connect
        (Vsearch.Replace_Entry, "changed",
         Replace_Text_Changed'Access, Vsearch);
      Kernel_Callback.Connect
        (Vsearch.Context_Entry, "changed", Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Search_All_Check, "toggled", Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Case_Check, "toggled", Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Whole_Word_Check, "toggled", Reset_Search'Access, Handle);
      Kernel_Callback.Connect
        (Vsearch.Regexp_Check, "toggled", Reset_Search'Access, Handle);

      --  Include all the patterns that have been predefined so far, and make
      --  sure that new patterns will be automatically added.
      Widget_Callback.Object_Connect
        (Get_List (Vsearch.Pattern_Combo), "selection_changed",
         Selection_Changed'Access, Vsearch);

      --  Fill the replace combo first, so that the selection remains in
      --  the pattern combo
      Get_History
        (Get_History (Handle).all, Replace_Hist_Key, Vsearch.Replace_Combo,
         Clear_Combo => False, Prepend => True);
      Get_History
        (Get_History (Handle).all, Pattern_Hist_Key, Vsearch.Pattern_Combo,
         Clear_Combo => False, Prepend => True);

      Associate
        (Get_History (Handle).all,
         "case_sensitive_search",
         Vsearch.Case_Check);
      Associate
        (Get_History (Handle).all,
         "all_occurrences_search",
         Vsearch.Search_All_Check);
      Associate
        (Get_History (Handle).all,
         "whole_word_search",
         Vsearch.Whole_Word_Check);
      Associate
        (Get_History (Handle).all,
         "regexp_search",
         Vsearch.Regexp_Check);

      Associate
        (Get_History (Vsearch.Kernel).all,
         Auto_Hide_Hist_Key,
         Vsearch.Auto_Hide_Check);
      Associate
        (Get_History (Vsearch.Kernel).all,
         Select_Window_Hist_Key,
         Vsearch.Select_Editor_Check);

      Add_Hook (Handle, Search_Reset_Hook,
                Wrapper (Set_First_Next_Mode_Cb'Access),
                Name => "vsearch.search_reset");
      Add_Hook (Handle, Search_Functions_Changed_Hook,
                Wrapper (Search_Functions_Changed'Access),
                Name => "vsearch.search_functions");
      Add_Hook (Handle, Search_Regexps_Changed_Hook,
                Wrapper (New_Predefined_Regexp'Access),
                Name => "vsearch.search_regexps");
   end Initialize;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete (Search : access Gtk_Widget_Record'Class)
      return Boolean
   is
      Vsearch : constant Vsearch_Extended := Vsearch_Extended (Search);
   begin
      --  This is called when the user presses "Escape" in the dialog.
      Close_Vsearch (Vsearch);
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Delete;

   ---------------------------
   -- Get_Or_Create_Vsearch --
   ---------------------------

   function Get_Or_Create_Vsearch
     (Kernel       : access Kernel_Handle_Record'Class;
      Raise_Widget : Boolean := False;
      Float_Widget : Boolean := True) return Vsearch_Extended
   is
      Child   : GPS_MDI_Child;
      Success : Boolean;

      Text : constant UTF8_String :=
        Wait_For_Text (Gtk.Clipboard.Get (Selection_Primary));

      pragma Unreferenced (Success);
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Vsearch_Extended_Record'Tag));

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
              (Vsearch_Module_Id.Search, "delete_event", On_Delete'Access);
         end if;

         Gtk_New (Child, Vsearch_Module_Id.Search,
                  Flags => All_Buttons or Float_As_Transient
                     or Always_Destroy_Float,
                  Focus_Widget =>
                    Gtk_Widget (Vsearch_Module_Id.Search.Pattern_Combo),
                  Group    => Group_View,
                  Module => Vsearch_Module_Id,
                  Desktop_Independent => True);
         Set_Title (Child, -"Search");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Left);
         Float_Vsearch (Child);

         Widget_Callback.Connect (Child, "float_child", Float_Vsearch'Access);
         Widget_Callback.Connect
           (Child, "unfloat_child", Float_Vsearch'Access);

         Float_Child (Child, Float_Widget);
         Set_Focus_Child (Child);

         On_Options_Toggled (Vsearch_Module_Id.Search);
      end if;

      --  Automatically fill the pattern text entry with the selection, if
      --  there is one which does not contain multiple lines.

      if Text /= ""
        and then Text'Length < 128
        and then Index (Text, (1 => ASCII.LF)) = 0
      then
         Set_Text (Vsearch_Module_Id.Search.Pattern_Entry, Text);
      end if;

      --  Reset the all occurrences check box.
      Set_Active (Vsearch_Module_Id.Search.Search_All_Check, False);

      if Raise_Widget then
         Raise_Child (Child);
      end if;

      return Vsearch_Module_Id.Search;
   end Get_Or_Create_Vsearch;

   --------------------
   -- Search_Menu_Cb --
   --------------------

   procedure Search_Menu_Cb
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Vsearch : Vsearch_Extended;
      Module  : constant Module_ID := Get_Current_Module (Kernel);
   begin
      --  We must create the search dialog only after we have found the current
      --  context, otherwise it would return the context of the search widget
      --  itself

      Vsearch := Get_Or_Create_Vsearch (Kernel, Raise_Widget => True);

      --  ??? Temporarily: reset the entry. The correct fix would be to
      --  separate the find and replace tabs, so that having a default
      --  entry in this combo doesn't look strange when the combo is
      --  insensitive.
      Set_Text (Get_Entry (Vsearch.Replace_Combo), "");

      if Module /= null then
         declare
            Search : constant Search_Module_Data :=
              Vsearch_Ext.Search_Context_From_Module (Module);
         begin
            if Search /= No_Search then
               Set_Text (Get_Entry (Vsearch.Context_Combo), Search.Label);
            end if;
         end;
      end if;

      Grab_Focus (Vsearch.Pattern_Entry);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Search_Menu_Cb;

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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Search_Previous_Cb;

   ------------------------------
   -- Register_Search_Function --
   ------------------------------

   procedure Register_Search_Function
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : Search_Module_Data) is
   begin
      Prepend (Vsearch_Module_Id.Search_Modules, Data);

      if Data.Extra_Information /= null then
         Ref (Data.Extra_Information);
         Sink (Data.Extra_Information);
      end if;

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
         if Data (List).Label = Label then
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
     (Id : access Abstract_Module_ID_Record'Class)
      return Find_Utils.Search_Module_Data
   is
      List : List_Node := First (Vsearch_Module_Id.Search_Modules);
   begin
      while List /= Null_Node loop
         if Data (List).Id = Abstract_Module_ID (Id) then
            return Data (List);
         end if;

         List := Next (List);
      end loop;

      return No_Search;
   end Search_Context_From_Module;

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

   procedure Customize
     (Module : access Vsearch_Module_Record;
      File   : VFS.Virtual_File;
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
      Mitem    : Gtk_Menu_Item;
   begin
      Vsearch_Module_Id := new Vsearch_Module_Record;
      Register_Module
        (Module      => Module_ID (Vsearch_Module_Id),
         Kernel      => Kernel,
         Module_Name => Search_Module_Name,
         Priority    => Default_Priority);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Vsearch_Module_Id.Tab_Width := Natural (Get_Pref (Tab_Width));

      --  Register the menus
      Register_Menu
        (Kernel, Navigate, null, Ref_Item => -"Edit", Add_Before => False);

      Register_Menu
        (Kernel, Navigate, -"_Find or Replace...",
         Stock_Find, Search_Menu_Cb'Access,
         Ref_Item => Find_All,
         Accel_Key => GDK_F, Accel_Mods => Control_Mask);

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
   end Register_Module;

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
      Vsearch_Module_Id.Tab_Width := Natural (Get_Pref (Tab_Width));
   end Preferences_Changed;

end Vsearch_Ext;
