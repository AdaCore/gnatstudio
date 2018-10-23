------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Fixed;

with Glib;                      use Glib;
with Glib.Values;
with Pango.Layout;              use Pango.Layout;

with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;     use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;       use Gtk.Tree_Model_Sort;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Terminal;
with Gtkada.Types;              use Gtkada.Types;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with GPS.Main_Window;           use GPS.Main_Window;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Search;                use GPS.Search;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Default_Preferences.Enums;
with Interactive_Consoles;      use Interactive_Consoles;
with Generic_Views;
with Glib_Values_Utils;         use Glib_Values_Utils;
with Filter_Panels;             use Filter_Panels;

package body Log_File_Views is

   Me : constant Trace_Handle := Create ("GPS.VIEWS.LOG_VIEW");

   package Strings is new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Interceptor_Type is new Trace_Decorator_Record with null record;

   overriding procedure After_Message
     (Self   : in out Interceptor_Type;
      Handle : not null Logger;
      Msg    : in out Msg_Strings.XString);

   package Preferences_Map is
     new Ada.Containers.Indefinite_Ordered_Maps (String, Boolean_Preference);

   type Log_View_Record is new Interactive_Console_Record with record
      Is_Filtering : Boolean := False;
      --  Do we need a filtration of messages
      Preferences  : Preferences_Map.Map;
      --  Map of preferences which control filtration
   end record;
   type Log_View is access all Log_View_Record'Class;

   function Initialize (View : access Log_View_Record'Class) return Gtk_Widget;
   --  Create a new log view

   procedure Refresh (View : access Log_View_Record'Class);
   --  Refresh the contents of the log view

   procedure Append
     (View : access Log_View_Record'Class;
      Str  : String);

   function Is_Allowed
     (View : access Log_View_Record'Class;
      Str  : String)
      return Boolean;
   --  Checks whether trace's output is allowed in local config menu

   procedure On_Preferences_Changed (View : access Log_View_Record'Class);
   --  Update view properties due to preferences changing

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Log_View",
      View_Name          => "Log",
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Formal_View_Record => Log_View_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Consoles);
   subtype Log_View_Access is Generic_View.View_Access;
   use Generic_View;

   package Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   procedure Register_Preferences
     (Kernel : Kernel_Handle;
      View   : access Log_View_Record'Class);
   --  Creates preference for each trace

   type Save_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Save_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Saves the contents of view in a file

   type Configure_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Configure_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Sets configuration

   type Properties_Editor_Record is new Gtk_Dialog_Record with record
      Kernel             : access Kernel_Handle_Record'Class;
      View               : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Disable_Filtering  : Boolean := False;
      Filter_Panel       : Filter_Panels.Filter_Panel;
      Filter_Pattern     : Search_Pattern_Access;
      Toggle             : Gtk_Check_Button;
   end record;
   type Properties_Editor is access all Properties_Editor_Record'Class;

   procedure On_Filter_Changed
     (Self : access Gtk_Widget_Record'Class);

   procedure Initialize
     (Self   : not null access Properties_Editor_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Create the properties editor

   procedure Fill (Self : not null access Properties_Editor_Record'Class);

   package Properties_Editor_Visible_Funcs is new
     Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data (Properties_Editor);
   function Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Properties_Editor) return Boolean;
   --  Selects whether a given row should be visible in the editor.

   package Tree_View_Column_Callbacks is
     new Gtk.Handlers.User_Callback
       (Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record, Properties_Editor);

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Properties_Editor);
   --  Called on click on the column header

   package Cell_Renderer_Toggle_Callbacks is
     new Gtk.Handlers.User_Callback
       (Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record,
        Properties_Editor);

   package Cell_Renderer_Toggle_Callbacks_Marshallers is
     new Cell_Renderer_Toggle_Callbacks.Marshallers.Generic_Marshaller
       (Gtkada.Types.Chars_Ptr, Glib.Values.Get_Chars);

   procedure On_Select_Trace_Toggled
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Chars_Ptr;
      Self   : Properties_Editor);
   --  Called on click on the list's item

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Log_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   Name_Column   : constant := 0;
   Toggle_Column : constant := 1;

   Registered             : Boolean := False;
   Preferences_Registered : Boolean := False;
   Lines                  : Strings.Vector;
   Kernel                 : Kernel_Handle;
   Has_View               : Boolean := False;

   type Log_View_Type is (Off, Only_When_Opened, Always);

   package Log_View_Kind_Preferences is new
     Default_Preferences.Enums.Generics (Log_View_Type);
   use Log_View_Kind_Preferences;

   Log_View_Preference : Log_View_Kind_Preferences.Preference;

   -------------------
   -- After_Message --
   -------------------

   overriding procedure After_Message
     (Self   : in out Interceptor_Type;
      Handle : not null Logger;
      Msg    : in out Msg_Strings.XString)
   is
      pragma Unreferenced (Self);
      View     : View_Access;
      Kernel   : constant Kernel_Handle := Log_File_Views.Kernel;
      Log_Type : Log_View_Type;
   begin
      if Handle.Unit_Name = Gtkada.Terminal.Trace_Name then
         --  Inserting in a terminal activates this trace,
         --  which calls After_Message recursively and so on.
         return;
      end if;

      --  Do nothing if GPS is exiting or if the Enable_Log_View
      --  preference is set to False.

      if Kernel /= null
        and then Kernel.Is_In_Destruction
      then
         return;
      end if;

      if Log_View_Preference /= null then
         Log_Type := Log_View_Preference.Get_Pref;

         case Log_Type is
            when Off =>
               return;

            when Only_When_Opened =>
               if Generic_View.Retrieve_View
                 (Log_File_Views.Kernel, False) = null
               then
                  return;
               end if;

            when Always =>
               null;
         end case;
      end if;

      declare
         Str : constant String := Msg_Strings.To_String (Msg);
      begin
         Strings.Append (Lines, Str);

         if Log_File_Views.Kernel /= null
           and then Has_View
         then
            View := Generic_View.Retrieve_View (Log_File_Views.Kernel, False);

            if View /= null then
               Log_View_Access (View).Append (Str);
            end if;
         end if;
      end;
   end After_Message;

   ------------
   -- Append --
   ------------

   procedure Append
     (View : access Log_View_Record'Class;
      Str  : String) is
   begin
      if not View.Is_Filtering
        or else View.Is_Allowed (Str)
      then
         View.Insert_UTF8 (Str, Show_Prompt => False);
      end if;
   end Append;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Save_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      View  : constant Log_View_Access :=
        Generic_View.Retrieve_View (Get_Kernel (Context.Context));
      Dummy : Boolean;
   begin
      if View = null then
         return Commands.Success;
      end if;

      declare
         File : constant Virtual_File :=
                  Select_File
                    (Title             => "Save log as",
                     Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                     Kind              => Save_File,
                     Parent            => Get_Current_Window (View.Kernel),
                     History           => Get_History (View.Kernel));
      begin
         if File /= GNATCOLL.VFS.No_File then
            Dummy := View.Export (File);
         end if;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Configure_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      View     : constant Log_View_Access :=
        Generic_View.Retrieve_View (Get_Kernel (Context.Context));

      Props : Properties_Editor;
      Dummy : Gtk_Response_Type;
   begin
      if View = null then
         return Commands.Success;
      end if;

      Props := new Properties_Editor_Record;
      Initialize (Props, View.Kernel);

      Dummy := Props.Run;

      View.Is_Filtering := False;
      for Pref of View.Preferences loop
         if not Pref.Get_Pref then
            View.Is_Filtering := True;
            exit;
         end if;
      end loop;

      View.Refresh;
      GPS.Search.Free (Props.Filter_Pattern);
      Props.Destroy;

      return Commands.Success;
   end Execute;

   ----------
   -- Fill --
   ----------

   procedure Fill (Self : not null access Properties_Editor_Record'Class) is
      Has_Active   : Boolean := False;
      Has_Inactive : Boolean := False;
      Iter         : Gtk_Tree_Iter;
      Path         : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Column       : Gtk_Tree_View_Column;
      Active       : Boolean;

      Manager : constant Default_Preferences.Preferences_Manager :=
        Self.Kernel.Get_Preferences;
      C       : Preference_Cursor := Manager.Get_First_Reference;
      Set     : Sets.Set;
      P       : Default_Preferences.Preference;

   begin
      --  Disable tree filtering while refreshing the contents of the tree.
      --  This works around a bug in gtk+.
      Self.Disable_Filtering := True;

      Self.View.Get_Cursor (Path, Column);
      Clear (Self.Model);

      loop
         P := Get_Pref (C, Manager);
         exit when P = null;

         if Starts_With (P.Get_Name, "trace-log-")
           and then P.all in Boolean_Preference_Record'Class
         then
            Set.Insert (P.Get_Name);
         end if;
         Next (C);
      end loop;

      for Name of Set loop
         Append (Self.Model, Iter, Null_Iter);

         Active := Boolean_Preference
           (Manager.Get_Pref_From_Name (Name)).Get_Pref;

         if Active then
            Has_Active := True;
         else
            Has_Inactive := True;
         end if;

         Set_And_Clear
           (Self.Model,
            Iter,
            (Name_Column, Toggle_Column),
            (1 => As_String (Name (Name'First + 10 .. Name'Last)),
             2 => As_Boolean (Active)));
      end loop;

      Self.Toggle.Set_Active (False);
      Self.Toggle.Set_Inconsistent (False);

      if Has_Active
        and then Has_Inactive
      then
         Self.Toggle.Set_Inconsistent (True);

      elsif Has_Active then
         Self.Toggle.Set_Active (True);
      end if;

      Self.Disable_Filtering := False;

      --  Select old selected item
      if Path /= Null_Gtk_Tree_Path then
         Self.View.Set_Cursor (Path, Column, False);
         Path_Free (Path);
      end if;

      Refilter (Self.Filter);
   end Fill;

   -----------------------
   -- On_Filter_Changed --
   -----------------------

   procedure On_Filter_Changed
     (Self : access Gtk_Widget_Record'Class)
   is
      E : constant Properties_Editor := Properties_Editor (Self);

      Pattern : constant Search_Pattern_Access :=
        E.Filter_Panel.Get_Filter_Pattern;
   begin
      Free (E.Filter_Pattern);
      E.Filter_Pattern := Pattern;
      E.Filter.Refilter;
   end On_Filter_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View : access Log_View_Record'Class) return Gtk_Widget
   is
      Hook : access On_Pref_Changed;
   begin
      Register_Preferences (View.Kernel, View);

      Initialize
        (View,
         View.Kernel,
         "",
         null,
         View.Kernel.all'Address,
         Highlight     => null,
         History_List  => null,
         ANSI_Support  => True,
         Key           => "",
         Wrap_Mode     => Wrap_Char,
         Manage_Prompt => False);

      Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);

      View.Enable_Prompt_Display (False);
      Set_Font_And_Colors (View.Get_View, Fixed_Font => True);

      Hook := new On_Pref_Changed;
      Hook.View := Log_View (View);
      Preferences_Changed_Hook.Add (Hook, Watch => View);
      View.On_Preferences_Changed;

      Has_View := True;

      if Log_View_Preference.Get_Pref /= Always then
         Strings.Clear (Lines);
      else
         Refresh (View);
      end if;

      return Gtk_Widget (View.Get_View);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Properties_Editor_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      Scrolled      : Gtk_Scrolled_Window;
      Col           : Gtk_Tree_View_Column;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Toggle_Render : Gtk_Cell_Renderer_Toggle;
      Ignore        : Gint;
   begin
      Gtk.Dialog.Initialize
        (Self,
         "Configure log representation",
         Parent => Kernel.Get_Main_Window,
         Flags  => Destroy_With_Parent);

      Set_Default_Size_From_History
        (Self, "log_prop_editor", Kernel, 600, 500);

      Self.Kernel := Kernel;

      Gtk_New
        (Self.Model,
         (Name_Column   => GType_String,
          Toggle_Column => GType_Boolean));

      Gtk_New (Self.Filter, +Self.Model);
      Properties_Editor_Visible_Funcs.Set_Visible_Func
        (Self.Filter, Is_Visible'Access, Self);

      Gtk_New_With_Model (Self.Sort, +Self.Filter);

      Gtk_New
        (Self.Filter_Panel,
         Kernel_Handle (Kernel),
         "log-config-",
         Tooltip     => "Filter the contents",
         Placeholder => "filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);

      Self.Get_Content_Area.Pack_Start (Self.Filter_Panel, False, False);
      Widget_Callback.Object_Connect
        (Self.Filter_Panel,
         Signal_Filter_Changed,
         Widget_Callback.To_Marshaller (On_Filter_Changed'Access),
         Self);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Get_Content_Area.Pack_Start (Scrolled, True, True);

      Gtk_New (Self.View, Self.Sort);
      Self.View.Set_Name ("Log traces editor tree"); --  for testsuite
      Scrolled.Add (Self.View);

      --  The tree

      Gtk_New (Col);
      Col.Set_Clickable (True);
      Tree_View_Column_Callbacks.Connect
        (Col,
         Gtk.Tree_View_Column.Signal_Clicked,
         Tree_View_Column_Callbacks.To_Marshaller
           (On_Select_All_Toggled'Access),
         Properties_Editor (Self));

      Gtk.Check_Button.Gtk_New (Self.Toggle, "");
      Self.Toggle.Set_Inconsistent (False);
      Self.Toggle.Set_Active (False);
      Self.Toggle.Show;
      Col.Set_Widget (Self.Toggle);

      Gtk_New (Toggle_Render);
      Col.Pack_End (Toggle_Render, False);
      Col.Add_Attribute (Toggle_Render, "active", Toggle_Column);
      Ignore := Self.View.Append_Column (Col);

      Cell_Renderer_Toggle_Callbacks.Connect
        (Toggle_Render,
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         Cell_Renderer_Toggle_Callbacks_Marshallers.To_Marshaller
           (On_Select_Trace_Toggled'Access),
         Properties_Editor (Self),
         True);

      Gtk_New (Col);
      Ignore := Self.View.Append_Column (Col);
      Set_Title (Col, "Name");
      Gtk_New (Text_Render);
      Set_Property
        (Text_Render,
         Gtk.Cell_Renderer_Text.Ellipsize_Property, Ellipsize_Middle);
      Pack_Start (Col, Text_Render, True);
      Add_Attribute (Col, Text_Render, "text", Name_Column);
      Set_Clickable (Col, True);
      Set_Resizable (Col, True);
      Set_Sort_Column_Id (Col, Name_Column);

      Clicked (Col);

      Self.Fill;
      Self.Show_All;

   exception
      when E : others =>
         Trace (Me, E);
   end Initialize;

   ----------------
   -- Is_Allowed --
   ----------------

   function Is_Allowed
     (View : access Log_View_Record'Class;
      Str  : String)
      return Boolean
   is
      S, E : Natural;
      P    : Boolean_Preference;
      C    : Preferences_Map.Cursor;
   begin
      S := Ada.Strings.Fixed.Index (Str, "[");
      if S < Str'First then
         return True;
      end if;

      E := Ada.Strings.Fixed.Index (Str, "]", S + 1);
      if E < Str'First then
         return True;
      end if;

      C := View.Preferences.Find (Str (S + 1 .. E - 1));
      if Preferences_Map.Has_Element (C) then
         P := Preferences_Map.Element (C);
      end if;

      return P = null or else P.Get_Pref;
   end Is_Allowed;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Properties_Editor) return Boolean
   is
      Row_Visible : Boolean := True;
      Child       : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Data.Disable_Filtering then
         return True;
      end if;

      --  Compute the row itself should be visible (not withstanding its
      --  children).

      if Data.Filter_Pattern /= null then
         Row_Visible := Data.Filter_Pattern.Start
           (Get_String (Model, Iter, Name_Column)) /= GPS.Search.No_Match;
      end if;

      --  If the row should be invisible, but any of its children is visible,
      --  we display it anyway.

      if not Row_Visible then
         Child := Children (Model, Iter);
         while Child /= Null_Iter loop
            if Data.Filter_Pattern.Start
              (Get_String (Model, Child, Name_Column)) /= GPS.Search.No_Match
            then
               return True;
            end if;
            Next (Model, Child);
         end loop;
      end if;

      return Row_Visible;

   exception
      when others =>
         return True;
   end Is_Visible;

   ---------------------------
   -- On_Select_All_Toggled --
   ---------------------------

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Properties_Editor)
   is
      pragma Unreferenced (Object);

      procedure Set (Value : Boolean);

      procedure Set (Value : Boolean) is
         Manager : constant Default_Preferences.Preferences_Manager :=
           Self.Kernel.Get_Preferences;

      begin
         Manager.Freeze;
         declare
            C : Preference_Cursor := Manager.Get_First_Reference;
            P : Default_Preferences.Preference;
         begin
            loop
               P := Get_Pref (C, Manager);
               exit when P = null;

               if Starts_With (P.Get_Name, "trace-log-")
                 and then P.all in Boolean_Preference_Record'Class
               then
                  Set_Pref (Boolean_Preference (P), Manager, Value);
               end if;

               Next (C);
            end loop;
         exception
            when E : others =>
               Trace (Me, E);
         end;
         Manager.Thaw;
      end Set;

   begin
      if Self.Toggle.Get_Active then
         Set (False);
      else
         Set (True);
      end if;

      Self.Fill;
   end On_Select_All_Toggled;

   -----------------------------
   -- On_Select_Trace_Toggled --
   -----------------------------

   procedure On_Select_Trace_Toggled
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Chars_Ptr;
      Self   : Properties_Editor)
   is
      pragma Unreferenced (Object);

      Manager : constant Default_Preferences.Preferences_Manager :=
        Self.Kernel.Get_Preferences;
      P       : Default_Preferences.Preference;

      Sort_Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
        Gtk.Tree_Model.Get_Iter_From_String
          (Gtk.Tree_Model.To_Interface (Self.Sort),
           Value (Path));

      Filter_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value       : Boolean;
   begin
      Self.Sort.Convert_Iter_To_Child_Iter (Filter_Iter, Sort_Iter);
      Self.Filter.Convert_Iter_To_Child_Iter (Model_Iter, Filter_Iter);

      P := Manager.Get_Pref_From_Name
        ("trace-log-" & Self.Model.Get_String (Model_Iter, Name_Column));

      if P /= null
        and then P.all in Boolean_Preference_Record'Class
      then
         Value := not Boolean_Preference (P).Get_Pref;
         Set_Pref (Boolean_Preference (P), Manager, Value);
         Set_And_Clear
           (Self.Model,
            Model_Iter,
            (1 => Toggle_Column),
            (1 => As_Boolean (Value)));
      end if;
   end On_Select_Trace_Toggled;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Log_View_Record'Class) is
   begin
      View.Clear;

      for Str of Lines loop
         View.Append (Str);
      end loop;
   end Refresh;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Kernel);
   begin
      if Pref = null
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Default_Style)
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Numbers_Style)
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Types_Style)
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Strings_Style)
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Keywords_Style)
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Bookmark_Color)
        or else Pref = Default_Preferences.Preference
          (GPS.Kernel.Preferences.Comments_Style)
      then
         Self.View.On_Preferences_Changed;
      end if;
   end Execute;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (View);
   begin
      Has_View := False;

      if Log_View_Preference.Get_Pref /= Always then
         Strings.Clear (Lines);
      end if;
   end On_Destroy;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed (View : access Log_View_Record'Class) is
   begin
      View.Set_Foreground
        (Gtkada.Terminal.Black,
         GPS.Kernel.Preferences.Default_Style.Get_Pref_Fg);
      View.Set_Foreground
        (Gtkada.Terminal.Red,
         GPS.Kernel.Preferences.Numbers_Style.Get_Pref_Fg);
      View.Set_Foreground
        (Gtkada.Terminal.Green,
         GPS.Kernel.Preferences.Types_Style.Get_Pref_Fg);
      View.Set_Foreground
        (Gtkada.Terminal.Yellow,
         GPS.Kernel.Preferences.Strings_Style.Get_Pref_Fg);
      View.Set_Foreground
        (Gtkada.Terminal.Blue,
         GPS.Kernel.Preferences.Keywords_Style.Get_Pref_Fg);
      View.Set_Foreground
        (Gtkada.Terminal.Magenta,
         GPS.Kernel.Preferences.Bookmark_Color.Get_Pref);
      View.Set_Foreground
        (Gtkada.Terminal.Cyan,
         GPS.Kernel.Preferences.Comments_Style.Get_Pref_Fg);
      View.Set_Foreground
        (Gtkada.Terminal.White,
         GPS.Kernel.Preferences.Default_Style.Get_Pref_Bg);
   end On_Preferences_Changed;

   --------------------------
   -- Register_Interceptor --
   --------------------------

   procedure Register_Interceptor is
   begin
      if not Registered then
         Registered := True;
         Add_Global_Decorator (new Interceptor_Type, "LOGVIEWER");
         Set_Active (Create ("LOGVIEWER"), True);
      end if;
   end Register_Interceptor;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Log_File_Views.Kernel := Kernel_Handle (Kernel);

      Log_View_Preference := Log_View_Kind_Preferences.Create
        (Get_Preferences (Kernel),
         Path    => "Traces",
         Name    => "Log-View-Type",
         Label   => "Log collecting policy",
         Doc     => "When the Log view collects messages",
         Default => Only_When_Opened);

      if Log_View_Preference.Get_Pref = Off then
         Strings.Clear (Lines);
         return;
      end if;

      Register_Action
        (Kernel, "log save",
         new Save_Command,
         "Save to file",
         Icon_Name => "gps-save-symbolic");

      Register_Action
        (Kernel, "log configure",
         new Configure_Command,
         "Configure log view",
         Icon_Name => "gps-settings-symbolic");

      Generic_View.Register_Module (Kernel);
   end Register_Module;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : Kernel_Handle;
      View   : access Log_View_Record'Class)
   is
      Set : Sets.Set;

      procedure Process (Handle : Trace_Handle);

      -------------
      -- Process --
      -------------

      procedure Process (Handle : Trace_Handle)
      is
         Name : constant String := Handle.Unit_Name;
      begin
         if (Starts_With (Name, "GPS.")
             and then not Starts_With (Name, "GPS.INTERNAL"))
           or else Starts_With (Name, "SQL.")
           or else Starts_With (Name, "PYTHON")
           or else Starts_With (Name, "MAKE")
           or else Starts_With (Name, "VCS")
           or else Starts_With (Name, "ENTITIES")
           or else Starts_With (Name, "PRJ_NORMALIZE")
           or else Starts_With (Name, "MODELING")
         then
            Set.Insert (Name);
         end if;
      end Process;

      Pref : Boolean_Preference;
   begin
      if Preferences_Registered then
         return;
      end if;

      Preferences_Registered := True;

      GNATCOLL.Traces.For_Each_Handle (Process'Unrestricted_Access);

      for Item of Set loop
         Pref := Kernel.Get_Preferences.Create_Invisible_Pref
           ("trace-log-" & Item, True, Label => Item);

         View.Preferences.Insert (Item, Pref);

         if not Pref.Get_Pref then
            View.Is_Filtering := True;
         end if;
      end loop;
   end Register_Preferences;

end Log_File_Views;
