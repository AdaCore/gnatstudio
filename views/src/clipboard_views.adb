------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Strings;              use GNAT.Strings;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib_Values_Utils;         use Glib_Values_Utils;
with Glib.Unicode;              use Glib.Unicode;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;

with Gtkada.Handlers;           use Gtkada.Handlers;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Generic_Views;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with Tooltips;                  use Tooltips;

with GNATCOLL.Arg_Lists;    use GNATCOLL.Arg_Lists;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

package body Clipboard_Views is
   Me : constant Trace_Handle := Create ("GPS.VIEWS.CLIPBOARD");

   type Clipboard_View_Record is new Generic_Views.View_Record with record
      Tree    : Gtk_Tree_View;
   end record;

   function Initialize
     (View   : access Clipboard_View_Record'Class) return Gtk_Widget;
   --  Create a new clipboard view

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Clipboard_View",
      View_Name          => "Clipboard",
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Formal_View_Record => Clipboard_View_Record,
      Areas              => Gtkada.MDI.Sides_Only);
   use Generic_View;
   subtype Clipboard_View_Access is Generic_View.View_Access;

   type On_Clipboard_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Clipboard_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the contents of the clipboard has changed

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   procedure Refresh (View : access Clipboard_View_Record'Class);
   --  Refresh the contents of the clipboard view

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   function Get_Selected_From_Event
     (View  : access Clipboard_View_Record'Class;
      Event : Gdk_Event) return Integer;
   --  Return the entry selected by event

   type Merge_With_Previous_Command
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Merge_With_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Merge the selected entry with the previous one

   type Remove_Entry_Command
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Entry_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove the currently selected entry

   --------------
   -- Tooltips --
   --------------

   type Clipboard_View_Tooltips is new Tooltips.Tooltips with record
      Kernel : Kernel_Handle;
   end record;
   type Clipboard_View_Tooltips_Access is access all Clipboard_View_Tooltips;
   overriding function Create_Contents
     (Tooltip  : not null access Clipboard_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Clipboard_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      Tree  : constant Gtk_Tree_View := Gtk_Tree_View (Widget);
      Model      : constant Gtk_Tree_Model := Get_Model (Tree);
      Iter       : Gtk_Tree_Iter;
      Selected   : Integer;
      Label      : Gtk_Label;
      Area       : Gdk_Rectangle;
   begin
      Initialize_Tooltips (Tree, X, Y, Area, Iter);
      if Iter /= Null_Iter then
         Tooltip.Set_Tip_Area (Area);

         Selected := Integer (Get_Int (Model, Iter, 2));

         Gtk_New
           (Label,
            Get_Content (Get_Clipboard (Tooltip.Kernel)) (Selected).all);
      end if;
      return Gtk_Widget (Label);
   end Create_Contents;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Merge_With_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Selected : Integer;
      View : constant Clipboard_View_Access :=
        Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      if View /= null then
         Get_Selected (View.Tree.Get_Selection, Model, Iter);
         if Iter /= Null_Iter then
            Selected := Integer (Get_Int (Model, Iter, 2));
            if Selected /= -1 then
               Merge_Clipboard
                 (Get_Clipboard (View.Kernel), Selected, Selected + 1);
               return Success;
            end if;
         end if;
      end if;
      return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Entry_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Selected : Integer;
      View : constant Clipboard_View_Access :=
        Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      if View /= null then
         Get_Selected (View.Tree.Get_Selection, Model, Iter);
         if Iter /= Null_Iter then
            Selected := Integer (Get_Int (Model, Iter, 2));
            if Selected /= -1 then
               Remove_Clipboard_Entry (Get_Clipboard (View.Kernel), Selected);
               return Success;
            end if;
         end if;
      end if;
      return Failure;
   end Execute;

   -----------------------------
   -- Get_Selected_From_Event --
   -----------------------------

   function Get_Selected_From_Event
     (View  : access Clipboard_View_Record'Class;
      Event : Gdk_Event) return Integer
   is
      Model : constant Gtk_Tree_Store := -Get_Model (View.Tree);
      Iter : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (View.Tree, Event);
      if Iter /= Null_Iter then
         return Integer (Get_Int (Model, Iter, 2));
      else
         return -1;
      end if;
   end Get_Selected_From_Event;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      View : constant Clipboard_View_Access := Clipboard_View_Access (Clip);
      Selected : Integer;
      CL       : Arg_List;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Selected := Get_Selected_From_Event (View, Event);
         if Selected > 0 then
            --  Put the focus back on the current editor

            CL := Create ("EditorBuffer.get");
            Execute_GPS_Shell_Command (View.Kernel, CL);

            CL := Create ("EditorBuffer.current_view %1");
            Execute_GPS_Shell_Command (View.Kernel, CL);

            CL := Create ("MDI.get_by_child %1");
            Execute_GPS_Shell_Command (View.Kernel, CL);

            CL := Create ("MDIWindow.raise_window %1");
            Execute_GPS_Shell_Command (View.Kernel, CL);

            Paste_Clipboard
              (Get_Clipboard (View.Kernel),
               Get_Current_Focus_Widget (View.Kernel),
               Selected);
            return True;
         end if;
      end if;
      return False;
   end Button_Press;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Clipboard_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Refresh (Generic_View.Get_Or_Create_View (Kernel, Focus => False));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Self);
      View : constant Clipboard_View_Access :=
        Generic_View.Get_Or_Create_View (Kernel, Focus => False);
   begin
      Set_Font_And_Colors (View.Tree, Fixed_Font => True, Pref => Pref);
   end Execute;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Clipboard_View_Record'Class) is
      Model           : constant Gtk_Tree_Store := -Get_Model (View.Tree);
      Selection       : constant Selection_List :=
                          Get_Content (Get_Clipboard (View.Kernel));
      Iter            : Gtk_Tree_Iter;
      Last_Paste      : constant Integer :=
                          Get_Last_Paste (Get_Clipboard (View.Kernel));
      Index, First    : Natural;
      Start_Truncated : Boolean;
      End_Truncated   : Boolean;
      Result          : Unbounded_String;
      Ellipsis        : String (1 .. 6);
      Ellipsis_Last   : Integer;

   begin
      Clear (Model);

      for S in Selection'Range loop
         if Selection (S) /= null then
            Append (Model, Iter, Null_Iter);

            Start_Truncated := False;
            End_Truncated := False;

            --  Show only the first line of the selection
            First := Selection (S)'First;
            while First <= Selection (S)'Last
              and then Is_Blank (Selection (S)(First))
            loop
               Start_Truncated := True;
               First := First + 1;
            end loop;

            Index := First;

            --  and only display the first line starting from there
            while Index <= Selection (S)'Last loop
               if Selection (S) (Index) = ASCII.LF then
                  End_Truncated := True;
                  exit;
               end if;

               Index := Index + 1;
            end loop;

            if First > Selection (S)'Last then
               Result := Null_Unbounded_String;
            else
               Result :=
                 To_Unbounded_String (Selection (S) (First .. Index - 1));
            end if;

            if Start_Truncated or End_Truncated then
               Unichar_To_UTF8 (8230, Ellipsis, Ellipsis_Last);
            end if;

            if Start_Truncated then
               Result := Ellipsis (Ellipsis'First .. Ellipsis_Last) & Result;
            end if;

            if End_Truncated then
               Result := Result & Ellipsis (Ellipsis'First .. Ellipsis_Last);
            end if;

            --  There is a pathological case here: if only ASCII.LF was
            --  selected, it will be shown in the view on 2 lines.

            Set_And_Clear
              (Model, Iter,
               (0 => As_String
                    (if Last_Paste = S
                     then "gps-forward-symbolic"
                     else ""),
                1 => As_String (To_String (Result)),
                2 => As_Int    (Gint (S))));
         end if;
      end loop;
   exception
      when E : others => Trace (Me, E);
   end Refresh;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Clipboard_View_Record'Class) return Gtk_Widget
   is
      Tooltip  : Clipboard_View_Tooltips_Access;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (View, Homogeneous => False);
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      View.Pack_Start (Scrolled, Expand => True, Fill => True);

      View.Tree := Create_Tree_View
        (Column_Types       => (0 => GType_Icon_Name_String,
                                1 => GType_String,
                                2 => GType_Int),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Selection_Mode     => Selection_Single,
         Sortable_Columns   => False,
         Hide_Expander      => True);
      Scrolled.Add (View.Tree);

      Modify_Font (View.Tree, View_Fixed_Font.Get_Pref);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Clipboard_Changed_Hook.Add (new On_Clipboard_Changed, Watch => View);
      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => View);
      Refresh (View);

      --  Initialize tooltips

      Tooltip := new Clipboard_View_Tooltips;
      Tooltip.Kernel := View.Kernel;
      Set_Tooltip (Tooltip, View.Tree);

      return Gtk_Widget (View.Tree);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Generic_View.Register_Module (Kernel);

      Register_Action
        (Kernel, "Clipboard View Append To Previous",
         new Merge_With_Previous_Command,
         -"Append to previous clipboard entry",
         Icon_Name => "gps-add-symbolic",
         Category => -"Clipboard");

      Register_Action
        (Kernel, "Clipboard View Remove Entry", new Remove_Entry_Command,
         -"Remove selected clipboard entry",
         Icon_Name => "gps-remove-symbolic",
         Category => -"Clipboard");
   end Register_Module;

end Clipboard_Views;
