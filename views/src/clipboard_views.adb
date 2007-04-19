-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2007                      --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.Strings;              use GNAT.Strings;

with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Unicode;              use Glib.Unicode;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Generic_Views;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Clipboard;      use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Pixmaps_IDE;               use Pixmaps_IDE;
with String_Utils;              use String_Utils;
with Tooltips;                  use Tooltips;
with Traces;                    use Traces;

package body Clipboard_Views is

   type Clipboard_View_Record is new Generic_Views.View_Record with record
      Tree    : Gtk_Tree_View;
      Kernel  : Kernel_Handle;
      Current : Gdk_Pixbuf;
   end record;

   procedure Initialize
     (View   : access Clipboard_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new clipboard view

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Clipboard_View",
      View_Name          => "Clipboard",
      Formal_View_Record => Clipboard_View_Record);
   subtype Clipboard_View_Access is Generic_View.View_Access;

   procedure On_Clipboard_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the contents of the clipboard has changed

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Refresh (View : access Clipboard_View_Record'Class);
   --  Refresh the contents of the clipboard view

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Context factory when creating contextual menus

   function Get_Selected_From_Event
     (View  : access Clipboard_View_Record'Class;
      Event : Gdk_Event) return Integer;
   --  Return the entry selected by event

   type Merge_With_Previous_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Merge_With_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Merge the selected entry with the previous one

   type Remove_Entry_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Remove_Entry_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove the currently selected entry

   --------------
   -- Tooltips --
   --------------

   type Clipboard_View_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Clipboard_View : Clipboard_View_Access;
   end record;
   type Clipboard_View_Tooltips_Access is
     access all Clipboard_View_Tooltips'Class;
   procedure Draw
     (Tooltip : access Clipboard_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access Clipboard_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Model      : constant Gtk_Tree_Model :=
                     Get_Model (Tooltip.Clipboard_View.Tree);
      Iter       : Gtk_Tree_Iter;
      Selected   : Integer;

      Text       : GNAT.Strings.String_Access;
   begin
      Pixmap := null;

      Initialize_Tooltips (Tooltip.Clipboard_View.Tree, Area, Iter);
      if Iter /= Null_Iter then
         Selected := Integer (Get_Int (Model, Iter, 2));

         Text := new String'
           (Get_Content
              (Get_Clipboard (Tooltip.Clipboard_View.Kernel)) (Selected).all);

         if Text /= null then
            Create_Pixmap_From_Text
              (Text.all,
               Get_Pref (Default_Font),
               White (Get_Default_Colormap),
               Tooltip.Clipboard_View.Tree,
               Pixmap);
            Free (Text);
         end if;
      end if;
   end Draw;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Merge_With_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Selected : Integer;
      View : constant Clipboard_View_Access :=
        Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
   begin
      if Context.Event /= null then
         Selected := Get_Selected_From_Event (View, Context.Event);
         if Selected /= -1 then
            Merge_Clipboard
              (Get_Clipboard (View.Kernel), Selected, Selected + 1);
            return Success;
         end if;
      end if;
      return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Remove_Entry_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Selected : Integer;
      View : constant Clipboard_View_Access :=
        Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
   begin
      if Context.Event /= null then
         Selected := Get_Selected_From_Event (View, Context.Event);
         if Selected /= -1 then
            Remove_Clipboard_Entry (Get_Clipboard (View.Kernel), Selected);
            return Success;
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
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      Iter : Gtk_Tree_Iter;
   begin
      Iter := Find_Iter_For_Event (View.Tree, Model, Event);
      if Iter /= Null_Iter then
         return Integer (Get_Int (Model, Iter, 2));
      else
         return -1;
      end if;
   end Get_Selected_From_Event;

   --------------------------
   -- View_Context_Factory --
   --------------------------

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced
        (Kernel, Event_Widget, Object, Event, Menu, Context);
      --  Nothing special in the context, just the module itself so that people
      --  can still add information if needed
   begin
      null;
   end View_Context_Factory;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      View : constant Clipboard_View_Access := Clipboard_View_Access (Clip);
      Selected : Integer;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Selected := Get_Selected_From_Event (View, Event);
         if Selected > 0 then
            --  Put the focus back on the current editor
            Execute_GPS_Shell_Command
              (View.Kernel, "EditorBuffer.get;"
               & "EditorBuffer.current_view %1;"
               & "MDI.get_by_child %1; MDIWindow.raise_window %1");

            Paste_Clipboard
              (Get_Clipboard (View.Kernel),
               Get_Current_Focus_Widget (View.Kernel),
               Selected);
            return True;
         end if;
      end if;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Information (E));
         return False;
   end Button_Press;

   --------------------------
   -- On_Clipboard_Changed --
   --------------------------

   procedure On_Clipboard_Changed
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Refresh (Generic_View.Get_Or_Create_View (Kernel, Focus => False));
   end On_Clipboard_Changed;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      View : constant Clipboard_View_Access :=
               Generic_View.Get_Or_Create_View (Kernel, Focus => False);
   begin
      Modify_Font (View.Tree, Get_Pref (View_Fixed_Font));
   end On_Preferences_Changed;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Clipboard_View_Record'Class) is
      Model           : constant Gtk_Tree_Store :=
                          Gtk_Tree_Store (Get_Model (View.Tree));
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

            if Last_Paste = S then
               Set (Model, Iter, 0, C_Proxy (View.Current));
            else
               Set (Model, Iter, 0, C_Proxy'(null));
            end if;

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

            Set (Model, Iter, 1, To_String (Result));
            Set (Model, Iter, 2, Gint (S));
         end if;
      end loop;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Information (E));
   end Refresh;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Clipboard_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Tooltip : Clipboard_View_Tooltips_Access;
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Gtk.Scrolled_Window.Initialize (View);
      Set_Policy (View, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (0 => Gdk.Pixbuf.Get_Type,
                                1 => GType_String,
                                2 => GType_Int),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Selection_Mode     => Selection_None,
         Sortable_Columns   => False,
         Hide_Expander      => True);
      Add (View, View.Tree);

      Modify_Font (View.Tree, Get_Pref (View_Fixed_Font));

      View.Current := Gdk_New_From_Xpm_Data (arrow_xpm);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Generic_View.Get_Module,
         Context_Func    => View_Context_Factory'Access);

      Add_Hook (Kernel, Clipboard_Changed_Hook,
                Wrapper (On_Clipboard_Changed'Access),
                Name => "clipboard_views.on_clipboard_changed",
                Watch => GObject (View));
      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (On_Preferences_Changed'Access),
                Name  => "clipboard_views.preferences_changed",
                Watch => GObject (View));
      Refresh (View);

      --  Initialize tooltips

      Tooltip := new Clipboard_View_Tooltips;
      Tooltip.Clipboard_View := Clipboard_View_Access (View);
      Set_Tooltip (Tooltip, View.Tree, 250);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      Generic_View.Register_Module
        (Kernel, Menu_Name => -"_Clipboard", Before_Menu => -"Remote");
      Command := new Merge_With_Previous_Command;
      Register_Contextual_Menu
        (Kernel, "Clipboard View Append To Previous",
         Action => Command,
         Filter => Create (Module => "Clipboard_View"),
         Label  => -"Append To Previous");

      Command := new Remove_Entry_Command;
      Register_Contextual_Menu
        (Kernel, "Clipboard View Remove Entry",
         Action => Command,
         Filter => Create (Module => "Clipboard_View"),
         Label  => -"Delete entry");
   end Register_Module;

end Clipboard_Views;
