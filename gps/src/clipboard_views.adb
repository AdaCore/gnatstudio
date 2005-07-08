-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                            AdaCore                                --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Commands.Interactive; use Commands, Commands.Interactive;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GPS.Kernel;           use GPS.Kernel;
with GPS.Kernel.Clipboard; use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;       use GPS.Kernel.MDI;
with GPS.Kernel.Modules;   use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;   use GPS.Kernel.Scripts;
with GPS.Intl;             use GPS.Intl;
with GUI_Utils;            use GUI_Utils;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Xml_Int;         use Glib.Xml_Int;
with Gdk.Event;            use Gdk.Event;
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gtk.Box;              use Gtk.Box;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Scrolled_Window;  use Gtk.Scrolled_Window;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.MDI;           use Gtkada.MDI;
with Pixmaps_IDE;          use Pixmaps_IDE;
with String_Utils;         use String_Utils;
with Traces; use Traces;

package body Clipboard_Views is
--   Me : constant Debug_Handle := Create ("Clipboard");

   Clipboard_Views_Module : Module_ID;

   type Clipboard_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Tree    : Gtk_Tree_View;
      Kernel  : Kernel_Handle;
      Current : Gdk_Pixbuf;
   end record;
   type Clipboard_View_Access is access all Clipboard_View_Record'Class;

   procedure Gtk_New
     (View   : out Clipboard_View_Access;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new clipboard view

   procedure On_Open_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Create the clipboard view (or raise the existing one)

   function Open_View
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Create the clipboard view if needed

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Handling of desktops

   procedure On_Clipboard_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the contents of the clipboard has changed

   procedure Refresh (View : access Clipboard_View_Record'Class);
   --  Refresh the contents of the clipboard view

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
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

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Merge_With_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Selected : Integer;
      View : constant Clipboard_View_Access := Clipboard_View_Access
        (Get_Widget (Open_View (Get_Kernel (Context.Context))));
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

   function View_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Object, Event, Menu);
      --  Nothing special in the context, just the module itself so that people
      --  can still add information if needed
      Context : constant Selection_Context_Access := new Selection_Context;
   begin
      return Context;
   end View_Context_Factory;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      View  : constant Clipboard_View_Access := Clipboard_View_Access (Clip);
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
     (Kernel : access Kernel_Handle_Record'Class)
   is
   begin
      Refresh (Clipboard_View_Access (Get_Widget (Open_View (Kernel))));
   end On_Clipboard_Changed;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Clipboard_View_Record'Class) is
      Model     : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));
      Selection : constant Selection_List :=
        Get_Content (Get_Clipboard (View.Kernel));
      Iter      : Gtk_Tree_Iter;
      Last_Paste : constant Integer :=
        Get_Last_Paste (Get_Clipboard (View.Kernel));
      Index, First : Natural;
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

            --  Show only the first line of the selection
            First := Selection (S)'First;
            while First <= Selection (S)'Last
              and then Is_Blank (Selection (S)(First))
            loop
               First := First + 1;
            end loop;

            if First > Selection (S)'Last then
               First := Selection (S)'Last;
            end if;

            Index := Selection (S)'First;
            while Index <= Selection (S)'Last
              and then Selection (S)(Index) /= ASCII.LF
            loop
               Index := Index + 1;
            end loop;
            Set (Model, Iter, 1,
                 Selection (S) (First .. Index - 1));
            Set (Model, Iter, 2, Gint (S));
         end if;
      end loop;
   end Refresh;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Clipboard_View_Access;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
   begin
      View := new Clipboard_View_Record;
      View.Kernel := Kernel_Handle (Kernel);
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      Pack_Start (View, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (0 => Gdk.Pixbuf.Get_Type,
                                1 => GType_String,
                                2 => GType_Int),
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Selection_Mode     => Selection_None,
         Sortable_Columns   => False,
         Hide_Expander      => True);
      Add (Scrolled, View.Tree);
      --  Modify_Font (View.Tree, Get_Pref (Kernel, Outline_View_Font));

      View.Current := Gdk_New_From_Xpm_Data (arrow_xpm);

      Return_Callback.Object_Connect
        (View.Tree,
         "button_press_event",
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Clipboard_Views_Module,
         Context_Func    => View_Context_Factory'Access);

      Add_Hook (Kernel, Clipboard_Changed_Hook, On_Clipboard_Changed'Access,
                Watch => GObject (View));
      Refresh (View);
   end Gtk_New;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Clipboard_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Clipboard_View");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Clipboard_View" then
         return Open_View (User);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- On_Open_View --
   ------------------

   procedure On_Open_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      View : MDI_Child;
      pragma Unreferenced (Widget);
   begin
      View := Open_View (Kernel);
      Raise_Child (View);
      Set_Focus_Child (Get_MDI (Kernel), View);
   end On_Open_View;

   ---------------
   -- Open_View --
   ---------------

   function Open_View
     (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child
   is
      Child   : MDI_Child;
      View    : Clipboard_View_Access;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Clipboard_View_Record'Tag);

      if Child = null then
         Gtk_New (View, Kernel);
         Child := Put
           (Kernel, View,
            Default_Width  => 215,
            Default_Height => 600,
            Position       => Position_Left,
            Module         => Clipboard_Views_Module);
         Set_Title (Child, -"Clipboard View", -"Clipboard View");
      end if;

      return Child;
   end Open_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      Register_Module
        (Module      => Clipboard_Views_Module,
         Module_Name => "Clipboard_View",
         Kernel      => Kernel);
      Register_Menu
        (Kernel,
         "/" & (-"Tools"), -"Clipboard View", "", On_Open_View'Access);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Merge_With_Previous_Command;
      Register_Contextual_Menu
        (Kernel, "Clipboard View Append To Previous",
         Action => Command,
         Filter => Create (Module => "Clipboard_View"),
         Label  => -"Append To Previous");
   end Register_Module;

end Clipboard_Views;
