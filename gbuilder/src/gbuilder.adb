
with Gdk.Event;    use Gdk.Event;
with Gtk.Combo;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Event_Box;
with Gtk.GEntry;
with Gtk.Handle_Box;
with Gtk.Label;
with Gtk.Main;     use Gtk.Main;
with Gtk.Menu_Bar;
with Gtk.Toolbar;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Window;   use Gtk.Window;

with Widget_Lists;     use Widget_Lists;
with Property_Editors; use Property_Editors;
with Common;           use Common;
with Placeholders;     use Placeholders;

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

procedure Gbuilder is
   Combo_Xpm      : aliased chars_ptr_array (0 .. 0);
   Entry_Xpm      : aliased chars_ptr_array (0 .. 0);
   Event_Box_Xpm  : aliased chars_ptr_array (0 .. 0);
   Handle_Box_Xpm : aliased chars_ptr_array (0 .. 0);
   Label_Xpm      : aliased chars_ptr_array (0 .. 0);
   Menu_Bar_Xpm   : aliased chars_ptr_array (0 .. 0);
   Toolbar_Xpm    : aliased chars_ptr_array (0 .. 0);
   Window_Xpm     : aliased chars_ptr_array (0 .. 0);
   pragma Import (C, Combo_Xpm, "combo_xpm");
   pragma Import (C, Entry_Xpm, "entry_xpm");
   pragma Import (C, Event_Box_Xpm, "eventbox_xpm");
   pragma Import (C, Handle_Box_Xpm, "handlebox_xpm");
   pragma Import (C, Label_Xpm, "label_xpm");
   pragma Import (C, Menu_Bar_Xpm, "menubar_xpm");
   pragma Import (C, Toolbar_Xpm, "toolbar_xpm");
   pragma Import (C, Window_Xpm, "window_xpm");


   procedure My_Event_Handler (Event : System.Address; Data : System.Address);


   Win  : Gtk_Window;
   Win2 : Gtk_Window;
   List : Widget_Lists.Widget_List;
   Prop : Property_Editor;

   ----------------------
   -- My_Event_Handler --
   ----------------------

   procedure My_Event_Handler
     (Event : System.Address; Data : System.Address)
   is
      E : Gdk_Event := From_Address (Event);
      Widget : Gtk_Widget;
      Win : Gtk_Widget;
   begin
      case Get_Event_Type (E) is
         when Button_Press =>
            Widget := Gtk.Main.Get_Event_Widget (E);

            if Is_Placeholder (Widget) then
               Widget := Get_Parent (Widget);
            end if;

            Win := Get_Toplevel (Widget);

            if Is_Gui_Builder_Window (Win) then
               Inspect (Prop, Widget);
            end if;

         when Button_Release =>
            Widget := Gtk.Main.Get_Event_Widget (E);

         when others =>
            null;
      end case;
      Gtk.Main.Do_Event (E);
   end My_Event_Handler;

begin
   Gtk.Main.Init;

   Gtk_New (Win, Window_Toplevel);
   Gtk_New (List);
   Add (Win, List);

   Set_Page_Title (List, 0, "containers");
   Add_Widget (List, 0, "Window", Window_Xpm, Gtk.Window.Get_Type);
   Add_Widget (List, 0, "Menu Bar", Menu_Bar_Xpm, Gtk.Menu_Bar.Get_Type);
   Add_Widget (List, 0, "Toolbar", Toolbar_Xpm, Gtk.Toolbar.Get_Type);
   Add_Widget (List, 0, "Handle Box", Handle_Box_Xpm, Gtk.Handle_Box.Get_Type);

   Set_Page_Title (List, 1, "standard");
   Add_Widget (List, 1, "Label", Label_Xpm, Gtk.Label.Get_Type);
   Add_Widget (List, 1, "Entry", Entry_Xpm, Gtk.GEntry.Get_Type);
   Add_Widget (List, 1, "Combo", Combo_Xpm, Gtk.Combo.Get_Type);

   Set_Page_Title (List, 2, "additional");
   Add_Widget (List, 2, "Event Box", Event_Box_Xpm, Gtk.Event_Box.Get_Type);

   Show_All (Win);

   Gtk_New (Win2, Window_Toplevel);
   Set_Default_Size (Win2, 300, 600);
   Gtk_New (Prop);
   Add (Win2, Prop);
   Show_All (Win2);

   Event_Handler_Set (My_Event_Handler'Unrestricted_Access, Null_Address);

   Gtk.Main.Main;
end Gbuilder;
