with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Radical; use Callbacks_Radical;
with Radical_Intl; use Radical_Intl;
with Gui_Builder_Pkg.Callbacks; use Gui_Builder_Pkg.Callbacks;
with RAD.Pixmaps; use RAD.Pixmaps;

package body Gui_Builder_Pkg is

procedure Gtk_New (Gui_Builder : out Gui_Builder_Access) is
begin
   Gui_Builder := new Gui_Builder_Record;
   Gui_Builder_Pkg.Initialize (Gui_Builder);
end Gtk_New;

procedure Initialize (Gui_Builder : access Gui_Builder_Record'Class) is
   pragma Suppress (All_Checks);
   Combo2_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Gui_Builder, Window_Toplevel);
   Return_Callback.Connect
     (Gui_Builder, "delete_event", On_Gui_Builder_Delete_Event'Access);
   Set_Title (Gui_Builder, -"RADical");
   Set_Policy (Gui_Builder, False, True, False);
   Set_Position (Gui_Builder, Win_Pos_None);
   Set_Modal (Gui_Builder, False);
   Set_Default_Size (Gui_Builder, 500, 400);

   Gtk_New_Vbox (Gui_Builder.Vbox4, False, 0);
   Add (Gui_Builder, Gui_Builder.Vbox4);

   Gtk_New (Gui_Builder.Menubar1);
   Pack_Start (Gui_Builder.Vbox4, Gui_Builder.Menubar1, False, False, 0);
   Set_Shadow_Type (Gui_Builder.Menubar1, Shadow_Out);

   Gtk_New (Gui_Builder.Menuitem13, -"File");
   Add (Gui_Builder.Menubar1, Gui_Builder.Menuitem13);
   Set_Right_Justify (Gui_Builder.Menuitem13, False);

   Gtk_New (Gui_Builder.Menu1);
   Set_Submenu (Gui_Builder.Menuitem13, Gui_Builder.Menu1);

   Gtk_New (Gui_Builder.Menuitem14, -"Quit");
   Menu_Item_Callback.Connect
     (Gui_Builder.Menuitem14, "activate",
      Menu_Item_Callback.To_Marshaller (On_Quit1_Activate'Access));
   Add (Gui_Builder.Menu1, Gui_Builder.Menuitem14);
   Set_Right_Justify (Gui_Builder.Menuitem14, False);

   Gtk_New (Gui_Builder.Menuitem15, -"Edit");
   Menu_Item_Callback.Connect
     (Gui_Builder.Menuitem15, "activate",
      Menu_Item_Callback.To_Marshaller (On_Edit1_Activate'Access));
   Add (Gui_Builder.Menubar1, Gui_Builder.Menuitem15);
   Set_Right_Justify (Gui_Builder.Menuitem15, False);

   Gtk_New (Gui_Builder.Menuitem16, -"Settings");
   Menu_Item_Callback.Connect
     (Gui_Builder.Menuitem16, "activate",
      Menu_Item_Callback.To_Marshaller (On_Settings1_Activate'Access));
   Add (Gui_Builder.Menubar1, Gui_Builder.Menuitem16);
   Set_Right_Justify (Gui_Builder.Menuitem16, False);

   Gtk_New (Gui_Builder.Menuitem17, -"Help");
   Menu_Item_Callback.Connect
     (Gui_Builder.Menuitem17, "activate",
      Menu_Item_Callback.To_Marshaller (On_Help1_Activate'Access));
   Add (Gui_Builder.Menubar1, Gui_Builder.Menuitem17);
   Set_Right_Justify (Gui_Builder.Menuitem17, False);

   Gtk_New_Hpaned (Gui_Builder.Hpaned1);
   Pack_Start (Gui_Builder.Vbox4, Gui_Builder.Hpaned1, True, True, 0);
   Set_Handle_Size (Gui_Builder.Hpaned1, 10);
   Set_Gutter_Size (Gui_Builder.Hpaned1, 6);

   Gtk_New_Vbox (Gui_Builder.Vbox5, False, 0);
   Add (Gui_Builder.Hpaned1, Gui_Builder.Vbox5);

   Gtk_New (Gui_Builder.Handlebox2);
   Pack_Start (Gui_Builder.Vbox5, Gui_Builder.Handlebox2, False, False, 0);
   Set_Shadow_Type (Gui_Builder.Handlebox2, Shadow_Out);
   Set_Handle_Position (Gui_Builder.Handlebox2, Pos_Left);
   Set_Snap_Edge (Gui_Builder.Handlebox2, Pos_Top);

   Gtk_New (Gui_Builder.Toolbar4, Orientation_Horizontal, Toolbar_Both);
   Add (Gui_Builder.Handlebox2, Gui_Builder.Toolbar4);
   Set_Space_Size (Gui_Builder.Toolbar4, 5);
   Set_Space_Style (Gui_Builder.Toolbar4, Toolbar_Space_Empty);
   Set_Tooltips (Gui_Builder.Toolbar4, True);
   Set_Button_Relief (Gui_Builder.Toolbar4, Relief_Normal);
   Gui_Builder.Button10 := Append_Element
     (Toolbar => Gui_Builder.Toolbar4,
      The_Type => Toolbar_Child_Button,
      Text => -"Open");
   Gui_Builder.Button11 := Append_Element
     (Toolbar => Gui_Builder.Toolbar4,
      The_Type => Toolbar_Child_Button,
      Text => -"Save");
   Gui_Builder.Button12 := Append_Element
     (Toolbar => Gui_Builder.Toolbar4,
      The_Type => Toolbar_Child_Button,
      Text => -"Build");

   Gtk_New (Gui_Builder.Handlebox3);
   Pack_Start (Gui_Builder.Vbox5, Gui_Builder.Handlebox3, False, True, 0);
   Set_Shadow_Type (Gui_Builder.Handlebox3, Shadow_Out);
   Set_Handle_Position (Gui_Builder.Handlebox3, Pos_Left);
   Set_Snap_Edge (Gui_Builder.Handlebox3, Pos_Top);

   Gtk_New (Gui_Builder.Notebook6);
   Add (Gui_Builder.Handlebox3, Gui_Builder.Notebook6);
   Set_Scrollable (Gui_Builder.Notebook6, True);
   Set_Show_Border (Gui_Builder.Notebook6, True);
   Set_Show_Tabs (Gui_Builder.Notebook6, True);
   Set_Tab_Hborder (Gui_Builder.Notebook6, 2);
   Set_Tab_Vborder (Gui_Builder.Notebook6, 2);
   Set_Tab_Pos (Gui_Builder.Notebook6, Pos_Top);

   Gtk_New (Gui_Builder.Frame18);
   Add (Gui_Builder.Notebook6, Gui_Builder.Frame18);
   Set_Shadow_Type (Gui_Builder.Frame18, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Toolbar5, Orientation_Horizontal, Toolbar_Icons);
   Add (Gui_Builder.Frame18, Gui_Builder.Toolbar5);
   Set_Space_Size (Gui_Builder.Toolbar5, 5);
   Set_Space_Style (Gui_Builder.Toolbar5, Toolbar_Space_Empty);
   Set_Tooltips (Gui_Builder.Toolbar5, True);
   Set_Button_Relief (Gui_Builder.Toolbar5, Relief_None);
   Gui_Builder.Togglebutton1 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Horizontal Box",
      Icon => Gtk_Widget (Create_Pixmap (hbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton2 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Vertical Box",
      Icon => Gtk_Widget (Create_Pixmap (vbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton3 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Table",
      Icon => Gtk_Widget (Create_Pixmap (table_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton4 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Packer",
      Icon => Gtk_Widget (Create_Pixmap (packer_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton5 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Fixed Positions",
      Icon => Gtk_Widget (Create_Pixmap (fixed_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton6 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Horizontal Button Box",
      Icon => Gtk_Widget (Create_Pixmap (hbuttonbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton7 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Vertical Button Box",
      Icon => Gtk_Widget (Create_Pixmap (vbuttonbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton8 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Horizontal Pane",
      Icon => Gtk_Widget (Create_Pixmap (hpaned_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton9 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Vertical Pane",
      Icon => Gtk_Widget (Create_Pixmap (vpaned_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton10 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Frame",
      Icon => Gtk_Widget (Create_Pixmap (frame_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton11 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Scrolled Window",
      Icon => Gtk_Widget (Create_Pixmap (scrolledwindow_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton12 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Layou",
      Icon => Gtk_Widget (Create_Pixmap (layout_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton13 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Viewport",
      Icon => Gtk_Widget (Create_Pixmap (viewport_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton14 := Append_Element
     (Toolbar => Gui_Builder.Toolbar5,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Alignment",
      Icon => Gtk_Widget (Create_Pixmap (alignment_xpm, Gui_Builder)));

   Gtk_New (Gui_Builder.Label40, -("Containers"));
   Set_Alignment (Gui_Builder.Label40, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label40, 0, 0);
   Set_Justify (Gui_Builder.Label40, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label40, False);
   Set_Tab (Gui_Builder.Notebook6, 0, Gui_Builder.Label40);

   Gtk_New (Gui_Builder.Frame19);
   Add (Gui_Builder.Notebook6, Gui_Builder.Frame19);
   Set_Shadow_Type (Gui_Builder.Frame19, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label41, -("Standard"));
   Set_Alignment (Gui_Builder.Label41, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label41, 0, 0);
   Set_Justify (Gui_Builder.Label41, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label41, False);
   Set_Tab (Gui_Builder.Notebook6, 1, Gui_Builder.Label41);

   Gtk_New (Gui_Builder.Frame20);
   Add (Gui_Builder.Notebook6, Gui_Builder.Frame20);
   Set_Shadow_Type (Gui_Builder.Frame20, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label42, -("Additional"));
   Set_Alignment (Gui_Builder.Label42, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label42, 0, 0);
   Set_Justify (Gui_Builder.Label42, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label42, False);
   Set_Tab (Gui_Builder.Notebook6, 2, Gui_Builder.Label42);

   Gtk_New (Gui_Builder.Frame21);
   Add (Gui_Builder.Notebook6, Gui_Builder.Frame21);
   Set_Shadow_Type (Gui_Builder.Frame21, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label43, -("Dialogs"));
   Set_Alignment (Gui_Builder.Label43, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label43, 0, 0);
   Set_Justify (Gui_Builder.Label43, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label43, False);
   Set_Tab (Gui_Builder.Notebook6, 3, Gui_Builder.Label43);

   Gtk_New (Gui_Builder.Frame22);
   Add (Gui_Builder.Notebook6, Gui_Builder.Frame22);
   Set_Shadow_Type (Gui_Builder.Frame22, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label44, -("Gnome"));
   Set_Alignment (Gui_Builder.Label44, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label44, 0, 0);
   Set_Justify (Gui_Builder.Label44, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label44, False);
   Set_Tab (Gui_Builder.Notebook6, 4, Gui_Builder.Label44);

   Gtk_New (Gui_Builder.Scrolledwindow8);
   Pack_Start (Gui_Builder.Vbox5, Gui_Builder.Scrolledwindow8, True, True, 0);
   Set_Policy (Gui_Builder.Scrolledwindow8, Policy_Automatic, Policy_Automatic);

   Gtk_New (Gui_Builder.Ctree1, 2);
   Add (Gui_Builder.Scrolledwindow8, Gui_Builder.Ctree1);
   Set_Selection_Mode (Gui_Builder.Ctree1, Selection_Single);
   Set_Shadow_Type (Gui_Builder.Ctree1, Shadow_In);
   Set_Show_Titles (Gui_Builder.Ctree1, False);
   Set_Column_Width (Gui_Builder.Ctree1, 0, 80);
   Set_Column_Width (Gui_Builder.Ctree1, 1, 80);

   Gtk_New (Gui_Builder.Label45, -("label7"));
   Set_Alignment (Gui_Builder.Label45, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label45, 0, 0);
   Set_Justify (Gui_Builder.Label45, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label45, False);
   Set_Column_Widget (Gui_Builder.Ctree1, 0, Gui_Builder.Label45);

   Gtk_New (Gui_Builder.Label46, -("label8"));
   Set_Alignment (Gui_Builder.Label46, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label46, 0, 0);
   Set_Justify (Gui_Builder.Label46, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label46, False);
   Set_Column_Widget (Gui_Builder.Ctree1, 1, Gui_Builder.Label46);

   Gtk_New (Gui_Builder.Statusbar1);
   Pack_Start (Gui_Builder.Vbox5, Gui_Builder.Statusbar1, False, False, 0);

   Gtk_New_Vbox (Gui_Builder.Vbox6, False, 5);
   Add (Gui_Builder.Hpaned1, Gui_Builder.Vbox6);
   Set_Border_Width (Gui_Builder.Vbox6, 3);

   Gtk_New (Gui_Builder.Combo2);
   Pack_Start (Gui_Builder.Vbox6, Gui_Builder.Combo2, False, False, 0);
   Set_Case_Sensitive (Gui_Builder.Combo2, False);
   Set_Use_Arrows (Gui_Builder.Combo2, True);
   Set_Use_Arrows_Always (Gui_Builder.Combo2, False);
   String_List.Append (Combo2_Items, -"");
   Combo.Set_Popdown_Strings (Gui_Builder.Combo2, Combo2_Items);
   Free_String_List (Combo2_Items);

   Gui_Builder.Entry1 := Get_Entry (Gui_Builder.Combo2);
   Set_Editable (Gui_Builder.Entry1, True);
   Set_Max_Length (Gui_Builder.Entry1, 0);
   Set_Text (Gui_Builder.Entry1, -"");
   Set_Visibility (Gui_Builder.Entry1, True);

   Gtk_New (Gui_Builder.Notebook7);
   Pack_Start (Gui_Builder.Vbox6, Gui_Builder.Notebook7, True, True, 0);
   Set_Scrollable (Gui_Builder.Notebook7, False);
   Set_Show_Border (Gui_Builder.Notebook7, True);
   Set_Show_Tabs (Gui_Builder.Notebook7, True);
   Set_Tab_Hborder (Gui_Builder.Notebook7, 2);
   Set_Tab_Vborder (Gui_Builder.Notebook7, 2);
   Set_Tab_Pos (Gui_Builder.Notebook7, Pos_Top);

   Gtk_New (Gui_Builder.Scrolledwindow11);
   Add (Gui_Builder.Notebook7, Gui_Builder.Scrolledwindow11);
   Set_Policy (Gui_Builder.Scrolledwindow11, Policy_Automatic, Policy_Automatic);

   Gtk_New (Gui_Builder.Clist6, 2);
   Add (Gui_Builder.Scrolledwindow11, Gui_Builder.Clist6);
   Set_Selection_Mode (Gui_Builder.Clist6, Selection_Single);
   Set_Shadow_Type (Gui_Builder.Clist6, Shadow_In);
   Set_Show_Titles (Gui_Builder.Clist6, False);
   Set_Column_Width (Gui_Builder.Clist6, 0, 80);
   Set_Column_Width (Gui_Builder.Clist6, 1, 80);

   Gtk_New (Gui_Builder.Label55, -("label3"));
   Set_Alignment (Gui_Builder.Label55, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label55, 0, 0);
   Set_Justify (Gui_Builder.Label55, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label55, False);
   Set_Column_Widget (Gui_Builder.Clist6, 0, Gui_Builder.Label55);

   Gtk_New (Gui_Builder.Label56, -("label4"));
   Set_Alignment (Gui_Builder.Label56, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label56, 0, 0);
   Set_Justify (Gui_Builder.Label56, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label56, False);
   Set_Column_Widget (Gui_Builder.Clist6, 1, Gui_Builder.Label56);

   Gtk_New (Gui_Builder.Label57, -("Properties"));
   Set_Alignment (Gui_Builder.Label57, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label57, 0, 0);
   Set_Justify (Gui_Builder.Label57, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label57, False);
   Set_Tab (Gui_Builder.Notebook7, 0, Gui_Builder.Label57);

   Gtk_New (Gui_Builder.Scrolledwindow12);
   Add (Gui_Builder.Notebook7, Gui_Builder.Scrolledwindow12);
   Set_Policy (Gui_Builder.Scrolledwindow12, Policy_Automatic, Policy_Automatic);

   Gtk_New (Gui_Builder.Clist7, 2);
   Add (Gui_Builder.Scrolledwindow12, Gui_Builder.Clist7);
   Set_Selection_Mode (Gui_Builder.Clist7, Selection_Single);
   Set_Shadow_Type (Gui_Builder.Clist7, Shadow_In);
   Set_Show_Titles (Gui_Builder.Clist7, True);
   Set_Column_Width (Gui_Builder.Clist7, 0, 80);
   Set_Column_Width (Gui_Builder.Clist7, 1, 80);

   Gtk_New (Gui_Builder.Label58, -("Signal"));
   Set_Alignment (Gui_Builder.Label58, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label58, 0, 0);
   Set_Justify (Gui_Builder.Label58, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label58, False);
   Set_Column_Widget (Gui_Builder.Clist7, 0, Gui_Builder.Label58);

   Gtk_New (Gui_Builder.Label59, -("Handler"));
   Set_Alignment (Gui_Builder.Label59, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label59, 0, 0);
   Set_Justify (Gui_Builder.Label59, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label59, False);
   Set_Column_Widget (Gui_Builder.Clist7, 1, Gui_Builder.Label59);

   Gtk_New (Gui_Builder.Label60, -("Signals"));
   Set_Alignment (Gui_Builder.Label60, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label60, 0, 0);
   Set_Justify (Gui_Builder.Label60, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label60, False);
   Set_Tab (Gui_Builder.Notebook7, 1, Gui_Builder.Label60);

end Initialize;

end Gui_Builder_Pkg;
