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
   Combo3_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Gui_Builder, Window_Toplevel);
   Return_Callback.Connect
     (Gui_Builder, "delete_event", On_Gui_Builder_Delete_Event'Access);
   Set_Title (Gui_Builder, -"RADical");
   Set_Policy (Gui_Builder, False, True, False);
   Set_Position (Gui_Builder, Win_Pos_None);
   Set_Modal (Gui_Builder, False);
   Set_Default_Size (Gui_Builder, 630, 400);

   Gtk_New_Vbox (Gui_Builder.Vbox4, False, 0);
   Add (Gui_Builder, Gui_Builder.Vbox4);

   Gtk_New (Gui_Builder.Menubar1);
   Pack_Start (Gui_Builder.Vbox4, Gui_Builder.Menubar1, False, False, 0);
   --  Set_Shadow_Type (Gui_Builder.Menubar1, Shadow_Out);

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

   Gtk_New_Hbox (Gui_Builder.Hbox1, False, 0);
   Pack_Start (Gui_Builder.Vbox4, Gui_Builder.Hbox1, True, True, 0);

   Gtk_New_Vbox (Gui_Builder.Vbox10, False, 0);
   Pack_Start (Gui_Builder.Hbox1, Gui_Builder.Vbox10, False, True, 0);

   Gtk_New (Gui_Builder.Handlebox5);
   Pack_Start (Gui_Builder.Vbox10, Gui_Builder.Handlebox5, False, False, 0);
   Set_Shadow_Type (Gui_Builder.Handlebox5, Shadow_Out);
   Set_Handle_Position (Gui_Builder.Handlebox5, Pos_Left);
   Set_Snap_Edge (Gui_Builder.Handlebox5, Pos_Top);

   Gtk_New (Gui_Builder.Toolbar7, Orientation_Horizontal, Toolbar_Both);
   Add (Gui_Builder.Handlebox5, Gui_Builder.Toolbar7);
   --  Set_Space_Size (Gui_Builder.Toolbar7, 5);
   --  Set_Space_Style (Gui_Builder.Toolbar7, Toolbar_Space_Empty);
   Set_Tooltips (Gui_Builder.Toolbar7, True);
   --  Set_Button_Relief (Gui_Builder.Toolbar7, Relief_Normal);
   Gui_Builder.Button15 := Append_Element
     (Toolbar => Gui_Builder.Toolbar7,
      The_Type => Toolbar_Child_Button,
      Text => -"Open ",
      Icon => Gtk_Widget (Create_Pixmap (open_xpm, Gui_Builder)));
   Gui_Builder.Button16 := Append_Element
     (Toolbar => Gui_Builder.Toolbar7,
      The_Type => Toolbar_Child_Button,
      Text => -"Save",
      Icon => Gtk_Widget (Create_Pixmap (save_xpm, Gui_Builder)));
   Gui_Builder.Button17 := Append_Element
     (Toolbar => Gui_Builder.Toolbar7,
      The_Type => Toolbar_Child_Button,
      Text => -"Build",
      Icon => Gtk_Widget (Create_Pixmap (source_xpm, Gui_Builder)));

   Gtk_New (Gui_Builder.Handlebox6);
   Pack_Start (Gui_Builder.Vbox10, Gui_Builder.Handlebox6, False, True, 0);
   Set_Shadow_Type (Gui_Builder.Handlebox6, Shadow_Out);
   Set_Handle_Position (Gui_Builder.Handlebox6, Pos_Left);
   Set_Snap_Edge (Gui_Builder.Handlebox6, Pos_Top);

   Gtk_New (Gui_Builder.Notebook10);
   Add (Gui_Builder.Handlebox6, Gui_Builder.Notebook10);
   Set_Scrollable (Gui_Builder.Notebook10, True);
   Set_Show_Border (Gui_Builder.Notebook10, True);
   Set_Show_Tabs (Gui_Builder.Notebook10, True);
   Set_Tab_Hborder (Gui_Builder.Notebook10, 2);
   Set_Tab_Vborder (Gui_Builder.Notebook10, 2);
   Set_Tab_Pos (Gui_Builder.Notebook10, Pos_Top);

   Gtk_New_Vbox (Gui_Builder.Vbox12, False, 0);
   Add (Gui_Builder.Notebook10, Gui_Builder.Vbox12);

   Gtk_New (Gui_Builder.Frame32);
   Pack_Start (Gui_Builder.Vbox12, Gui_Builder.Frame32, True, True, 0);
   Set_Shadow_Type (Gui_Builder.Frame32, Shadow_Etched_In);

   Gtk_New_Vbox (Gui_Builder.Vbox13, False, 0);
   Add (Gui_Builder.Frame32, Gui_Builder.Vbox13);

   Gtk_New (Gui_Builder.Toolbar8, Orientation_Horizontal, Toolbar_Icons);
   Pack_Start (Gui_Builder.Vbox13, Gui_Builder.Toolbar8, False, False, 0);
   --  Set_Space_Size (Gui_Builder.Toolbar8, 5);
   --  Set_Space_Style (Gui_Builder.Toolbar8, Toolbar_Space_Empty);
   Set_Tooltips (Gui_Builder.Toolbar8, True);
   --  Set_Button_Relief (Gui_Builder.Toolbar8, Relief_None);
   Gui_Builder.Togglebutton67 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Horizontal Box",
      Icon => Gtk_Widget (Create_Pixmap (hbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton68 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Vertical Box",
      Icon => Gtk_Widget (Create_Pixmap (vbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton69 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Table",
      Icon => Gtk_Widget (Create_Pixmap (table_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton70 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Packer",
      Icon => Gtk_Widget (Create_Pixmap (packer_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton71 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Fixed Positions",
      Icon => Gtk_Widget (Create_Pixmap (fixed_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton72 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Horizontal Button Box",
      Icon => Gtk_Widget (Create_Pixmap (hbuttonbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton73 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Vertical Button Box",
      Icon => Gtk_Widget (Create_Pixmap (vbuttonbox_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton74 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Horizontal Pane",
      Icon => Gtk_Widget (Create_Pixmap (hpaned_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton75 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Vertical Pane",
      Icon => Gtk_Widget (Create_Pixmap (vpaned_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton76 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Frame",
      Icon => Gtk_Widget (Create_Pixmap (frame_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton77 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Scrolled Window",
      Icon => Gtk_Widget (Create_Pixmap (scrolledwindow_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton78 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Layou",
      Icon => Gtk_Widget (Create_Pixmap (layout_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton79 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Viewport",
      Icon => Gtk_Widget (Create_Pixmap (viewport_xpm, Gui_Builder)));
   Gui_Builder.Togglebutton80 := Append_Element
     (Toolbar => Gui_Builder.Toolbar8,
      The_Type => Toolbar_Child_ToggleButton,
      Text => -"",
      Tooltip_Text => -"Alignment",
      Icon => Gtk_Widget (Create_Pixmap (alignment_xpm, Gui_Builder)));

   Gtk_New (Gui_Builder.Label74, -("Containers"));
   Set_Alignment (Gui_Builder.Label74, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label74, 0, 0);
   Set_Justify (Gui_Builder.Label74, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label74, False);
   Set_Tab (Gui_Builder.Notebook10, 0, Gui_Builder.Label74);

   Gtk_New (Gui_Builder.Frame28);
   Add (Gui_Builder.Notebook10, Gui_Builder.Frame28);
   Set_Shadow_Type (Gui_Builder.Frame28, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label75, -("Standard"));
   Set_Alignment (Gui_Builder.Label75, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label75, 0, 0);
   Set_Justify (Gui_Builder.Label75, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label75, False);
   Set_Tab (Gui_Builder.Notebook10, 1, Gui_Builder.Label75);

   Gtk_New (Gui_Builder.Frame29);
   Add (Gui_Builder.Notebook10, Gui_Builder.Frame29);
   Set_Shadow_Type (Gui_Builder.Frame29, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label76, -("Additional"));
   Set_Alignment (Gui_Builder.Label76, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label76, 0, 0);
   Set_Justify (Gui_Builder.Label76, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label76, False);
   Set_Tab (Gui_Builder.Notebook10, 2, Gui_Builder.Label76);

   Gtk_New (Gui_Builder.Frame30);
   Add (Gui_Builder.Notebook10, Gui_Builder.Frame30);
   Set_Shadow_Type (Gui_Builder.Frame30, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label77, -("Dialogs"));
   Set_Alignment (Gui_Builder.Label77, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label77, 0, 0);
   Set_Justify (Gui_Builder.Label77, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label77, False);
   Set_Tab (Gui_Builder.Notebook10, 3, Gui_Builder.Label77);

   Gtk_New (Gui_Builder.Frame31);
   Add (Gui_Builder.Notebook10, Gui_Builder.Frame31);
   Set_Shadow_Type (Gui_Builder.Frame31, Shadow_Etched_In);

   Gtk_New (Gui_Builder.Label78, -("Gnome"));
   Set_Alignment (Gui_Builder.Label78, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label78, 0, 0);
   Set_Justify (Gui_Builder.Label78, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label78, False);
   Set_Tab (Gui_Builder.Notebook10, 4, Gui_Builder.Label78);

   Gtk_New (Gui_Builder.Scrolledwindow16);
   Pack_Start (Gui_Builder.Vbox10, Gui_Builder.Scrolledwindow16, True, True, 0);
   Set_Policy (Gui_Builder.Scrolledwindow16, Policy_Automatic, Policy_Automatic);

   Gtk_New (Gui_Builder.Ctree2, 2);
   Add (Gui_Builder.Scrolledwindow16, Gui_Builder.Ctree2);
   Set_Selection_Mode (Gui_Builder.Ctree2, Selection_Single);
   Set_Shadow_Type (Gui_Builder.Ctree2, Shadow_In);
   Set_Show_Titles (Gui_Builder.Ctree2, False);
   Set_Column_Width (Gui_Builder.Ctree2, 0, 80);
   Set_Column_Width (Gui_Builder.Ctree2, 1, 80);

   Gtk_New (Gui_Builder.Label79, -("label7"));
   Set_Alignment (Gui_Builder.Label79, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label79, 0, 0);
   Set_Justify (Gui_Builder.Label79, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label79, False);
   Set_Column_Widget (Gui_Builder.Ctree2, 0, Gui_Builder.Label79);

   Gtk_New (Gui_Builder.Label80, -("label8"));
   Set_Alignment (Gui_Builder.Label80, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label80, 0, 0);
   Set_Justify (Gui_Builder.Label80, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label80, False);
   Set_Column_Widget (Gui_Builder.Ctree2, 1, Gui_Builder.Label80);

   Gtk_New (Gui_Builder.Statusbar2);
   Pack_Start (Gui_Builder.Vbox10, Gui_Builder.Statusbar2, False, False, 0);

   Gtk_New_Vbox (Gui_Builder.Vbox11, False, 5);
   Pack_Start (Gui_Builder.Hbox1, Gui_Builder.Vbox11, True, True, 0);
   Set_Border_Width (Gui_Builder.Vbox11, 3);

   Gtk_New (Gui_Builder.Combo3);
   Pack_Start (Gui_Builder.Vbox11, Gui_Builder.Combo3, False, False, 0);
   Set_Case_Sensitive (Gui_Builder.Combo3, False);
   Set_Use_Arrows (Gui_Builder.Combo3, True);
   Set_Use_Arrows_Always (Gui_Builder.Combo3, False);
   String_List.Append (Combo3_Items, -"");
   Combo.Set_Popdown_Strings (Gui_Builder.Combo3, Combo3_Items);
   Free_String_List (Combo3_Items);

   Gui_Builder.Entry2 := Get_Entry (Gui_Builder.Combo3);
   Set_Editable (Gui_Builder.Entry2, True);
   Set_Max_Length (Gui_Builder.Entry2, 0);
   Set_Text (Gui_Builder.Entry2, -"");
   Set_Visibility (Gui_Builder.Entry2, True);

   Gtk_New (Gui_Builder.Notebook11);
   Pack_Start (Gui_Builder.Vbox11, Gui_Builder.Notebook11, True, True, 0);
   Set_Scrollable (Gui_Builder.Notebook11, False);
   Set_Show_Border (Gui_Builder.Notebook11, True);
   Set_Show_Tabs (Gui_Builder.Notebook11, True);
   Set_Tab_Hborder (Gui_Builder.Notebook11, 2);
   Set_Tab_Vborder (Gui_Builder.Notebook11, 2);
   Set_Tab_Pos (Gui_Builder.Notebook11, Pos_Top);

   Gtk_New (Gui_Builder.Scrolledwindow17);
   Add (Gui_Builder.Notebook11, Gui_Builder.Scrolledwindow17);
   Set_Policy (Gui_Builder.Scrolledwindow17, Policy_Automatic, Policy_Automatic);

   Gtk_New (Gui_Builder.Clist9, 2);
   Add (Gui_Builder.Scrolledwindow17, Gui_Builder.Clist9);
   Set_Selection_Mode (Gui_Builder.Clist9, Selection_Single);
   Set_Shadow_Type (Gui_Builder.Clist9, Shadow_In);
   Set_Show_Titles (Gui_Builder.Clist9, False);
   Set_Column_Width (Gui_Builder.Clist9, 0, 80);
   Set_Column_Width (Gui_Builder.Clist9, 1, 80);

   Gtk_New (Gui_Builder.Label81, -("label3"));
   Set_Alignment (Gui_Builder.Label81, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label81, 0, 0);
   Set_Justify (Gui_Builder.Label81, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label81, False);
   Set_Column_Widget (Gui_Builder.Clist9, 0, Gui_Builder.Label81);

   Gtk_New (Gui_Builder.Label82, -("label4"));
   Set_Alignment (Gui_Builder.Label82, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label82, 0, 0);
   Set_Justify (Gui_Builder.Label82, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label82, False);
   Set_Column_Widget (Gui_Builder.Clist9, 1, Gui_Builder.Label82);

   Gtk_New (Gui_Builder.Label83, -("Properties"));
   Set_Alignment (Gui_Builder.Label83, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label83, 0, 0);
   Set_Justify (Gui_Builder.Label83, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label83, False);
   Set_Tab (Gui_Builder.Notebook11, 0, Gui_Builder.Label83);

   Gtk_New (Gui_Builder.Scrolledwindow18);
   Add (Gui_Builder.Notebook11, Gui_Builder.Scrolledwindow18);
   Set_Policy (Gui_Builder.Scrolledwindow18, Policy_Automatic, Policy_Automatic);

   Gtk_New (Gui_Builder.Clist10, 2);
   Add (Gui_Builder.Scrolledwindow18, Gui_Builder.Clist10);
   Set_Selection_Mode (Gui_Builder.Clist10, Selection_Single);
   Set_Shadow_Type (Gui_Builder.Clist10, Shadow_In);
   Set_Show_Titles (Gui_Builder.Clist10, True);
   Set_Column_Width (Gui_Builder.Clist10, 0, 80);
   Set_Column_Width (Gui_Builder.Clist10, 1, 80);

   Gtk_New (Gui_Builder.Label84, -("Signal"));
   Set_Alignment (Gui_Builder.Label84, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label84, 0, 0);
   Set_Justify (Gui_Builder.Label84, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label84, False);
   Set_Column_Widget (Gui_Builder.Clist10, 0, Gui_Builder.Label84);

   Gtk_New (Gui_Builder.Label85, -("Handler"));
   Set_Alignment (Gui_Builder.Label85, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label85, 0, 0);
   Set_Justify (Gui_Builder.Label85, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label85, False);
   Set_Column_Widget (Gui_Builder.Clist10, 1, Gui_Builder.Label85);

   Gtk_New (Gui_Builder.Label86, -("Signals"));
   Set_Alignment (Gui_Builder.Label86, 0.5, 0.5);
   Set_Padding (Gui_Builder.Label86, 0, 0);
   Set_Justify (Gui_Builder.Label86, Justify_Center);
   Set_Line_Wrap (Gui_Builder.Label86, False);
   Set_Tab (Gui_Builder.Notebook11, 1, Gui_Builder.Label86);

end Initialize;

end Gui_Builder_Pkg;
