-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Object;             use Glib.Object;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gtk.Enums;               use Gtk.Enums;
with Factory_Data;            use Factory_Data;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Pixmap;              use Gtk.Pixmap;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Handlers;         use Gtkada.Handlers;

with GVD.Menu;                use GVD.Menu;
with GVD.Types;               use GVD.Types;
with GVD.Toolbar;             use GVD.Toolbar;
with GVD.Process;             use GVD.Process;
with Debugger;                use Debugger;

with Glide_Page;              use Glide_Page;
with Glide_Main_Window;       use Glide_Main_Window;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Intl;              use Glide_Intl;
with Pixmaps_IDE;             use Pixmaps_IDE;
with Traces;                  use Traces;

with Ada.Exceptions;          use Ada.Exceptions;

package body GVD_Module is

   GVD_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Debugger");

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Another Executable menu

   procedure On_Step
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Step menu

   procedure On_Step_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Step Instruction menu

   procedure On_Next
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Next menu

   procedure On_Next_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Next Instruction menu

   procedure On_Finish
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Finish menu

   procedure On_Continue
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Continue menu

   -------------------------
   -- On_Debug_Executable --
   -------------------------

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         Configure (Page, Gdb_Type, "", (1 .. 0 => null), "");
      end if;

      GVD.Menu.On_Open_Program (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Executable;

   -------------
   -- On_Step --
   -------------

   procedure On_Step
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Step (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Step;

   -------------------------
   -- On_Step_Instruction --
   -------------------------

   procedure On_Step_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Step_Instruction (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Step_Instruction;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Next (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Next;

   -------------------------
   -- On_Next_Instruction --
   -------------------------

   procedure On_Next_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Next_Instruction (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Next_Instruction;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Finish (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Finish;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      GVD.Menu.On_Continue (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Continue;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar     : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Window      : constant Gtk_Window  := Get_Main_Window (Kernel);
      Button      : Gtk_Widget;
      Debug       : constant String := '/' & (-"Debug") & '/';
      Debug_Sub   : constant String := Debug & (-"Debug") & '/';
      Data_Sub    : constant String := Debug & (-"Data") & '/';
      Session_Sub : constant String := Debug & (-"Session") & '/';
      Mitem       : Gtk_Menu_Item;

   begin
      --  Add debugger menus

      Register_Menu (Kernel, Debug, -"Start", "", null, Ref_Item => -"Data");
      Register_Menu (Kernel, Debug, -"Add Symbols...", "", null,
                     Ref_Item => -"Data");
      Register_Menu (Kernel, Debug_Sub, Ref_Item => -"Data");
      Register_Menu (Kernel, Debug_Sub, -"Another Executable...", "",
                     On_Debug_Executable'Access);
      Register_Menu (Kernel, Debug_Sub, -"Running Process...", "", null);
      Register_Menu (Kernel, Debug_Sub, -"Core File...", "", null);
      Register_Menu (Kernel, Session_Sub, -"Open...", Stock_Open, null);
      Register_Menu (Kernel, Session_Sub, -"Save As...", Stock_Save_As, null);
      Register_Menu (Kernel, Session_Sub, -"Command History",
                     Stock_Index, null);
      Register_Menu (Kernel, Data_Sub, -"Threads", "", null);
      Register_Menu (Kernel, Data_Sub, -"Tasks", "", null);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);

      Register_Menu (Kernel, Data_Sub, -"Edit Breakpoints", "", null);
      Register_Menu (Kernel, Data_Sub, -"Examine Memory", "", null);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);

      Register_Menu (Kernel, Data_Sub, -"Display Local Variables", "", null,
                     GDK_L, Mod1_Mask);
      Register_Menu (Kernel, Data_Sub, -"Display Arguments", "", null,
                     GDK_U, Mod1_Mask);
      Register_Menu (Kernel, Data_Sub, -"Display Registers", "", null);
      Register_Menu (Kernel, Data_Sub, -"Display Any Expression...", "", null);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);

      Register_Menu (Kernel, Data_Sub, -"Refresh", Stock_Refresh, null,
                     GDK_L, Control_Mask);
      Register_Menu (Kernel, Data_Sub, -"Show", "", null);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);

      Register_Menu (Kernel, Debug, -"Step", "", On_Step'Access, GDK_F5);
      Register_Menu (Kernel, Debug, -"Step Instruction", "",
                     On_Step_Instruction'Access, GDK_F5, Shift_Mask);
      Register_Menu (Kernel, Debug, -"Next", "", On_Next'Access, GDK_F6);
      Register_Menu (Kernel, Debug, -"Next Instruction", "",
                     On_Next_Instruction'Access, GDK_F6, Shift_Mask);
      Register_Menu (Kernel, Debug, -"Finish", "", On_Finish'Access, GDK_F7);
      Register_Menu (Kernel, Debug, -"Continue", "", On_Continue'Access,
                     GDK_F8);
      Register_Menu
        (Kernel, Debug, -"Interrupt", Stock_Stop, null, GDK_Escape);
      Register_Menu (Kernel, Debug, -"Detach Process", "", null);

      --  Add debugger buttons in the toolbar

      Append_Space (Toolbar);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Start the debugged program",
         Icon => Gtk_Widget (Create_Pixmap (run_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Run'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text =>
           (-"Start the debugged program, ") &
           (-"stopping at the beginning of the main procedure"),
         Icon => Gtk_Widget (Create_Pixmap (start_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Start'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text =>
           -"Step program until it reaches a different source line",
         Icon => Gtk_Widget (Create_Pixmap (step_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Step'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Step program, proceeding through subroutine calls",
         Icon => Gtk_Widget (Create_Pixmap (next_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Next'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text => -"Execute until selected stack frame returns",
         Icon => Gtk_Widget (Create_Pixmap (finish_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Finish'Access, Window);
      Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Text => "",
         Tooltip_Text =>
           -"Continue program being debugged, after signal or breakpoint",
         Icon => Gtk_Widget (Create_Pixmap (cont_xpm, Window)));
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Continue'Access, Window);
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      GVD_Module_ID := Register_Module
        (Module_Name             => GVD_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => null);
   end Register_Module;

end GVD_Module;
