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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;

with Traces;                    use Traces;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

with Ada.Exceptions; use Ada.Exceptions;

package body Welcome_Page is

   type Pic_Data is record
      Image     : Gtk_Image;
      Mouse_On  : Gdk_Pixbuf;
      Mouse_Off : Gdk_Pixbuf;
   end record;

   package Kernel_Return_Package is new User_Return_Callback
     (GObject_Record, Boolean, Kernel_Handle);
   use Kernel_Return_Package;

   package Reacting_Button_Package is new User_Return_Callback
     (GObject_Record, Boolean, Pic_Data);
   use Reacting_Button_Package;

   function Create_Reacting_Button
     (Window : Gtk_Window;
      Mouse_On, Mouse_Off : String) return Gtk_Event_Box;
   --  Create a button that has Mouse_On and Mouse_Off as on-mouse-over images.
   --  Window is used as base for creating the graphical contexts.

   function On_Overview
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) return Boolean;
   --  Display the overview page

   function On_Tutorial
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) return Boolean;
   --  Display the overview page

   function On_UG
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) return Boolean;
   --  Display the overview page

   function On_Enter
     (Widget : access GObject_Record'Class; Data : Pic_Data) return Boolean;
   --  Called when mouse enters Widget

   function On_Leave
     (Widget : access GObject_Record'Class; Data : Pic_Data) return Boolean;
   --  Called when mouse leaves Widget

   -----------------
   -- On_Overview --
   -----------------

   function On_Overview
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Open_Html (Kernel, "gps-welcome.html");
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Overview;

   -----------------
   -- On_Tutorial --
   -----------------

   function On_Tutorial
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Open_Html (Kernel, "gps-tutorial.html");
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
      return False;
   end On_Tutorial;

   -----------
   -- On_UG --
   -----------

   function On_UG
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Open_Html (Kernel, "gps.html");
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
      return False;
   end On_UG;

   --------------------------
   -- Display_Welcome_Page --
   --------------------------

   procedure Display_Welcome_Page (Kernel : Kernel_Handle) is
      Child : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Welcome_Page_Record'Tag);

      if Child = null then
         Child := Create_Welcome_Page (Kernel);
      end if;

      Raise_Child (Child);
   end Display_Welcome_Page;

   --------------
   -- On_Enter --
   --------------

   function On_Enter
     (Widget : access GObject_Record'Class; Data : Pic_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Set (Data.Image, Data.Mouse_On);
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Enter;

   --------------
   -- On_Leave --
   --------------

   function On_Leave
     (Widget : access GObject_Record'Class; Data : Pic_Data) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Set (Data.Image, Data.Mouse_Off);
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Leave;

   ----------------------------
   -- Create_Reacting_Button --
   ----------------------------

   function Create_Reacting_Button
     (Window : Gtk_Window;
      Mouse_On, Mouse_Off : String) return Gtk_Event_Box
   is
      Data   : Pic_Data;
      Box    : Gtk_Event_Box;
   begin
      Data.Mouse_On := Render_Icon (Window, Mouse_On, Icon_Size_Large_Toolbar);
      Data.Mouse_Off := Render_Icon
        (Window, Mouse_Off, Icon_Size_Large_Toolbar);

      Gtk_New (Data.Image, Pixbuf => Data.Mouse_Off);
      Gtk_New (Box);

      Add (Box, Data.Image);

      Set_Events (Box,
                  Get_Events (Data.Image)
                  or Pointer_Motion_Hint_Mask
                  or Button_Press_Mask
                  or Enter_Notify_Mask
                  or Leave_Notify_Mask);

      Connect (Box, Signal_Enter_Notify_Event,
               To_Marshaller (On_Enter'Access), Data);

      Connect (Box, Signal_Leave_Notify_Event,
               To_Marshaller (On_Leave'Access), Data);

      return Box;
   end Create_Reacting_Button;

   -------------------------
   -- Create_Welcome_Page --
   -------------------------

   function Create_Welcome_Page (Kernel : Kernel_Handle) return MDI_Child is
      Main_Box    : Welcome_Page_Access;
      Main_Vbox   : Gtk_Vbox;
      Hbox        : Gtk_Hbox;
      Vbox        : Gtk_Vbox;
      Label       : Gtk_Label;
      Child       : MDI_Child;
      Image       : Gtk_Image;
      Scroll      : Gtk_Scrolled_Window;
      Box         : Gtk_Event_Box;
      Requisition : Gtk_Requisition;
      Win         : Gtk_Window;

   begin
      Main_Box := new Welcome_Page_Record;
      Initialize_Vbox (Main_Box);

      Win := Get_Main_Window (Kernel);

      Gtk_New
        (Image,
         Pixbuf => Render_Icon
           (Win, "welcome-header", Icon_Size_Large_Toolbar));
      Pack_Start (Main_Box, Image);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add_With_Viewport (Scroll, Main_Box);

      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 3);
      Pack_Start (Main_Box, Hbox);

      Gtk_New_Vbox (Main_Vbox);
      Pack_Start (Hbox, Main_Vbox);

      Gtk_New (Label,
        -("GPS is a powerful and simple-to-use IDE that"
          & " streamlines your software development process"
          & " from the initial coding stage through testing,"
          & " debugging, system integration, and maintenance."));
      Set_Line_Wrap (Label, True);
      Pack_Start (Main_Vbox, Label, True, True, 3);
      Requisition := Get_Child_Requisition (Image);
      Set_Size_Request (Label, Requisition.Width, -1);

      Gtk_New_Vbox (Vbox);
      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 3);
      Pack_Start (Main_Vbox, Vbox, True, True, 3);
      Pack_Start (Vbox, Hbox, False, False, 3);
      Set_Size_Request (Hbox, Requisition.Width, -1);
      Set_Size_Request (Vbox, Requisition.Width, -1);

      Box := Create_Reacting_Button
        (Win, "button-overview", "button-overview-over");
      Gtk_New_Vbox (Vbox);
      Pack_Start (Vbox, Box, False, False, 3);
      Connect
        (Box, Signal_Button_Press_Event,
         To_Marshaller (On_Overview'Access), Kernel);
      Pack_Start (Hbox, Vbox, False, False, 3);

      Box := Create_Reacting_Button
        (Win, "button-guide", "button-guide-over");
      Gtk_New_Vbox (Vbox);
      Pack_Start (Vbox, Box, False, False, 3);
      Connect
        (Box, Signal_Button_Press_Event,
         To_Marshaller (On_UG'Access), Kernel);
      Pack_Start (Hbox, Vbox, False, False, 3);

      Box := Create_Reacting_Button
        (Win, "button-tutorial", "button-tutorial-over");
      Gtk_New_Vbox (Vbox);
      Pack_Start (Vbox, Box, False, False, 3);
      Connect
        (Box, Signal_Button_Press_Event,
         To_Marshaller (On_Tutorial'Access), Kernel);
      Pack_Start (Hbox, Vbox, False, False, 3);

      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 3);
      Pack_Start (Main_Vbox, Hbox, True, True, 3);
      Gtk_New (Label);
      Set_Use_Markup (Label, True);

      Set_Markup
        (Label, -("<span color=""dark grey"">Note the overview, "
         & "tutorial and user's guide require an HTML browser. If you are "
         & "having difficulties viewing under UNIX systems you can configure "
         & "your browser by going to the menu Edit&gt;Preferences and "
         & "visiting the section 'External Commands'</span>"));
      Set_Line_Wrap (Label, True);
      Pack_Start (Hbox, Label, False, False, 3);
      Set_Size_Request (Label, Requisition.Width, -1);

      Show_All (Scroll);

      Set_Size_Request (Scroll, -1, 500);
      Gtk_New (Child, Scroll);
      Set_Title (Child, -"Welcome to GPS");
      Put (Get_MDI (Kernel), Child);

      Modify_Bg (Get_Parent (Main_Box),
                 State_Normal, White (Get_Default_Colormap));

      return Child;
   end Create_Welcome_Page;

end Welcome_Page;
