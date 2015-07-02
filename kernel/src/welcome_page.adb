------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gdk.Event;                 use Gdk.Event;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;

package body Welcome_Page is

   type Pic_Data is record
      Image     : Gtk_Image;
      Mouse_On  : Unbounded_String;
      Mouse_Off : Unbounded_String;
   end record;

   package Kernel_Return_Package is new User_Return_Callback
     (GObject_Record, Boolean, Kernel_Handle);
   use Kernel_Return_Package;

   package Reacting_Button_Package is new User_Return_Callback
     (GObject_Record, Boolean, Pic_Data);
   use Reacting_Button_Package;

   function Create_Reacting_Button
     (Mouse_On, Mouse_Off : String) return Gtk_Event_Box;
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
      Html_Action_Hook.Run (Kernel, "gps-welcome.html");
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
      Html_Action_Hook.Run (Kernel, "tutorial/index.html");
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
      Html_Action_Hook.Run (Kernel, "users_guide/index.html");
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
      Data.Image.Set_From_Icon_Name
         (To_String (Data.Mouse_On), Icon_Size_Large_Toolbar);
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
      Data.Image.Set_From_Icon_Name
         (To_String (Data.Mouse_Off), Icon_Size_Large_Toolbar);
      return False;
   end On_Leave;

   ----------------------------
   -- Create_Reacting_Button --
   ----------------------------

   function Create_Reacting_Button
     (Mouse_On, Mouse_Off : String) return Gtk_Event_Box
   is
      Data   : Pic_Data;
      Box    : Gtk_Event_Box;
   begin
      Data.Mouse_On := To_Unbounded_String (Mouse_On);
      Data.Mouse_Off := To_Unbounded_String (Mouse_Off);

      Gtk_New_From_Icon_Name
         (Data.Image, To_String (Data.Mouse_Off), Icon_Size_Large_Toolbar);
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
      Child       : GPS_MDI_Child;
      Image       : Gtk_Image;
      Scroll      : Gtk_Scrolled_Window;
      Box         : Gtk_Event_Box;

   begin
      Main_Box := new Welcome_Page_Record;
      Initialize_Vbox (Main_Box);

      Gtk_New_From_Icon_Name
        (Image, "welcome_header", Icon_Size_Large_Toolbar);
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
      Label.Set_Alignment (0.0, 0.0);
      Label.Set_Padding (60, 0);
      Label.Override_Color (Gtk_State_Flag_Normal, Black_RGBA);
      Set_Line_Wrap (Label, True);
      Main_Vbox.Pack_Start (Label, True, True, 3);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Main_Vbox, Vbox, True, True, 3);

      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 3);
      Pack_Start (Vbox, Hbox, False, False, 3);

      Gtk_New_Vbox (Vbox);
      Hbox.Pack_Start (Vbox, False, False, 3);
      Box := Create_Reacting_Button
        ("button_overview", "button_overview_over");
      Vbox.Pack_Start (Box, False, False, 3);
      Connect
        (Box, Signal_Button_Release_Event,
         To_Marshaller (On_Overview'Access), Kernel);

      Gtk_New_Vbox (Vbox);
      Hbox.Pack_Start (Vbox, False, False, 3);
      Box := Create_Reacting_Button
        ("button_guide", "button_guide_over");
      Vbox.Pack_Start (Box, False, False, 3);
      Connect
        (Box, Signal_Button_Release_Event,
         To_Marshaller (On_UG'Access), Kernel);

      Gtk_New_Vbox (Vbox);
      Hbox.Pack_Start (Vbox, False, False, 3);
      Box := Create_Reacting_Button
        ("button_tutorial", "button_tutorial_over");
      Vbox.Pack_Start (Box, False, False, 3);
      Connect
        (Box, Signal_Button_Release_Event,
         To_Marshaller (On_Tutorial'Access), Kernel);

      Gtk_New (Label);
      Label.Set_Markup
        (-("<span color=""dark grey"">Note the overview, "
         & "tutorial and user's guide require an HTML browser. If you are "
         & "having difficulties viewing under UNIX systems you can configure "
         & "your browser by going to the menu Edit&gt;Preferences and "
         & "visiting the section 'External Commands'</span>"));
      Set_Line_Wrap (Label, True);
      Label.Set_Alignment (0.0, 0.0);
      Label.Set_Padding (60, 0);
      Label.Override_Color (Gtk_State_Flag_Normal, Black_RGBA);
      Main_Vbox.Pack_Start (Label, True, True, 3);

      Show_All (Scroll);

      Gtk_New (Child, Scroll, Kernel, Desktop_Independent => False);
      Set_Title (Child, -"Welcome to GPS");
      Put (Get_MDI (Kernel), Child);

      Override_Background_Color
        (Get_Parent (Main_Box), Gtk_State_Flag_Normal, White_RGBA);

      return MDI_Child (Child);
   end Create_Welcome_Page;

end Welcome_Page;
