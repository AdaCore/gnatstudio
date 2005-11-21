-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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

with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Color;   use Gdk.Color;
with Gtk.Button;  use Gtk.Button;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Image;   use Gtk.Image;
with Gtk.Label;   use Gtk.Label;
with Gtk.Widget;  use Gtk.Widget;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;

with VFS;         use VFS;
with Traces;      use Traces;
with GPS.Intl;  use GPS.Intl;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

with GNAT.OS_Lib;    use GNAT.OS_Lib;
with Ada.Exceptions; use Ada.Exceptions;

package body Welcome_Page is

   procedure On_Overview
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Display the overview page.

   procedure On_Tutorial
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Display the overview page.

   procedure On_UG
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Display the overview page.

   -----------------
   -- On_Overview --
   -----------------

   procedure On_Overview
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Html (Kernel, Create_From_Base ("gps-welcome.html"));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Overview;

   -----------------
   -- On_Tutorial --
   -----------------

   procedure On_Tutorial
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Html (Kernel, Create_From_Base ("gps-tutorial.html"));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Tutorial;

   -----------
   -- On_UG --
   -----------

   procedure On_UG
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Html (Kernel, Create_From_Base ("gps.html"));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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

   -------------------------
   -- Create_Welcome_Page --
   -------------------------

   function Create_Welcome_Page (Kernel : Kernel_Handle) return MDI_Child is
      Main_Box : Welcome_Page_Access;
      Hbox     : Gtk_Hbox;
      Vbox     : Gtk_Vbox;
      Button   : Gtk_Button;
      Label    : Gtk_Label;
      Child    : MDI_Child;
      Image    : Gtk_Image;
      Scroll   : Gtk_Scrolled_Window;

      Pics_Dir : constant String := Get_System_Dir (Kernel)
        & "share" & Directory_Separator & "doc" & Directory_Separator
        & "gps" & Directory_Separator & "html" & Directory_Separator;
      Requisition : Gtk_Requisition;

   begin
      Main_Box := new Welcome_Page_Record;
      Initialize_Vbox (Main_Box);

      Gtk_New (Image, Pics_Dir & "gps_title.gif");
      Pack_Start (Main_Box, Image);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add_With_Viewport (Scroll, Main_Box);

      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 3);
      Pack_Start (Main_Box, Hbox);

      Gtk_New (Label,
        -("GPS is a complete integrated development environment that gives"
          & " access to a wide range of tools and integrates them smoothly.")
        & ASCII.LF & ASCII.LF &
        (-("For more information, click on one of the buttons below, which"
          & " will launch an HTML browser. To configure the HTML browser"
           & " under unix systems, you can go to the menu Edit->Preferences"
           & " and select the 'External Commands' section.")));
      Set_Line_Wrap (Label, True);
      Pack_Start (Hbox, Label);
      Requisition := Get_Child_Requisition (Image);
      Set_Size_Request (Label, Requisition.Width, -1);

      Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 3);
      Pack_Start (Main_Box, Hbox);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Hbox, Vbox, False, False, 3);

      Gtk_New (Button, "");
      Gtk_New (Image, Pics_Dir & "overview.png");
      Add (Button, Image);
      Set_Relief (Button, Relief_None);
      Pack_Start (Vbox, Button, False, False);
      Kernel_Callback.Connect (Button, "clicked", On_Overview'Access, Kernel);
      Gtk_New (Label, -"Overview");
      Pack_Start (Vbox, Label, False, False);
      Grab_Focus (Button);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Hbox, Vbox, False, False, 3);

      Gtk_New (Button, "");
      Gtk_New (Image, Pics_Dir & "tutorial.png");
      Add (Button, Image);
      Set_Relief (Button, Relief_None);
      Pack_Start (Vbox, Button, False, False);
      Kernel_Callback.Connect (Button, "clicked", On_Tutorial'Access, Kernel);
      Gtk_New (Label, -"Tutorial");
      Pack_Start (Vbox, Label, False, False);

      Gtk_New_Vbox (Vbox);
      Pack_Start (Hbox, Vbox, False, False, 3);

      Gtk_New (Button, "");
      Gtk_New (Image, Pics_Dir & "user_guide.png");
      Add (Button, Image);
      Set_Relief (Button, Relief_None);
      Pack_Start (Vbox, Button, False, False);
      Kernel_Callback.Connect (Button, "clicked", On_UG'Access, Kernel);
      Gtk_New (Label, -"User's Guide");
      Pack_Start (Vbox, Label, False, False);

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
