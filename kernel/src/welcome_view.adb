------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib.Object;           use Glib.Object;
with Glib.Unicode;          use Glib.Unicode;
with Glib;                  use Glib;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Image;             use Gtk.Image;
with Gtk.Label;             use Gtk.Label;
with Gtk.Link_Button;       use Gtk.Link_Button;
with Gtk.Style_Context;     use Gtk.Style_Context;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.MDI;            use Gtkada.MDI;

with Dialog_Utils;          use Dialog_Utils;
with Generic_Views;         use Generic_Views;
with GPS.Kernel.Hooks;      use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;        use GPS.Kernel.MDI;
with GUI_Utils;             use GUI_Utils;

package body Welcome_View is

   type Welcome_Page_Record is new Generic_Views.View_Record with record
      null;
   end record;
   type Welcome_Page is access all Welcome_Page_Record'Class with Unreferenced;

   function Initialize
     (Self : access Welcome_Page_Record'Class) return Gtk_Widget;

   package Welcome_Page_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Welcome",
      View_Name                 => "Welcome",
      Formal_View_Record        => Welcome_Page_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Reuse_If_Exist            => True,
      Group                     => Group_Default,
      Areas                     => Gtkada.MDI.Both,
      Position                  => Position_Automatic,
      Initialize                => Initialize);

   type GPS_Link_Button_Record is new Gtk_Link_Button_Record with record
      Kernel      : Kernel_Handle;
      URL_Or_File : Unbounded_String;
   end record;
   type GPS_Link_Button is access all GPS_Link_Button_Record'Class;

   procedure On_Link_Clicked (Self : access Gtk_Button_Record'Class);

   ---------------------
   -- On_Link_Clicked --
   ---------------------

   procedure On_Link_Clicked (Self : access Gtk_Button_Record'Class)
   is
      Link_Button : constant GPS_Link_Button := GPS_Link_Button (Self);
   begin
      Html_Action_Hook.Run
        (Link_Button.Kernel,
         Url_Or_File => To_String (Link_Button.URL_Or_File));
   end On_Link_Clicked;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Welcome_Page_Record'Class) return Gtk_Widget
   is
      Main_View    : Dialog_View;
      Help_Vbox    : Gtk_Vbox;
      Desc_Label   : Gtk_Label;
      Help_Label   : Gtk_Label;
      Github_Image : Gtk_Image;
      Github_Link  : GPS_Link_Button;

      procedure Create_Help_Entry
        (Before_Text : String;
         Hyperlink   : String;
         After_Text  : String;
         URL_Or_File : String);

      -----------------------
      -- Create_Help_Entry --
      -----------------------

      procedure Create_Help_Entry
        (Before_Text : String;
         Hyperlink   : String;
         After_Text  : String;
         URL_Or_File : String)
      is
         Hbox        : Gtk_Hbox;
         Label       : Gtk_Label;
         Link_Button : GPS_Link_Button;
      begin
         Gtk_New_Hbox (Hbox, Homogeneous => False);
         Help_Vbox.Pack_Start (Hbox, Expand => False, Padding => 3);

         Gtk_New (Label);
         Label.Set_Use_Markup (True);
         Label.Set_Markup (Before_Text);
         Hbox.Pack_Start (Label, Expand => False);

         Link_Button := new GPS_Link_Button_Record'
           (GObject_Record with
            Kernel      => Self.Kernel,
            URL_Or_File => To_Unbounded_String (URL_Or_File));
         Initialize_With_Label
           (Link_Button,
            URI   => "",
            Label => Hyperlink);
         Link_Button.On_Clicked (On_Link_Clicked'Access);
         Hbox.Pack_Start (Link_Button, Expand => False);

         Gtk_New (Label);
         Label.Set_Use_Markup (True);
         Label.Set_Markup (After_Text);
         Hbox.Pack_Start (Label, Expand => False);
      end Create_Help_Entry;

   begin
      Initialize_Vbox (Self);
      Get_Style_Context (Self).Add_Class ("gps-welcome-view");

      Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Main_View);

      Self.Pack_Start (Main_View);

      Main_View.Append (Create_Logo_And_Title_Area, Expand => False);

      Gtk_New (Desc_Label,
               ("GPS is a powerful and simple-to-use IDE that"
                & " streamlines your software development process"
                & " from the initial coding stage through testing,"
                & " debugging, system integration, and maintenance."));
      Get_Style_Context (Desc_Label).Add_Class ("gps-welcome-view-desc");
      Desc_Label.Set_Line_Wrap (True);
      Main_View.Append (Desc_Label, Expand => False, Add_Separator => False);

      Gtk_New_Vbox (Help_Vbox, Homogeneous => False);
      Help_Vbox.Set_Halign (Align_Center);
      Main_View.Append (Help_Vbox, Expand => False, Add_Separator => False);

      Gtk_New (Help_Label,
               "For more information and help, please visit:");
      Help_Label.Set_Alignment (0.0, 0.0);
      Help_Vbox.Pack_Start (Help_Label, Expand => False, Padding => 5);

      declare
         Bullet_Char : String (1 .. 6);
         Last        : Natural;
      begin
         Unichar_To_UTF8
           (C      => 8_226,
            Buffer => Bullet_Char,
            Last   => Last);

         Create_Help_Entry
           (Before_Text => "   " & Bullet_Char (1 .. Last) & " The ",
            Hyperlink   => "GPS User's Guide",
            After_Text  => ", for guides and API reference",
            URL_Or_File => "users_guide/index.html");

         Create_Help_Entry
           (Before_Text => "   " & Bullet_Char (1 .. Last) & " The ",
            Hyperlink   => "GPS Tutorial",
            After_Text  => ", to help you starting with GPS",
            URL_Or_File => "tutorial/index.html");

      end;

      Gtk_New_From_Icon_Name
        (Github_Image,
         Icon_Name => "gps-github-symbolic",
         Size      => Icon_Size_Button);

      Github_Link := new GPS_Link_Button_Record'
        (GObject_Record with
         Kernel      => Self.Kernel,
         URL_Or_File =>
           To_Unbounded_String ("https://github.com/AdaCore/gps"));

      Initialize_With_Label (Github_Link, " Contribute to GPS on GitHub!");
      Github_Link.Set_Name ("gps-welcome-view-github-link");
      Github_Link.Set_Always_Show_Image (True);
      Github_Link.Set_Image (Github_Image);
      Github_Link.On_Clicked (On_Link_Clicked'Access);

      Main_View.Append (Github_Link, Expand => False, Add_Separator => False);

      return null;
   end Initialize;

   --------------------------
   -- Display_Welcome_View --
   --------------------------

   procedure Display_Welcome_View
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      View : constant Welcome_Page_Views.View_Access :=
        Welcome_Page_Views.Get_Or_Create_View (Kernel) with Unreferenced;
   begin
      null;
   end Display_Welcome_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Welcome_Page_Views.Register_Module (Kernel);
   end Register_Module;

end Welcome_View;
