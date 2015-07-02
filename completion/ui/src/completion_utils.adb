------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

with Glib.Object;               use Glib.Object;
with Gdk.Window;                use Gdk.Window;
with Gtk.Button;                use Gtk.Button;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Label;                 use Gtk.Label;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with String_Utils;              use String_Utils;

package body Completion_Utils is

   type Kernel_And_Location is record
      Kernel   : Kernel_Handle;
      Location : File_Location;
   end record;

   package Cb is new Gtk.Handlers.User_Callback
     (GObject_Record, Kernel_And_Location);
   use Cb;

   procedure On_Location_Button_Clicked
     (Object    : access GObject_Record'Class;
      User_Data : Kernel_And_Location);
   --  Callback on a click on the location button

   --------------------------------
   -- On_Location_Button_Clicked --
   --------------------------------

   procedure On_Location_Button_Clicked
     (Object    : access GObject_Record'Class;
      User_Data : Kernel_And_Location)
   is
      pragma Unreferenced (Object);
   begin
      Open_File_Action_Hook.Run
        (User_Data.Kernel,
         File    => User_Data.Location.File_Path,
         Project => No_Project,  --   ??? unknown
         Line    => User_Data.Location.Line,
         Column  => User_Data.Location.Column);
   end On_Location_Button_Clicked;

   -----------------------
   -- Add_Next_Item_Doc --
   -----------------------

   procedure Add_Next_Item_Doc
     (Notes_Info       : in out Notes_Window_Info;
      Kernel           : Kernel_Handle;
      Fixed_Width_Font : Pango_Font_Description)
   is
      Frame   : Gtk_Frame;
      VBox2   : Gtk_Vbox;
      HBox    : Gtk_Hbox;

      use Proposals_List;

      function Location_To_Label (Loc : File_Location) return String;
      --  Return a pango markup label corresponding to Loc.

      -----------------------
      -- Location_To_Label --
      -----------------------

      function Location_To_Label (Loc : File_Location) return String is
      begin
         return "<span color=""blue""><u>" & Display_Base_Name (Loc.File_Path)
           & ":" & Image (Loc.Line) & "</u></span>";
      end Location_To_Label;

      use type Ada.Containers.Count_Type;
   begin

      if Has_Element (Notes_Info.C) then
         declare
            Doc      : constant String :=
              Element (Notes_Info.C).Get_Documentation (Kernel);
            Location : constant File_Location :=
              Get_Location (Element (Notes_Info.C).all, Kernel.Databases);
            Button         : Gtk_Button;
            Label          : Gtk_Label;
            Title          : Gtk_Label;
            Button_Label   : Gtk_Label;
         begin

            --  Create the label
            Gtk_New (Label);
            Set_Selectable (Label, True);
            Set_Line_Wrap (Label, False);
            Set_Use_Markup (Label, True);
            Modify_Font (Label, Fixed_Width_Font);

            Gtk_New (Frame);

            if Doc /= "" then
               Set_Markup (Label, Doc);
            else
               Set_Markup
                 (Label, "<span color=""darkgrey"">No documentation</span>");
            end if;

            --  If there is only one documentation to display, do not draw a
            --  border around the frame, as this is just graphical noise in
            --  this case.

            if not Notes_Info.Multiple_Items then
               Set_Shadow_Type (Frame, Shadow_None);
            end if;

            Gtk_New_Hbox (HBox);
            Pack_Start (HBox, Label, False, False, 3);

            Gtk_New_Vbox (VBox2);
            Pack_Start (VBox2, HBox, False, False, 3);
            Add (Frame, VBox2);

            --  If there is a file location, create a link to it

            if Location /= Null_File_Location then
               Gtk_New_Hbox (HBox);
               Set_Label_Widget (Frame, HBox);

               --  Create a title
               Gtk_New (Title);
               Set_Use_Markup (Title, True);
               Set_Markup (Title, "<b>Declaration:</b>");
               Pack_Start (HBox, Title, False, False, 1);
               Modify_Font (Title, Fixed_Width_Font);

               --  Create a button
               Gtk_New (Button, "");
               Gtk_New (Button_Label);
               Modify_Font (Button_Label, Fixed_Width_Font);
               Pack_Start (HBox, Button, False, False, 0);
               Add (Button, Button_Label);
               Set_Use_Markup (Button_Label, True);
               Set_Relief (Button, Relief_None);
               Set_Markup (Button_Label, Location_To_Label (Location));

               Object_Connect
                 (Button, Gtk.Button.Signal_Clicked,
                  To_Marshaller (On_Location_Button_Clicked'Access),
                  Button,
                  After => False,
                  User_Data => (Kernel, Location));
            end if;
         end;

         Gtk_New_Hbox (HBox);
         HBox.Pack_Start (Frame, True, True, 3);
         Notes_Info.Notes_Box.Pack_Start (HBox, False, False, 3);
         Notes_Info.Notes_Box.Show_All;
         Next (Notes_Info.C);
      end if;
   end Add_Next_Item_Doc;

end Completion_Utils;
