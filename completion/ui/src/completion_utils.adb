------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib.Object;               use Glib.Object;
with Gtk.Button;                use Gtk.Button;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Link_Button;           use Gtk.Link_Button;
with Gtk.Style_Context;         use Gtk.Style_Context;

with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
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
      Declaration_Frame : Gtk_Frame;
      Doc_Label         : Gtk_Label;
      Doc_Frame         : Gtk_Frame;

      use Proposals_List;

      function Location_To_Text (Loc : File_Location) return String;
      --  Return a string concatenating the location's filename and its line

      ----------------------
      -- Location_To_Text --
      ----------------------

      function Location_To_Text (Loc : File_Location) return String is
      begin
         return Display_Base_Name (Loc.File_Path) & ":" & Image (Loc.Line);
      end Location_To_Text;

   begin
      --  If no element, return
      if not Has_Element (Notes_Info.C) then
         return;
      end if;

      declare
         Doc      : constant String :=
                      Element (Notes_Info.C).Get_Documentation (Kernel);
         Location : constant File_Location :=
                      Get_Location (Element (Notes_Info.C).all,
                                    Kernel.Databases);
      begin
         --  Create the frame containing the declaration documentation
         Gtk_New (Declaration_Frame);
         Notes_Info.Notes_Box.Pack_Start (Declaration_Frame, Expand => False);

         --  If there is only one documentation to display, do not draw a
         --  border around the frame, as this is just graphical noise in
         --  this case.
         if not Notes_Info.Multiple_Items then
            Set_Shadow_Type (Declaration_Frame, Shadow_None);
         end if;

         --  Create the label containing the documentaion
         Gtk_New (Doc_Label);
         Doc_Label.Set_Halign (Align_Start);
         Set_Selectable (Doc_Label, True);
         Set_Line_Wrap (Doc_Label, False);
         Set_Use_Markup (Doc_Label, True);
         Modify_Font (Doc_Label, Fixed_Width_Font);

         if Doc /= "" then
            Set_Markup (Doc_Label, Doc);
         else
            Set_Markup
              (Doc_Label,
               "<span color=""darkgrey"">No documentation</span>");
         end if;

         --  Add the label containing the documentaion within a frame so that
         --  it can be easily aligned in CSS.
         Gtk_New (Doc_Frame);
         Get_Style_Context (Doc_Frame).Add_Class ("notes-doc-frames");
         Doc_Frame.Add (Doc_Label);
         Add (Declaration_Frame, Doc_Frame);

         --  If there is a file location, create a link to it
         if Location /= Null_File_Location then
            declare
               Location_Text     : constant String :=
                                     Location_To_Text (Location);
               Declaration_Link  : Gtk_Link_Button;
               Declaration_Label : Gtk_Label;
               Title_Box         : Gtk_Hbox;
            begin
               --  Create the declaration label with a link to its declaration
               Gtk_New_Hbox (Title_Box);
               Declaration_Frame.Set_Label_Widget (Title_Box);

               Gtk_New (Declaration_Label);
               Set_Use_Markup (Declaration_Label, True);
               Set_Markup (Declaration_Label, "<b>Declaration:</b>");
               Title_Box.Pack_Start (Declaration_Label, Expand => False);
               Modify_Font (Declaration_Label, Fixed_Width_Font);

               Gtk_New (Declaration_Link, Location_Text);
               Declaration_Link.Set_Label (Location_Text);
               Title_Box.Pack_Start (Declaration_Link, Expand => False);

               Object_Connect
                 (Declaration_Link, Gtk.Button.Signal_Clicked,
                  To_Marshaller (On_Location_Button_Clicked'Access),
                  Declaration_Link,
                  After     => False,
                  User_Data => (Kernel, Location));
            end;
         end if;
      end;

      Notes_Info.Notes_Box.Show_All;
      Next (Notes_Info.C);
   end Add_Next_Item_Doc;

end Completion_Utils;
