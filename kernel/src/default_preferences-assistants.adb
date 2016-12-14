------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with Glib;          use Glib;

with Gtk.Assistant; use Gtk.Assistant;
with Gtk.Arrow;     use Gtk.Arrow;
with Gtk.Box;       use Gtk.Box;
with Gtk.Button;    use Gtk.Button;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Label;     use Gtk.Label;
with Gtk.Main;
with Gtk.Style_Context; use Gtk.Style_Context;

with Dialog_Utils;    use Dialog_Utils;
with GPS.Main_Window; use GPS.Main_Window;

package body Default_Preferences.Assistants is

   procedure On_Cancel (Self : access Gtk_Assistant_Record'Class);
   procedure On_Apply (Self : access Gtk_Assistant_Record'Class);

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel (Self : access Gtk_Assistant_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
      Self.Destroy;
   end On_Cancel;

   --------------
   -- On_Apply --
   --------------

   procedure On_Apply (Self : access Gtk_Assistant_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Gtk.Main.Main_Quit;
   end On_Apply;

   ------------
   -- Create --
   ------------

   function Create
     (Pref_Page : not null access Preferences_Page_Record'Class;
      Label     : String;
      Message   : String) return Preferences_Assistant_Page
   is
     (Preferences_Assistant_Page'
        (Pref_Page  => Preferences_Page (Pref_Page),
         Label      => To_Unbounded_String (Label),
         Message    => To_Unbounded_String (Message)));

   -----------------------------------
   -- Display_Preferences_Assistant --
   -----------------------------------

   procedure Display_Preferences_Assistant
     (Kernel : not null access Kernel_Handle_Record'Class;
      Pages  : Preferences_Assistant_Page_Array)
   is
      Manager       : constant Preferences_Manager := Kernel.Get_Preferences;
      Assistant     : Gtk_Assistant;

      procedure Rename_Standard_Butttons;
      --  Rename some standard Gtk.Assistant buttons (e.g: "Cancel" to "Skip &
      --  Use Defaults").

      procedure Create_Assistant_Page_View (Page_Index : Integer);
      --  Create and append a page refered by Page_Index to the assistant

      ------------------------------
      -- Rename_Standard_Butttons --
      ------------------------------

      procedure Rename_Standard_Butttons is
         Label    : Gtk_Label;
         Hbox     : Gtk_Box;
         Children : Widget_List.Glist;
         Button   : Gtk_Button;

         use Widget_List;
      begin
         --  Little hack to use to retrieve the Gtk.Assistant action area

         Gtk_New (Label, " ");
         Assistant.Add_Action_Widget (Label);
         Hbox := Gtk_Box (Label.Get_Parent);
         Hbox.Remove (Label);

         Children := Hbox.Get_Children;

         --  Loop over all the action area children and rename some of the
         --  buttons.

         while Children /= Widget_List.Null_List loop
            Button := Gtk_Button (Widget_List.Get_Data (Children));

            if Button.Get_Label = "_Cancel" then
               Button.Set_Label ("Skip & Use Defaults");
            end if;

            if Button.Get_Label = "_Apply" then
               Button.Set_Label ("Start using GPS");
            end if;

            Children := Next (Children);
         end loop;

         Widget_List.Free (Children);
      end Rename_Standard_Butttons;

      --------------------------------
      -- Create_Assistant_Page_View --
      --------------------------------

      procedure Create_Assistant_Page_View (Page_Index : Integer) is
         Page           : Preferences_Assistant_Page := Pages (Page_Index);
         Page_View      : Dialog_View;
         Group_Widget   : Dialog_Group_Widget;
         Pref_Page_View : constant Gtk_Widget :=
                            Page.Pref_Page.Get_Widget (Manager);
         Progress_Box   : Gtk_Box;
         Page_Label     : Gtk_Label;
         Right_Arrow    : Gtk_Arrow;
         Message_Label  : Gtk_Label;
         Page_Num       : Gint with Unreferenced;
         Page_Type      : constant Gtk_Assistant_Page_Type :=
                            (if Page_Index = Pages'First then
                                Gtk_Assistant_Page_Intro
                             elsif Page_Index = Pages'Last then
                                Gtk_Assistant_Page_Confirm
                             else
                                Gtk_Assistant_Page_Content);
      begin
         Page_View := new Dialog_View_Record;
         Dialog_Utils.Initialize (Page_View);

         --  Add a new CSS class for these assistants to override the dialog
         --  views default style (e.g: bigger font for the page's title).
         Get_Style_Context (Page_View).Add_Class ("assistant-page-views");

         --  Add a 'progress box' at the top of the page, which lists all
         --  the pages composing the assistant and that highlights the current
         --  one.

         Gtk_New_Hbox (Progress_Box, Homogeneous => False);
         Page_View.Append (Progress_Box, Expand => False);

         for I in Pages'Range loop
            Gtk_New (Page_Label);
            Progress_Box.Pack_Start (Page_Label, Expand => False);

            if I = Page_Index then
               Page_Label.Set_Markup
                 (To_String ("<b>" & Pages (I).Label & "</b>"));
            else
               Page_Label.Set_Text (To_String (Pages (I).Label));
            end if;

            if I /= Pages'Last then
               Gtk_New (Right_Arrow, Arrow_Right, Shadow_None);
               Progress_Box.Pack_Start (Right_Arrow, Expand => False);
            end if;
         end loop;

         --  Add the page title's label

         Group_Widget := new Dialog_Group_Widget_Record;
         Initialize
           (Group_Widget,
            Parent_View         => Page_View,
            Group_Name          => To_String (Page.Label),
            Allow_Multi_Columns => False);

         --  Add the preferences page view

         Group_Widget := new Dialog_Group_Widget_Record;
         Initialize
           (Group_Widget,
            Parent_View         => Page_View,
            Allow_Multi_Columns => False);
         Group_Widget.Append_Child
           (Pref_Page_View,
            Expand => True,
            Fill   => True);

         --  Add the message label

         Group_Widget := new Dialog_Group_Widget_Record;
         Initialize
           (Group_Widget,
            Parent_View         => Page_View,
            Allow_Multi_Columns => False);
         Gtk_New (Message_Label);
         Set_Markup (Message_Label, To_String (Page.Message));
         Message_Label.Set_Alignment (0.0, 0.5);
         Group_Widget.Append_Child (Message_Label, Expand => False);

         Page_Num := Assistant.Append_Page (Page_View);
         Assistant.Set_Page_Type (Page_View, Page_Type);
         Assistant.Set_Page_Complete (Page_View, True);
      end Create_Assistant_Page_View;

   begin
      Gtk_New (Assistant);
      Assistant.Set_Position (Win_Pos_Center);

      Set_Default_Size_From_History
        (Assistant,
         Name   => "gps-preferences-assistant-dialog",
         Kernel => Kernel,
         Width  => 600,
         Height => 400);

      Assistant.On_Cancel (On_Cancel'Access);
      Assistant.On_Apply (On_Apply'Access);

      for Page_Index in Pages'Range loop
         Create_Assistant_Page_View (Page_Index);
      end loop;

      Rename_Standard_Butttons;

      --  Show the assistant and launch a main loop: we do not want to leave
      --  this procedure while the assistant is running.
      Assistant.Show_All;
      Gtk.Main.Main;
   end Display_Preferences_Assistant;

end Default_Preferences.Assistants;
