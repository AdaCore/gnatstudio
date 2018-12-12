------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

with Glib;              use Glib;

with Gtk.Assistant;     use Gtk.Assistant;
with Gtk.Arrow;         use Gtk.Arrow;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Main;
with Gtk.Style_Context; use Gtk.Style_Context;

with Dialog_Utils;      use Dialog_Utils;
with GPS.Main_Window;   use GPS.Main_Window;

with Glib.Main;

package body Default_Preferences.Assistants is

   Preferences_Assistant_Title : constant String := "Customize GNAT Studio";

   type Preferences_Assistant_Record is new Gtk_Assistant_Record with record
      Running_Main_Loop : Boolean := True;
      --  True if the preferences assistant is running a nested main loop

      Skip_Button        : Gtk_Button;
      Back_Button        : Gtk_Button;
      Next_Button        : Gtk_Button;
      Apply_Button       : Gtk_Button;
      --  The buttons used to navigate in the preferences assistant
   end record;
   type Preferences_Assistant is access all Preferences_Assistant_Record;

   procedure On_Finish (Self : access GObject_Record'Class);
   procedure On_Next (Self : access GObject_Record'Class);
   procedure On_Back (Self : access GObject_Record'Class);
   procedure On_Prepare
     (Self : access Gtk_Assistant_Record'Class;
      Page : not null access Gtk_Widget_Record'Class);

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish (Self : access GObject_Record'Class)
   is
      Assistant : constant Preferences_Assistant :=
                    Preferences_Assistant (Self);
   begin
      --  Quit the preferences assistant's main loop only if it's still running
      if Assistant.Running_Main_Loop then
         Gtk.Main.Main_Quit;
      end if;

      Assistant.Destroy;
   end On_Finish;

   -------------
   -- On_Next --
   -------------

   procedure On_Next (Self : access GObject_Record'Class) is
      Assistant : constant Preferences_Assistant :=
        Preferences_Assistant (Self);
   begin
      Assistant.Set_Current_Page (Assistant.Get_Current_Page + 1);
      Assistant.Set_Title (Preferences_Assistant_Title);
   end On_Next;

   -------------
   -- On_Back --
   -------------

   procedure On_Back (Self : access GObject_Record'Class) is
      Assistant : constant Preferences_Assistant :=
        Preferences_Assistant (Self);
   begin
      Assistant.Set_Current_Page (Assistant.Get_Current_Page - 1);
      Assistant.Set_Title (Preferences_Assistant_Title);
   end On_Back;

   ----------------
   -- On_Prepare --
   ----------------

   procedure On_Prepare
     (Self : access Gtk_Assistant_Record'Class;
      Page : not null access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Page);
      Assistant  : constant Preferences_Assistant :=
                     Preferences_Assistant (Self);
      First_Page : constant Gint := 0;
      Last_Page  : constant Gint := Assistant.Get_N_Pages - 1;
      Page_Index : constant Gint := Assistant.Get_Current_Page;
   begin
      if Page_Index = First_Page then
         Assistant.Back_Button.Hide;
         Assistant.Next_Button.Show_All;
         Assistant.Skip_Button.Show_All;
         Assistant.Apply_Button.Hide;
      elsif Page_Index = Last_Page then
         Assistant.Back_Button.Show_All;
         Assistant.Next_Button.Hide;
         Assistant.Skip_Button.Hide;
         Assistant.Apply_Button.Show_All;
      else
         Assistant.Back_Button.Show_All;
         Assistant.Next_Button.Show_All;
         Assistant.Skip_Button.Show_All;
         Assistant.Apply_Button.Hide;
      end if;
   end On_Prepare;

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
      Assistant     : Preferences_Assistant;

      procedure Create_Assistant_Page_View (Page_Index : Integer);
      --  Create and append a page refered by Page_Index to the assistant

      procedure Add_Navigation_Buttons;
      --  Add our own custom navigation buttons to have more flexibility

      function Auto_Next return Boolean;
      --  Auto-click next. For testing purposes.

      ----------------------------
      -- Add_Navigation_Buttons --
      ----------------------------

      procedure Add_Navigation_Buttons is
      begin
         Gtk_New (Assistant.Apply_Button, "Apply");
         Assistant.Add_Action_Widget (Assistant.Apply_Button);
         Assistant.Apply_Button.On_Clicked (On_Finish'Access, Assistant);

         Gtk_New (Assistant.Skip_Button, "Skip & Use Defaults");
         Assistant.Skip_Button.On_Clicked (On_Finish'Access, Assistant);
         Assistant.Add_Action_Widget (Assistant.Skip_Button);

         Gtk_New (Assistant.Next_Button, "Next");
         Assistant.Next_Button.On_Clicked (On_Next'Access, Assistant);
         Assistant.Add_Action_Widget (Assistant.Next_Button);

         Gtk_New (Assistant.Back_Button, "Back");
         Assistant.Back_Button.On_Clicked (On_Back'Access, Assistant);
         Assistant.Add_Action_Widget (Assistant.Back_Button);
      end Add_Navigation_Buttons;

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
           Gtk_Assistant_Page_Custom;
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

      ---------------
      -- Auto_Next --
      ---------------

      function Auto_Next return Boolean is
      begin
         while not Assistant.Apply_Button.Is_Visible loop
            Assistant.Next_Button.Clicked;
         end loop;
         Assistant.Apply_Button.Clicked;
         return False;
      end Auto_Next;

   begin
      Assistant := new Preferences_Assistant_Record;
      Gtk.Assistant.Initialize (Assistant);
      Assistant.Set_Position (Win_Pos_Center);

      Set_Default_Size_From_History
        (Assistant,
         Name   => "gps-preferences-assistant-dialog",
         Kernel => Kernel,
         Width  => 900,
         Height => 600);

      Assistant.On_Cancel (On_Finish'Access, Slot => Assistant);
      Assistant.On_Prepare (On_Prepare'Access);

      for Page_Index in Pages'Range loop
         Create_Assistant_Page_View (Page_Index);
      end loop;

      Add_Navigation_Buttons;

      --  Show the assistant and launch a main loop: we do not want to leave
      --  this procedure while the assistant is running.
      Assistant.Show_All;

      --  Set the title once the assistant is shown: it seems to be the only
      --  way to override the pages' titles
      Assistant.Set_Title (Preferences_Assistant_Title);

      --  A trace allows automatically pressing Next on the assistant. This
      --  is used for testing purposes.

      if Auto_Run_Assistant.Active then
         declare
            Ignored : Glib.Main.G_Source_Id;
         begin
            Ignored := Glib.Main.Timeout_Add
              (500, Auto_Next'Unrestricted_Access);
         end;
      end if;

      Gtk.Main.Main;
   end Display_Preferences_Assistant;

end Default_Preferences.Assistants;
