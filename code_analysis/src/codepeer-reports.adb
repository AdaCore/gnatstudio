------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2026, AdaCore                     --
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

with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;

with Gtkada.Handlers;

with Gtk.Button;
with Gtk.Enums;
with Gtk.Hbutton_Box;
with Gtk.Label;
with Gtk.Notebook;
with Gtk.Widget;

with GPS.Kernel.Actions;
with GPS.Kernel.Project;

with Pango.Layout; use Pango.Layout;

package body CodePeer.Reports is

   use Ada.Strings.Unbounded;

   procedure Bump_Clicked (Rec : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Set_Clicked (Rec : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Replace_Clicked (Rec : access Gtk.Widget.Gtk_Widget_Record'Class);

   ------------------
   -- Bump_Clicked --
   ------------------

   procedure Bump_Clicked (Rec : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Dummy : Boolean;
   begin
      Dummy := GPS.Kernel.Actions.Execute_Action
        (Report (Rec).Kernel, CodePeer.Package_Name & " bump");
   end Bump_Clicked;

   ---------------------
   -- Replace_Clicked --
   ---------------------

   procedure Replace_Clicked
     (Rec : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dummy : Boolean;
   begin
      Dummy := GPS.Kernel.Actions.Execute_Action
        (Report (Rec).Kernel, CodePeer.Package_Name & " baseline replace");
   end Replace_Clicked;

   -----------------
   -- Set_Clicked --
   -----------------

   procedure Set_Clicked
     (Rec : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Dummy : Boolean;
   begin
      Dummy := GPS.Kernel.Actions.Execute_Action
        (Report (Rec).Kernel, CodePeer.Package_Name & " baseline set");
   end Set_Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget  : out Report;
      Kernel  : GPS.Kernel.Kernel_Handle;
      Version : Supported_Format_Version;
      Tree    : Code_Analysis.Code_Analysis_Tree) is
   begin
      Widget := new Report_Record;
      Initialize (Widget, Kernel, Version, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : not null access Report_Record'Class;
      Kernel  : GPS.Kernel.Kernel_Handle;
      Version : Supported_Format_Version;
      Tree    : Code_Analysis.Code_Analysis_Tree)
   is
      use type Ada.Calendar.Time;

      Header_Box          : Gtk.Box.Gtk_Vbox;
      Box                 : Gtk.Box.Gtk_Hbox;
      Inspection_Switches : Gtk.Label.Gtk_Label;
      Notebook            : Gtk.Notebook.Gtk_Notebook;
      Button_Box          : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
      Button              : Gtk.Button.Gtk_Button;
      Project_Data        : CodePeer.Project_Data'Class renames
        CodePeer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
            (Tree,
             GPS.Kernel.Project.Get_Root_Project_View
               (Kernel)).Analysis_Data.CodePeer_Data.all);

      procedure Add_Run (Name, Label, Main, Switches : String);
      --  Add information about report generation

      function Time_Span (From : Ada.Calendar.Time) return String;
      --  Calculates time that passed from the report generation

      -------------
      -- Add_Run --
      -------------

      procedure Add_Run (Name, Label, Main, Switches : String)
      is
         Inspection : Gtk.Label.Gtk_Label;
      begin
         Gtk.Box.Gtk_New_Hbox (Box, True);
         Box.Set_Spacing (5);
         Header_Box.Pack_Start (Box, False);

         Gtk.Label.Gtk_New (Inspection, Name);
         Inspection.Set_Alignment (0.0, 0.0);
         Inspection.Set_Label
           ("   " & Label
            & Ada.Characters.Latin_1.LF & "      " & Main
            & (if Switches /= "" then
                   " " & Switches
              else
                 "")
            & Ada.Characters.Latin_1.LF
           );
         Inspection.Set_Tooltip_Text
           (Main
            & (if Switches /= "" then
                   Ada.Characters.Latin_1.LF & Switches
              else
                 ""));
         Box.Pack_End (Inspection);
      end Add_Run;

      ---------------
      -- Time_Span --
      ---------------

      function Time_Span (From : Ada.Calendar.Time) return String
      is
         use Ada.Calendar;
         use Ada.Calendar.Formatting;

         Spent      : constant Duration := Ada.Calendar.Clock - From;
         Pr         : constant String := " /";
         Sf         : constant String := "(s) ago";
         Hour       : Hour_Number;
         Minute     : Minute_Number;
         Second     : Second_Number;
         Sub_Second : Second_Duration;

      begin
         if Spent < 1.0 then
            return Pr & " 1 second";

         elsif Spent > Day_Duration'Last then
            return Pr & Integer'Image
              (Integer (Spent / Day_Duration'Last)) & " day" & Sf;

         else
            Split (Spent, Hour, Minute, Second, Sub_Second);
            if Hour > 0 then
               return Pr & Hour_Number'Image (Hour) & " hour" & Sf;

            elsif Minute > 0 then
               return Pr & Minute_Number'Image (Minute) & " minute" & Sf;

            else
               return Pr & Second_Number'Image (Second) & " second" & Sf;
            end if;
         end if;
      end Time_Span;

   begin
      Gtk.Box.Initialize_Vbox (Self);
      Self.Kernel := Kernel;

      --  Baseline and current review' ids

      Gtk.Box.Gtk_New_Vbox (Header_Box, True);
      Self.Pack_Start (Header_Box, False);

      Gtk.Hbutton_Box.Gtk_New (Button_Box);
      Button_Box.Set_Layout (Gtk.Enums.Buttonbox_Start);
      Header_Box.Add (Button_Box);

      Gtk.Button.Gtk_New (Button);
      Button.Set_Label ("Bump baseline");
      Button.Set_Tooltip_Text ("Bump the Baseline of current timeline to the"
                               & " current Run.");
      Button_Box.Pack_Start (Button, False, False);
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Bump_Clicked'Access, Self);

      Gtk.Button.Gtk_New (Button);
      Button.Set_Label ("Set baseline");
      Button.Set_Tooltip_Text ("Select a Run (SAM file) that will become the"
                               & " new Baseline for the current timeline.");
      Button_Box.Pack_Start (Button, False, False);
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Set_Clicked'Access, Self);

      Gtk.Button.Gtk_New (Button);
      Button.Set_Label ("Replace current run");
      Button.Set_Tooltip_Text ("Select a Run (SAM file) that will replace the"
                               & " current Run.");
      Button_Box.Pack_Start (Button, False, False);
      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Button, Gtk.Button.Signal_Clicked, Replace_Clicked'Access, Self);

      Add_Run
        (Name     => "baseline",
         Label    => (if Is_GNATSAS then "Baseline: "
                      else "Base run #")
         & To_String (Project_Data.Baseline.Inspection)
         & (if Project_Data.Baseline.Timestamp = Unknown_Timestamp then ""
           else " " & Ada.Calendar.Formatting.Image
             (Project_Data.Baseline.Timestamp)
              & Time_Span (Project_Data.Baseline.Timestamp)),
         Main     => To_String (Project_Data.Baseline.Main),
         Switches => To_String (Project_Data.Baseline.Switches));

      Add_Run
        (Name     => "current",
         Label    => (if Is_GNATSAS then "Current run: "
                      else "Current run #")
         & To_String (Project_Data.Current.Inspection)
         & (if Project_Data.Current.Timestamp = Unknown_Timestamp then ""
           else " " & Ada.Calendar.Formatting.Image
             (Project_Data.Current.Timestamp)
              & Time_Span (Project_Data.Current.Timestamp)),
         Main     => To_String (Project_Data.Current.Main),
         Switches => To_String (Project_Data.Current.Switches));

      if Project_Data.Current.Switches.Length > 0 then
         Gtk.Box.Gtk_New_Hbox (Box, True);
         Header_Box.Pack_End (Box, False);

         Gtk.Label.Gtk_New (Inspection_Switches, "switches");
         Inspection_Switches.Set_Alignment (0.0, 1.0);
         Inspection_Switches.Set_Label
           (To_String (Project_Data.Current.Switches));
         Inspection_Switches.Set_Ellipsize (Ellipsize_End);
         Box.Pack_Start (Inspection_Switches, True, True);
      end if;

      --  Notebook

      Gtk.Notebook.Gtk_New (Notebook);
      Self.Pack_Start (Notebook);

      --  Messages report tab

      CodePeer.Messages_Reports.Gtk_New
        (Self.Messages_Report,
         Kernel,
         Version,
         Tree);
      Notebook.Append_Page (Self.Messages_Report);
      Notebook.Set_Tab_Label_Text (Self.Messages_Report, "Messages");

      --  Race condition report tab

      CodePeer.Race_Condition_Reports.Gtk_New
        (Self.Race_Report, Kernel, Tree);
      Notebook.Append_Page (Self.Race_Report);
      Notebook.Set_Tab_Label_Text (Self.Race_Report, "Race conditions");
   end Initialize;

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context
     (Self  : not null access Report_Record'Class;
      Event : Gdk.Event.Gdk_Event)
      return GPS.Kernel.Selection_Context is
   begin
      return CodePeer.Messages_Reports.Build_Context
        (Self.Messages_Report, Event);
   end Build_Context;

   ---------------------
   -- Messages_Report --
   ---------------------

   function Messages_Report
     (Self : not null access Report_Record'Class)
      return CodePeer.Messages_Reports.Messages_Report is
   begin
      return Self.Messages_Report;
   end Messages_Report;

end CodePeer.Reports;
