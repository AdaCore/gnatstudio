------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2011-2023, AdaCore                     --
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

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;

with Gtk.Label;
with Gtk.Notebook;

with Pango.Layout; use Pango.Layout;

with GPS.Kernel.Project;

package body CodePeer.Reports is

   use Ada.Strings.Unbounded;

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
      use Ada.Strings;

      Header_Box          : Gtk.Box.Gtk_Vbox;
      Box                 : Gtk.Box.Gtk_Hbox;
      Inspection_Switches : Gtk.Label.Gtk_Label;
      Notebook            : Gtk.Notebook.Gtk_Notebook;
      Project_Data        : CodePeer.Project_Data'Class renames
        CodePeer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
            (Tree,
             GPS.Kernel.Project.Get_Root_Project_View
               (Kernel)).Analysis_Data.CodePeer_Data.all);

      procedure Add_Run (Name, Label, Tooltip : String);
      --  Add information about report generation

      function Time_Span (From : Ada.Calendar.Time) return String;
      --  Calculates time that passed from the report generation

      -------------
      -- Add_Run --
      -------------

      procedure Add_Run (Name, Label, Tooltip : String)
      is
         Inspection : Gtk.Label.Gtk_Label;
      begin
         Gtk.Box.Gtk_New_Hbox (Box, True);
         Header_Box.Pack_Start (Box, False);

         Gtk.Label.Gtk_New (Inspection, Name);
         Inspection.Set_Alignment (0.0, 0.0);
         Inspection.Set_Label
           ("   " & Label
            & Ada.Characters.Latin_1.LF & "      " & Tooltip);
         Inspection.Set_Tooltip_Text (Tooltip);
         Box.Pack_Start (Inspection);
      end Add_Run;

      ---------------
      -- Time_Span --
      ---------------

      function Time_Span (From : Ada.Calendar.Time) return String
      is
         use Ada.Calendar.Arithmetic;
         use Ada.Calendar.Formatting;

         Now          : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Days         : Day_Count;
         Seconds      : Duration;
         Leap_Seconds : Leap_Seconds_Count;
         Value        : Integer;
         Pr           : constant String := " /";
         Sf           : constant String := "(s) ago";

      begin
         Ada.Calendar.Arithmetic.Difference
           (Now, From, Days, Seconds, Leap_Seconds);

         if Days > 0 then
            return Pr & Day_Count'Image (Days) & " day" & Sf;

         else
            Value := Integer (Hour (Now) - Hour (From));

            if Value > 0 then
               return Pr & Integer'Image (Value) & " hour" & Sf;

            else
               Value := Integer (Minute (Now) - Minute (From));

               if Value > 0 then
                  return Pr & Integer'Image (Value) & " minute" & Sf;

               else
                  return Pr & Integer'Image
                    (Integer (Second (Now) - Second (From)))
                    & " second" & Sf;
               end if;
            end if;
         end if;
      end Time_Span;

   begin
      Gtk.Box.Initialize_Vbox (Self);

      --  Baseline and current review' ids

      Gtk.Box.Gtk_New_Vbox (Header_Box, True);
      Self.Pack_Start (Header_Box, False);

      Add_Run
        (Name  => "baseline",
         Label => (if Is_GNATSAS then
               "Base run: "
          else
             "Base run #")
         & To_String (Project_Data.Baseline.Inspection)
         & (if Project_Data.Baseline.Timestamp = Unknown_Timestamp then ""
           else " " & Ada.Calendar.Formatting.Image
             (Project_Data.Baseline.Timestamp)
              & Time_Span (Project_Data.Baseline.Timestamp)),
         Tooltip => To_String (Project_Data.Baseline.Main)
         & Ada.Characters.Latin_1.LF
         & To_String (Project_Data.Baseline.Switches));

      Add_Run
        (Name  => "current",
         Label => (if Is_GNATSAS then
            "Current run: "
         else
            "Current run #")
         & To_String (Project_Data.Current.Inspection)
         & (if Project_Data.Current.Timestamp = Unknown_Timestamp then ""
           else " " & Ada.Calendar.Formatting.Image
             (Project_Data.Current.Timestamp)
              & Time_Span (Project_Data.Current.Timestamp)),
         Tooltip => To_String (Project_Data.Current.Main)
         & Ada.Characters.Latin_1.LF
         & To_String (Project_Data.Current.Switches));

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
