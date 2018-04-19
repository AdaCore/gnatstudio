------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Gtkada.MDI;                   use Gtkada.MDI;

with Debugger_Pixmaps;             use Debugger_Pixmaps;
with GNATCOLL.Projects;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.GtkAda;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Simple;   use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Style_Manager;     use GPS.Kernel.Style_Manager;
with GVD.Preferences;              use GVD.Preferences;
with GVD.Process;                  use GVD.Process;

package body GVD.Code_Editors is

   Messages_Category_For_Current_Line : constant String :=
     "debugger-current-line";
   Breakpoints_Current_Line_Flags     : constant Message_Flags :=
     (Editor_Line => True, Locations => False, Editor_Side => False);

   Current_Line_Style      : Style_Access;
   --  style used for highlighting the current line.
   --  Doesn't need to be freed, it is handled by the style manager

   -------------------------------
   -- Set_Current_File_And_Line --
   -------------------------------

   procedure Set_Current_File_And_Line
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Process   : access Base_Visual_Debugger'Class := null;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Highlight : Boolean := True;
      Focus     : Boolean := True)
   is
      P   : constant Visual_Debugger := Visual_Debugger (Process);
      Msg : Simple_Message_Access;
   begin
      if File = GNATCOLL.VFS.No_File
        and then P = null
      then
         return;
      end if;

      --  Highlight the current line if the debugger is active

      if Highlight
        and then P /= null
      then
         if P.Current_File /= File
           or else P.Current_Line /= Line
         then
            P.Current_File := File;
            P.Current_Line := Line;

            Unhighlight_Current_Line (Kernel);

            if File /= GNATCOLL.VFS.No_File
              and then Line /= 0
            then
               Msg := Create_Simple_Message
                 (Get_Messages_Container (Kernel),
                  Category                 =>
                    Messages_Category_For_Current_Line,
                  File                     => File,
                  Line                     => Line,
                  Column                   => 1,
                  Text                     => "Current line in debugger",
                  Weight                   => 0,
                  Flags                    => Breakpoints_Current_Line_Flags,
                  Allow_Auto_Jump_To_First => False);

               if Current_Line_Style = null then
                  Current_Line_Style :=
                    Get_Style_Manager (Kernel_Handle (Kernel))
                      .Create_From_Preferences
                        ("debugger current line",
                         Fg_Pref => null,
                         Bg_Pref => Editor_Current_Line_Color);
                  Set_In_Speedbar (Current_Line_Style, True);
               end if;

               Msg.Set_Highlighting (Current_Line_Style, Highlight_Whole_Line);
               Msg.Set_Action
                 (new Line_Information_Record'
                    (Text         => Null_Unbounded_String,
                     Tooltip_Text =>
                       To_Unbounded_String ("Current line in debugger"),
                     Image        => Current_Line_Pixbuf,
                     others       => <>));
            end if;
         end if;

         if P.Debugger.Is_Started then
            Debugger_Location_Changed_Hook.Run (Kernel, P);
         end if;
      end if;

      --  Jump to current location

      if File /= GNATCOLL.VFS.No_File
        and then Line /= 0
      then
         declare
            Buffer : constant Editor_Buffer'Class :=
              Kernel.Get_Buffer_Factory.Get
                (File, Open_Buffer => True, Focus => Focus);
         begin
            Buffer.Current_View.Cursor_Goto
              (Location   => Buffer.New_Location_At_Line (Line),
               Raise_View => Focus);

            if not Focus then
               --  raise the source editor without giving a focus
               declare
                  C : constant MDI_Child := GPS.Editors.GtkAda.Get_MDI_Child
                    (Buffer.Current_View);
               begin
                  if C /= null then
                     Raise_Child (C, False);
                  end if;
               end;
            end if;
         end;
      end if;
   end Set_Current_File_And_Line;

   -----------------------
   -- Goto_Current_Line --
   -----------------------

   procedure Goto_Current_Line
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Process : not null access Base_Visual_Debugger'Class)
   is
      P    : constant Visual_Debugger := Visual_Debugger (Process);
      Buffer : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get (P.Current_File);
   begin
      Buffer.Current_View.Cursor_Goto
        (Location   => Buffer.New_Location_At_Line (P.Current_Line),
         Raise_View => True);
   end Goto_Current_Line;

   ------------------------------
   -- Unhighlight_Current_Line --
   ------------------------------

   procedure Unhighlight_Current_Line
     (Kernel  : not null access Kernel_Handle_Record'Class) is
   begin
      Get_Messages_Container (Kernel).Remove_Category
        (Messages_Category_For_Current_Line,
         Breakpoints_Current_Line_Flags);
   end Unhighlight_Current_Line;

end GVD.Code_Editors;
