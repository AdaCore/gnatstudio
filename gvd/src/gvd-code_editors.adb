------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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
with GVD.Process;                  use GVD.Process;
with GPS.Default_Styles;           use GPS.Default_Styles;

package body GVD.Code_Editors is

   Debugger_Messages_Category  : constant String :=
     "debugger-current-line";
   Debugger_Current_Line_Flags : constant Message_Flags :=
     (Editor_Line => True, Locations => False, Editor_Side => False);

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
      P      : constant Visual_Debugger := Visual_Debugger (Process);
      Msg    : Simple_Message_Access;
      Notify : Boolean := False;

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
                    Debugger_Messages_Category,
                  File                     => File,
                  Line                     => Line,
                  Column                   => 1,
                  Text                     => "Current line in debugger",
                  Importance               => Unspecified,
                  Flags                    => Debugger_Current_Line_Flags,
                  Allow_Auto_Jump_To_First => False);

               Msg.Set_Highlighting
                 (Debugger_Current_Line_Style, Highlight_Whole_Line);

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
            Notify := True;
            --  Postpone notification till complete of operations on source
            --  code editor, some plugins (Qgen for instance) may use this
            --  notification to open own views.
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

      --  Run "debugger location changed" hook.

      if Notify then
         Debugger_Location_Changed_Hook.Run (Kernel, P);
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
        (Debugger_Messages_Category,
         Debugger_Current_Line_Flags);
   end Unhighlight_Current_Line;

end GVD.Code_Editors;
