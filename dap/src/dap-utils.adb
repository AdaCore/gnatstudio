------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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
with GPS.Default_Styles;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.GtkAda;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;   use GPS.Kernel.Messages.Simple;

package body DAP.Utils is

   Debugger_Messages_Category : constant String := "debugger-current-line";
   Current_Line_Pixbuf        : constant Unbounded_String :=
     To_Unbounded_String ("gps-emblem-debugger-current");

   -------------------------------------
   -- Highlight_Current_File_And_Line --
   -------------------------------------

   procedure Highlight_Current_File_And_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer)
   is
      Msg    : Simple_Message_Access;
      Action : GPS.Editors.Line_Information.Line_Information_Access;
   begin
      Unhighlight_Current_Line (Kernel);

      if File = No_File
        or else Line = 0
      then
         return;
      end if;

      Msg := Create_Simple_Message
        (GPS.Kernel.Get_Messages_Container (Kernel),
         Category                 => Debugger_Messages_Category,
         File                     => File,
         Line                     => Line,
         Column                   => 1,
         Text                     => "",
         Importance               => Unspecified,
         Flags                    => GPS.Kernel.Messages.Sides_Only,
         Allow_Auto_Jump_To_First => False);

      Msg.Set_Highlighting
        (GPS.Default_Styles.Debugger_Current_Line_Style, Highlight_Whole_Line);

      Action := new Line_Information_Record'
        (Text         => Null_Unbounded_String,
         Tooltip_Text => To_Unbounded_String ("Current line in debugger"),
         Image        => Current_Line_Pixbuf,
         others       => <>);
      Msg.Set_Action (Action);

      DAP.Utils.Goto_Location (Kernel, File, Line);
   end Highlight_Current_File_And_Line;

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer)
   is
      Buffer : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File,
           Open_Buffer   => True,
           Focus         => True,
           Unlocked_Only => True);
   begin
      Buffer.Current_View.Cursor_Goto
        (Location   => Buffer.New_Location_At_Line (Line),
         Raise_View => True);

      --  raise the source editor without giving a focus
      declare
         C : constant MDI_Child := GPS.Editors.GtkAda.Get_MDI_Child
           (Buffer.Current_View);
      begin
         if C /= null then
            Raise_Child (C, False);
         end if;
      end;
   end Goto_Location;

   ------------------------------
   -- Unhighlight_Current_Line --
   ------------------------------

   procedure Unhighlight_Current_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      GPS.Kernel.Get_Messages_Container (Kernel).Remove_Category
        (Debugger_Messages_Category, GPS.Kernel.Messages.Sides_Only);
   end Unhighlight_Current_Line;

   -------------
   -- To_File --
   -------------

   function To_File
     (Item : VSS.Strings.Virtual_String'Class)
      return GNATCOLL.VFS.Virtual_File
   is
      use GNATCOLL.VFS;
   begin
      return Create_From_UTF8 (UTF8 (Item));
   end To_File;

end DAP.Utils;
