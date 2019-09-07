------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

--  This package handles the editor status bar

with Basic_Types;       use Basic_Types;
with GPS.Editors;       use GPS.Editors;
with Gtk.Box;           use Gtk.Box;
with Gtk.Event_Box;     use Gtk.Event_Box;
with Gtk.Label;
with Gtk.Tool_Button;   use Gtk.Tool_Button;
with Gtk.Toolbar;       use Gtk.Toolbar;
with Gtk.Widget;        use Gtk.Widget;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with Src_Editor_View;   use Src_Editor_View;

package Src_Editor_Status_Bar is

   type Source_Editor_Status_Bar_Record is new Gtk_Box_Record with private;
   type Source_Editor_Status_Bar is access all
     Source_Editor_Status_Bar_Record'Class;

   procedure Gtk_New
     (Bar    : out Source_Editor_Status_Bar;
      Box    : Gtk_Event_Box;
      View   : Source_View;
      Buffer : Source_Buffer);
   --  Initialize a status bar
   --  Box is in fact the containing Source_Editor_Box

   procedure Update_Status
     (Bar    : not null access Source_Editor_Status_Bar_Record'Class);
   --  Update the status icon showing the state Saved/Unsaved for the editor,
   --  as well as the writable/read-only status

   procedure Update_Subprogram_Name
     (Bar : not null access Source_Editor_Status_Bar_Record'Class);
   --  Update the suprogram name label in Bar

private

   type Frames_Array is array (Natural range <>) of Gtk_Tool_Button;
   type Frames_Array_Access is access Frames_Array;

   type Source_Editor_Status_Bar_Record is new Gtk_Box_Record with record
      View          : Src_Editor_View.Source_View;
      Buffer        : Src_Editor_Buffer.Source_Buffer;
      Box           : Gtk_Event_Box;   --  Source_Editor_Box

      Buffer_Info_Frames   : Frames_Array_Access := null;
      --  Extra information displayed in the status bar, like the VCS-version1
      --  status.

      Function_Label       : Gtk.Label.Gtk_Label;
      Read_Only            : Gtk_Tool_Button;
      Cursor_Loc           : Gtk_Tool_Button;
      VCS_Status           : Gtk_Tool_Button;
      Modified_Status      : Gtk_Tool_Button;
      Toolbar              : Gtk_Toolbar;

      Current_Line         : Editable_Line_Type := 1;
      --  Cache for the current line

   end record;

end Src_Editor_Status_Bar;
