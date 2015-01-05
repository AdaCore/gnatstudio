------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2015, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with Gtk.Box;       use Gtk.Box;
with Gtk.Event_Box; use Gtk.Event_Box;

with Src_Editor_Buffer; use Src_Editor_Buffer;
with Src_Editor_View; use Src_Editor_View;
with Gtk.Label;
with Gtk.Image;
with Gtk.Handlers;
with Gtk.Widget;

package Src_Editor_Status_Bar is

   type Source_Editor_Status_Bar_Record is
     new Gtk.Event_Box.Gtk_Event_Box_Record with private;
   type Source_Editor_Status_Bar is access all
     Source_Editor_Status_Bar_Record'Class;

   procedure Gtk_New
     (Bar    : out Source_Editor_Status_Bar;
      Box    : Gtk_Event_Box;
      View   : Source_View;
      Buffer : Source_Buffer);
   procedure Initialize
     (Bar    : not null access Source_Editor_Status_Bar_Record'Class;
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

   type Frame_Separator is record
      Label     : Gtk.Widget.Gtk_Widget;  --  label or image
   end record;

   type Frames_Array is array (Natural range <>) of Frame_Separator;
   type Frames_Array_Access is access Frames_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Frames_Array, Frames_Array_Access);

   type Source_Editor_Status_Bar_Record is
     new Gtk.Event_Box.Gtk_Event_Box_Record
   with record
      HBox          : Gtk_Hbox;
      View          : Src_Editor_View.Source_View;
      Buffer        : Src_Editor_Buffer.Source_Buffer;

      Box    : Gtk_Event_Box;

      Function_Label       : Gtk.Label.Gtk_Label;
      Read_Only_Label      : Gtk.Image.Gtk_Image;
      Modified_Label       : Gtk.Image.Gtk_Image;
      Cursor_Loc_Label     : Gtk.Label.Gtk_Label;

      Current_Line         : Editable_Line_Type;
      --  Cache for the current line

      Cursor_Handler       : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "cursor_position_changed" in
      --  the Source_Buffer.

      Buffer_Info_Handler  : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "buffer_information_changed" from the
      --  source buffer.

      Buffer_Info_Frames   : Frames_Array_Access := null;

   end record;

end Src_Editor_Status_Bar;
