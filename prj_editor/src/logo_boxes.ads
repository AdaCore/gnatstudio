-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a window organized like:
--
--    ___________________________________________________
--    |                                                 |
--    | Logo               Title                        |
--    |      ________Error String_______________________|
--    |      |                                          |
--    | Side |                                          |
--    |      |            Contents                      |
--    |      |                                          |
--    |      |                                          |
--    ---------------------------------------------------
--
--  The Action_Area of the dialog is unaffected.

with Gtk.Box;
with Gtk.Dialog;
with Gtk.Label;
with Gtk.Style;
with Gtk.Window;
with Pango.Font;

package Logo_Boxes is

   type Logo_Box_Record is new Gtk.Dialog.Gtk_Dialog_Record with private;
   type Logo_Box is access all Logo_Box_Record'Class;

   procedure Gtk_New
     (Win    : out Logo_Box;
      Title  : String;
      Parent : Gtk.Window.Gtk_Window := null;
      Show_Toc   : Boolean := True;
      Title_Font : Pango.Font.Pango_Font_Description := null);
   procedure Initialize
     (Win : access Logo_Box_Record'Class;
      Title  : String;
      Parent : Gtk.Window.Gtk_Window;
      Show_Toc   : Boolean := True;
      Title_Font : Pango.Font.Pango_Font_Description);
   --  Create a new welcome dialog. Project_Name is the project that should be
   --  suggested by default (empty string for the default project)

   function Get_Side_Box (Win : access Logo_Box_Record) return Gtk.Box.Gtk_Box;
   --  Return the box on the side.
   --  It already contains one child which is the logo

   function Get_Title_Label (Win : access Logo_Box_Record)
      return Gtk.Label.Gtk_Label;
   --  Return the label used to display the title.

   function Get_Contents (Win : access Logo_Box_Record)
      return Gtk.Box.Gtk_Box;
   --  Return the box that contains the actual contents of the window. It is
   --  empty initially

   procedure Display_Message
     (Win      : access Logo_Box_Record;
      Msg      : String;
      As_Error : Boolean := False);
   --  Display a message (or hide it if Msg is the empty string).
   --  If As_Error is True, the message is displayed with a special style

private
   type Logo_Box_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Side_Box    : Gtk.Box.Gtk_Box;
      Title       : Gtk.Label.Gtk_Label;
      Content     : Gtk.Box.Gtk_Box;
      Error_Style : Gtk.Style.Gtk_Style;
      Message     : Gtk.Label.Gtk_Label;
   end record;

end Logo_Boxes;
