-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Ada.Text_IO;                 use Ada.Text_IO;

with System;                      use System;
with Glib;                        use Glib;
with Gdk.Event;                   use Gdk.Event;
with Gdk.Types;                   use Gdk.Types;
with Gtk.Accel_Group;             use Gtk.Accel_Group;
with Gtk.Object;                  use Gtk.Object;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Style;                   use Gtk.Style;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Label;                   use Gtk.Label;
with Gtk.Text;                    use Gtk.Text;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Enums;                   use Gtk.Enums;
use Gtk.Enums.String_List;
with Gtk.Combo;                   use Gtk.Combo;
with Gtk.List;                    use Gtk.List;

with Codefix;                     use Codefix;
with Codefix.Text_Manager;        use Codefix.Text_Manager;
with Codefix.Errors_Manager;      use Codefix.Errors_Manager;
with Codefix.Errors_Parser;       use Codefix.Errors_Parser;
with Codefix.Formal_Errors;       use Codefix.Formal_Errors;
with Codefix.File_Io;             use Codefix.File_Io;
with Codefix.Text_Navigators;
use Codefix.Formal_Errors.Extract_List;
with Codefix.Graphics; use Codefix.Graphics;

package body Codefix_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------------------
   -- On_Codefix_Window_Delete_Event --
   ------------------------------------

   function On_Codefix_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
      Arg1 : constant Gdk_Event := To_Event (Params, 1);
   begin
      Quit (Graphic_Codefix);
      return False;
   end On_Codefix_Window_Delete_Event;

   --------------------------
   -- On_Fix_Entry_Changed --
   --------------------------

   procedure On_Fix_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);

   begin
      if Get_Text (Graphic_Codefix.Fix_Entry) /= "" then
         Set_Page
           (Graphic_Codefix.Choices_Proposed,
            Get_Nth_Solution (Graphic_Codefix) - 1);
      end if;
   end On_Fix_Entry_Changed;

   --------------------------------
   -- On_Skip_Correction_Clicked --
   --------------------------------

   procedure On_Skip_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Load_Next_Error (Graphic_Codefix);
   end On_Skip_Correction_Clicked;

   ----------------------------------
   -- On_Accept_Correction_Clicked --
   ----------------------------------

   procedure On_Accept_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Validate
        (Graphic_Codefix.Corrector,
         Graphic_Codefix.Current_Error,
         Integer (Get_Current_Page (Graphic_Codefix.Choices_Proposed) + 1));

      Load_Next_Error (Graphic_Codefix);
   end On_Accept_Correction_Clicked;

   -------------------------------
   -- On_Cancel_Changes_Clicked --
   -------------------------------

   procedure On_Cancel_Changes_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      null;
   end On_Cancel_Changes_Clicked;

   ------------------------------
   -- On_Apply_Changes_Clicked --
   ------------------------------

   procedure On_Apply_Changes_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
      Success : Boolean;
   begin
      Update
        (Graphic_Codefix.Corrector,
         Success,
         Graphic_Codefix.Current_Text,
         null);

      Quit (Graphic_Codefix);
   end On_Apply_Changes_Clicked;

end Codefix_Window_Pkg.Callbacks;
