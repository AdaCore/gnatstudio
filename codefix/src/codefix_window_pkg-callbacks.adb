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

with Glib;                        use Glib;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Enums;                   use Gtk.Enums;
use Gtk.Enums.String_List;

with Codefix;                     use Codefix;
with Codefix.Errors_Manager;      use Codefix.Errors_Manager;
with Codefix.Formal_Errors;       use Codefix.Formal_Errors;
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
      pragma Unreferenced (Params);

      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
      --  Arg1 : constant Gdk_Event := To_Event (Params, 1);
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
      Valid_Current_Solution (Graphic_Codefix);
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
      Quit (Graphic_Codefix);
   end On_Cancel_Changes_Clicked;

   ------------------------------
   -- On_Apply_Changes_Clicked --
   ------------------------------

   procedure On_Apply_Changes_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Commit
        (Graphic_Codefix.Corrector,
         Graphic_Codefix.Current_Text);
      Quit (Graphic_Codefix);
   end On_Apply_Changes_Clicked;

end Codefix_Window_Pkg.Callbacks;
