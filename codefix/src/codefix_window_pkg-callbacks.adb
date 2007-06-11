-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2007                    --
--                                AdaCore                            --
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

with Glib;                  use Glib;

with Codefix;               use Codefix;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;
with Codefix.Graphics;      use Codefix.Graphics;

with Traces;                use Traces;

package body Codefix_Window_Pkg.Callbacks is

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

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Fix_Entry_Changed;

   ---------------------
   -- On_Prev_Clicked --
   ---------------------

   procedure On_Prev_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Load_Previous_Error (Graphic_Codefix);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Prev_Clicked;

   ---------------------
   -- On_Next_Clicked --
   ---------------------

   procedure On_Next_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Load_Next_Error (Graphic_Codefix);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Next_Clicked;

   ----------------------
   -- On_Apply_Clicked --
   ----------------------

   procedure On_Apply_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Valid_Current_Solution (Graphic_Codefix);
      Load_Next_Error (Graphic_Codefix);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Apply_Clicked;

   ---------------------
   -- On_Undo_Clicked --
   ---------------------

   procedure On_Undo_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Undo_Last_Fix (Graphic_Codefix);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Undo_Clicked;

   ------------------------
   -- On_Refresh_Clicked --
   ------------------------

   procedure On_Refresh_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
      Success_Load    : Boolean;
   begin
      Load_Error (Graphic_Codefix, Success_Load);
      --  ??? What may I do when the fix is no longer pertinent ?
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Refresh_Clicked;

end Codefix_Window_Pkg.Callbacks;
