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

with Ada.Exceptions;              use Ada.Exceptions;

with Glib;                        use Glib;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Enums;                   use Gtk.Enums;
use Gtk.Enums.String_List;

with Codefix;                     use Codefix;
with Codefix.Errors_Manager;      use Codefix.Errors_Manager;
with Codefix.Formal_Errors;       use Codefix.Formal_Errors;
use Codefix.Formal_Errors.Command_List;
with Codefix.Graphics; use Codefix.Graphics;

with Traces;                      use Traces;

package body Codefix_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   Me : constant Debug_Handle := Create ("Codefix_Window_Pkg.Callbacks");

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
   begin
      Quit (Graphic_Codefix);
      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Skip_Correction_Clicked;

   ----------------------------------
   -- On_Accept_Correction_Clicked --
   ----------------------------------

   procedure On_Accept_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Valid_Current_Solution (Graphic_Codefix);
      Load_Next_Error (Graphic_Codefix);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Accept_Correction_Clicked;

   -------------------------------------
   -- On_Skip_All_Corrections_Clicked --
   -------------------------------------

   procedure On_Skip_All_Corrections_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Set_Error_State
        (Graphic_Codefix.Automatic_Skip,
         Get_Category (Graphic_Codefix.Current_Error),
         Enabled);
      On_Skip_Correction_Clicked (Object);
   end On_Skip_All_Corrections_Clicked;

   ---------------------------------------
   -- On_Accept_All_Corrections_Clicked --
   ---------------------------------------

   procedure On_Accept_All_Corrections_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : constant Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Set_Error_State
        (Graphic_Codefix.Automatic_Fix,
         Get_Category (Graphic_Codefix.Current_Error),
         Enabled);
      On_Accept_Correction_Clicked (Object);
   end On_Accept_All_Corrections_Clicked;

   ---------------------
   -- On_Undo_Clicked --
   ---------------------

   procedure On_Undo_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      null;
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Refresh_Clicked;

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

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Cancel_Changes_Clicked;

end Codefix_Window_Pkg.Callbacks;
