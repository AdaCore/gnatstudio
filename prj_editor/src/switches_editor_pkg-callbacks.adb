-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  with System; use System;
--  with Glib; use Glib;
--  with Gdk.Event; use Gdk.Event;
--  with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Switches_Editors; use Switches_Editors;

package body Switches_Editor_Pkg.Callbacks is

--     use Gtk.Arguments;

   ---------------------------
   -- Refresh_Make_Switches --
   ---------------------------

   procedure Refresh_Make_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Cmdline (Switches_Edit (Object), Gnatmake);
   end Refresh_Make_Switches;

   ------------------------------------
   -- On_Make_Switches_Entry_Changed --
   ------------------------------------

   procedure On_Make_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Gui_From_Cmdline (Switches_Edit (Object), Gnatmake);
   end On_Make_Switches_Entry_Changed;

   --------------------------
   -- Refresh_Ada_Switches --
   --------------------------

   procedure Refresh_Ada_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Cmdline (Switches_Edit (Object), Ada_Compiler);
   end Refresh_Ada_Switches;

   -----------------------------------
   -- On_Ada_Switches_Entry_Changed --
   -----------------------------------

   procedure On_Ada_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Gui_From_Cmdline (Switches_Edit (Object), Ada_Compiler);
   end On_Ada_Switches_Entry_Changed;

   ------------------------
   -- Refresh_C_Switches --
   ------------------------

   procedure Refresh_C_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Cmdline (Switches_Edit (Object), C_Compiler);
   end Refresh_C_Switches;

   ---------------------------------
   -- On_C_Switches_Entry_Changed --
   ---------------------------------

   procedure On_C_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Gui_From_Cmdline (Switches_Edit (Object), C_Compiler);
   end On_C_Switches_Entry_Changed;

   --------------------------
   -- Refresh_Cpp_Switches --
   --------------------------

   procedure Refresh_Cpp_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Cmdline (Switches_Edit (Object), Cpp_Compiler);
   end Refresh_Cpp_Switches;

   -----------------------------------
   -- On_Cpp_Switches_Entry_Changed --
   -----------------------------------

   procedure On_Cpp_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Gui_From_Cmdline (Switches_Edit (Object), Cpp_Compiler);
   end On_Cpp_Switches_Entry_Changed;

   --------------------------------------
   -- On_Binder_Switches_Entry_Changed --
   --------------------------------------

   procedure On_Binder_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Gui_From_Cmdline (Switches_Edit (Object), Binder);
   end On_Binder_Switches_Entry_Changed;

   ---------------------------
   -- Refresh_Bind_Switches --
   ---------------------------

   procedure Refresh_Bind_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Cmdline (Switches_Edit (Object), Binder);
   end Refresh_Bind_Switches;

   --------------------------------------
   -- On_Linker_Switches_Entry_Changed --
   --------------------------------------

   procedure On_Linker_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Gui_From_Cmdline (Switches_Edit (Object), Linker);
   end On_Linker_Switches_Entry_Changed;

   -----------------------------
   -- Refresh_Linker_Switches --
   -----------------------------

   procedure Refresh_Linker_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Update_Cmdline (Switches_Edit (Object), Linker);
   end Refresh_Linker_Switches;

end Switches_Editor_Pkg.Callbacks;
