with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Switches_Editor_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------
   -- Refresh_All_Switches --
   --------------------------

   procedure Refresh_All_Switches
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Address := To_Address (Params, 1);
      Arg2 : Guint := To_Guint (Params, 2);
   begin
      null;
   end Refresh_All_Switches;

   ---------------------------
   -- Refresh_Make_Switches --
   ---------------------------

   procedure Refresh_Make_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end Refresh_Make_Switches;

   ------------------------------------
   -- On_Make_Switches_Entry_Changed --
   ------------------------------------

   procedure On_Make_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Make_Switches_Entry_Changed;

   ---------------------------
   -- Refresh_Comp_Switches --
   ---------------------------

   procedure Refresh_Comp_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end Refresh_Comp_Switches;

   ----------------------------------------
   -- On_Compiler_Switches_Entry_Changed --
   ----------------------------------------

   procedure On_Compiler_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Compiler_Switches_Entry_Changed;

   --------------------------------------
   -- On_Binder_Switches_Entry_Changed --
   --------------------------------------

   procedure On_Binder_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Binder_Switches_Entry_Changed;

   ---------------------------
   -- Refresh_Bind_Switches --
   ---------------------------

   procedure Refresh_Bind_Switches
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end Refresh_Bind_Switches;

   --------------------------------------
   -- On_Linker_Switches_Entry_Changed --
   --------------------------------------

   procedure On_Linker_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Linker_Switches_Entry_Changed;

end Switches_Editor_Pkg.Callbacks;
