-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2007                      --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gdk.Event;          use Gdk.Event;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;

with Glib.Object;        use Glib.Object;

with Breakpoints_Editor; use Breakpoints_Editor;
with Debugger;           use Debugger;
with GPS.Intl;           use GPS.Intl;
with GUI_Utils;          use GUI_Utils;
with GVD.Code_Editors;   use GVD.Code_Editors;
with GVD.Process;        use GVD.Process;
with GVD.Types;          use GVD.Types;
with Traces;             use Traces;

package body Breakpoints_Pkg.Callbacks is

   ---------------------------------
   -- On_Breakpoints_Delete_Event --
   ---------------------------------

   function On_Breakpoints_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
   begin
      Hide (Object);
      return True;

   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Breakpoints_Delete_Event;

   ------------------------------------
   -- On_Breakpoints_Key_Press_Event --
   ------------------------------------

   function On_Breakpoints_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Event : constant Gdk_Event := To_Event (Params, 1);
      use type Gdk.Types.Gdk_Key_Type;
   begin
      if Get_Key_Val (Event) = GDK_Delete then
         On_Remove_Clicked (Object);
      end if;
      return False;

   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end On_Breakpoints_Key_Press_Event;

   ----------------------------------
   -- On_Location_Selected_Toggled --
   ----------------------------------

   procedure On_Location_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      Breakpoints : constant Breakpoints_Access :=
        Breakpoints_Access (Object);
   begin
      Set_Sensitive (Breakpoints.File_Combo, True);
      Set_Sensitive (Breakpoints.Line_Spin, True);
      Set_Sensitive (Breakpoints.Address_Combo, False);
      Set_Sensitive (Breakpoints.Subprogram_Combo, False);
      Set_Sensitive (Breakpoints.Regexp_Combo, False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Location_Selected_Toggled;

   -----------------------------------
   -- On_Subprogam_Selected_Toggled --
   -----------------------------------

   procedure On_Subprogam_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      Breakpoints : constant Breakpoints_Access :=
        Breakpoints_Access (Object);
   begin
      Set_Sensitive (Breakpoints.File_Combo, False);
      Set_Sensitive (Breakpoints.Line_Spin, False);
      Set_Sensitive (Breakpoints.Address_Combo, False);
      Set_Sensitive (Breakpoints.Subprogram_Combo, True);
      Set_Sensitive (Breakpoints.Regexp_Combo, False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Subprogam_Selected_Toggled;

   ---------------------------------
   -- On_Address_Selected_Toggled --
   ---------------------------------

   procedure On_Address_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      Breakpoints : constant Breakpoints_Access := Breakpoints_Access (Object);
   begin
      Set_Sensitive (Breakpoints.File_Combo, False);
      Set_Sensitive (Breakpoints.Line_Spin, False);
      Set_Sensitive (Breakpoints.Address_Combo, True);
      Set_Sensitive (Breakpoints.Subprogram_Combo, False);
      Set_Sensitive (Breakpoints.Regexp_Combo, False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Address_Selected_Toggled;

   --------------------------------
   -- On_Regexp_Selected_Toggled --
   --------------------------------

   procedure On_Regexp_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      Breakpoints : constant Breakpoints_Access := Breakpoints_Access (Object);
   begin
      Set_Sensitive (Breakpoints.File_Combo, False);
      Set_Sensitive (Breakpoints.Line_Spin, False);
      Set_Sensitive (Breakpoints.Address_Combo, False);
      Set_Sensitive (Breakpoints.Subprogram_Combo, False);
      Set_Sensitive (Breakpoints.Regexp_Combo, True);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Regexp_Selected_Toggled;

   -----------------------------
   -- On_Add_Location_Clicked --
   -----------------------------

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class) is
   begin
      Set_Location_Breakpoint (Breakpoint_Editor_Access (Object));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Add_Location_Clicked;

   -------------------------------
   -- On_Add_Watchpoint_Clicked --
   -------------------------------

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class) is
   begin
      Set_Watchpoint (Breakpoint_Editor_Access (Object));

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Add_Watchpoint_Clicked;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : constant Breakpoint_Editor_Access :=
                 Breakpoint_Editor_Access (Object);
   begin
      Set_Busy (Editor.Process, True);

      declare
         Exception_Arr : Exception_Array :=
                           List_Exceptions (Editor.Process.Debugger);
      begin
         if Exception_Arr'Length > 0 then
            Set_Sensitive (Editor.Hbox4, True);
            Add_Unique_Combo_Entry
              (Editor.Exception_Name, -"All exceptions");
            Add_Unique_Combo_Entry
              (Editor.Exception_Name, -"All assertions");

            for J in Exception_Arr'Range loop
               Add_Unique_Combo_Entry
                 (Editor.Exception_Name, Exception_Arr (J).Name.all);
            end loop;
         else
            Set_Sensitive (Editor.Hbox4, False);
         end if;

         Free (Exception_Arr);
      end;

      Set_Busy (Editor.Process, False);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Exception_List_Clicked;

   ------------------------------
   -- On_Add_Exception_Clicked --
   ------------------------------

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);

   begin
      Set_Exception_Breakpoint (Editor);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Add_Exception_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
      Selection : constant Integer := Get_Selection_Index (Editor);

   begin
      if Selection /= -1 then
         Remove_Breakpoint
           (Editor.Process.Debugger,
            Editor.Process.Breakpoints (Selection).Num,
            Mode => GVD.Types.Visible);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Remove_Clicked;

   ---------------------
   -- On_View_Clicked --
   ---------------------

   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
      Selection : constant Integer := Get_Selection_Index (Editor);

   begin
      if Selection /= -1 then
         Load_File
           (Editor.Process.Editor_Text,
            Editor.Process.Breakpoints (Selection).File);
         Set_Line
           (Editor.Process.Editor_Text,
            Editor.Process.Breakpoints (Selection).Line,
            GObject (Editor.Process));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_View_Clicked;

   ----------------------------------
   -- On_Advanced_Location_Clicked --
   ----------------------------------

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
      Selection : constant Integer := Get_Selection_Index (Editor);

   begin
      if Selection /= -1 then
         Run_Advanced_Dialog (Editor, Selection);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Advanced_Location_Clicked;

   ----------------------
   -- On_Ok_Bp_Clicked --
   ----------------------

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Hide (Object);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Ok_Bp_Clicked;

end Breakpoints_Pkg.Callbacks;
