-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;               use Glib;
with GVD.Types;          use GVD.Types;
with Basic_Types;        use Basic_Types;
with Gtk.Arguments;      use Gtk.Arguments;
with Debugger;           use Debugger;
with Odd_Intl;           use Odd_Intl;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Combo;          use Gtk.Combo;
with GVD.Utils;          use GVD.Utils;
with GVD.Code_Editors;   use GVD.Code_Editors;
with Gdk.Event;          use Gdk.Event;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;
with Breakpoints_Editor; use Breakpoints_Editor;

package body Breakpoints_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Breakpoints_Delete_Event --
   ---------------------------------

   function On_Breakpoints_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
   begin
      Hide (Object);
      return True;
   end On_Breakpoints_Delete_Event;

   ------------------------------------
   -- On_Breakpoints_Key_Press_Event --
   ------------------------------------

   function On_Breakpoints_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Event : Gdk_Event := To_Event (Params, 1);
      use type Gdk.Types.Gdk_Key_Type;
   begin
      if Get_Key_Val (Event) = GDK_Delete then
         On_Remove_Clicked (Object);
      end if;
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
   end On_Regexp_Selected_Toggled;

   -----------------------------
   -- On_Add_Location_Clicked --
   -----------------------------

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      B : Breakpoint_Identifier;
   begin
      B := Set_Location_Breakpoint (Breakpoint_Editor_Access (Object));
   end On_Add_Location_Clicked;

   --------------------------------
   -- On_Update_Location_Clicked --
   --------------------------------

   procedure On_Update_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
      Selection : constant Integer := Get_Selection_Index (Editor);
      Num       : Breakpoint_Identifier;

   begin
      --  Only update the breakpoint if its type matches the current page.
      if Selection /= -1
        and then Editor.Process.Breakpoints (Selection).Except = null
      then
         Num := Set_Location_Breakpoint (Editor, Selection);

         for B in Editor.Process.Breakpoints'Range loop
            if Editor.Process.Breakpoints (B).Num = Num then
               Select_Row (Editor.Breakpoint_List, Gint (B) - 1, -1);
               exit;
            end if;
         end loop;
      end if;
   end On_Update_Location_Clicked;

   ----------------------------------
   -- On_Advanced_Location_Clicked --
   ----------------------------------

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
   begin
      Toggle_Advanced_Dialog (Editor);
   end On_Advanced_Location_Clicked;

   -------------------------------
   -- On_Add_Watchpoint_Clicked --
   -------------------------------

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Watchpoint_Clicked;

   ----------------------------------
   -- On_Update_Watchpoint_Clicked --
   ----------------------------------

   procedure On_Update_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Update_Watchpoint_Clicked;

   ------------------------------------
   -- On_Advanced_Watchpoint_Clicked --
   ------------------------------------

   procedure On_Advanced_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
   begin
      Toggle_Advanced_Dialog (Editor);
   end On_Advanced_Watchpoint_Clicked;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
   begin
      Set_Busy_Cursor (Get_Window (Editor), True);

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

      Set_Busy_Cursor (Get_Window (Editor), False);
   end On_Load_Exception_List_Clicked;

   ------------------------------
   -- On_Add_Exception_Clicked --
   ------------------------------

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      B      : Breakpoint_Identifier;
      Editor : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);

   begin
      B := Set_Exception_Breakpoint (Editor);
   end On_Add_Exception_Clicked;

   ---------------------------------
   -- On_Update_Exception_Clicked --
   ---------------------------------

   procedure On_Update_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
      Selection : constant Integer := Get_Selection_Index (Editor);
      Num : Breakpoint_Identifier;
   begin
      --  Only update the breakpoint if its type matches the current page.
      if Selection /= -1
        and then Editor.Process.Breakpoints (Selection).Except /= null
      then
         Num := Set_Exception_Breakpoint (Editor, Selection);

         for B in Editor.Process.Breakpoints'Range loop
            if Editor.Process.Breakpoints (B).Num = Num then
               Select_Row (Editor.Breakpoint_List, Gint (B), -1);
               exit;
            end if;
         end loop;
      end if;
   end On_Update_Exception_Clicked;

   -----------------------------------
   -- On_Advanced_Exception_Clicked --
   -----------------------------------

   procedure On_Advanced_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
   begin
      Toggle_Advanced_Dialog (Editor);
   end On_Advanced_Exception_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      use Gint_List;
      Editor    : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Object);
      Selection : Gint;
   begin
      if Get_Selection (Editor.Breakpoint_List) /= Null_List then
         Selection := Get_Data (Get_Selection (Editor.Breakpoint_List));
         Remove_Breakpoint
           (Editor.Process.Debugger,
            Breakpoint_Identifier'Value
              (Get_Text (Editor.Breakpoint_List, Selection, 0)),
            Mode => GVD.Types.Visible);
      end if;
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
            Find_File (Editor.Process.Debugger,
                       Editor.Process.Breakpoints (Selection).File.all));
         Set_Line
           (Editor.Process.Editor_Text,
            Editor.Process.Breakpoints (Selection).Line);
      end if;
   end On_View_Clicked;

   ----------------------
   -- On_Ok_Bp_Clicked --
   ----------------------

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Hide (Object);
   end On_Ok_Bp_Clicked;

end Breakpoints_Pkg.Callbacks;
