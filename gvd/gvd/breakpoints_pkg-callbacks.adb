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

with Glib;                    use Glib;
with Advanced_Breakpoint_Pkg; use Advanced_Breakpoint_Pkg;
with GVD.Types;               use GVD.Types;
with Gtkada.Types;            use Gtkada.Types;
with Gtk.Arguments;           use Gtk.Arguments;
with Ada.Text_IO;             use Ada.Text_IO;
with GVD.Process;             use GVD.Process;
with Debugger;                use Debugger;
with Odd_Intl;                use Odd_Intl;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Combo;               use Gtk.Combo;
with Gtkada.Handlers;         use Gtkada.Handlers;
with GVD.Utils;               use GVD.Utils;

package body Breakpoints_Pkg.Callbacks is

   Enable_Column : constant := 1;
   --  Column in the clist display that contains the enable/disable state
   --  of the breakpoints.

   procedure Breakpoint_Row_Selected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was selected.

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

   ----------------------------------
   -- On_Location_Selected_Toggled --
   ----------------------------------

   procedure On_Location_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      Breakpoints : constant Breakpoints_Access := Breakpoints_Access (Object);
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
      Breakpoints : constant Breakpoints_Access := Breakpoints_Access (Object);
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
      Editor    : constant Breakpoints_Access := Breakpoints_Access (Object);
      Temporary : Boolean;

   begin
      Temporary := Get_Active (Editor.Temporary_Location);

      if Get_Active (Editor.Location_Selected) then
         Break_Source
           (Editor.Process.Debugger,
            File      => Get_Chars (Get_Entry (Editor.File_Combo)),
            Line      => Integer (Get_Value_As_Int (Editor.Line_Spin)),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
         Add_Unique_Combo_Entry
           (Editor.File_Combo, Get_Chars (Get_Entry (Editor.File_Combo)));

      elsif Get_Active (Editor.Subprogram_Selected) then
         Break_Subprogram
           (Editor.Process.Debugger,
            Name      => Get_Chars (Get_Entry (Editor.Subprogram_Combo)),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
         Add_Unique_Combo_Entry
           (Editor.Subprogram_Combo,
            Get_Chars (Get_Entry (Editor.Subprogram_Combo)));

      elsif Get_Active (Editor.Address_Selected) then
         Break_Address
           (Editor.Process.Debugger,
            Address   => Get_Chars (Get_Entry (Editor.Address_Combo)),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
         Add_Unique_Combo_Entry
           (Editor.Address_Combo,
            Get_Chars (Get_Entry (Editor.Address_Combo)));

      else
         Break_Regexp
           (Editor.Process.Debugger,
            Regexp    => Get_Chars (Get_Entry (Editor.Regexp_Combo)),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
         Add_Unique_Combo_Entry
           (Editor.Regexp_Combo,
            Get_Chars (Get_Entry (Editor.Regexp_Combo)));
      end if;
   end On_Add_Location_Clicked;

   ----------------------------------
   -- On_Advanced_Location_Clicked --
   ----------------------------------

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      pragma Warnings (Off, Object);
      Descriptor : Advanced_Breakpoint_Descriptor;
   begin
      Advanced_Breakpoint_Editor (Descriptor);
      Free (Descriptor);
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

   ------------------------------------
   -- On_Advanced_Watchpoint_Clicked --
   ------------------------------------

   procedure On_Advanced_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      pragma Warnings (Off, Object);
      Descriptor : Advanced_Breakpoint_Descriptor;
   begin
      Advanced_Breakpoint_Editor (Descriptor);
      Free (Descriptor);
   end On_Advanced_Watchpoint_Clicked;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Breakpoints_Access := Breakpoints_Access (Object);
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
      Editor    : constant Breakpoints_Access := Breakpoints_Access (Object);
      Temporary : Boolean;
      Name      : constant String :=
        Get_Chars (Get_Entry (Editor.Exception_Name));

   begin
      Temporary := Get_Active (Editor.Temporary_Exception);

      if Name = -"All exceptions" then
         Break_Exception
           (Editor.Process.Debugger,
            Name      => "",
            Unhandled => Get_Active (Editor.Stop_Not_Handled_Exception),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);

      elsif Name = -"All assertions" then
         Break_Subprogram
           (Editor.Process.Debugger,
            Name      => "assert",
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);

      else
         Break_Exception
           (Editor.Process.Debugger,
            Name      => Name,
            Unhandled => Get_Active (Editor.Stop_Not_Handled_Exception),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;
   end On_Add_Exception_Clicked;

   -----------------------------------
   -- On_Advanced_Exception_Clicked --
   -----------------------------------

   procedure On_Advanced_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      pragma Warnings (Off, Object);
      Descriptor : Advanced_Breakpoint_Descriptor;
   begin
      Advanced_Breakpoint_Editor (Descriptor);
      Free (Descriptor);
   end On_Advanced_Exception_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      use Gint_List;
      Editor    : constant Breakpoints_Access := Breakpoints_Access (Object);
      Selection : Gint;
   begin
      if Get_Selection (Editor.Clist1) /= Null_List then
         Selection := Get_Data (Get_Selection (Editor.Clist1));
         Remove_Breakpoint
           (Editor.Process.Debugger,
            Integer'Value (Get_Text (Editor.Clist1, Selection, 0)),
            Mode => GVD.Types.Visible);
      end if;
   end On_Remove_Clicked;

   ---------------------
   -- On_View_Clicked --
   ---------------------

   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
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

   -----------------------------
   -- Breakpoint_Row_Selected --
   -----------------------------

   procedure Breakpoint_Row_Selected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Editor  : constant Breakpoints_Access := Breakpoints_Access (Widget);
      Row     : constant Gint := To_Gint (Args, 1);
      Column  : constant Gint := To_Gint (Args, 2);
      Br      : Breakpoint_Data;
      Br_Num  : Natural;

   begin
      --  Click in the second column => change the enable/disable state

      if Column = Enable_Column then

         --  For efficiency, no need to reparse the list of breakpoints, since
         --  only the state of one of them as changed and we know all about
         --  it.

         Freeze (Editor.Clist1);

         if Toggle_Breakpoint_State
           (Editor.Process,
            Breakpoint_Num => Integer'Value (Get_Text (Editor.Clist1, Row, 0)))
         then
            Set_Pixmap
              (Editor.Clist1, Row, Enable_Column,
               Editor.Enabled_Pixmap, Editor.Enabled_Mask);
         else
            Set_Text (Editor.Clist1, Row, Enable_Column, "");
         end if;

         --  Make sure the row is selected

         Emit_Stop_By_Name (Editor.Clist1, "select_row");
         Unselect_Row (Editor.Clist1, Row, -1);

         Thaw (Editor.Clist1);

      --  Otherwise, display the information in the correct tab

      else
         --  Get the information on the breakpoint

         Br_Num := Integer'Value (Get_Text (Editor.Clist1, Row, 0));

         for B in Editor.Process.Breakpoints'Range loop
            if Editor.Process.Breakpoints (B).Num = Br_Num then
               Br := Editor.Process.Breakpoints (B);
               exit;
            end if;
         end loop;

         --  Fill the information

         if Br.Except /= null then
            Set_Page (Editor.Notebook1, 2);
            Set_Active (Editor.Stop_Always_Exception, True);

            if Br.Except.all = "all" then
               Set_Text (Get_Entry (Editor.Exception_Name), -"All exceptions");
            elsif Br.Except.all = "unhandled" then
               Set_Text (Get_Entry (Editor.Exception_Name), -"All exceptions");
               Set_Active (Editor.Stop_Not_Handled_Exception, True);
            else
               Set_Text (Get_Entry (Editor.Exception_Name), Br.Except.all);
            end if;

            Set_Active (Editor.Temporary_Exception, Br.Disposition /= Keep);

         else
            Set_Page (Editor.Notebook1, 0);

            if Br.File /= null then
               Set_Active (Editor.Location_Selected, True);
               Set_Text (Get_Entry (Editor.File_Combo), Br.File.all);
               Set_Value (Editor.Line_Spin, Gfloat (Br.Line));
            else
               Set_Active (Editor.Address_Selected, True);
               Set_Text (Get_Entry (Editor.Address_Combo), Br.Address.all);
            end if;
         end if;
      end if;
   end Breakpoint_Row_Selected;

   ----------------------------
   -- Update_Breakpoint_List --
   ----------------------------

   procedure Update_Breakpoint_List
     (Editor : access Breakpoints_Pkg.Breakpoints_Record'Class)
   is
      Row  : Gint;
      Br   : Breakpoint_Data;
      Size : Gint;
   begin
      Clear (Editor.Clist1);

      if Editor.Process.Breakpoints = null
        or else Editor.Process.Breakpoints'Length <= 0
      then
         --  Put at least one empty line, so that the columns are resized
         --  correctly.
         Set_Text (Editor.Clist1, 0, 0, Natural'Image (1));
         return;
      end if;

      Freeze (Editor.Clist1);

      for B in Editor.Process.Breakpoints'Range loop
         Br := Editor.Process.Breakpoints (B);

         --  Create a new line
         --  WARNING: We must have at least as many elements as there are
         --  columns in the clist.
         Row := Append
           (Editor.Clist1, "" + "" + "" + "" + "" + "" + "" + "" + "");

         Set_Text (Editor.Clist1, Row, 0, Natural'Image (Br.Num));

         if Br.Enabled then
            Set_Pixmap (Editor.Clist1, Row, Enable_Column,
                        Editor.Enabled_Pixmap, Editor.Enabled_Mask);
         end if;

         case Br.The_Type is
            when Breakpoint => Set_Text (Editor.Clist1, Row, 2, -"break");
            when Watchpoint => Set_Text (Editor.Clist1, Row, 2, -"watch");
         end case;

         case Br.Disposition is
            when Delete  => Set_Text (Editor.Clist1, Row, 3, -"delete");
            when Disable => Set_Text (Editor.Clist1, Row, 3, -"disable");
            when Keep    => Set_Text (Editor.Clist1, Row, 3, -"keep");
         end case;

         if Br.Expression /= null then
            Set_Text
              (Editor.Clist1, Row, 4, Br.Expression.all);
         end if;

         if Br.File /= null then
            Set_Text (Editor.Clist1, Row, 4, Br.File.all);
            Set_Text (Editor.Clist1, Row, 5, Integer'Image (Br.Line));
         end if;

         if Br.Except /= null then
            Set_Text (Editor.Clist1, Row, 6, Br.Except.all);
         end if;

         if Br.Subprogram /= null then
            Set_Text
              (Editor.Clist1, Row, 7, Br.Subprogram.all);
         end if;
      end loop;

      Set_Column_Min_Width (Editor.Clist1, 1, 20);
      Set_Column_Justification (Editor.Clist1, 1, Justify_Center);
      Size := Columns_Autosize (Editor.Clist1);

      Widget_Callback.Object_Connect
        (Editor.Clist1, "select_row",
         Breakpoint_Row_Selected'Access,
         Slot_Object => Editor);
      --  Why is the following code commented out ???
      --  Widget_Callback.Object_Connect
      --    (Editor.Clist1, "unselect_row",
      --     Breakpoint_Row_Selected'Access,
      --     Slot_Object => Editor);

      Thaw (Editor.Clist1);
   end Update_Breakpoint_List;

end Breakpoints_Pkg.Callbacks;
