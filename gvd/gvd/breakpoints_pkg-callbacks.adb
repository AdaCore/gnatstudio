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
with Basic_Types;             use Basic_Types;
with Gtkada.Types;            use Gtkada.Types;
with Gtk.Arguments;           use Gtk.Arguments;
with GVD.Process;             use GVD.Process;
with Debugger;                use Debugger;
with Odd_Intl;                use Odd_Intl;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Combo;               use Gtk.Combo;
with Gtkada.Handlers;         use Gtkada.Handlers;
with GVD.Utils;               use GVD.Utils;
with GVD.Code_Editors;        use GVD.Code_Editors;
with Gtk.Text;                use Gtk.Text;
with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;

package body Breakpoints_Pkg.Callbacks is

   Enable_Column : constant := 1;
   --  Column in the clist display that contains the enable/disable state
   --  of the breakpoints.

   procedure Breakpoint_Row_Selected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was selected.

   procedure Breakpoint_Row_Unselected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was unselected.

   function Get_Selection_Index (Editor : Breakpoints_Access) return Integer;
   --  Return the index of the currently selected line in the breakpoint
   --  editor. The index is the element in Editor.Process.Breakpoints, or
   --  -1 if there is no selection

   procedure Toggle_Advanced_Dialog (Editor : Breakpoints_Access);
   --  Toggle the visibility of the advanced breakpoints dialog

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data);
   --  Fills the contents of the Advanced dialog with the values contained in
   --  Br.

   function Set_Location_Breakpoint
     (Editor : Breakpoints_Access; Current : Integer := -1)
      return Breakpoint_Identifier;
   --  Set the breakpoint that is currently described in the location page.
   --  If Current is not -1, then the breakpoint currently displayed at line
   --  Current is updated (first removed if some of the information has
   --  changed).

   function Set_Exception_Breakpoint
     (Editor : Breakpoints_Access; Current : Integer := -1)
      return Breakpoint_Identifier;
   --  Set the breakpoint that is currently described in the exception page.

   procedure Set_Advanced
     (Editor : Breakpoints_Access;
      Adv    : Advanced_Breakpoint_Access;
      Bpt    : Breakpoint_Identifier);
   --  Set the advanced options for the breakpoint Bpt, based on the contents
   --  of the advanced breakpoint editor Adv.

   use Gtk.Arguments;

   -------------------------
   -- Get_Selection_Index --
   -------------------------

   function Get_Selection_Index (Editor : Breakpoints_Access) return Integer
   is
      use Gint_List;
      Selection : constant Gint_List.Glist :=
        Get_Selection (Editor.Breakpoint_List);
      Br_Num  : Breakpoint_Identifier;
   begin
      if Selection /= Null_List then
         Br_Num := Breakpoint_Identifier'Value
           (Get_Text (Editor.Breakpoint_List,
                      Get_Data (First (Selection)), 0));

         for B in Editor.Process.Breakpoints'Range loop
            if Editor.Process.Breakpoints (B).Num = Br_Num then
               return B;
            end if;
         end loop;
      end if;
      return -1;
   end Get_Selection_Index;

   ----------------------------
   -- Toggle_Advanced_Dialog --
   ----------------------------

   procedure Toggle_Advanced_Dialog (Editor : Breakpoints_Access) is
   begin
      --  Create all three dialogs
      if Editor.Advanced_Breakpoints_Location = null then
         --  Location
         Gtk_New (Editor.Advanced_Breakpoints_Location);
         Ref (Editor.Advanced_Breakpoints_Location.Main_Box);
         Unparent (Editor.Advanced_Breakpoints_Location.Main_Box);
         Pack_Start
           (Editor.Hbox2, Editor.Advanced_Breakpoints_Location.Main_Box);
         Unref (Editor.Advanced_Breakpoints_Location.Main_Box);
         Set_Sensitive
           (Editor.Advanced_Breakpoints_Location.Record_Button, False);
         Set_Sensitive
           (Editor.Advanced_Breakpoints_Location.End_Button, False);

         --  Watchpoints
         Gtk_New (Editor.Advanced_Breakpoints_Watchpoints);
         Ref (Editor.Advanced_Breakpoints_Watchpoints.Main_Box);
         Unparent (Editor.Advanced_Breakpoints_Watchpoints.Main_Box);
         Pack_Start
           (Editor.Hbox3, Editor.Advanced_Breakpoints_Watchpoints.Main_Box);
         Unref (Editor.Advanced_Breakpoints_Watchpoints.Main_Box);
         Set_Sensitive
           (Editor.Advanced_Breakpoints_Watchpoints.Record_Button, False);
         Set_Sensitive
           (Editor.Advanced_Breakpoints_Watchpoints.End_Button, False);

         --  Exceptions
         Gtk_New (Editor.Advanced_Breakpoints_Exceptions);
         Ref (Editor.Advanced_Breakpoints_Exceptions.Main_Box);
         Unparent (Editor.Advanced_Breakpoints_Exceptions.Main_Box);
         Pack_Start
           (Editor.Hbox4, Editor.Advanced_Breakpoints_Exceptions.Main_Box);
         Unref (Editor.Advanced_Breakpoints_Exceptions.Main_Box);
         Set_Sensitive
           (Editor.Advanced_Breakpoints_Exceptions.Record_Button, False);
         Set_Sensitive
           (Editor.Advanced_Breakpoints_Exceptions.End_Button, False);
      end if;

      if Visible_Is_Set (Editor.Advanced_Breakpoints_Location.Main_Box) then
         Hide_All (Editor.Advanced_Breakpoints_Location.Main_Box);
         Hide_All (Editor.Advanced_Breakpoints_Watchpoints.Main_Box);
         Hide_All (Editor.Advanced_Breakpoints_Exceptions.Main_Box);
      else
         Show_All (Editor.Advanced_Breakpoints_Location.Main_Box);
         Show_All (Editor.Advanced_Breakpoints_Watchpoints.Main_Box);
         Show_All (Editor.Advanced_Breakpoints_Exceptions.Main_Box);
      end if;
   end Toggle_Advanced_Dialog;

   --------------------------
   -- Fill_Advanced_Dialog --
   --------------------------

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data)
   is
      Position : Gint := 0;
   begin
      if Advanced /= null then
         if Br.Condition /= null then
            Set_Text (Get_Entry (Advanced.Condition_Combo), Br.Condition.all);
            Add_Unique_Combo_Entry
              (Advanced.Condition_Combo, Br.Condition.all);
         else
            Set_Text (Get_Entry (Advanced.Condition_Combo), "");
         end if;

         Set_Value (Advanced.Ignore_Count_Combo, Gfloat (Br.Ignore));

         Delete_Text (Advanced.Command_Descr);
         if Br.Commands /= null then
            Insert_Text (Advanced.Command_Descr, Br.Commands.all, Position);
         end if;
      end if;
   end Fill_Advanced_Dialog;

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

   ------------------
   -- Set_Advanced --
   ------------------

   procedure Set_Advanced
     (Editor : Breakpoints_Access;
      Adv    : Advanced_Breakpoint_Access;
      Bpt    : Breakpoint_Identifier)
   is
      Modified : Boolean := False;
   begin
      if Adv /= null
        and then Visible_Is_Set (Adv.Main_Box)
      then
         declare
            S : constant String := Get_Chars (Get_Entry (Adv.Condition_Combo));
            C : constant Integer :=
              Integer (Get_Value_As_Int (Adv.Ignore_Count_Combo));
            T : constant String := Get_Chars (Adv.Command_Descr);
         begin
            --  Send all these commands in "internal" mode, so that no
            --  "info breakpoint" is emitted each time. However, we must
            --  make sure to send at least one
            if S /= "" then
               Set_Breakpoint_Condition
                 (Editor.Process.Debugger, Bpt, S, Internal);
               Modified := True;
            end if;

            if C /= 0 then
               Set_Breakpoint_Ignore_Count
                 (Editor.Process.Debugger, Bpt, C, Internal);
               Modified := True;
            end if;

            if T /= "" then
               Set_Breakpoint_Command
                 (Editor.Process.Debugger, Bpt, T, Internal);
               Modified := True;
            end if;

            if Modified then
               Update_Breakpoints (Editor.Process, Force => True);
            end if;
         end;
      end if;
   end Set_Advanced;

   -----------------------------
   -- Set_Location_Breakpoint --
   -----------------------------

   function Set_Location_Breakpoint
     (Editor : Breakpoints_Access; Current : Integer := -1)
      return Breakpoint_Identifier
   is
      Temporary : Boolean;
      Br : Breakpoint_Data;
      B : Breakpoint_Identifier;
      Remove : Boolean := False;
   begin
      if Current /= -1 then
         Br := Editor.Process.Breakpoints (Current);
      end if;

      Temporary := Get_Active (Editor.Temporary_Location);

      if Get_Active (Editor.Location_Selected) then
         declare
            File : constant String :=
              Get_Chars (Get_Entry (Editor.File_Combo));
            Line : constant Integer :=
              Integer (Get_Value_As_Int (Editor.Line_Spin));
         begin
            --  ??? Should also check Temporary
            if Current = -1
              or else Br.File = null
              or else Br.File.all /= File
              or else Br.Line /= Line
            then
               Remove := True;
               B := Break_Source
                 (Editor.Process.Debugger,
                  File      => File,
                  Line      => Line,
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end if;
         end;

      elsif Get_Active (Editor.Subprogram_Selected) then
         declare
            Name : constant String :=
              Get_Chars (Get_Entry (Editor.Subprogram_Combo));
         begin
            --  ??? Should also check Temporary
            if Current = -1
              or else Br.Except = null
              or else Br.Except.all = Name
            then
               Remove := True;
               B := Break_Subprogram
                 (Editor.Process.Debugger,
                  Name      => Name,
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end if;
         end;

      elsif Get_Active (Editor.Address_Selected) then
         declare
            Addr : constant String :=
              Get_Chars (Get_Entry (Editor.Address_Combo));
         begin
            if Current = -1
              or else Br.Address = null
              or else Br.Address.all = Addr
            then
               Remove := True;
               B := Break_Address
                 (Editor.Process.Debugger,
                  Address   => Addr,
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end if;
         end;

      else
         Remove := True;
         B := Break_Regexp
           (Editor.Process.Debugger,
            Regexp    => Get_Chars (Get_Entry (Editor.Regexp_Combo)),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;

      if Remove then
         if Current /= -1 then
            Remove_Breakpoint (Editor.Process.Debugger, Br.Num);
         end if;
      else
         B := Br.Num;
      end if;

      Set_Advanced (Editor, Editor.Advanced_Breakpoints_Location, B);
      return B;
   end Set_Location_Breakpoint;

   -----------------------------
   -- On_Add_Location_Clicked --
   -----------------------------

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      B : Breakpoint_Identifier;
   begin
      B := Set_Location_Breakpoint (Breakpoints_Access (Object));
   end On_Add_Location_Clicked;

   --------------------------------
   -- On_Update_Location_Clicked --
   --------------------------------

   procedure On_Update_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoints_Access := Breakpoints_Access (Object);
      Selection : constant Integer := Get_Selection_Index (Editor);
      Num : Breakpoint_Identifier;
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
      Editor : constant Breakpoints_Access := Breakpoints_Access (Object);
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
      Editor : constant Breakpoints_Access := Breakpoints_Access (Object);
   begin
      Toggle_Advanced_Dialog (Editor);
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
   -- Set_Exception_Breakpoint --
   ------------------------------

   function Set_Exception_Breakpoint
     (Editor : Breakpoints_Access; Current : Integer := -1)
      return Breakpoint_Identifier
   is
      Temporary : Boolean;
      Name      : constant String :=
        Get_Chars (Get_Entry (Editor.Exception_Name));
      Unhandled : constant Boolean :=
        Get_Active (Editor.Stop_Not_Handled_Exception);
      Br : Breakpoint_Data;
      B         : Breakpoint_Identifier;
      Remove    : Boolean := False;
   begin
      if Current /= -1 then
         Br := Editor.Process.Breakpoints (Current);
      end if;

      Temporary := Get_Active (Editor.Temporary_Exception);

      --  Some of the strings below deal with the GUI, and thus should be
      --  translated for internationalization. Others come from gdb, and should
      --  not be translated. This explains why some are preceded by '-'.

      if Name = -"All exceptions" then
         if Current = -1
           or else Br.Except = null
           or else (Unhandled and then Br.Except.all /= "unhandled exception")
           or else (not Unhandled and then Br.Except.all /= "all exceptions")
         then
            Remove := True;
            B := Break_Exception
              (Editor.Process.Debugger,
               Name      => "",
               Unhandled => Unhandled,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end if;

      elsif Name = -"All assertions" then
         if Current = -1
           or else Br.Except = null
           or else Br.Except.all /= "assert failure"
         then
            Remove := True;
            B := Break_Subprogram
              (Editor.Process.Debugger,
               Name      => "assert",
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end if;

      elsif Current = -1
        or else Br.Except = null
        or else (not Unhandled and then Br.Except.all /= Name)
        or else (Unhandled and then Br.Except.all /= "unhandled exception")
      then
         Remove := True;
         B := Break_Exception
           (Editor.Process.Debugger,
            Name      => Name,
            Unhandled => Unhandled,
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;

      if Remove then
         if Current /= -1 then
            Remove_Breakpoint (Editor.Process.Debugger,
                               Editor.Process.Breakpoints (Current).Num);
         end if;
      else
         B := Br.Num;
      end if;

      Set_Advanced (Editor, Editor.Advanced_Breakpoints_Exceptions, B);
      return B;
   end Set_Exception_Breakpoint;

   ------------------------------
   -- On_Add_Exception_Clicked --
   ------------------------------

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      B : Breakpoint_Identifier;
   begin
      B := Set_Exception_Breakpoint (Breakpoints_Access (Object));
   end On_Add_Exception_Clicked;

   ---------------------------------
   -- On_Update_Exception_Clicked --
   ---------------------------------

   procedure On_Update_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor    : constant Breakpoints_Access := Breakpoints_Access (Object);
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
      Editor : constant Breakpoints_Access := Breakpoints_Access (Object);
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
      Editor    : constant Breakpoints_Access := Breakpoints_Access (Object);
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
      Editor  : constant Breakpoints_Access := Breakpoints_Access (Object);
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

   -------------------------------
   -- Breakpoint_Row_Unselected --
   -------------------------------

   procedure Breakpoint_Row_Unselected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Editor  : constant Breakpoints_Access := Breakpoints_Access (Widget);
   begin
      Set_Sensitive (Editor.Update_Location, False);
      Set_Sensitive (Editor.Update_Watchpoint, False);
      Set_Sensitive (Editor.Update_Exception, False);
   end Breakpoint_Row_Unselected;

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
      Br_Num  : Breakpoint_Identifier;

   begin
      --  Click in the second column => change the enable/disable state

      if Column = Enable_Column then

         --  For efficiency, no need to reparse the list of breakpoints, since
         --  only the state of one of them as changed and we know all about
         --  it.

         Freeze (Editor.Breakpoint_List);

         if Toggle_Breakpoint_State
           (Editor.Process,
            Breakpoint_Num => Breakpoint_Identifier'Value
              (Get_Text (Editor.Breakpoint_List, Row, 0)))
         then
            Set_Pixmap
              (Editor.Breakpoint_List, Row, Enable_Column,
               Editor.Enabled_Pixmap, Editor.Enabled_Mask);
         else
            Set_Text (Editor.Breakpoint_List, Row, Enable_Column, "");
         end if;

         --  Make sure the row is selected

         Emit_Stop_By_Name (Editor.Breakpoint_List, "select_row");
         Unselect_Row (Editor.Breakpoint_List, Row, -1);

         Thaw (Editor.Breakpoint_List);

      --  Otherwise, display the information in the correct tab

      else
         --  Get the information on the breakpoint

         Br_Num := Breakpoint_Identifier'Value
           (Get_Text (Editor.Breakpoint_List, Row, 0));

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
               Add_Unique_Combo_Entry (Editor.Exception_Name, Br.Except.all);
               Set_Text (Get_Entry (Editor.Exception_Name), Br.Except.all);
            end if;

            Set_Active (Editor.Temporary_Exception, Br.Disposition /= Keep);
            Set_Sensitive (Editor.Update_Exception, True);

            Fill_Advanced_Dialog (Editor.Advanced_Breakpoints_Exceptions, Br);
         else
            Set_Page (Editor.Notebook1, 0);

            if Br.File /= null then
               Set_Active (Editor.Location_Selected, True);
               Add_Unique_Combo_Entry (Editor.File_Combo, Br.File.all);
               Set_Text (Get_Entry (Editor.File_Combo), Br.File.all);
               Set_Value (Editor.Line_Spin, Gfloat (Br.Line));
            else
               Set_Active (Editor.Address_Selected, True);
               Add_Unique_Combo_Entry (Editor.Address_Combo, Br.Address.all);
               Set_Text (Get_Entry (Editor.Address_Combo), Br.Address.all);
            end if;
            Set_Sensitive (Editor.Update_Location, True);
            Fill_Advanced_Dialog (Editor.Advanced_Breakpoints_Location, Br);
         end if;
      end if;
   end Breakpoint_Row_Selected;

   ----------------------------
   -- Update_Breakpoint_List --
   ----------------------------

   procedure Update_Breakpoint_List
     (Editor : access Breakpoints_Pkg.Breakpoints_Record'Class)
   is
      Selection : constant Integer :=
        Get_Selection_Index (Breakpoints_Access (Editor));
      Selected : Breakpoint_Identifier := 0;
      Selected_Row : Gint := -1;
      Row  : Gint;
      Br   : Breakpoint_Data;
      Size : Gint;
   begin
      Clear (Editor.Breakpoint_List);

      if Selection /= -1 then
         Selected := Editor.Process.Breakpoints  (Selection).Num;
      end if;

      if Editor.Process.Breakpoints = null
        or else Editor.Process.Breakpoints'Length <= 0
      then
         --  Put at least one empty line, so that the columns are resized
         --  correctly.
         Set_Text (Editor.Breakpoint_List, 0, 0, Natural'Image (1));
         return;
      end if;

      Freeze (Editor.Breakpoint_List);

      for B in Editor.Process.Breakpoints'Range loop
         Br := Editor.Process.Breakpoints (B);

         --  Create a new line
         --  WARNING: We must have at least as many elements as there are
         --  columns in the clist.
         Row := Append
           (Editor.Breakpoint_List, "" + "" + "" + "" + "" + "" + "" + "");

         Set_Text
           (Editor.Breakpoint_List, Row, 0,
            Breakpoint_Identifier'Image (Br.Num));
         if Selection /= -1 and then Br.Num = Selected then
            Selected_Row := Row;
         end if;

         if Br.Enabled then
            Set_Pixmap (Editor.Breakpoint_List, Row, Enable_Column,
                        Editor.Enabled_Pixmap, Editor.Enabled_Mask);
         end if;

         case Br.The_Type is
            when Breakpoint =>
               Set_Text (Editor.Breakpoint_List, Row, 2, -"break");
            when Watchpoint =>
               Set_Text (Editor.Breakpoint_List, Row, 2, -"watch");
         end case;

         case Br.Disposition is
            when Delete =>
               Set_Text (Editor.Breakpoint_List, Row, 3, -"delete");
            when Disable =>
               Set_Text (Editor.Breakpoint_List, Row, 3, -"disable");
            when Keep =>
               Set_Text (Editor.Breakpoint_List, Row, 3, -"keep");
         end case;

         if Br.Expression /= null then
            Set_Text
              (Editor.Breakpoint_List, Row, 4, Br.Expression.all);
         end if;

         if Br.File /= null then
            Set_Text (Editor.Breakpoint_List, Row, 4, Br.File.all);
            Set_Text (Editor.Breakpoint_List, Row, 5, Integer'Image (Br.Line));
         end if;

         if Br.Except /= null then
            Set_Text (Editor.Breakpoint_List, Row, 6, Br.Except.all);
         end if;

         if Br.Subprogram /= null then
            Set_Text
              (Editor.Breakpoint_List, Row, 7, Br.Subprogram.all);
         end if;
      end loop;

      Set_Column_Min_Width (Editor.Breakpoint_List, 1, 20);
      Set_Column_Justification (Editor.Breakpoint_List, 1, Justify_Center);
      Size := Columns_Autosize (Editor.Breakpoint_List);

      Widget_Callback.Object_Connect
        (Editor.Breakpoint_List, "select_row",
         Breakpoint_Row_Selected'Access,
         Slot_Object => Editor);
      Widget_Callback.Object_Connect
        (Editor.Breakpoint_List, "unselect_row",
         Breakpoint_Row_Unselected'Access,
         Slot_Object => Editor);

      Thaw (Editor.Breakpoint_List);

      --  Reselect the same item as before
      if Selected_Row /= -1 then
         Select_Row (Editor.Breakpoint_List, Selected_Row, -1);
      end if;
   end Update_Breakpoint_List;

end Breakpoints_Pkg.Callbacks;
