-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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

with Glib;             use Glib;
with Gdk.Color;        use Gdk.Color;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Window;       use Gdk.Window;

with Gtk;              use Gtk;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Clist;        use Gtk.Clist;
with Gtk.Combo;        use Gtk.Combo;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.List;         use Gtk.List;
with Gtk.Main;         use Gtk.Main;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Spin_Button;  use Gtk.Spin_Button;
with Gtk.Style;        use Gtk.Style;
with Gtk.Text;         use Gtk.Text;
with Gtk.Widget;       use Gtk.Widget;

with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtkada.Types;     use Gtkada.Types;
with Basic_Types;      use Basic_Types;
with Odd_Intl; use Odd_Intl;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;
with Pixmaps_IDE;      use Pixmaps_IDE;
with GVD.Code_Editors; use GVD.Code_Editors;
with GVD.Process;      use GVD.Process;
with GVD.Types;        use GVD.Types;
with GUI_Utils;        use GUI_Utils;
with Debugger;         use Debugger;
with VFS;              use VFS;


package body Breakpoints_Editor is

   Enable_Column : constant := 1;
   --  Column in the clist display that contains the enable/disable state
   --  of the breakpoints.

   procedure Breakpoint_Row_Selected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was selected.

   procedure Breakpoint_Row_Unselected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  Called when a row of the breakpoint editor was unselected.

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data);
   --  Fills the contents of the Advanced dialog with the values contained in
   --  Br.

   procedure Set_Advanced
     (Editor : access Breakpoint_Editor_Record'Class;
      Br     : Breakpoint_Data);
   --  Set the advanced options for the breakpoint Br, based on the contents
   --  of its advanced breakpoint editor.

   -----------------------
   -- Breakpoint_Editor --
   -----------------------

   procedure Breakpoint_Editor
     (Editor  : in out Breakpoint_Editor_Access;
      Process : access GVD.Process.Visual_Debugger_Record'Class)
   is
      Style : Gtk_Style;
   begin
      if Editor = null then
         Editor := new Breakpoint_Editor_Record;
         Breakpoints_Pkg.Initialize (Editor);
         Set_Sensitive (Editor.Advanced_Location, False);
         Set_Sensitive (Editor.Remove, False);
         Set_Sensitive (Editor.View, False);
         Set_Transient_For (Editor, Process.Window);

         Realize (Editor);
         Create_From_Xpm_D
           (Editor.Enabled_Pixmap,
            Get_Window (Editor),
            Editor.Enabled_Mask,
            White (Get_Default_Colormap),
            break_xpm);

         --  Grey background when the combo boxes are insensitive
         Gtk_New (Style);
         Set_Base
           (Style, State_Insensitive,
            Gdk_Color'
              (Get_Background (Get_Style (Editor), State_Normal)));
         Set_Style (Get_Entry (Editor.File_Combo), Style);
         Set_Style (Get_Entry (Editor.Address_Combo), Style);
         Set_Style (Get_Entry (Editor.Subprogram_Combo), Style);
         Set_Style (Get_Entry (Editor.Regexp_Combo), Style);

         --  Return in the combo boxes should activate them

         Disable_Activate (Editor.File_Combo);
         Widget_Callback.Object_Connect
           (Get_Entry (Editor.File_Combo), "activate",
            Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
            Editor);
         Disable_Activate (Editor.Address_Combo);
         Widget_Callback.Object_Connect
           (Get_Entry (Editor.Address_Combo), "activate",
            Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
            Editor);
         Disable_Activate (Editor.Subprogram_Combo);
         Widget_Callback.Object_Connect
           (Get_Entry (Editor.Subprogram_Combo), "activate",
            Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
            Editor);
         Disable_Activate (Editor.Regexp_Combo);
         Widget_Callback.Object_Connect
           (Get_Entry (Editor.Regexp_Combo), "activate",
            Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
            Editor);

         --  ??? Disable watchpoint page for now, until we support them
         --  properly

         Set_Sensitive (Editor.Hbox3, False);
      end if;

      Set_Page (Editor.Notebook1, 0);
      Set_Process (Editor, Process);

      Gdk_Raise (Get_Window (Editor));
      Show_All (Editor);
   end Breakpoint_Editor;

   -----------------------------
   -- Breakpoint_Row_Selected --
   -----------------------------

   procedure Breakpoint_Row_Selected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Editor  : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Widget);
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

         --  Stop propagation of the current signal, to avoid extra calls
         --  to Select/Unselect row.

         Emit_Stop_By_Name (Editor.Breakpoint_List, "select_row");

         --  Put the selection back to its previous state.

         Select_Row (Editor.Breakpoint_List, Row, -1);

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

         else
            Set_Page (Editor.Notebook1, 0);

            if Br.File /= VFS.No_File then
               Set_Active (Editor.Location_Selected, True);
               Add_Unique_Combo_Entry
                 (Editor.File_Combo, Base_Name (Br.File).all);
               Set_Text
                 (Get_Entry (Editor.File_Combo), Base_Name (Br.File).all);
               Set_Value (Editor.Line_Spin, Grange_Float (Br.Line));
            else
               Set_Active (Editor.Address_Selected, True);
               Add_Unique_Combo_Entry (Editor.Address_Combo, Br.Address.all);
               Set_Text (Get_Entry (Editor.Address_Combo), Br.Address.all);
            end if;
         end if;

         Set_Sensitive (Editor.Advanced_Location, True);
         Set_Sensitive (Editor.Remove, True);
         Set_Sensitive (Editor.View, True);
      end if;
   end Breakpoint_Row_Selected;

   -------------------------------
   -- Breakpoint_Row_Unselected --
   -------------------------------

   procedure Breakpoint_Row_Unselected
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Editor  : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Widget);
      Row     : constant Gint := To_Gint (Args, 1);
      Column  : constant Gint := To_Gint (Args, 2);
   begin
      if Column = Enable_Column then
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

         Emit_Stop_By_Name (Editor.Breakpoint_List, "unselect_row");
         Thaw (Editor.Breakpoint_List);
      end if;

      Set_Sensitive (Editor.Advanced_Location, False);
      Set_Sensitive (Editor.Remove, False);
      Set_Sensitive (Editor.View, False);
   end Breakpoint_Row_Unselected;

   -------------------------
   -- Get_Selection_Index --
   -------------------------

   function Get_Selection_Index
     (Editor : access Breakpoint_Editor_Record) return Integer
   is
      use Gint_List;
      Selection : constant Gint_List.Glist :=
        Get_Selection (Editor.Breakpoint_List);
      Br_Num    : Breakpoint_Identifier;

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

   --------------------------
   -- Fill_Advanced_Dialog --
   --------------------------

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data)
   is
      Position : Gint := 0;
   begin
      if Advanced = null then
         return;
      end if;

      if Br.Condition /= null then
         Set_Text (Get_Entry (Advanced.Condition_Combo), Br.Condition.all);
         Add_Unique_Combo_Entry
           (Advanced.Condition_Combo, Br.Condition.all);
      else
         Set_Text (Get_Entry (Advanced.Condition_Combo), "");
      end if;

      Set_Value (Advanced.Ignore_Count_Combo, Grange_Float (Br.Ignore));

      Delete_Text (Advanced.Command_Descr);

      if Br.Commands /= null then
         Insert_Text (Advanced.Command_Descr, Br.Commands.all, Position);
      end if;

      --  Set the scope and action, if appropriate
      if Br.Scope /= No_Scope then
         if Br.Scope = Current_Task then
            Set_Active (Advanced.Scope_Task, True);
         elsif Br.Scope = Tasks_In_PD then
            Set_Active (Advanced.Scope_Pd, True);
         elsif Br.Scope = Any_Task then
            Set_Active (Advanced.Scope_Any, True);
         end if;
      end if;

      if Br.Action /= No_Action then
         if Br.Action = Current_Task then
            Set_Active (Advanced.Action_Task, True);
         elsif Br.Action = Tasks_In_PD then
            Set_Active (Advanced.Action_Pd, True);
         elsif Br.Action = All_Tasks then
            Set_Active (Advanced.Action_All, True);
         end if;
      end if;

      Set_Active (Advanced.Set_Default, False);
   end Fill_Advanced_Dialog;

   ------------------
   -- Set_Advanced --
   ------------------

   procedure Set_Advanced
     (Editor : access Breakpoint_Editor_Record'Class;
      Br     : Breakpoint_Data)
   is
      Adv      : constant Advanced_Breakpoint_Access :=
        Editor.Advanced_Breakpoints;
      Modified : Boolean := False;

   begin
      if Visible_Is_Set (Adv.Condition_Box) then
         declare
            S : constant String :=
              Get_Text (Get_Entry (Adv.Condition_Combo));
            C : constant Integer :=
              Integer (Get_Value_As_Int (Adv.Ignore_Count_Combo));
            T : constant String := Get_Chars (Adv.Command_Descr);

         begin
            --  Send all these commands in "internal" mode, so that no
            --  "info breakpoint" is emitted each time. However, we must
            --  make sure to send at least one.

            if S /= ""
              or else (Br.Condition /= null and then Br.Condition.all /= "")
            then
               Set_Breakpoint_Condition
                 (Editor.Process.Debugger, Br.Num, S, Internal);
               Modified := True;
            end if;

            if C /= 0
              or else Br.Ignore /= 0
            then
               Set_Breakpoint_Ignore_Count
                 (Editor.Process.Debugger, Br.Num, C, Internal);
               Modified := True;
            end if;

            if T /= ""
              or else (Br.Commands /= null and then Br.Commands.all /= "")
            then
               Set_Breakpoint_Command
                 (Editor.Process.Debugger, Br.Num, T, Internal);
               Modified := True;
            end if;

            if Modified then
               Update_Breakpoints (Editor.Process, Force => True);
            end if;
         end;
      end if;
   end Set_Advanced;

   ------------------------------
   -- Set_Exception_Breakpoint --
   ------------------------------

   procedure Set_Exception_Breakpoint
     (Editor : access Breakpoint_Editor_Record'Class; Current : Integer := -1)
   is
      Temporary : Boolean;
      Name      : constant String :=
        Get_Text (Get_Entry (Editor.Exception_Name));
      Unhandled : constant Boolean :=
        Get_Active (Editor.Stop_Not_Handled_Exception);
      Br        : Breakpoint_Data;
      Remove    : Boolean := False;

   begin
      if Current /= -1 then
         Br := Editor.Process.Breakpoints (Current);
      end if;

      Temporary := Get_Active (Editor.Temporary_Exception);

      --  Some of the strings below deal with the GUI, and thus should be
      --  translated for internationalization. Others come from gdb, and
      --  should not be translated.
      --  This explains why some are preceded by '-'.

      if Name = -"All exceptions" then
         if Current = -1
           or else Br.Except = null
           or else (Unhandled and then Br.Except.all /= "unhandled exception")
           or else (not Unhandled and then Br.Except.all /= "all exceptions")
         then
            Remove := True;
            Break_Exception
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
            Break_Subprogram
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
         Break_Exception
           (Editor.Process.Debugger,
            Name      => Name,
            Unhandled => Unhandled,
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;

      if Remove and then Current /= -1 then
         Remove_Breakpoint
           (Editor.Process.Debugger,
            Editor.Process.Breakpoints (Current).Num);
      end if;
   end Set_Exception_Breakpoint;

   -----------------------------
   -- Set_Location_Breakpoint --
   -----------------------------

   procedure Set_Location_Breakpoint
     (Editor : access Breakpoint_Editor_Record'Class; Current : Integer := -1)
   is
      Temporary : Boolean;
      Br        : Breakpoint_Data;
      Remove    : Boolean := False;

   begin
      if Current /= -1 then
         Br := Editor.Process.Breakpoints (Current);
      end if;

      Temporary := Get_Active (Editor.Temporary_Location);

      if Get_Active (Editor.Location_Selected) then
         declare
            File : constant String :=
              Get_Text (Get_Entry (Editor.File_Combo));
            Line : constant Integer :=
              Integer (Get_Value_As_Int (Editor.Line_Spin));

         begin
            --  ??? Should also check Temporary

            if Current = -1
              or else Base_Name (Br.File).all /= File
              or else Br.Line /= Line
            then
               Remove := True;
               Break_Source
                 (Editor.Process.Debugger,
                  File      => Create_From_Base (File),
                  Line      => Line,
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end if;
         end;

      elsif Get_Active (Editor.Subprogram_Selected) then
         declare
            Name : constant String :=
              Get_Text (Get_Entry (Editor.Subprogram_Combo));

         begin
            --  ??? Should also check Temporary

            if Current = -1
              or else Br.Except = null
              or else Br.Except.all = Name
            then
               Remove := True;
               Break_Subprogram
                 (Editor.Process.Debugger,
                  Name      => Name,
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end if;
         end;

      elsif Get_Active (Editor.Address_Selected) then
         declare
            Addr : constant String :=
              Get_Text (Get_Entry (Editor.Address_Combo));
         begin
            if Current = -1
              or else Br.Address = null
              or else Br.Address.all = Addr
            then
               Remove := True;
               Break_Address
                 (Editor.Process.Debugger,
                  Address   => Addr,
                  Temporary => Temporary,
                  Mode      => GVD.Types.Visible);
            end if;
         end;

      else
         Remove := True;
         Break_Regexp
           (Editor.Process.Debugger,
            Regexp    => Get_Text (Get_Entry (Editor.Regexp_Combo)),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;

      if Remove and then Current /= -1 then
         Remove_Breakpoint (Editor.Process.Debugger, Br.Num);
      end if;
   end Set_Location_Breakpoint;

   -----------------
   -- Set_Process --
   -----------------

   procedure Set_Process
     (Editor  : access Breakpoint_Editor_Record;
      Process : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      Editor.Process := GVD.Process.Visual_Debugger (Process);
      Update_Breakpoint_List (Editor);

      --  Reinitialize the contents of the file combo boxes
      Set_Text
        (Get_Entry (Editor.File_Combo),
         Base_Name (Get_Current_File (Process.Editor_Text)).all);

      --  Clear the contents of the exceptions combo (its contents is in fact
      --  cached in gdb, so it is fast enough to call "info exceptions" again)
      Clear_Items (Get_List (Editor.Exception_Name), 0, -1);
      Add_Unique_Combo_Entry
        (Editor.Exception_Name, -"All exceptions");
      Add_Unique_Combo_Entry
        (Editor.Exception_Name, -"All assertions");
   end Set_Process;

   -------------------------
   -- Run_Advanced_Dialog --
   -------------------------

   procedure Run_Advanced_Dialog
     (Editor  : access Breakpoint_Editor_Record'Class;
      Current : Integer)
   is
      WTX_Version : Natural;
   begin
      --  Create all three dialogs

      if Editor.Advanced_Breakpoints = null then
         Gtk_New (Editor.Advanced_Breakpoints);
         Set_Transient_For (Editor.Advanced_Breakpoints, Editor);
         Set_Sensitive (Editor.Advanced_Breakpoints.Record_Button, False);
         Set_Sensitive (Editor.Advanced_Breakpoints.End_Button, False);
      end if;

      Fill_Advanced_Dialog
        (Editor.Advanced_Breakpoints, Editor.Process.Breakpoints (Current));
      Show_All (Editor.Advanced_Breakpoints);

      Info_WTX (Editor.Process.Debugger, WTX_Version);

      if WTX_Version = 3 then
         Set_Show_Tabs (Editor.Advanced_Breakpoints.Main_Notebook);
      else
         Hide (Editor.Advanced_Breakpoints.Scope_Box);
         Set_Show_Tabs (Editor.Advanced_Breakpoints.Main_Notebook, False);
      end if;

      Editor.Advanced_Breakpoints.Response_Action := Gtk_Response_None;
      Gtk.Main.Main;

      if Editor.Advanced_Breakpoints.Response_Action = Gtk_Response_Apply then
         --  If we are using AE and the user has activated the "Set as
         --  default" checkbox for the scope and action values, send the
         --  appropriate commands to the debugger

         if WTX_Version = 3 then
            declare
               Scope_Value  : Scope_Type;
               Action_Value : Action_Type;
            begin
               if Get_Active (Editor.Advanced_Breakpoints.Scope_Task) then
                  Scope_Value := Current_Task;
               elsif Get_Active (Editor.Advanced_Breakpoints.Scope_Pd) then
                  Scope_Value := Tasks_In_PD;
               elsif Get_Active (Editor.Advanced_Breakpoints.Scope_Any) then
                  Scope_Value := Any_Task;
               end if;

               if Get_Active (Editor.Advanced_Breakpoints.Action_Task) then
                  Action_Value := Current_Task;
               elsif Get_Active (Editor.Advanced_Breakpoints.Action_Pd) then
                  Action_Value := Tasks_In_PD;
               elsif Get_Active (Editor.Advanced_Breakpoints.Action_All) then
                  Action_Value := All_Tasks;
               end if;

               if Get_Active (Editor.Advanced_Breakpoints.Set_Default) then
                  Set_Scope_Action
                    (Editor.Process.Debugger, Scope_Value, Action_Value);
               end if;

               Set_Scope_Action
                 (Editor.Process.Debugger, Scope_Value,
                  Action_Value, Editor.Process.Breakpoints (Current).Num);
            end;
         end if;

         Set_Advanced (Editor, Editor.Process.Breakpoints (Current));
      end if;
   end Run_Advanced_Dialog;

   ----------------------------
   -- Update_Breakpoint_List --
   ----------------------------

   procedure Update_Breakpoint_List
     (Editor : access Breakpoint_Editor_Record'Class)
   is
      Selection    : constant Integer := Get_Selection_Index (Editor);
      Selected     : Breakpoint_Identifier := 0;
      Selected_Row : Gint := -1;
      Row          : Gint;
      Br           : Breakpoint_Data;
      Size         : Gint;
      pragma Unreferenced (Size);

   begin
      Clear (Editor.Breakpoint_List);

      if Selection /= -1 then
         Selected := Editor.Process.Breakpoints (Selection).Num;
      end if;

      if Editor.Process.Breakpoints = null
        or else Editor.Process.Breakpoints'Length <= 0
      then
         --  Put at least one empty line, so that the columns are resized
         --  correctly.

         Set_Text (Editor.Breakpoint_List, 0, 0, " 1");
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

         if Br.File /= VFS.No_File then
            Set_Text (Editor.Breakpoint_List, Row, 4, Base_Name (Br.File).all);
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

end Breakpoints_Editor;
