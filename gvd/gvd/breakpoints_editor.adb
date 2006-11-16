-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
--                              AdaCore                              --
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

with GNAT.Strings;     use GNAT.Strings;

with Glib;             use Glib;
with Gdk.Color;        use Gdk.Color;
with Gdk.Event;        use Gdk.Event;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Window;       use Gdk.Window;

with Gtk;              use Gtk;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo;        use Gtk.Combo;
with Gtk.Dialog;       use Gtk.Dialog;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.List;         use Gtk.List;
with Gtk.Main;         use Gtk.Main;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Spin_Button;  use Gtk.Spin_Button;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;

with Gtk.Text_View;    use Gtk.Text_View;
with Gtk.Text_Buffer;  use Gtk.Text_Buffer;
with Gtk.Text_Iter;    use Gtk.Text_Iter;

with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_Selection;   use Gtk.Tree_Selection;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;

with Gtkada.Handlers;  use Gtkada.Handlers;
with GPS.Intl;       use GPS.Intl;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;
with Pixmaps_IDE;      use Pixmaps_IDE;
with GVD.Code_Editors; use GVD.Code_Editors;
with GVD.Process;      use GVD.Process;
with GVD.Types;        use GVD.Types;
with GUI_Utils;        use GUI_Utils;
with Debugger;         use Debugger;
with VFS;              use VFS;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Exceptions;          use Ada.Exceptions;
with Traces;                  use Traces;

package body Breakpoints_Editor is

   procedure Breakpoint_Row_Selection_Change
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was selected.

   function Breakpoint_Clicked
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Called when receiving a click on the breakpoint tree.

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
      end if;

      Set_Page (Editor.Notebook1, 0);
      Set_Process (Editor, Process);

      Gdk_Raise (Get_Window (Editor));
      Show_All (Editor);
   end Breakpoint_Editor;

   ------------------------
   -- Breakpoint_Clicked --
   ------------------------

   function Breakpoint_Clicked
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Editor  : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Widget);

      Iter    : Gtk_Tree_Iter;
      Col     : Gtk_Tree_View_Column;

      Model   : constant Gtk_Tree_Store := Gtk_Tree_Store
        (Get_Model (Editor.Breakpoint_List));
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         Coordinates_For_Event
           (Editor.Breakpoint_List,
            Get_Model (Editor.Breakpoint_List),
            Event, Iter, Col);

         if Col = Get_Column (Editor.Breakpoint_List, Col_Enb) then
            --  Click in the second column => change the enable/disable state
            --  For efficiency, no need to reparse the list of breakpoints,
            --  since only the state of one of them as changed and we know all
            --  about it.

            Set (Model, Iter, Col_Enb,
                 Toggle_Breakpoint_State
                   (Editor.Process,
                    Breakpoint_Num => Breakpoint_Identifier'Value
                      (Get_String (Model, Iter, Col_Num))));

            --  Stop propagation of the current signal, to avoid extra calls
            --  to Select/Unselect row.

            return True;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Breakpoint_Clicked;

   -------------------------------------
   -- Breakpoint_Row_Selection_Change --
   -------------------------------------

   procedure Breakpoint_Row_Selection_Change
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      pragma Unreferenced (Args);

      Editor  : constant Breakpoint_Editor_Access :=
        Breakpoint_Editor_Access (Widget);

      Model   : Gtk_Tree_Model;
      Iter    : Gtk_Tree_Iter;
      Br      : Breakpoint_Data;
      Br_Num  : Breakpoint_Identifier;

   begin
      Get_Selected (Get_Selection (Editor.Breakpoint_List), Model, Iter);

      if Iter = Null_Iter then
         Set_Sensitive (Editor.Advanced_Location, False);
         Set_Sensitive (Editor.Remove, False);
         Set_Sensitive (Editor.View, False);
         return;
      end if;

      Br_Num := Breakpoint_Identifier'Value
        (Get_String (Model, Iter, Col_Num));

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
              (Editor.File_Combo, Base_Name (Br.File));
            Set_Text
              (Get_Entry (Editor.File_Combo), Base_Name (Br.File));
            Set_Value (Editor.Line_Spin, Grange_Float (Br.Line));
         else
            Set_Active (Editor.Address_Selected, True);
            Add_Unique_Combo_Entry
              (Editor.Address_Combo, Address_To_String (Br.Address));
            Set_Text
              (Get_Entry (Editor.Address_Combo),
               Address_To_String (Br.Address));
         end if;
      end if;

      Set_Sensitive (Editor.Advanced_Location, True);
      Set_Sensitive (Editor.Remove, True);
      Set_Sensitive (Editor.View, True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Breakpoint_Row_Selection_Change;

   -------------------------
   -- Get_Selection_Index --
   -------------------------

   function Get_Selection_Index
     (Editor : access Breakpoint_Editor_Record) return Integer
   is
      use Gint_List;
      Br_Num    : Breakpoint_Identifier;
      Iter      : Gtk_Tree_Iter;
      The_Model : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Editor.Breakpoint_List), The_Model, Iter);

      if Iter /= Null_Iter then
         Br_Num := Breakpoint_Identifier'Value
           (Get_String (The_Model, Iter, Col_Num));

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
      Start, The_End : Gtk_Text_Iter;
      Buffer         : Gtk_Text_Buffer;

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

      Buffer := Get_Buffer (Advanced.Command_Descr);

      Get_Bounds (Buffer, Start, The_End);
      Delete (Buffer, Start, The_End);

      if Br.Commands /= null then
         Insert_At_Cursor (Buffer, Br.Commands.all);
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
      Start, The_End : Gtk_Text_Iter;

   begin
      if Visible_Is_Set (Adv.Condition_Box) then
         Get_Bounds (Get_Buffer (Adv.Command_Descr), Start, The_End);

         declare
            S : constant String :=
              Get_Text (Get_Entry (Adv.Condition_Combo));
            C : constant Integer :=
              Integer (Get_Value_As_Int (Adv.Ignore_Count_Combo));
            T : constant String := Get_Text
              (Get_Buffer (Adv.Command_Descr), Start, The_End);

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
              or else Base_Name (Br.File) /= File
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
            Address : constant Address_Type :=
                        String_To_Address
                          (Get_Text (Get_Entry (Editor.Address_Combo)));
         begin
            if Current = -1
              or else Br.Address = Invalid_Address
              or else Br.Address = Address
            then
               Remove := True;
               Break_Address
                 (Editor.Process.Debugger,
                  Address   => Address,
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
         Base_Name (Get_Current_File (Process.Editor_Text)));

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
      Br           : Breakpoint_Data;
      Size         : Gint;
      pragma Unreferenced (Size);
      Model : constant Gtk_Tree_Store := Gtk_Tree_Store
        (Get_Model (Editor.Breakpoint_List));
      Iter          : Gtk_Tree_Iter;
      Selected_Iter : Gtk_Tree_Iter := Null_Iter;

   begin
      Clear (Model);

      if Selection /= -1 then
         Selected := Editor.Process.Breakpoints (Selection).Num;
      end if;

      if Editor.Process.Breakpoints = null
        or else Editor.Process.Breakpoints'Length <= 0
      then
         return;
      end if;

      for B in Editor.Process.Breakpoints'Range loop
         Br := Editor.Process.Breakpoints (B);

         --  Create a new line

         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, Col_Num, Breakpoint_Identifier'Image (Br.Num));

         if Selection /= -1 and then Br.Num = Selected then
            Selected_Iter := Iter;
         end if;

         Set (Model, Iter, Col_Enb, Br.Enabled);

         case Br.The_Type is
            when Breakpoint =>
               Set (Model, Iter, Col_Type, -"break");
            when Watchpoint =>
               Set (Model, Iter, Col_Type, -"watch");
         end case;

         Set (Model, Iter, Col_Disp, To_Lower (Br.Disposition'Img));

         if Br.Expression /= null then
            Set (Model, Iter, Col_File, Br.Expression.all);
         end if;

         if Br.File /= VFS.No_File then
            Set (Model, Iter, Col_File, Base_Name (Br.File));
            Set (Model, Iter, Col_Line, Integer'Image (Br.Line));
         end if;

         if Br.Except /= null then
            Set (Model, Iter, Col_Exception, Br.Except.all);
         end if;

         if Br.Subprogram /= null then
            Set (Model, Iter, Col_Subprogs, Br.Subprogram.all);
         end if;
      end loop;

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.Breakpoint_List), "changed",
         Breakpoint_Row_Selection_Change'Access,
         Slot_Object => Editor,
         After       => True);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Editor.Breakpoint_List,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Breakpoint_Clicked'Access),
         Slot_Object => Editor,
         After       => False);

      --  Reselect the same item as before

      if Selected_Iter /= Null_Iter then
         Select_Iter (Get_Selection (Editor.Breakpoint_List), Selected_Iter);
      end if;
   end Update_Breakpoint_List;

   --------------------
   -- Set_Watchpoint --
   --------------------

   procedure Set_Watchpoint
     (Editor : access Breakpoint_Editor_Record'Class; Current : Integer := -1)
   is
      Watchpoint_Name : constant String :=
        Get_Text (Editor.Watchpoint_Name);
      Watchpoint_Type : constant String :=
        Get_Text (Get_Entry (Editor.Watchpoint_Type));
      Watchpoint_Cond : constant String :=
        Get_Text (Editor.Watchpoint_Cond);

      Trigger : GVD.Types.Watchpoint_Trigger;
      --  Encodes the value we get from Watchpoint_Type for the call to Watch

      Br : Breakpoint_Data;
      --  Used to manipulate breakpoint list if Current is set

      Remove : Boolean := False;
      --  If set, will remove the currently selected breakpoint from the
      --  breakpoint list.
   begin
      if Current /= -1 then
         Br := Editor.Process.Breakpoints (Current);
      end if;

      if Watchpoint_Type = -"read" then
         Trigger := GVD.Types.Read;
      elsif Watchpoint_Type = -"read or written" then
         Trigger := GVD.Types.Read_Write;
      else
         --  presumably, Watchpoint_Type = -"written"
         Trigger := GVD.Types.Write;
      end if;

      if Current = -1
        or else Br.Expression.all /= Watchpoint_Name
        or else Br.Trigger /= Trigger
        or else Br.Condition.all /= Watchpoint_Cond
      then
         Remove := True;
         Watch
           (Editor.Process.Debugger,
            Name      => Watchpoint_Name,
            Trigger   => Trigger,
            Condition => Watchpoint_Cond,
            Mode      => GVD.Types.Visible);
      end if;

      if Remove and Current /= -1 then
         Remove_Breakpoint (Editor.Process.Debugger, Br.Num);
      end if;
   end Set_Watchpoint;

end Breakpoints_Editor;
