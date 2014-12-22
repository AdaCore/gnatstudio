------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNAT.Strings;       use GNAT.Strings;

with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gdk.Event;          use Gdk.Event;
with Gdk.Types;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;
with Gdk.Window;         use Gdk.Window;

with Gtk;                use Gtk;
with Gtk.Arguments;      use Gtk.Arguments;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Check_Button;   use Gtk.Check_Button;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.GEntry;         use Gtk.GEntry;
with Gtk.List_Store;     use Gtk.List_Store;
with Gtk.Main;           use Gtk.Main;
with Gtk.Notebook;       use Gtk.Notebook;
with Gtk.Radio_Button;   use Gtk.Radio_Button;
with Gtk.Spin_Button;    use Gtk.Spin_Button;
with Gtk.Widget;         use Gtk.Widget;

with Gtk.Text_View;    use Gtk.Text_View;
with Gtk.Text_Buffer;  use Gtk.Text_Buffer;
with Gtk.Text_Iter;    use Gtk.Text_Iter;

with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_Store;       use Gtk.Tree_Store;
with Gtk.Tree_Selection;   use Gtk.Tree_Selection;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;

with Advanced_Breakpoint_Pkg; use Advanced_Breakpoint_Pkg;
with Breakpoints_Pkg;         use Breakpoints_Pkg;
with Commands.Interactive;    use Commands, Commands.Interactive;
with Generic_Views;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Gtkada.MDI;         use Gtkada.MDI;
with GPS.Kernel.MDI;     use GPS.Kernel.MDI;
with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Actions; use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks; use GPS.Kernel;
with GVD_Module;       use GVD_Module;
with GVD.Code_Editors; use GVD, GVD.Code_Editors;
with GVD.Process;      use GVD.Process;
with GVD.Scripts;      use GVD.Scripts;
with GVD.Types;        use GVD.Types;
with GVD.Views;        use GVD.Views;
with GUI_Utils;        use GUI_Utils;
with Debugger;         use Debugger;
with GNATCOLL.VFS;     use GNATCOLL.VFS;
with Process_Proxies;  use Process_Proxies;

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Breakpoints_Editor is
   type Breakpoint_Editor_Record is new Base_Views.Process_View_Record with
      record
         Editor               : Breakpoints_Access;
         Advanced_Breakpoints : Advanced_Breakpoint_Access;
      end record;
   type Breakpoint_Editor is access all Breakpoint_Editor_Record'Class;

   overriding procedure Update (View   : access Breakpoint_Editor_Record);
   overriding procedure On_Process_Terminated
     (View : access Breakpoint_Editor_Record);
   --  See inherited documentation

   function Initialize
     (Widget : access Breakpoint_Editor_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget;
   --  Internal initialization function
   --  Returns the focus child

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Generic_Views.Abstract_View_Access;
   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Generic_Views.Abstract_View_Access);
   --  Store or retrieve the view from the process

   package Simple_Views is new Base_Views.Simple_Views
     (Module_Name        => "Breakpoints",
      View_Name          => -"Breakpoints",
      Formal_View_Record => Breakpoint_Editor_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View,
      Group              => Group_Default,
      Position           => Position_Automatic,
      Initialize         => Initialize);

   type Breakpoint_Editor_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Breakpoint_Editor_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Breakpoints

   procedure On_Breakpoints_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Hook for "debugger_breakpoints_changed"

   procedure Update_Breakpoint_List
     (View   : access Breakpoint_Editor_Record'Class);
   --  Update the list of breakpoints in the dialog.
   --  The list is taken from the one stored in the current debugger session.

   function Get_Selection_Index
     (View : access Breakpoint_Editor_Record'Class) return Integer;
   --  Return the index of the currently selected line in the breakpoint
   --  editor. The index is the element in Editor.Process.Breakpoints, or
   --  -1 if there is no selection

   procedure Breakpoint_Row_Selection_Change
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk.Arguments.Gtk_Args);
   --  Called when a row of the breakpoint editor was selected.

   function Breakpoint_Clicked
     (Widget : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   --  Called when receiving a click on the breakpoint tree.

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  Run the advanced breakpoints dialog

   procedure On_Add_Location_Clicked (Object : access Gtk_Widget_Record'Class);
   --  Set the breakpoint that is currently described in the location page.
   --  If Current is not -1, then the breakpoint currently displayed at line
   --  Current is updated (first removed if some of the information has
   --  changed).

   procedure On_Load_Exception_List_Clicked
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class);
   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class);
   function On_Breakpoints_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callbacks for the various buttons

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  Set the breakpoint that is currently described in the exception page.

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class);
   --  Set the watchpoint that is currently described in the variables page.

   procedure Fill_Advanced_Dialog
     (Advanced : Advanced_Breakpoint_Access; Br : Breakpoint_Data);
   --  Fills the contents of the Advanced dialog with the values contained in
   --  Br.

   procedure Set_Advanced
     (View : access Breakpoint_Editor_Record'Class;
      Br   : Breakpoint_Data);
   --  Set the advanced options for the breakpoint Br, based on the contents
   --  of its advanced breakpoint editor.

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Generic_Views.Abstract_View_Access is
   begin
      return Generic_Views.Abstract_View_Access (Process.Breakpoints_Editor);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Generic_Views.Abstract_View_Access)
   is
      use type Generic_Views.Abstract_View_Access;
      Old : constant Breakpoint_Editor :=
        Breakpoint_Editor (Process.Breakpoints_Editor);
   begin
      Process.Breakpoints_Editor := Gtk_Widget (View);

      --  If we are detaching, clear the old view. This can only be done after
      --  the above, since otherwise the action on the GUI will result into
      --  actions on the debugger.

      if View = null and then Old /= null then
         On_Process_Terminated (Old);
      end if;
   end Set_View;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Breakpoint_Editor_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Process : Visual_Debugger;
      List    : Debugger_List_Link := Get_Debugger_List (Kernel);

   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            Attach_To_Breakpoints (Process, Create_If_Necessary => True);
         end if;

         List := List.Next;
      end loop;
      return Commands.Success;
   end Execute;

   ----------------------------
   -- Update_Breakpoint_List --
   ----------------------------

   procedure Update_Breakpoint_List
     (View   : access Breakpoint_Editor_Record'Class)
   is
      Process      : constant Visual_Debugger := Get_Process (View);
      Selection    : constant Integer := Get_Selection_Index (View);
      Selected     : Breakpoint_Identifier := 0;
      Br           : Breakpoint_Data;
      Size         : Gint;
      pragma Unreferenced (Size);
      Model : constant Gtk_Tree_Store :=
        -Get_Model (View.Editor.Breakpoint_List);
      Iter          : Gtk_Tree_Iter;
      Selected_Iter : Gtk_Tree_Iter := Null_Iter;

   begin
      Clear (Model);

      if Selection /= -1 then
         Selected := Process.Breakpoints (Selection).Num;
      end if;

      if Process.Breakpoints = null
        or else Process.Breakpoints'Length <= 0
      then
         return;
      end if;

      for B in Process.Breakpoints'Range loop
         Br := Process.Breakpoints (B);

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

         if Br.File /= GNATCOLL.VFS.No_File then
            Set (Model, Iter, Col_File, +Base_Name (Br.File));
            Set (Model, Iter, Col_Line, Integer'Image (Br.Line));
         end if;

         if Br.Except /= null then
            Set (Model, Iter, Col_Exception, Br.Except.all);
         end if;

         if Br.Subprogram /= null then
            Set (Model, Iter, Col_Subprogs, Br.Subprogram.all);
         end if;
      end loop;

      --  Reselect the same item as before

      if Selected_Iter /= Null_Iter then
         Select_Iter
           (Get_Selection (View.Editor.Breakpoint_List), Selected_Iter);
      end if;
   end Update_Breakpoint_List;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View   : access Breakpoint_Editor_Record) is
      Process  : Process_Proxy_Access;
      M        : Gtk_List_Store;
   begin
      if Get_Process (View) /= null then
         Process := Get_Process (Get_Process (View).Debugger);
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         return;
      end if;

      Update_Breakpoint_List (View);

      --  Reinitialize the contents of the file name entry
      Set_Text
        (View.Editor.File_Name,
         +Base_Name (Get_Current_File (Get_Process (View).Editor_Text)));
      --  ??? What if the filesystem path is non-UTF8?

      --  Clear the contents of the exceptions combo (its contents is in fact
      --  cached in gdb, so it is fast enough to call "info exceptions" again)
      M := -View.Editor.Exception_Name.Get_Model;
      M.Clear;
      Add_Unique_Combo_Entry
        (View.Editor.Exception_Name, -"All Ada exceptions");
      Add_Unique_Combo_Entry
        (View.Editor.Exception_Name, -"Ada assertions");

      --  Reset the Exception page
      Set_Sensitive (View.Editor.Hbox4, True);
   end Update;

   ----------------------------
   -- On_Breakpoints_Changed --
   ----------------------------

   procedure On_Breakpoints_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      Process : constant Visual_Debugger :=
        Get_Process (Debugger_Hooks_Data_Access (Data));
      View : constant Breakpoint_Editor :=
        Breakpoint_Editor (Process.Breakpoints_Editor);
   begin
      if View /= null then
         Update_Breakpoint_List (View);
      end if;
   end On_Breakpoints_Changed;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : access Breakpoint_Editor_Record)
   is
      Model : constant Gtk_Tree_Store :=
        -Get_Model (View.Editor.Breakpoint_List);
   begin
      Clear (Model);
   end On_Process_Terminated;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Breakpoint_Editor_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
   begin
      Gtk.Box.Initialize_Hbox (Widget);
      Gtk_New (Widget.Editor);
      Pack_Start (Widget, Widget.Editor, True, Padding => 4);

      Set_Sensitive (Widget.Editor.Advanced_Location, False);
      Set_Sensitive (Widget.Editor.Remove, False);
      Set_Sensitive (Widget.Editor.View, False);

      --  Return in the combo boxes should activate them

      Widget_Callback.Object_Connect
        (Widget.Editor.File_Name,
         Gtk.GEntry.Signal_Activate,
         Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Gtk_Entry (Widget.Editor.Address_Combo.Get_Child),
         Gtk.GEntry.Signal_Activate,
         Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Gtk_Entry (Widget.Editor.Subprogram_Combo.Get_Child),
         Gtk.GEntry.Signal_Activate,
         Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Gtk_Entry (Widget.Editor.Regexp_Combo.Get_Child),
         Gtk.GEntry.Signal_Activate,
         Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
         Widget);

      Widget_Callback.Object_Connect
        (Get_Selection (Widget.Editor.Breakpoint_List),
         Gtk.Tree_Selection.Signal_Changed,
         Breakpoint_Row_Selection_Change'Access,
         Slot_Object => Widget,
         After       => True);

      Return_Callback.Object_Connect
        (Widget.Editor, Signal_Key_Press_Event,
         On_Breakpoints_Key_Press_Event'Access, Widget);

      Widget_Callback.Object_Connect
        (Widget.Editor.Add_Location, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Add_Watchpoint, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Add_Watchpoint_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Add_Exception, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Add_Exception_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Advanced_Location, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Advanced_Location_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Load_Exception_List, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Load_Exception_List_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Remove, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Remove_Clicked'Access), Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.View, Gtk.Button.Signal_Clicked,
         Widget_Callback.To_Marshaller (On_View_Clicked'Access), Widget);

      Widget.Editor.Breakpoint_List.On_Button_Press_Event
        (Breakpoint_Clicked'Access, Widget);

      Add_Hook
        (Kernel, Debugger_Breakpoints_Changed_Hook,
         Wrapper (On_Breakpoints_Changed'Access),
         Watch => GObject (Widget),
         Name  => "breakpoints_editor.on_breakpoints_changed");

      Widget.Editor.Notebook1.Set_Current_Page (0);

      Show_All (Widget);
      return Gtk_Widget (Widget);
   end Initialize;

   ---------------------------
   -- Attach_To_Breakpoints --
   ---------------------------

   procedure Attach_To_Breakpoints
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
      renames Simple_Views.Attach_To_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Simple_Views.Register_Desktop_Functions (Kernel);
      Register_Action
        (Kernel, "open breakpoints editor", new Breakpoint_Editor_Command,
         -"Open the Breakpoints Editor for the debugger",
         Category => -"Views");
   end Register_Module;

   -------------------------------------
   -- Breakpoint_Row_Selection_Change --
   -------------------------------------

   procedure Breakpoint_Row_Selection_Change
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      pragma Unreferenced (Args);

      View    : constant Breakpoint_Editor := Breakpoint_Editor (Widget);
      Process : constant Visual_Debugger := Get_Process (View);
      Model   : Gtk_Tree_Model;
      Iter    : Gtk_Tree_Iter;
      Br      : Breakpoint_Data;
      Br_Num  : Breakpoint_Identifier;

   begin
      Get_Selected (Get_Selection (View.Editor.Breakpoint_List), Model, Iter);

      if Iter = Null_Iter then
         Set_Sensitive (View.Editor.Advanced_Location, False);
         Set_Sensitive (View.Editor.Remove, False);
         Set_Sensitive (View.Editor.View, False);
         return;
      end if;

      Br_Num := Breakpoint_Identifier'Value
        (Get_String (Model, Iter, Col_Num));

      for B in Process.Breakpoints'Range loop
         if Process.Breakpoints (B).Num = Br_Num then
            Br := Process.Breakpoints (B);
            exit;
         end if;
      end loop;

      --  Fill the information

      if Br.Except /= null then
         View.Editor.Notebook1.Set_Current_Page (2);
         Set_Active (View.Editor.Stop_Always_Exception, True);

         if Br.Except.all = "all" then
            Set_Active_Text
              (View.Editor.Exception_Name, -"All Ada exceptions");
         elsif Br.Except.all = "unhandled" then
            Set_Active_Text
              (View.Editor.Exception_Name, -"All Ada exceptions");
            Set_Active (View.Editor.Stop_Not_Handled_Exception, True);
         else
            Add_Unique_Combo_Entry
              (View.Editor.Exception_Name, Br.Except.all, Select_Text => True);
         end if;

         Set_Active (View.Editor.Temporary_Exception, Br.Disposition /= Keep);

      else
         View.Editor.Notebook1.Set_Current_Page (0);

         if Br.File /= GNATCOLL.VFS.No_File then
            Set_Active (View.Editor.Location_Selected, True);
            Set_Text (View.Editor.File_Name, +Base_Name (Br.File));
            --  ??? What if the filesystem path is non-UTF8?
            Set_Value (View.Editor.Line_Spin, Grange_Float (Br.Line));

            if Br.Subprogram /= null then
               Add_Unique_Combo_Entry
                 (View.Editor.Subprogram_Combo, Br.Subprogram.all, True);
            end if;

         else
            Set_Active (View.Editor.Address_Selected, True);
            Add_Unique_Combo_Entry
              (View.Editor.Address_Combo, Address_To_String (Br.Address));
            Set_Text
              (Gtk_Entry (View.Editor.Address_Combo.Get_Child),
               Address_To_String (Br.Address));
         end if;
      end if;

      Set_Sensitive (View.Editor.Advanced_Location, True);
      Set_Sensitive (View.Editor.Remove, True);
      Set_Sensitive (View.Editor.View, True);
   end Breakpoint_Row_Selection_Change;

   ------------------------
   -- Breakpoint_Clicked --
   ------------------------

   function Breakpoint_Clicked
     (Widget : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean
   is
      View  : constant Breakpoint_Editor := Breakpoint_Editor (Widget);
      Iter  : Gtk_Tree_Iter;
      Col   : Gtk_Tree_View_Column;
      Model : constant Gtk_Tree_Store :=
        -Get_Model (View.Editor.Breakpoint_List);
   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Press
      then
         Coordinates_For_Event
           (View.Editor.Breakpoint_List,
            Event, Iter, Col);

         if Col = Get_Column (View.Editor.Breakpoint_List, Col_Enb) then
            --  Click in the second column => change the enable/disable state
            --  For efficiency, no need to reparse the list of breakpoints,
            --  since only the state of one of them as changed and we know all
            --  about it.

            Set (Model, Iter, Col_Enb,
                 Toggle_Breakpoint_State
                   (Get_Process (View),
                    Breakpoint_Num => Breakpoint_Identifier'Value
                      (Get_String (Model, Iter, Col_Num))));

            --  Stop propagation of the current signal, to avoid extra calls
            --  to Select/Unselect row.

            return True;
         end if;

      elsif Event.Button = 1
        and then Event.The_Type = Gdk_2button_Press
      then
         On_View_Clicked (Gtk_Widget (Widget));
      end if;

      return False;
   end Breakpoint_Clicked;

   -------------------------
   -- Get_Selection_Index --
   -------------------------

   function Get_Selection_Index
     (View : access Breakpoint_Editor_Record'Class) return Integer
   is
      Process   : constant Visual_Debugger := Get_Process (View);
      Br_Num    : Breakpoint_Identifier;
      Iter      : Gtk_Tree_Iter;
      The_Model : Gtk_Tree_Model;
   begin
      Get_Selected
        (Get_Selection (View.Editor.Breakpoint_List), The_Model, Iter);

      if Iter /= Null_Iter then
         Br_Num := Breakpoint_Identifier'Value
           (Get_String (The_Model, Iter, Col_Num));

         for B in Process.Breakpoints'Range loop
            if Process.Breakpoints (B).Num = Br_Num then
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
         Add_Unique_Combo_Entry
           (Advanced.Condition_Combo, Br.Condition.all, Select_Text => True);
      else
         Advanced.Condition_Combo.Set_Active (-1);
      end if;

      Set_Value (Advanced.Ignore_Count_Combo, Grange_Float (Br.Ignore));

      Buffer := Get_Buffer (Advanced.Command_Descr);

      Get_Bounds (Buffer, Start, The_End);
      Delete (Buffer, Start, The_End);

      if Br.Commands /= null then
         Insert_At_Cursor (Buffer, Br.Commands.all);
      end if;

      --  Set the scope and action, if appropriate
      case Br.Scope is
         when No_Scope =>
            null;
         when Current_Task =>
            Set_Active (Advanced.Scope_Task, True);
         when Tasks_In_PD =>
            Set_Active (Advanced.Scope_Pd, True);
         when Any_Task =>
            Set_Active (Advanced.Scope_Any, True);
      end case;

      case Br.Action is
         when No_Action =>
            null;
         when Current_Task =>
            Set_Active (Advanced.Action_Task, True);
         when Tasks_In_PD =>
            Set_Active (Advanced.Action_Pd, True);
         when All_Tasks =>
            Set_Active (Advanced.Action_All, True);
      end case;

      Set_Active (Advanced.Set_Default, False);
   end Fill_Advanced_Dialog;

   ------------------
   -- Set_Advanced --
   ------------------

   procedure Set_Advanced
     (View : access Breakpoint_Editor_Record'Class;
      Br   : Breakpoint_Data)
   is
      Adv  : constant Advanced_Breakpoint_Access := View.Advanced_Breakpoints;
      Process : constant Visual_Debugger := Get_Process (View);
      Modified : Boolean := False;
      Start, The_End : Gtk_Text_Iter;

   begin
      if Adv.Condition_Box.Get_Visible then
         Get_Bounds (Get_Buffer (Adv.Command_Descr), Start, The_End);

         declare
            S : constant String :=
                  Get_Active_Text (Adv.Condition_Combo);
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
                 (Process.Debugger, Br.Num, S, Internal);
               Modified := True;
            end if;

            if C /= 0
              or else Br.Ignore /= 0
            then
               Set_Breakpoint_Ignore_Count
                 (Process.Debugger, Br.Num, C, Internal);
               Modified := True;
            end if;

            if T /= ""
              or else (Br.Commands /= null and then Br.Commands.all /= "")
            then
               Set_Breakpoint_Command
                 (Process.Debugger, Br.Num, T, Internal);
               Modified := True;
            end if;

            if Modified then
               Update_Breakpoints (Process, Force => True);
            end if;
         end;
      end if;
   end Set_Advanced;

   ------------------------------
   -- On_Add_Exception_Clicked --
   ------------------------------

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (Object);

      Temporary : Boolean;
      Process   : constant Visual_Debugger := Get_Process (View);
      Name      : constant String :=
                    Get_Text
                      (Gtk_Entry (View.Editor.Exception_Name.Get_Child));
      Unhandled : constant Boolean :=
                    Get_Active (View.Editor.Stop_Not_Handled_Exception);

   begin
      Temporary := Get_Active (View.Editor.Temporary_Exception);

      --  Some of the strings below deal with the GUI, and thus should be
      --  translated for internationalization. Others come from gdb, and
      --  should not be translated.
      --  This explains why some are preceded by '-'.

      if Name = -"All Ada exceptions" then
         Break_Exception
           (Process.Debugger,
            Name      => "",
            Unhandled => Unhandled,
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);

      elsif Name = -"Ada assertions" then
         Break_Subprogram
           (Process.Debugger,
            Name      => "assert",
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);

      else
         Break_Exception
           (Process.Debugger,
            Name      => Name,
            Unhandled => Unhandled,
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;
   end On_Add_Exception_Clicked;

   -----------------------------
   -- On_Add_Location_Clicked --
   -----------------------------

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Process   : constant Visual_Debugger := Get_Process (View);
      Temporary : Boolean;

   begin
      Temporary := Get_Active (View.Editor.Temporary_Location);

      if Get_Active (View.Editor.Location_Selected) then
         declare
            File : constant Filesystem_String :=
                     +Get_Text (View.Editor.File_Name);
            --  ??? What if the filesystem path is non-UTF8?

            Line : constant Integer :=
              Integer (Get_Value_As_Int (View.Editor.Line_Spin));

         begin
            --  ??? Should also check Temporary
            Break_Source
              (Process.Debugger,
               File      => Create_From_Base (File),
               Line      => Line,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end;

      elsif Get_Active (View.Editor.Subprogram_Selected) then
         declare
            Name : constant String :=
                     Get_Active_Text (View.Editor.Subprogram_Combo);

         begin
            --  ??? Should also check Temporary
            Break_Subprogram
              (Process.Debugger,
               Name      => Name,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end;

      elsif Get_Active (View.Editor.Address_Selected) then
         declare
            Address : constant Address_Type :=
                        String_To_Address
                          (Get_Active_Text (View.Editor.Address_Combo));
         begin
            Break_Address
              (Process.Debugger,
               Address   => Address,
               Temporary => Temporary,
               Mode      => GVD.Types.Visible);
         end;

      else
         Break_Regexp
           (Process.Debugger,
            Regexp    => Get_Active_Text (View.Editor.Regexp_Combo),
            Temporary => Temporary,
            Mode      => GVD.Types.Visible);
      end if;
   end On_Add_Location_Clicked;

   ----------------------------------
   -- On_Advanced_Location_Clicked --
   ----------------------------------

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Current : constant Integer := Get_Selection_Index (View);

      Process : constant Visual_Debugger := Get_Process (View);
      Adv  : Advanced_Breakpoint_Access := View.Advanced_Breakpoints;
   begin
      if Current = -1 then
         return;
      end if;

      --  Create all three dialogs

      if Adv = null then
         Gtk_New (View.Advanced_Breakpoints);
         Adv := View.Advanced_Breakpoints;
         Set_Sensitive (Adv.Record_Button, False);
         Set_Sensitive (Adv.End_Button, False);
      end if;

      Fill_Advanced_Dialog (Adv, Process.Breakpoints (Current));
      Show_All (Adv);

      if VxWorks_Version (Process.Debugger) = Vx653 then
         Set_Show_Tabs (Adv.Main_Notebook);
      else
         Hide (Adv.Scope_Box);
         Set_Show_Tabs (Adv.Main_Notebook, False);
      end if;

      Adv.Response_Action := Gtk_Response_None;
      Gtk.Main.Main;

      if Adv.Response_Action = Gtk_Response_Apply then
         --  If we are using AE and the user has activated the "Set as
         --  default" checkbox for the scope and action values, send the
         --  appropriate commands to the debugger

         if VxWorks_Version (Process.Debugger) = Vx653 then
            declare
               Scope_Value  : Scope_Type;
               Action_Value : Action_Type;
            begin
               if Get_Active (Adv.Scope_Task) then
                  Scope_Value := Current_Task;
               elsif Get_Active (Adv.Scope_Pd) then
                  Scope_Value := Tasks_In_PD;
               elsif Get_Active (Adv.Scope_Any) then
                  Scope_Value := Any_Task;
               end if;

               if Get_Active (Adv.Action_Task) then
                  Action_Value := Current_Task;
               elsif Get_Active (Adv.Action_Pd) then
                  Action_Value := Tasks_In_PD;
               elsif Get_Active (Adv.Action_All) then
                  Action_Value := All_Tasks;
               end if;

               if Get_Active (Adv.Set_Default) then
                  Set_Scope_Action
                    (Process.Debugger, Scope_Value, Action_Value);
               end if;

               Set_Scope_Action
                 (Process.Debugger, Scope_Value,
                  Action_Value, Process.Breakpoints (Current).Num);
            end;
         end if;

         Set_Advanced (View, Process.Breakpoints (Current));
      end if;
   end On_Advanced_Location_Clicked;

   -------------------------------
   -- On_Add_Watchpoint_Clicked --
   -------------------------------

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Process         : constant Visual_Debugger := Get_Process (View);
      Watchpoint_Name : constant String :=
                          Get_Text (View.Editor.Watchpoint_Name);
      Watchpoint_Type : constant String :=
                          Get_Active_Text (View.Editor.Watchpoint_Type);
      Watchpoint_Cond : constant String :=
                          Get_Text (View.Editor.Watchpoint_Cond);

      Trigger : GVD.Types.Watchpoint_Trigger;
      --  Encodes the value we get from Watchpoint_Type for the call to Watch

   begin
      if Watchpoint_Type = -"read" then
         Trigger := GVD.Types.Read;
      elsif Watchpoint_Type = -"read or written" then
         Trigger := GVD.Types.Read_Write;
      else
         --  presumably, Watchpoint_Type = -"written"
         Trigger := GVD.Types.Write;
      end if;

      Watch
        (Process.Debugger,
         Name      => Watchpoint_Name,
         Trigger   => Trigger,
         Condition => Watchpoint_Cond,
         Mode      => GVD.Types.Visible);
   end On_Add_Watchpoint_Clicked;

   ------------------------------------
   -- On_Load_Exception_List_Clicked --
   ------------------------------------

   procedure On_Load_Exception_List_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View    : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Process : constant Visual_Debugger := Get_Process (View);
   begin
      declare
         Exception_Arr : Exception_Array := List_Exceptions (Process.Debugger);
      begin
         if Exception_Arr'Length > 0 then
            Set_Sensitive (View.Editor.Hbox4, True);
            Add_Unique_Combo_Entry
              (View.Editor.Exception_Name, -"All Ada exceptions");
            Add_Unique_Combo_Entry
              (View.Editor.Exception_Name, -"Ada assertions");

            for J in Exception_Arr'Range loop
               Add_Unique_Combo_Entry
                 (View.Editor.Exception_Name, Exception_Arr (J).Name.all);
            end loop;
         else
            Set_Sensitive (View.Editor.Hbox4, False);
         end if;

         Free (Exception_Arr);
      end;
   end On_Load_Exception_List_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Process   : constant Visual_Debugger := Get_Process (View);
      Model     : constant Gtk_Tree_Store :=
        -Get_Model (View.Editor.Breakpoint_List);
      Selection : constant Integer := Get_Selection_Index (View);

   begin
      if Selection /= -1 then
         Remove_Breakpoint
           (Process.Debugger,
            Process.Breakpoints (Selection).Num,
            Mode => GVD.Types.Visible);

         --  Reselect the next line for convenience, so that the user can
         --  press "Remove" several times in a row

         if Gint (Selection) >= N_Children (Model, Null_Iter) then
            Select_Iter
              (Get_Selection (View.Editor.Breakpoint_List),
               Nth_Child
                 (Model,
                  Parent => Null_Iter,
                  N      => N_Children (Model, Null_Iter) - 1));
         else
            Select_Iter
              (Get_Selection (View.Editor.Breakpoint_List),
               Nth_Child
                 (Model,
                  Parent => Null_Iter,
                  N      => Gint (Selection)));
         end if;
      end if;
   end On_Remove_Clicked;

   ---------------------
   -- On_View_Clicked --
   ---------------------

   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View      : constant Breakpoint_Editor := Breakpoint_Editor (Object);
      Process   : constant Visual_Debugger := Get_Process (View);
      Selection : constant Integer := Get_Selection_Index (View);

   begin
      if Selection /= -1 then
         Load_File
           (Process.Editor_Text,
            Process.Breakpoints (Selection).File);
         Set_Line
           (Process.Editor_Text,
            Process.Breakpoints (Selection).Line,
            GObject (Process));
      end if;
   end On_View_Clicked;

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
   end On_Breakpoints_Key_Press_Event;

end Breakpoints_Editor;
