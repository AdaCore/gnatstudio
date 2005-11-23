-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                       Copyright (C) 2003-2005                     --
--                             AdaCore                               --
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

with Ada.Exceptions;         use Ada.Exceptions;

with Gdk.Event;              use Gdk.Event;

with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;
with Glib.Xml_Int;
with Glib;                   use Glib;

with Gtk.Arguments;          use Gtk.Arguments;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Check_Menu_Item;    use Gtk.Check_Menu_Item;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;

with Gtkada.Dialogs;         use Gtkada.Dialogs;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

with Basic_Types;            use Basic_Types;
with Config;                 use Config;
with Debugger;               use Debugger;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Intl;               use GPS.Intl;
with GVD.Code_Editors;       use GVD.Code_Editors;
with GVD_Module;             use GVD_Module;
with GVD.Process;            use GVD.Process;
with GVD.Types;              use GVD.Types;
with Process_Proxies;        use Process_Proxies;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;

package body GVD.Call_Stack is

   ---------------------
   -- Local constants --
   ---------------------

   Frame_Num_Column       : constant := 0;
   Program_Counter_Column : constant := 1;
   Subprog_Name_Column    : constant := 2;
   Params_Column          : constant := 3;
   File_Location_Column   : constant := 4;

   -----------------------
   -- Local subprograms --
   -----------------------

   type Stack_List_Mask is mod 2 ** 16;
   Frame_Num       : constant Stack_List_Mask := 2 ** 0;
   Program_Counter : constant Stack_List_Mask := 2 ** 1;
   Subprog_Name    : constant Stack_List_Mask := 2 ** 2;
   Params          : constant Stack_List_Mask := 2 ** 3;
   File_Location   : constant Stack_List_Mask := 2 ** 4;
   --  Lists the information to be displayed in the stack list window.

   type Call_Stack_Record is new Gtk_Scrolled_Window_Record with record
      Tree                       : Gtk_Tree_View;
      Model                      : Gtk_Tree_Store;
      Process                    : Visual_Debugger;
      Call_Stack_Contextual_Menu : Gtk.Menu.Gtk_Menu;
      Block                      : Boolean := False;
      --  Whether to process selection events.

      Backtrace_Mask             : Stack_List_Mask := Subprog_Name;
      --  What columns to be displayed in the stack list window
   end record;
   type Call_Stack is access all Call_Stack_Record'Class;

   procedure Gtk_New (Widget : out Call_Stack);
   --  Create a new call stack dialog.

   procedure Initialize (Widget : access Call_Stack_Record'Class);
   --  Internal initialization function.

   type Call_Stack_Frame_Record is record
      Stack : Call_Stack;
      Mask  : Stack_List_Mask;
   end record;
   package Call_Stack_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, Call_Stack_Frame_Record);

   procedure Update_Call_Stack
     (Stack : access Glib.Object.GObject_Record'Class);
   --  Update the call stack. This is meant as a callback for gtk+ signals

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   procedure Set_Column_Types (Tree : access Call_Stack_Record'Class);
   --  Setup the columns.

   function On_Button_Press_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : GValues) return Boolean;
   --  Callback for the "button_press" signal on the stack list.

   function Call_Stack_Contextual_Menu
     (Process : access Call_Stack_Record'Class)
      return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used in the
   --  debugger call stack window.

   procedure Change_Mask
     (Widget : access Gtk_Menu_Item_Record'Class;
      Mask   : Call_Stack_Frame_Record);
   --  Toggle the display of a specific column in the Stack_List window.

   procedure On_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class;
      Params : GValues);
   --  Callback for the selection change.

   function Create_Call_Stack
     (MDI : access MDI_Window_Record'Class) return MDI_Child;
   --  Create a new callstack in the MDI

   function Load_Desktop
     (MDI    : MDI_Window;
      Node   : Glib.Xml_Int.Node_Ptr;
      Kernel : GPS.Kernel.Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle) return Glib.Xml_Int.Node_Ptr;
   --  Desktop-related functions (see Gtkada.MDI)

   function On_Stack_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the Call Stack window.

   procedure On_Call_Stack
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Call Stack

   procedure On_Debugger_Terminate
     (Stack : access GObject_Record'Class);
   --  Called when the debugger is terminated

   ---------------------------
   -- On_Debugger_Terminate --
   ---------------------------

   procedure On_Debugger_Terminate
     (Stack : access GObject_Record'Class)
   is
      C : constant Call_Stack := Call_Stack (Stack);
   begin
      Destroy (C);
   end On_Debugger_Terminate;

   -------------------
   -- On_Call_Stack --
   -------------------

   procedure On_Call_Stack
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Process : Visual_Debugger;
      List    : Debugger_List_Link := Get_Debugger_List (Kernel);

   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            Attach_To_Call_Stack (Process, Create_If_Necessary => True);
         end if;

         List := List.Next;
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Call_Stack;

   ---------------------------
   -- On_Stack_Delete_Event --
   ---------------------------

   function On_Stack_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
      Process : constant Visual_Debugger := Visual_Debugger (Object);
   begin
      Process.Stack := null;
      return False;
   end On_Stack_Delete_Event;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class;
      Params : GValues)
   is
      pragma Unreferenced (Params);
      Stack : constant Call_Stack := Call_Stack (Object);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      if Stack.Process = null or else Stack.Block then
         return;
      end if;

      Get_Selected (Get_Selection (Stack.Tree), Model, Iter);
      Stack_Frame
        (Stack.Process.Debugger,
         Natural'Value (Get_String (Stack.Model, Iter, Frame_Num_Column)) + 1,
         GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Selection_Changed;

   -----------------
   -- Change_Mask --
   -----------------

   procedure Change_Mask
     (Widget : access Gtk_Menu_Item_Record'Class;
      Mask   : Call_Stack_Frame_Record)
   is
      pragma Unreferenced (Widget);
   begin
      Mask.Stack.Backtrace_Mask := Mask.Stack.Backtrace_Mask xor Mask.Mask;
      Set_Column_Types (Mask.Stack);
   end Change_Mask;

   --------------------------------
   -- Call_Stack_Contextual_Menu --
   --------------------------------

   function Call_Stack_Contextual_Menu
     (Process : access Call_Stack_Record'Class)
      return Gtk.Menu.Gtk_Menu
   is
      Check : Gtk_Check_Menu_Item;
   begin
      --  Destroy the old menu (We need to recompute the state of the toggle
      --  buttons)

      if Process.Call_Stack_Contextual_Menu /= null then
         Destroy (Process.Call_Stack_Contextual_Menu);
      end if;

      Gtk_New (Process.Call_Stack_Contextual_Menu);
      Gtk_New (Check, Label => -"Frame Number");
      Set_Active (Check, (Process.Backtrace_Mask and Frame_Num) /= 0);
      Append (Process.Call_Stack_Contextual_Menu, Check);
      Call_Stack_Cb.Connect
        (Check, "activate",
         Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
         (Call_Stack (Process), Frame_Num));

      Gtk_New (Check, Label => -"Program Counter");
      Set_Active (Check, (Process.Backtrace_Mask and Program_Counter) /= 0);
      Append (Process.Call_Stack_Contextual_Menu, Check);
      Call_Stack_Cb.Connect
        (Check, "activate",
         Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
         (Call_Stack (Process), Program_Counter));

      Gtk_New (Check, Label => -"Subprogram Name");
      Set_Active (Check, (Process.Backtrace_Mask and Subprog_Name) /= 0);
      Append (Process.Call_Stack_Contextual_Menu, Check);
      Call_Stack_Cb.Connect
        (Check, "activate",
         Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
         (Call_Stack (Process), Subprog_Name));

      Gtk_New (Check, Label => -"Parameters");
      Set_Active (Check, (Process.Backtrace_Mask and Params) /= 0);
      Append (Process.Call_Stack_Contextual_Menu, Check);
      Call_Stack_Cb.Connect
        (Check, "activate",
         Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
         (Call_Stack (Process), Params));

      Gtk_New (Check, Label => -"File Location");
      Set_Active (Check, (Process.Backtrace_Mask and File_Location) /= 0);
      Append (Process.Call_Stack_Contextual_Menu, Check);
      Call_Stack_Cb.Connect
        (Check, "activate",
         Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
         (Call_Stack (Process), File_Location));

      Show_All (Process.Call_Stack_Contextual_Menu);
      return Process.Call_Stack_Contextual_Menu;
   end Call_Stack_Contextual_Menu;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   function On_Button_Press_Event
     (Object : access GObject_Record'Class;
      Params : GValues) return Boolean
   is
      Arg1    : constant Gdk_Event := Get_Event (Nth (Params, 1));
      Process : constant Call_Stack := Call_Stack (Object);

   begin
      if Get_Button (Arg1) = 3
        and then Get_Event_Type (Arg1) = Button_Press
      then
         Popup (Call_Stack_Contextual_Menu (Process),
                Button        => Gdk.Event.Get_Button (Arg1),
                Activate_Time => Gdk.Event.Get_Time (Arg1));
         Emit_Stop_By_Name (Process.Tree, "button_press_event");

         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Button_Press_Event;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Frame_Num_Column       => GType_String,
         Program_Counter_Column => GType_String,
         Subprog_Name_Column    => GType_String,
         Params_Column          => GType_String,
         File_Location_Column   => GType_String);
   end Columns_Types;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : access Call_Stack_Record'Class) is
      Col           : Gtk_Tree_View_Column;
      Text_Rend     : Gtk_Cell_Renderer_Text;

      Dummy         : Gint;
      pragma Unreferenced (Dummy);

   begin
      --  Remove all columns

      Col := Get_Column (Tree.Tree, 0);

      while Col /= null loop
         Dummy := Remove_Column (Tree.Tree, Col);
         Col := Get_Column (Tree.Tree, 0);
      end loop;

      if (Tree.Backtrace_Mask and Frame_Num) /= 0 then
         Gtk_New (Text_Rend);
         Gtk_New (Col);
         Pack_Start (Col, Text_Rend, True);
         Add_Attribute (Col, Text_Rend, "text", Frame_Num_Column);
         Set_Title (Col, -"Num");
         Dummy := Append_Column (Tree.Tree, Col);
         Set_Expander_Column (Tree.Tree, Col);
         Set_Resizable (Col, True);
      end if;

      if (Tree.Backtrace_Mask and Program_Counter) /= 0 then
         Gtk_New (Text_Rend);
         Gtk_New (Col);
         Pack_Start (Col, Text_Rend, True);
         Add_Attribute (Col, Text_Rend, "text", Program_Counter_Column);
         Set_Title (Col, -"PC");
         Dummy := Append_Column (Tree.Tree, Col);
         Set_Resizable (Col, True);
      end if;

      if (Tree.Backtrace_Mask and Subprog_Name) /= 0 then
         Gtk_New (Text_Rend);
         Gtk_New (Col);
         Pack_Start (Col, Text_Rend, True);
         Add_Attribute (Col, Text_Rend, "text", Subprog_Name_Column);
         Set_Title (Col, -"Subprogram");
         Dummy := Append_Column (Tree.Tree, Col);
         Set_Resizable (Col, True);
      end if;

      if (Tree.Backtrace_Mask and Params) /= 0 then
         Gtk_New (Text_Rend);
         Gtk_New (Col);
         Pack_Start (Col, Text_Rend, True);
         Add_Attribute (Col, Text_Rend, "text", Params_Column);
         Set_Title (Col, -"Parameters");
         Dummy := Append_Column (Tree.Tree, Col);
         Set_Resizable (Col, True);
      end if;

      if (Tree.Backtrace_Mask and File_Location) /= 0 then
         Gtk_New (Text_Rend);
         Gtk_New (Col);
         Pack_Start (Col, Text_Rend, True);
         Add_Attribute (Col, Text_Rend, "text", File_Location_Column);
         Set_Title (Col, -"Location");
         Dummy := Append_Column (Tree.Tree, Col);
         Set_Resizable (Col, True);
      end if;
   end Set_Column_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Call_Stack) is
   begin
      Widget := new Call_Stack_Record;
      GVD.Call_Stack.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Call_Stack_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Widget);

      Set_Policy (Widget, Policy_Automatic, Policy_Automatic);

      Gtk_New (Widget.Model, Columns_Types);
      Gtk_New (Widget.Tree, Widget.Model);

      Add (Widget, Widget.Tree);

      Set_Column_Types (Widget);

      Gtkada.Handlers.Object_Return_Callback.Object_Connect
        (Widget.Tree, "button_press_event",
         On_Button_Press_Event'Access, Widget);

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Get_Selection (Widget.Tree), "changed",
         On_Selection_Changed'Access, Widget);
   end Initialize;

   --------------------------------
   -- Highlight_Call_Stack_Frame --
   --------------------------------

   procedure Highlight_Call_Stack_Frame
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
   is
      S           : Call_Stack;
      First, Last : Natural := 0;
      Frame_Info  : Frame_Info_Type := Location_Not_Found;
      Path        : Gtk_Tree_Path;
   begin
      if Process.Stack /= null then
         S := Call_Stack (Process.Stack);

         Found_Frame_Info
           (Process.Debugger,
            Process.Current_Output.all,
            First, Last, Frame_Info);

         if Frame_Info = Location_Found then
            S.Block := True;
            Path := Gtk_New (Process.Current_Output (First .. Last));
            Select_Path (Get_Selection (S.Tree), Path);
            Path_Free (Path);
            S.Block := False;

         elsif Frame_Info = No_Debug_Info then
            Show_Message (Process.Editor_Text,
                          -"There is no debug information for this frame.");
         end if;
      end if;
   end Highlight_Call_Stack_Frame;

   --------------------------
   -- Attach_To_Call_Stack --
   --------------------------

   procedure Attach_To_Call_Stack
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
   is
      MDI     : constant MDI_Window :=
        Get_MDI (Get_Kernel (Debugger_Module_ID.all));
      Button  : Message_Dialog_Buttons;
      Child   : MDI_Child;
      Iter    : Child_Iterator;
      Stack   : Call_Stack;
      pragma Unreferenced (Button);
   begin
      if Debugger.Stack = null then
         --  Do we have an existing unattached callstack ?
         Iter := First_Child (MDI);
         loop
            Child := Get (Iter);
            exit when Child = null;

            if Get_Widget (Child).all in Call_Stack_Record'Class then
               Stack := Call_Stack (Get_Widget (Child));
               if Stack.Process = null then
                  Stack.Process := Visual_Debugger (Debugger);
                  exit;
               end if;
               Stack := null;
            end if;

            Next (Iter);
         end loop;

         --  If no exsting call stack was found, create one

         if Child = null and then Create_If_Necessary then
            Child := Create_Call_Stack (MDI);
            Stack := Call_Stack (Get_Widget (Child));
            Stack.Process := Visual_Debugger (Debugger);
         end if;

         if Child /= null then
            if Debugger.Debugger_Num = 1 then
               Set_Title (Child, -"Call Stack");
            else
               Set_Title
                 (Child, (-"Call Stack") & " <"
                  & Image (Debugger.Debugger_Num) & ">");
            end if;
         end if;

         Debugger.Stack := Gtk_Widget (Stack);

         if Debugger.Stack /= null then
            Gtkada.Handlers.Object_Return_Callback.Object_Connect
              (Debugger.Stack, "delete_event",
               On_Stack_Delete_Event'Access, Debugger);

            Gtkada.Handlers.Object_Callback.Object_Connect
              (Debugger, "process_stopped",
               Update_Call_Stack'Access,
               Slot_Object => Debugger.Stack);
            Object_Callback.Object_Connect
              (Debugger, "context_changed",
               Update_Call_Stack'Access,
               Slot_Object => Debugger.Stack);
            Object_Callback.Object_Connect
              (Debugger.Debugger_Text, "destroy",
               On_Debugger_Terminate'Access,
               Slot_Object => Debugger.Stack);

            if Debugger.Debugger /= null
              and then Get_Process (Debugger.Debugger) /= null
            then
               if Command_In_Process (Get_Process (Debugger.Debugger)) then
                  Button := Message_Dialog
                    ((-"Cannot show call stack while the debugger is busy.") &
                     ASCII.LF &
                     (-"Interrupt the debugger or wait for its availability."),
                     Dialog_Type => Warning,
                     Buttons     => Button_OK);
               else
                  Update_Call_Stack (Debugger.Stack);
               end if;
            end if;
         end if;
      else
         Child := Find_MDI_Child (MDI, Debugger.Stack);
         if Child /= null then
            Raise_Child (Child);
         else
            --  Something really bad happened: the stack window is not
            --  part of the MDI, reset it.
            Destroy (Debugger.Stack);
            Debugger.Stack := null;
         end if;
      end if;
   end Attach_To_Call_Stack;

   -----------------------
   -- Create_Call_Stack --
   -----------------------

   function Create_Call_Stack
     (MDI : access MDI_Window_Record'Class) return MDI_Child
   is
      Stack : Call_Stack;
      Child : GPS_MDI_Child;
   begin
      Gtk_New (Stack);
      Gtk_New (Child, Stack,
               Group  => Group_Debugger_Stack,
               Module => Debugger_Module_ID);
      Set_Title (Child, -"Call Stack");
      Put (MDI, Child, Initial_Position => Position_Right);
      Set_Focus_Child (Child);

      Stack.Process := null;
      return  MDI_Child (Child);
   end Create_Call_Stack;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug          : constant String := '/' & (-"_Debug") & '/';
      Data_Sub       : constant String := Debug & (-"D_ata") & '/';
   begin
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu (Kernel, Data_Sub, -"_Call Stack", "",
                     On_Call_Stack'Access, Ref_Item => -"Data Window",
                     Add_Before => False);
   end Register_Module;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI    : MDI_Window;
      Node   : Glib.Xml_Int.Node_Ptr;
      Kernel : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (Kernel);
   begin
      if Node.Tag.all = "Call_Stack" then
         return Create_Call_Stack (MDI);
      end if;
      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Kernel : Kernel_Handle) return Glib.Xml_Int.Node_Ptr
   is
      pragma Unreferenced (Kernel);
      N : Glib.Xml_Int.Node_Ptr;
   begin
      if Widget.all in Call_Stack_Record'Class then
         N     := new Glib.Xml_Int.Node;
         N.Tag := new String'("Call_Stack");
         return N;
      end if;
      return null;
   end Save_Desktop;

   -----------------------
   -- Update_Call_Stack --
   -----------------------

   procedure Update_Call_Stack
     (Stack : access Glib.Object.GObject_Record'Class)
   is
      S        : constant Call_Stack := Call_Stack (Stack);
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Process  : Process_Proxy_Access;
      Index    : Integer;
      Subp     : String_Access;
      Iter     : Gtk_Tree_Iter;

   begin
      --  Remove previous stack information.

      S.Block := True;
      Clear (S.Model);

      if S.Process /= null then
         Process := Get_Process (S.Process.Debugger);
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         S.Block := False;
         return;
      end if;

      --  Parse the information from the debugger

      Backtrace (S.Process.Debugger, Bt, Len);

      --  Update the contents of the window

      for J in 1 .. Len loop
         --  ??? We currently consider that the list of parameters always
         --  starts at the first '(' character encountered

         Subp := Bt (J).Subprogram;
         Index := Subp'First;

         while Index <= Subp'Last and then Subp (Index) /= '(' loop
            Index := Index + 1;
         end loop;

         Append (S.Model, Iter, Null_Iter);

         Set (S.Model, Iter, Frame_Num_Column,
              Natural'Image (Bt (J).Frame_Id));

         Set (S.Model, Iter, Program_Counter_Column,
              Bt (J).Program_Counter.all);

         Set (S.Model, Iter, Subprog_Name_Column,
              Subp (Subp'First .. Index - 1));

         Set (S.Model, Iter, Params_Column,
              Subp (Index .. Subp'Last));

         Set (S.Model, Iter, File_Location_Column,
              Bt (J).Source_Location.all);
      end loop;

      Free (Bt (1 .. Len));

      S.Block := True;
      if Get_Iter_First (S.Model) /= Null_Iter then
         Select_Iter (Get_Selection (S.Tree), Get_Iter_First (S.Model));
      end if;
      S.Block := False;
   end Update_Call_Stack;

end GVD.Call_Stack;
