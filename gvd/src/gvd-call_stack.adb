------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2003-2016, AdaCore                     --
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

with GNAT.Strings;           use GNAT.Strings;

with Gdk.Event;              use Gdk.Event;
with Glib.Object;            use Glib.Object;
with Glib;                   use Glib;

with Gtk.Box;                use Gtk.Box;
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

with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

with Commands.Interactive;   use Commands, Commands.Interactive;
with Config;                 use Config;
with Debugger;               use Debugger;
with Generic_Views;          use Generic_Views;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Actions;     use GPS.Kernel.Actions;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Intl;               use GPS.Intl;
with GUI_Utils;              use GUI_Utils;
with GVD.Code_Editors;       use GVD.Code_Editors;
with GVD.Views;              use GVD.Views;
with GVD.Process;            use GVD.Process;
with GVD.Types;              use GVD.Types;
with GVD_Module;             use GVD_Module;
with Process_Proxies;        use Process_Proxies;
with XML_Utils;              use XML_Utils;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

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

   type Call_Stack_Record is new Base_Views.Process_View_Record with
      record
         Tree                       : Gtk_Tree_View;
         Model                      : Gtk_Tree_Store;
         Block                      : Boolean := False;
         --  Whether to process selection events.

         Backtrace_Mask             : Stack_List_Mask := Subprog_Name;
         --  What columns to be displayed in the stack list window
      end record;
   type Call_Stack is access all Call_Stack_Record'Class;

   overriding procedure Update (View   : access Call_Stack_Record);
   overriding procedure On_Process_Terminated
     (View : access Call_Stack_Record);
   overriding procedure On_State_Changed
     (View : access Call_Stack_Record; New_State : Debugger_State);
   overriding procedure Load_From_XML
     (View : access Call_Stack_Record; XML : XML_Utils.Node_Ptr);
   overriding procedure Save_To_XML
     (View : access Call_Stack_Record;
      XML  : in out XML_Utils.Node_Ptr);
   --  See inherited documentation

   function Initialize
     (Widget : access Call_Stack_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Generic_Views.Abstract_View_Access;
   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Generic_Views.Abstract_View_Access);
   --  Store or retrieve the view from the process

   type CS_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access CS_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context;

   package Simple_Views is new Base_Views.Simple_Views
     (Module_Name        => "Call_Stack",
      View_Name          => -"Call Stack",
      Formal_View_Record => Call_Stack_Record,
      Formal_MDI_Child   => CS_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);

   package Call_Stack_Cb is new Gtk.Handlers.User_Callback
     (Call_Stack_Record, Stack_List_Mask);

   procedure Set_Column_Types (Tree : access Call_Stack_Record'Class);
   --  Setup the columns.

   procedure Context_Factory
     (Context      : Selection_Context;
      Menu         : Gtk_Menu);
   --  Create the context for the contextual menus

   procedure Change_Mask
     (Stack  : access Call_Stack_Record'Class;
      Mask   : Stack_List_Mask);
   --  Toggle the display of a specific column in the Stack_List window.

   procedure On_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class);
   --  Callback for the selection change.

   type Call_Stack_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Call_Stack_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Debug->Data->Call Stack

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Generic_Views.Abstract_View_Access is
   begin
      return Generic_Views.Abstract_View_Access (Process.Stack);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Generic_Views.Abstract_View_Access)
   is
      Old : constant Call_Stack := Call_Stack (Process.Stack);
   begin
      Process.Stack := Gtk_Widget (View);

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
     (Command : access Call_Stack_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
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
      return Commands.Success;
   end Execute;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class)
   is
      Stack : constant Call_Stack := Call_Stack (Object);
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      if Get_Process (Stack) = null or else Stack.Block then
         return;
      end if;

      Get_Selected (Get_Selection (Stack.Tree), Model, Iter);
      if Iter /= Null_Iter then
         Stack_Frame
           (Get_Process (Stack).Debugger,
            Natural'Value
              (Get_String (Stack.Model, Iter, Frame_Num_Column)) + 1,
            GVD.Types.Visible);
      end if;
   end On_Selection_Changed;

   -----------------
   -- Change_Mask --
   -----------------

   procedure Change_Mask
     (Stack  : access Call_Stack_Record'Class;
      Mask   : Stack_List_Mask) is
   begin
      Stack.Backtrace_Mask := Stack.Backtrace_Mask xor Mask;
      Set_Column_Types (Stack);
   end Change_Mask;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access CS_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context
   is
      Stack   : constant Call_Stack :=
        Call_Stack (GPS_MDI_Child (Self).Get_Actual_Widget);
      Iter    : Gtk_Tree_Iter;
      Path    : Gtk_Tree_Path;
      Context : Selection_Context;
   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      if Event /= null then
         Iter := Find_Iter_For_Event (Stack.Tree, Event);
         if Iter /= Null_Iter then
            Path := Get_Path (Stack.Tree.Get_Model, Iter);
            Stack.Tree.Set_Cursor (Path, null, False);
            Path_Free (Path);
         end if;
      end if;
      return Context;
   end Build_Context;

   ---------------------
   -- Context_Factory --
   ---------------------

   procedure Context_Factory
     (Context : Selection_Context;
      Menu    : Gtk_Menu)
   is
      Stack : Call_Stack;
      Check : Gtk_Check_Menu_Item;
   begin
      --  ??? Should be in local config menu instead
      Stack := Call_Stack
        (GPS_MDI_Child (Get_MDI (Get_Kernel (Context)).Get_Focus_Child)
         .Get_Actual_Widget);
      if Stack = null then
         return;
      end if;

      Gtk_New (Check, Label => -"Frame Number");
      Set_Active (Check, (Stack.Backtrace_Mask and Frame_Num) /= 0);
      Append (Menu, Check);
      Call_Stack_Cb.Object_Connect
        (Check, Signal_Activate, Change_Mask'Access, Stack, Frame_Num);

      Gtk_New (Check, Label => -"Program Counter");
      Set_Active (Check, (Stack.Backtrace_Mask and Program_Counter) /= 0);
      Append (Menu, Check);
      Call_Stack_Cb.Object_Connect
        (Check, Signal_Activate,
         Change_Mask'Access, Stack, Program_Counter);

      Gtk_New (Check, Label => -"Subprogram Name");
      Set_Active (Check, (Stack.Backtrace_Mask and Subprog_Name) /= 0);
      Append (Menu, Check);
      Call_Stack_Cb.Object_Connect
        (Check, Signal_Activate, Change_Mask'Access, Stack, Subprog_Name);

      Gtk_New (Check, Label => -"Parameters");
      Set_Active (Check, (Stack.Backtrace_Mask and Params) /= 0);
      Append (Menu, Check);
      Call_Stack_Cb.Object_Connect
        (Check, Signal_Activate, Change_Mask'Access, Stack, Params);

      Gtk_New (Check, Label => -"File Location");
      Set_Active (Check, (Stack.Backtrace_Mask and File_Location) /= 0);
      Append (Menu, Check);
      Call_Stack_Cb.Object_Connect
        (Check, Signal_Activate, Change_Mask'Access, Stack, File_Location);
   end Context_Factory;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : access Call_Stack_Record'Class) is
   begin
      Set_Visible (Get_Column (Tree.Tree, 0),
                   (Tree.Backtrace_Mask and Frame_Num) /= 0);
      Set_Visible (Get_Column (Tree.Tree, 1),
                   (Tree.Backtrace_Mask and Program_Counter) /= 0);
      Set_Visible (Get_Column (Tree.Tree, 2),
                   (Tree.Backtrace_Mask and Subprog_Name) /= 0);
      Set_Visible (Get_Column (Tree.Tree, 3),
                   (Tree.Backtrace_Mask and Params) /= 0);
      Set_Visible (Get_Column (Tree.Tree, 4),
                   (Tree.Backtrace_Mask and File_Location) /= 0);
   end Set_Column_Types;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Call_Stack_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) return Gtk_Widget
   is
      pragma Unreferenced (Kernel);
      Name_Frame   : aliased String := -"Num";
      Name_Counter : aliased String := -"PC";
      Name_Subprog : aliased String := -"Subprogram";
      Name_Params  : aliased String := -"Parameters";
      Name_Loc     : aliased String := -"Location";
      Scrolled     : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (Widget, Homogeneous => False);

      Gtk_New (Scrolled);
      Widget.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      Widget.Tree := Create_Tree_View
        (Column_Types => (Frame_Num_Column       => GType_String,
                          Program_Counter_Column => GType_String,
                          Subprog_Name_Column    => GType_String,
                          Params_Column          => GType_String,
                          File_Location_Column   => GType_String),
         Column_Names =>
           (1 + Frame_Num_Column       => Name_Frame'Unchecked_Access,
            1 + Program_Counter_Column => Name_Counter'Unchecked_Access,
            1 + Subprog_Name_Column    => Name_Subprog'Unchecked_Access,
            1 + Params_Column          => Name_Params'Unchecked_Access,
            1 + File_Location_Column   => Name_Loc'Unchecked_Access),
         Sortable_Columns => False);
      Widget.Model := -Get_Model (Widget.Tree);

      Scrolled.Add (Widget.Tree);

      Set_Column_Types (Widget);

      Setup_Contextual_Menu
        (Kernel          => Get_Kernel (Debugger_Module_ID.all),
         Event_On_Widget => Widget.Tree,
         Context_Func    => Context_Factory'Access);

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Get_Selection (Widget.Tree), Signal_Changed,
         On_Selection_Changed'Access, Widget);

      return Gtk_Widget (Widget.Tree);
   end Initialize;

   --------------------------------
   -- Highlight_Call_Stack_Frame --
   --------------------------------

   procedure Highlight_Call_Stack_Frame
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
   is
      S           : Call_Stack;
      Frame       : Unbounded_String;
      Frame_Info  : Frame_Info_Type := Location_Not_Found;
      Path        : Gtk_Tree_Path;

   begin
      if Process.Stack /= null then
         S := Call_Stack (Process.Stack);

         Found_Frame_Info
           (Process.Debugger,
            Process.Current_Output
              (Process.Current_Output'First .. Process.Current_Output_Pos - 1),
            Frame, Frame_Info);

         if Frame_Info = Location_Found then
            S.Block := True;
            Gtk_New (Path, To_String (Frame));
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
      Create_If_Necessary : Boolean) renames Simple_Views.Attach_To_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Simple_Views.Register_Desktop_Functions (Kernel);
      Register_Action
        (Kernel, "open debugger call stack", new Call_Stack_Command,
         -"Open the Call Stack window for the debugger",
         Category => -"Views");
   end Register_Module;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (View : access Call_Stack_Record; XML : XML_Utils.Node_Ptr) is
   begin
      View.Backtrace_Mask := Stack_List_Mask'Value (XML.Value.all);
   exception
      when others =>
         View.Backtrace_Mask := Subprog_Name;
   end Load_From_XML;

   -----------------
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (View : access Call_Stack_Record;
      XML  : in out XML_Utils.Node_Ptr)
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("mask");
      N.Value := new String'(Stack_List_Mask'Image (View.Backtrace_Mask));
      XML.Child := N;
   end Save_To_XML;

   ----------------------
   -- On_State_Changed --
   ----------------------

   overriding procedure On_State_Changed
     (View : access Call_Stack_Record; New_State : Debugger_State)
   is
      Iter : Gtk_Tree_Iter;
      Prev : Boolean;
   begin
      if New_State = Debug_Busy then
         --  The debugger is now executing a command that will likely change
         --  the current stack trace. While it is executing, we do not want to
         --  keep a visible call stack displayed.

         if Is_Execution_Command (Get_Process (View)) then
            --  Calling Clear might cause the selection to jump from row to
            --  row, causing a query of every frame info. To prevent this,
            --  set the Block flag.
            Prev := View.Block;
            View.Block := True;
            Clear (View.Model);
            View.Block := Prev;

            Append (View.Model, Iter, Null_Iter);
            Set (View.Model, Iter, Frame_Num_Column, 0);
            Set (View.Model, Iter, Subprog_Name_Column, "Running...");
            Set_Mode (Get_Selection (View.Tree), Selection_None);
         end if;
      end if;
   end On_State_Changed;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : access Call_Stack_Record)
   is
      Prev : Boolean;
   begin
      Prev := View.Block;
      View.Block := True;
      Clear (View.Model);
      View.Block := Prev;
   end On_Process_Terminated;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : access Call_Stack_Record) is
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Process  : Process_Proxy_Access;
      Index    : Integer;
      Subp     : GNAT.Strings.String_Access;
      Iter     : Gtk_Tree_Iter;

   begin
      --  Remove previous stack information.

      View.Block := True;
      Clear (View.Model);

      if Get_Process (View) /= null then
         Process := Get_Process (Get_Process (View).Debugger);
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         View.Block := False;
         return;
      end if;

      --  Parse the information from the debugger

      Backtrace (Get_Process (View).Debugger, Bt, Len);

      --  Update the contents of the window

      for J in 1 .. Len loop
         --  ??? We currently consider that the list of parameters always
         --  starts at the first '(' character encountered

         Subp := Bt (J).Subprogram;
         Index := Subp'First;

         while Index <= Subp'Last and then Subp (Index) /= '(' loop
            Index := Index + 1;
         end loop;

         Append (View.Model, Iter, Null_Iter);

         Set (View.Model, Iter, Frame_Num_Column,
              Natural'Image (Bt (J).Frame_Id));

         Set (View.Model, Iter, Program_Counter_Column,
              Bt (J).Program_Counter.all);

         Set (View.Model, Iter, Subprog_Name_Column,
              Subp (Subp'First .. Index - 1));

         Set (View.Model, Iter, Params_Column,
              Subp (Index .. Subp'Last));

         Set (View.Model, Iter, File_Location_Column,
              Bt (J).Source_Location.all);
      end loop;

      Free (Bt (1 .. Len));

      Set_Mode (Get_Selection (View.Tree), Selection_Single);

      View.Block := True;
      if Get_Iter_First (View.Model) /= Null_Iter then
         Select_Iter (Get_Selection (View.Tree), Get_Iter_First (View.Model));
      end if;
      View.Block := False;
   end Update;

end GVD.Call_Stack;
