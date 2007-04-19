-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
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

with Ada.Exceptions;         use Ada.Exceptions;
with GNAT.Strings;           use GNAT.Strings;

with Gdk.Event;              use Gdk.Event;

with Glib.Object;            use Glib.Object;
with Glib.Xml_Int;           use Glib.Xml_Int;
with Glib;                   use Glib;

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

with Config;                 use Config;
with Debugger;               use Debugger;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Intl;               use GPS.Intl;
with GUI_Utils;              use GUI_Utils;
with GVD.Code_Editors;       use GVD.Code_Editors;
with GVD.Views;              use GVD.Views;
with GVD_Module;             use GVD_Module;
with GVD.Process;            use GVD.Process;
with GVD.Types;              use GVD.Types;
with Process_Proxies;        use Process_Proxies;
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

   type Call_Stack_Record is new Scrolled_Views.Process_View_Record with
      record
         Tree                       : Gtk_Tree_View;
         Model                      : Gtk_Tree_Store;
         Block                      : Boolean := False;
         --  Whether to process selection events.

         Backtrace_Mask             : Stack_List_Mask := Subprog_Name;
         --  What columns to be displayed in the stack list window
      end record;
   type Call_Stack is access all Call_Stack_Record'Class;

   procedure Update (View   : access Call_Stack_Record);
   procedure Load_From_XML
     (View : access Call_Stack_Record; XML : Glib.Xml_Int.Node_Ptr);
   function Save_To_XML
     (View : access Call_Stack_Record) return Glib.Xml_Int.Node_Ptr;
   --  See inherited documentation

   procedure Initialize
     (Widget : access Call_Stack_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Internal initialization function.

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window;
   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window);
   --  Store or retrieve the view from the process

   package Simple_Views is new Scrolled_Views.Simple_Views
     (Module_Name        => "Call_Stack",
      View_Name          => -"Call Stack",
      Formal_View_Record => Call_Stack_Record,
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
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu);
   --  Create the context for the contextual menus

   procedure Change_Mask
     (Stack  : access Call_Stack_Record'Class;
      Mask   : Stack_List_Mask);
   --  Toggle the display of a specific column in the Stack_List window.

   procedure On_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class);
   --  Callback for the selection change.

   procedure On_Call_Stack
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Call Stack

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window
   is
   begin
      return Gtk_Scrolled_Window (Process.Stack);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window) is
   begin
      Process.Stack := Gtk_Widget (View);
   end Set_View;

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
      Stack_Frame
        (Get_Process (Stack).Debugger,
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
     (Stack  : access Call_Stack_Record'Class;
      Mask   : Stack_List_Mask) is
   begin
      Stack.Backtrace_Mask := Stack.Backtrace_Mask xor Mask;
      Set_Column_Types (Stack);
   end Change_Mask;

   ---------------------
   -- Context_Factory --
   ---------------------

   procedure Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Kernel, Event_Widget, Event, Context);
      Stack : constant Call_Stack := Call_Stack (Object);
      Check : Gtk_Check_Menu_Item;
   begin
      if Menu /= null then
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
      end if;
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

   procedure Initialize
     (Widget : access Call_Stack_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Name_Frame   : aliased String := -"Num";
      Name_Counter : aliased String := -"PC";
      Name_Subprog : aliased String := -"Subprogram";
      Name_Params  : aliased String := -"Parameters";
      Name_Loc     : aliased String := -"Location";
   begin
      Gtk.Scrolled_Window.Initialize (Widget);
      Set_Policy (Widget, Policy_Automatic, Policy_Automatic);

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
      Widget.Model := Gtk_Tree_Store (Get_Model (Widget.Tree));

      Add (Widget, Widget.Tree);

      Set_Column_Types (Widget);

      Register_Contextual_Menu
        (Kernel          => Get_Kernel (Debugger_Module_ID.all),
         Event_On_Widget => Widget.Tree,
         Object          => Widget,
         ID              => Debugger_Module_ID,
         Context_Func    => Context_Factory'Access);

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Get_Selection (Widget.Tree), Signal_Changed,
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
      Create_If_Necessary : Boolean) renames Simple_Views.Attach_To_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug          : constant String := '/' & (-"_Debug") & '/';
      Data_Sub       : constant String := Debug & (-"D_ata") & '/';
   begin
      Simple_Views.Register_Desktop_Functions (Kernel);
      Register_Menu (Kernel, Data_Sub, -"_Call Stack", "",
                     On_Call_Stack'Access, Ref_Item => -"Data Window",
                     Add_Before => False);
   end Register_Module;

   -------------------
   -- Load_From_XML --
   -------------------

   procedure Load_From_XML
     (View : access Call_Stack_Record; XML : Glib.Xml_Int.Node_Ptr) is
   begin
      View.Backtrace_Mask := Stack_List_Mask'Value (XML.Value.all);
   exception
      when others =>
         View.Backtrace_Mask := Subprog_Name;
   end Load_From_XML;

   -----------------
   -- Save_To_XML --
   -----------------

   function Save_To_XML
     (View : access Call_Stack_Record) return Glib.Xml_Int.Node_Ptr
   is
      N : constant Node_Ptr := new Node;
   begin
      N.Tag   := new String'("mask");
      N.Value := new String'(Stack_List_Mask'Image (View.Backtrace_Mask));
      return N;
   end Save_To_XML;

   ------------
   -- Update --
   ------------

   procedure Update (View : access Call_Stack_Record) is
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

      View.Block := True;
      if Get_Iter_First (View.Model) /= Null_Iter then
         Select_Iter (Get_Selection (View.Tree), Get_Iter_First (View.Model));
      end if;
      View.Block := False;
   end Update;

end GVD.Call_Stack;
