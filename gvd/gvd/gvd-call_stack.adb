-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2003                        --
--                             ACT-Europe                            --
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

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;

with Gdk.Event;                use Gdk.Event;

with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Check_Menu_Item;      use Gtk.Check_Menu_Item;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Menu_Item;            use Gtk.Menu_Item;

with Gtkada.Handlers;          use Gtkada.Handlers;

with Odd_Intl;                 use Odd_Intl;
with GVD.Types;                use GVD.Types;
with Process_Proxies;          use Process_Proxies;
with Basic_Types;              use Basic_Types;
with String_Utils;             use String_Utils;

with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;

package body GVD.Call_Stack is

   Me : constant Debug_Handle := Create ("Debugger");

   type Call_Stack_Frame_Record is record
      Stack      : Call_Stack;
      Mask       : Stack_List_Mask;
   end record;
   package Call_Stack_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, Call_Stack_Frame_Record);

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
      if Stack.Debugger = null or else Stack.Block then
         return;
      end if;

      Get_Selected (Get_Selection (Stack.Tree), Model, Iter);
      Stack_Frame
        (Stack.Debugger,
         Natural'Value (Get_String (Stack.Model, Iter, Frame_Num_Column)) + 1,
         GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

      Object_Return_Callback.Object_Connect
        (Widget.Tree, "button_press_event",
         On_Button_Press_Event'Access, Widget);

      Object_Callback.Object_Connect
        (Get_Selection (Widget.Tree), "changed",
         On_Selection_Changed'Access, Widget);
   end Initialize;

   -------------------
   -- Get_List_Mask --
   -------------------

   function Get_List_Mask (Stack : Call_Stack) return Stack_List_Mask is
   begin
      return Stack.Backtrace_Mask;
   end Get_List_Mask;

   -------------------
   -- Set_List_Mask --
   -------------------

   procedure Set_List_Mask (Stack : Call_Stack; Mask : Stack_List_Mask) is
   begin
      Stack.Backtrace_Mask := Mask;
      Set_Column_Types (Stack);
   end Set_List_Mask;

   ---------------------
   -- Highlight_Frame --
   ---------------------

   procedure Highlight_Frame
     (Stack : Call_Stack;
      Frame : Natural)
   is
      Path : Gtk_Tree_Path;
   begin
      Stack.Block := True;

      Path := Gtk_New (Image (Frame));
      Select_Path (Get_Selection (Stack.Tree), Path);
      Path_Free (Path);

      Stack.Block := False;
   end Highlight_Frame;

   ------------
   -- Update --
   ------------

   procedure Update
     (Stack    : Call_Stack;
      Debugger : Debugger_Access)
   is
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Process  : constant Process_Proxy_Access := Get_Process (Debugger);
      Index    : Integer;
      Subp     : String_Access;
      Iter     : Gtk_Tree_Iter;

   begin
      --  Remove previous stack information.

      Stack.Block := True;
      Clear (Stack.Model);

      --  If the debugger was killed, no need to refresh

      if Process = null then
         Stack.Block := False;
         return;
      end if;

      --  Parse the information from the debugger

      Stack.Debugger := Debugger;
      Backtrace (Stack.Debugger, Bt, Len);

      --  Update the contents of the window

      for J in 1 .. Len loop
         --  ??? We currently consider that the list of parameters always
         --  starts at the first '(' character encountered

         Subp := Bt (J).Subprogram;
         Index := Subp'First;

         while Index <= Subp'Last and then Subp (Index) /= '(' loop
            Index := Index + 1;
         end loop;

         Append (Stack.Model, Iter, Null_Iter);

         Set (Stack.Model, Iter, Frame_Num_Column,
              Natural'Image (Bt (J).Frame_Id));

         Set (Stack.Model, Iter, Program_Counter_Column,
              Bt (J).Program_Counter.all);

         Set (Stack.Model, Iter, Subprog_Name_Column,
              Subp (Subp'First .. Index - 1));

         Set (Stack.Model, Iter, Params_Column,
              Subp (Index .. Subp'Last));

         Set (Stack.Model, Iter, File_Location_Column,
              Bt (J).Source_Location.all);
      end loop;

      Free (Bt (1 .. Len));

      Highlight_Frame (Stack, 0);
      Stack.Block := False;
   end Update;

end GVD.Call_Stack;
