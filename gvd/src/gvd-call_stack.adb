------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Glib;                   use Glib;
with Glib.Convert;           use Glib.Convert;
with Glib.Object;            use Glib.Object;
with Glib_Values_Utils;      use Glib_Values_Utils;

with Gtk.Box;                use Gtk.Box;
with Gtk.Check_Menu_Item;    use Gtk.Check_Menu_Item;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;

with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

with Debugger;               use Debugger;
with Default_Preferences;    use Default_Preferences;
with Generic_Views;          use Generic_Views;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GPS.Debuggers;          use GPS.Debuggers;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Intl;               use GPS.Intl;
with GUI_Utils;              use GUI_Utils;
with GVD.Code_Editors;       use GVD.Code_Editors;
with GVD.Generic_View;       use GVD.Generic_View;
with GVD.Process;            use GVD.Process;
with GVD.Types;              use GVD.Types;
with GVD_Module;             use GVD_Module;
with Process_Proxies;        use Process_Proxies;
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

   Column_Types : constant GType_Array :=
     (Frame_Num_Column       => GType_String,
      Program_Counter_Column => GType_String,
      Subprog_Name_Column    => GType_String,
      Params_Column          => GType_String,
      File_Location_Column   => GType_String);

   -----------------------
   -- Local subprograms --
   -----------------------

   Show_Frame_Number    : Boolean_Preference;
   Show_Program_Counter : Boolean_Preference;
   Show_Subprogram_Name : Boolean_Preference;
   Show_Parameters      : Boolean_Preference;
   Show_File_Location   : Boolean_Preference;

   type Call_Stack_Record is new Process_View_Record with record
      Tree                       : Gtk_Tree_View;
      Model                      : Gtk_Tree_Store;
      Block                      : Boolean := False;
      --  Whether to process selection events.
   end record;
   overriding procedure Update (View   : not null access Call_Stack_Record);
   overriding procedure On_Process_Terminated
     (View : not null access Call_Stack_Record);
   overriding procedure On_State_Changed
     (View : not null access Call_Stack_Record; New_State : Debugger_State);
   overriding procedure Create_Menu
     (Self : not null access Call_Stack_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   --  See inherited documentation

   function Initialize
     (Widget : access Call_Stack_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Call_Stack_Record'Class;
   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Call_Stack_Record'Class := null);
   --  Store or retrieve the view from the process

   type CS_Child_Record is new GPS_MDI_Child_Record with null record;

   package CS_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Call_Stack",
      View_Name          => -"Call Stack",
      Formal_View_Record => Call_Stack_Record,
      Formal_MDI_Child   => CS_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);
   subtype Call_Stack is CS_MDI_Views.View_Access;
   use type Call_Stack;

   package Simple_Views is new GVD.Generic_View.Simple_Views
     (Views              => CS_MDI_Views,
      Formal_View_Record => Call_Stack_Record,
      Formal_MDI_Child   => CS_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

   procedure Set_Column_Types (Self : not null access Call_Stack_Record'Class);
   --  Setup the columns.

   procedure On_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class);
   --  Callback for the selection change.

   type On_Location_Changed is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Location_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Hook for "debugger_location_changed"
   --  Highlight frame number Frame based on the current debugger output
   --  stored in Process.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Call_Stack_Record'Class is
   begin
      return Call_Stack (Visual_Debugger (Process).Stack);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Call_Stack_Record'Class := null)
   is
      Old : constant Call_Stack := Get_View (Process);
   begin
      Visual_Debugger (Process).Stack := Abstract_View_Access (View);

      --  If we are detaching, clear the old view. This can only be done after
      --  the above, since otherwise the action on the GUI will result into
      --  actions on the debugger.

      if View = null and then Old /= null then
         On_Process_Terminated (Old);
      elsif View /= null then
         --  Configure the backtrace info retrieved from the debugger
         Set_Column_Types (View);
      end if;
   end Set_View;

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
      if Get_Process (Stack) /= null and then not Stack.Block then
         Stack.Tree.Get_Selection.Get_Selected (Model, Iter);
         if Iter /= Null_Iter then
            Stack_Frame
              (Visual_Debugger (Get_Process (Stack)).Debugger,
               Natural'Value
                 (Get_String (Stack.Model, Iter, Frame_Num_Column)) + 1,
               GVD.Types.Visible);
         end if;
      end if;
   end On_Selection_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      Stack : Call_Stack;
   begin
      if Pref = null
        or else Pref = Preference (Show_Frame_Number)
        or else Pref = Preference (Show_Program_Counter)
        or else Pref = Preference (Show_Subprogram_Name)
        or else Pref = Preference (Show_Parameters)
      then
         Stack := CS_MDI_Views.Retrieve_View (Kernel);
         Set_Column_Types (Stack);
      end if;
   end Execute;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access Call_Stack_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      Append_Menu (Menu, Self.Kernel, Show_Frame_Number);
      Append_Menu (Menu, Self.Kernel, Show_Program_Counter);
      Append_Menu (Menu, Self.Kernel, Show_Subprogram_Name);
      Append_Menu (Menu, Self.Kernel, Show_Parameters);
      Append_Menu (Menu, Self.Kernel, Show_File_Location);
   end Create_Menu;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (Self : not null access Call_Stack_Record'Class)
   is
      Process : Visual_Debugger;
   begin
      Set_Visible (Get_Column (Self.Tree, 0), Show_Frame_Number.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 1), Show_Program_Counter.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 2), Show_Subprogram_Name.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 3), Show_Parameters.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 4), Show_File_Location.Get_Pref);

      Process := Get_Process (Self);
      if Process /= null then
         Process.Debugger.Configure_Backtrace
           (Show_Id              => Show_Frame_Number.Get_Pref,
            Show_PC              => Show_Program_Counter.Get_Pref,
            Show_Subprogram_Name => Show_Subprogram_Name.Get_Pref,
            Show_Parameters      => Show_Parameters.Get_Pref,
            Show_Location        => Show_File_Location.Get_Pref);
      end if;
   end Set_Column_Types;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Call_Stack_Record'Class) return Gtk_Widget
   is
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
        (Column_Types => Column_Types,
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

      Gtkada.Handlers.Object_Callback.Object_Connect
        (Get_Selection (Widget.Tree), Signal_Changed,
         On_Selection_Changed'Access, Widget);

      Debugger_Location_Changed_Hook.Add
        (new On_Location_Changed, Watch => Widget);
      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => Widget);

      return Gtk_Widget (Widget.Tree);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Location_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      Process     : constant Visual_Debugger := Visual_Debugger (Debugger);
      S           : constant Call_Stack := Get_View (Process);
      Frame       : Unbounded_String;
      Frame_Info  : Frame_Info_Type := Location_Not_Found;
      Path        : Gtk_Tree_Path;
   begin
      if S /= null
        and then Process.Current_Output /= null
      then
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
            Kernel.Insert (-"There is no debug information for this frame.");
            Set_Current_File_And_Line
              (Kernel, Process, GNATCOLL.VFS.No_File, 0);
         end if;
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open debugger call stack",
         Description => -"Open the Call Stack window for the debugger");

      Show_Frame_Number := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-frame-num", False,
         Label => -"Show Frame Number");
      Show_Program_Counter := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-program-counter", False,
         Label => -"Show Program Counter");
      Show_Subprogram_Name := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-subprogram", True,
         Label => -"Show Subprogram Name");
      Show_Parameters := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-parameters", False,
         Label => -"Show Parameters");
      Show_File_Location := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-file-loc", False,
         Label => -"Show File Location Number");
   end Register_Module;

   ----------------------
   -- On_State_Changed --
   ----------------------

   overriding procedure On_State_Changed
     (View : not null access Call_Stack_Record; New_State : Debugger_State)
   is
      Iter : Gtk_Tree_Iter;
      Prev : Boolean;
   begin
      if New_State = Debug_Busy then
         --  The debugger is now executing a command that will likely change
         --  the current stack trace. While it is executing, we do not want to
         --  keep a visible call stack displayed.

         if Visual_Debugger (View.Get_Process).Is_Execution_Command then
            --  Calling Clear might cause the selection to jump from row to
            --  row, causing a query of every frame info. To prevent this,
            --  set the Block flag.
            Prev := View.Block;
            View.Block := True;
            Clear (View.Model);
            View.Block := Prev;

            View.Model.Append (Iter, Null_Iter);

            Set_And_Clear
              (View.Model, Iter, (Frame_Num_Column, Subprog_Name_Column),
               (1 => As_String ("0"),
                2 => As_String ("Running...")));
            View.Tree.Get_Selection.Set_Mode (Selection_None);
         end if;
      end if;
   end On_State_Changed;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Call_Stack_Record)
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

   overriding procedure Update (View : not null access Call_Stack_Record) is
      Bt       : Backtrace_Vector;
      Process  : Process_Proxy_Access;
      Subp     : GNAT.Strings.String_Access;
      Iter     : Gtk_Tree_Iter;
      Params   : Ada.Strings.Unbounded.Unbounded_String;

      function Image (Value : Natural) return String;

      function Image (Value : Natural) return String is
         S : constant String := Value'Img;
      begin
         return S (S'First + 1 .. S'Last);
      end Image;

   begin
      --  Remove previous stack information.

      View.Block := True;
      Clear (View.Model);

      if Get_Process (View) /= null then
         Process :=
           Get_Process (Visual_Debugger (Get_Process (View)).Debugger);
      end if;

      --  If the debugger was killed, no need to refresh

      if Process = null then
         View.Block := False;
         return;
      end if;

      --  Parse the information from the debugger

      Backtrace (Visual_Debugger (Get_Process (View)).Debugger, Bt);

      --  Update the contents of the window

      for J of Bt loop
         --  ??? We currently consider that the list of parameters always
         --  starts at the first '(' character encountered

         Subp := J.Subprogram;

         View.Model.Append (Iter, Null_Iter);
         Params := Null_Unbounded_String;

         for P of J.Parameters loop
            if Params /= Null_Unbounded_String then
               Append (Params, ", ");
            end if;
            Append (Params, P.Value.all);
         end loop;

         Set_All_And_Clear
           (View.Model, Iter,
            (0 => As_String (Natural'Image (J.Frame_Id)),
             1 => As_String
               (Escape_Text ((if J.Address = Invalid_Address then "<>"
                   else Address_To_String (J.Address)))),
             2 => As_String (Escape_Text (Subp.all)),
             3 => As_String (Escape_Text (To_String (Params))),
             4 => As_String
               (Escape_Text
                  ((if J.File = No_File then "<>"
                   else +(Full_Name (J.File))) &
                   (if J.Line /= 0 then ":" & Image (J.Line)
                      else "")))));
      end loop;

      Free (Bt);

      View.Tree.Get_Selection.Set_Mode (Selection_Single);

      View.Block := True;
      if Get_Iter_First (View.Model) /= Null_Iter then
         View.Tree.Get_Selection.Select_Iter (View.Model.Get_Iter_First);
      end if;
      View.Block := False;
   end Update;

end GVD.Call_Stack;
