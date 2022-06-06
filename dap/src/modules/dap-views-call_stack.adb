------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with VSS.Strings.Conversions;

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Object;                use Glib.Object;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Toolbar;                use Gtk.Toolbar;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Tree_View;           use Gtkada.Tree_View;

with GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Search;                 use GPS.Search;

with Default_Preferences;        use Default_Preferences;
with Commands.Interactive;       use Commands.Interactive;
with Filter_Panels;              use Filter_Panels;

with DAP.Preferences;
with DAP.Requests.StackTraces;
with DAP.Tools;                  use DAP.Tools;
with DAP.Utils;

package body DAP.Views.Call_Stack is

   Me : constant Trace_Handle := Create ("GPS.DAP.Call_Stack", On);

   ---------------------
   -- Local constants --
   ---------------------

   Frame_Num_Column     : constant := 0;
   Name_Column          : constant := 1;
   Sourse_Column        : constant := 2;
   Location_Column      : constant := 3;
   Memory_Column        : constant := 4;
   Line_Column          : constant := 5;

   Column_Types : constant GType_Array :=
     (Frame_Num_Column     => GType_Int,
      Name_Column          => GType_String,
      Sourse_Column        => GType_String,
      Location_Column      => GType_String,
      Memory_Column        => GType_String,
      Line_Column          => GType_Int);

   -----------------------
   -- Local subprograms --
   -----------------------

   Show_Frame_Number : Boolean_Preference;
   Show_Name         : Boolean_Preference;
   Show_Sourse       : Boolean_Preference;
   Show_Location     : Boolean_Preference;
   Show_Address      : Boolean_Preference;

   type Call_Stack_Record is new View_Record with record
      Tree           : Tree_View;
      Model          : Gtk_Tree_Store;
      Selected_Frame : Integer;
      Last           : Integer := -1;
      Filter         : GPS.Search.Search_Pattern_Access := null;
   end record;
   overriding procedure Update (View : not null access Call_Stack_Record);
   overriding procedure On_Process_Terminated
     (View : not null access Call_Stack_Record);
   overriding procedure On_Status_Changed
     (View   : not null access Call_Stack_Record;
      Status : GPS.Debuggers.Debugger_State);

   overriding procedure Create_Menu
     (Self : not null access Call_Stack_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Create_Toolbar
     (View    : not null access Call_Stack_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Call_Stack_Record;
      Pattern : in out Search_Pattern_Access);

   function Initialize
     (Widget : access Call_Stack_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   procedure Send_Request
     (View : not null access Call_Stack_Record'Class;
      From : Integer;
      To   : Integer);

   procedure Goto_Location (Self : not null access Call_Stack_Record'Class);

   type Call_Stack_Tree_Record is new Tree_View_Record with record
      Filter       : GPS.Search.Search_Pattern_Access := null;
   end record;
   type Call_Stack_Tree_View is access all Call_Stack_Tree_Record'Class;
   overriding function Is_Visible
     (Self : not null access Call_Stack_Tree_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;

   package CS_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Call_Stack",
      View_Name                       => "Call Stack",
      Formal_View_Record              => Call_Stack_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => False,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Local_Config                    => True,
      Local_Toolbar                   => True,
      Areas                           => Gtkada.MDI.Sides_Only,
      Group                           => Group_Debugger_Stack,
      Position                        => Position_Right,
      Initialize                      => Initialize);
   subtype Call_Stack is CS_MDI_Views.View_Access;
   use type Call_Stack;

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Call_Stack_Record'Class;

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Call_Stack_Record'Class := null);

   package Simple_Views is new DAP.Views.Simple_Views
     (Formal_Views           => CS_MDI_Views,
      Formal_View_Record     => Call_Stack_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Get_View               => Get_View,
      Set_View               => Set_View);

   procedure Set_Column_Types (Self : not null access Call_Stack_Record'Class);

   type On_Pref_Changed is
     new GPS.Kernel.Hooks.Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type Fetch_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Fetch_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Fetch next portion of frames

   type Call_Stack_Fetch_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Call_Stack_Fetch_Filter;
      Context : Selection_Context) return Boolean;
   --  True if not all frames are fetched.

   procedure On_Clicked
     (Self   : access Glib.Object.GObject_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : not null
      access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class);

   -- StackTrace_Request --

   type StackTrace_Request is
     new DAP.Requests.StackTraces.StackTrace_DAP_Request
   with record
      Client : DAP.Clients.DAP_Client_Access;
      From   : Integer;
      To     : Integer;
   end record;

   type StackTrace_Request_Access is access all StackTrace_Request;

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   function Image (Value : Natural) return String;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access Call_Stack_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, Self.Kernel, Show_Frame_Number);
      Append_Menu (Menu, Self.Kernel, Show_Name);
      Append_Menu (Menu, Self.Kernel, Show_Sourse);
      Append_Menu (Menu, Self.Kernel, Show_Location);
      Append_Menu (Menu, Self.Kernel, Show_Address);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Call_Stack_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "call_stack",
         Tooltip     => "Filter the contents of the call stack view",
         Placeholder => "filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy,
         Name        => "Call Stack Filter");
   end Create_Toolbar;

   -----------
   -- Image --
   -----------

   function Image (Value : Natural) return String is
      S : constant String := Value'Img;
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (View : not null access Call_Stack_Record'Class;
      From : Integer;
      To   : Integer)
   is
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
      Req    : StackTrace_Request_Access :=
        new StackTrace_Request (View.Kernel);
   begin
      --  If the debugger was killed, no need to refresh

      if Client = null then
         Clear (View.Model);
         return;
      end if;

      Req.Client := Client;
      Req.From   := From;
      Req.To     := To;
      Req.Parameters.arguments.threadId := 0;
      if From /= -1 then
         Req.Parameters.arguments.startFrame := (True, From);
         Req.Parameters.arguments.levels     := (True, To - From + 1);
      end if;
      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Send_Request;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Call_Stack_Record;
      Pattern : in out Search_Pattern_Access)
   is
      View : constant Call_Stack_Tree_View := Call_Stack_Tree_View (Self.Tree);
   begin
      GPS.Search.Free (View.Filter);
      View.Filter := Pattern;
      Self.Tree.Refilter;
   end Filter_Changed;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Call_Stack_Fetch_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if DAP.Preferences.Frames_Limit.Get_Pref = 0 then
         return False;
      end if;

      declare
         View : constant Call_Stack :=
           Call_Stack (CS_MDI_Views.Retrieve_View (Get_Kernel (Context)));
      begin
         if View = null then
            return False;
         end if;

         return View.Last < Integer'Last;
      end;
   end Filter_Matches_Primitive;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Call_Stack_Record'Class is
   begin
      return Call_Stack (Client.Get_Call_Stack_View);
   end Get_View;

   --------------------
   -- Goto_Selection --
   --------------------

   procedure Goto_Location (Self : not null access Call_Stack_Record'Class) is
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Integer;
   begin
      if Client /= null then
         Self.Tree.Get_Selection.Get_Selected (Model, Iter);
         if Iter /= Null_Iter then
            begin
               File := GNATCOLL.VFS.Create
                 (+Model.Get_String (Iter, Sourse_Column));
               Line := Integer (Model.Get_Int (Iter, Line_Column));

               DAP.Utils.Highlight_Current_File_And_Line
                 (Self.Kernel, Client.Current_File, Client.Current_Line,
                  File, Line);
            exception
               when E : others =>
                  Trace (Me, E);
            end;
         end if;
      end if;
   end Goto_Location;

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
        or else Pref = Preference (Show_Name)
        or else Pref = Preference (Show_Sourse)
        or else Pref = Preference (Show_Location)
        or else Pref = Preference (Show_Address)
      then
         Stack := CS_MDI_Views.Retrieve_View (Kernel);
         Set_Column_Types (Stack);
         Update (Stack);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Fetch_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      View    : constant Call_Stack    :=
        Call_Stack (CS_MDI_Views.Retrieve_View (Kernel));
   begin
      View.Send_Request
        (View.Last + 1, View.Last + DAP.Preferences.Frames_Limit.Get_Pref);

      if View.Last = Integer'Last then
         Kernel.Context_Changed (No_Context);
      end if;

      return Commands.Success;
   end Execute;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (Self : not null access Call_Stack_Record'Class) is
   begin
      Set_Visible (Get_Column (Self.Tree, 0), Show_Frame_Number.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 1), Show_Name.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 2), Show_Sourse.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 3), Show_Location.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 4), Show_Address.Get_Pref);
   end Set_Column_Types;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Call_Stack_Record'Class) return Gtk_Widget
   is
      Scrolled     : Gtk_Scrolled_Window;

      procedure Add_Column (Name : String; Index : Gint);

      ----------------
      -- Add_Column --
      ----------------

      procedure Add_Column (Name : String; Index : Gint) is
         Column        : Gtk_Tree_View_Column;
         Text_Renderer : Gtk_Cell_Renderer_Text;
         Dummy         : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text_Renderer);
         Column.Set_Resizable (True);
         Column.Set_Title (Name);
         Column.Pack_Start (Text_Renderer, Expand => False);
         Column.Add_Attribute (Text_Renderer, "markup", Index);
         Dummy := Widget.Tree.Append_Column (Column);
      end Add_Column;

   begin
      Initialize_Vbox (Widget, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Widget.Pack_Start (Scrolled, Expand => True, Fill => True);

      Widget.Tree := new Call_Stack_Tree_Record;
      Initialize
        (Widget           => Widget.Tree,
         Column_Types     => Column_Types,
         Capability_Type  => Filtered,
         Set_Visible_Func => True);

      Add_Column ("Num", Frame_Num_Column);
      Add_Column ("Name", Name_Column);
      Add_Column ("Sourse", Sourse_Column);
      Add_Column ("Location", Location_Column);
      Add_Column ("Address", Memory_Column);

      --  Add_Column ("Line", Line_Column);
      --  Set_Visible (Get_Column (Widget.Tree, Line_Column), False);

      Set_Name (Widget.Tree, "Callstack tree");
      Widget.Tree.Get_Selection.Set_Mode (Selection_Single);
      Widget.Model := Widget.Tree.Model;

      Scrolled.Add (Widget.Tree);

      Set_Column_Types (Widget);

      Widget.Tree.Set_Activate_On_Single_Click (True);
      Widget.Tree.On_Row_Activated (On_Clicked'Access, Widget);
      GPS.Kernel.Hooks.Preferences_Changed_Hook.Add
        (new On_Pref_Changed, Watch => Widget);

      return Gtk_Widget (Widget.Tree);
   end Initialize;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self : not null access Call_Stack_Tree_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean is
   begin
      return
        Iter = Null_Iter
        or else Self.Filter = null
        or else
          Self.Filter.Start
            (Self.Model.Get_String (Iter, Name_Column)) /= No_Match;
   end Is_Visible;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked
     (Self   : access Glib.Object.GObject_Record'Class;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column : not null
      access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class)
   is
      pragma Unreferenced (Column);
      Stack : constant Call_Stack := Call_Stack (Self);
   begin
      Stack.Tree.Get_Selection.Select_Path (Path);
      Goto_Location (Stack);
   end On_Clicked;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Call_Stack_Record) is
   begin
      Clear (View.Model);
   end On_Process_Terminated;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
      View : constant Call_Stack := Get_View (Self.Client);
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
   begin
      if View = null then
         return;
      end if;

      if Length (Result.a_body.stackFrames) = 0 then
         View.Last := Integer'Last;

         if Self.From < 1 then
            --  we requested frames from the first one but have nothing
            Clear (View.Model);
         end if;

         return;
      end if;

      --  Update the contents of the window

      if Self.From < 1 then
         Clear (View.Model);
      end if;

      for Index in 1 .. Length (Result.a_body.stackFrames) loop
         declare
            Frame : constant StackFrame_Variable_Reference :=
              Get_StackFrame_Variable_Reference
                (Result.a_body.stackFrames, Index);
         begin
            View.Model.Append (Iter, Null_Iter);

            Set_All_And_Clear
              (View.Model, Iter,
               --  Id
               (Frame_Num_Column => As_Int (Gint (Frame.id)),
                --  Name
                Name_Column => As_String
                  (VSS.Strings.Conversions.To_UTF_8_String (Frame.name)),
                --  Sourse
                Sourse_Column => As_String
                  ((if Frame.source.Is_Set
                   then VSS.Strings.Conversions.To_UTF_8_String
                     (Frame.source.Value.name)
                   else "")),
                --  File location
                Location_Column => As_String
                  (Escape_Text
                     (Image (Frame.line) & ":" & Image (Frame.column))),
                --  Memory
                Memory_Column => As_String
                  (Escape_Text
                   ((if Frame.instructionPointerReference.Is_Empty then "<>"
                    else VSS.Strings.Conversions.To_UTF_8_String
                        (Frame.instructionPointerReference)))),
                Line_Column => As_Int (Gint (Frame.line))));

            View.Last := Frame.id;
         end;
      end loop;

      if View.Last < Self.To then
         View.Last := Integer'Last;
      end if;

      View.Tree.Refilter;

      if View.Selected_Frame /= -1 then
         Gtk_New (Path, Image (View.Selected_Frame));
         View.Tree.Get_Selection.Select_Path (Path);
         Path_Free (Path);
      else
         if View.Model.Get_Iter_First /= Null_Iter then
            View.Tree.Get_Selection.Select_Iter
              (View.Tree.Convert_To_Filter_Iter (View.Model.Get_Iter_First));
         end if;
      end if;

      View.Kernel.Context_Changed (No_Context);
   end On_Result_Message;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (View   : not null access Call_Stack_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      use GPS.Debuggers;
      Iter : Gtk_Tree_Iter;
   begin
      if Status = Debug_Busy then
         --  The debugger is now executing a command that will likely change
         --  the current stack trace. While it is executing, we do not want to
         --  keep a visible call stack displayed.

         Clear (View.Model);

         View.Model.Append (Iter, Null_Iter);

         Set_And_Clear
           (View.Model, Iter, (Frame_Num_Column, Name_Column),
            (1 => As_Int (0),
             2 => As_String ("Running...")));
      else
         View.Update;
      end if;
   end On_Status_Changed;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Call_Stack_Record'Class := null)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      if Client.Get_Call_Stack_View /= null then
         Call_Stack (Client.Get_Call_Stack_View).On_Process_Terminated;
      end if;

      Client.Set_Call_Stack_View (Generic_Views.Abstract_View_Access (View));
   end Set_View;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Call_Stack_Record) is
      Limit : constant Integer := DAP.Preferences.Frames_Limit.Get_Pref;
      From  : Integer;
      To    : Integer;
   begin
      if Limit = 0 then
         From := -1;
         To   := 0;
      else
         From := 0;
         To   := Limit - 1;
      end if;

      View.Selected_Frame := -1;
      View.Send_Request (From, To);
   end Update;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Fetch_Filter : constant Action_Filter :=
        new Call_Stack_Fetch_Filter;

   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open debugger call stack",
         Description => "Open the Call Stack window for the debugger");

      Show_Frame_Number := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-frame-num", True,
         Label => "Show Frame Number");
      Show_Name := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-name", False,
         Label => "Show Name");
      Show_Sourse := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-sourse", True,
         Label => "Show Sourse");
      Show_Location := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-location", False,
         Label => "Show Location");
      Show_Address := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debug-callstack-show-address", False,
         Label => "Show Address");

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug callstack fetch",
         new Fetch_Command,
         "Retrieve next portion of frames",
         Icon_Name => "gps-goto-symbolic",
         Category  => "Debug",
         Filter    => Fetch_Filter);
   end Register_Module;

end DAP.Views.Call_Stack;
