------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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
with GNATCOLL.Utils;

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Object;                use Glib.Object;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gdk.RGBA;
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

with GPS.Default_Styles;
with GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;   use GPS.Kernel.Style_Manager;
with GPS.Search;                 use GPS.Search;

with Default_Preferences;        use Default_Preferences;
with Commands.Interactive;       use Commands.Interactive;
with Filter_Panels;              use Filter_Panels;
with GUI_Utils;

with DAP.Types;                  use DAP.Types;
with DAP.Clients.Stack_Trace;    use DAP.Clients.Stack_Trace;

package body DAP.Views.Call_Stack is

   ---------------------
   -- Local constants --
   ---------------------

   Frame_Id_Column : constant := 0;
   Name_Column     : constant := 1;
   Location_Column : constant := 2;
   Memory_Column   : constant := 3;
   Sourse_Column   : constant := 4;
   Fg_Color_Column : constant := 5;

   Column_Types : constant GType_Array :=
     (Frame_Id_Column => GType_String,
      Name_Column     => GType_String,
      Location_Column => GType_String,
      Memory_Column   => GType_String,
      Sourse_Column   => GType_String,
      Fg_Color_Column => Gdk.RGBA.Get_Type);

   -----------------------
   -- Local subprograms --
   -----------------------

   Show_Frame_Number : Boolean_Preference;
   Show_Name         : Boolean_Preference;
   Show_Location     : Boolean_Preference;
   Show_Address      : Boolean_Preference;

   type Call_Stack_Record is new View_Record with record
      Tree   : Tree_View;
      Model  : Gtk_Tree_Store;
      Last   : Integer := -1;
      Filter : GPS.Search.Search_Pattern_Access := null;
   end record;
   overriding procedure Update (View : not null access Call_Stack_Record);
   overriding procedure On_Process_Terminated
     (View : not null access Call_Stack_Record);
   overriding procedure On_Status_Changed
     (View   : not null access Call_Stack_Record;
      Status : GPS.Debuggers.Debugger_State);
   overriding procedure On_Location_Changed
     (Self : not null access Call_Stack_Record);

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
      Reuse_If_Exist                  => True,
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

   package Simple_Views is new DAP.Views.Simple_Views
     (Formal_Views           => CS_MDI_Views,
      Formal_View_Record     => Call_Stack_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record);

   procedure Update_Columns_Visibility
     (Self : not null access Call_Stack_Record'Class);
   --  Update the view's columuns' visibility according to the user's
   --  preferences.

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

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access Call_Stack_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, Self.Kernel, Show_Frame_Number);
      Append_Menu (Menu, Self.Kernel, Show_Name);
      Append_Menu (Menu, Self.Kernel, Show_Location);
      Append_Menu (Menu, Self.Kernel, Show_Address);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Call_Stack_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
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
      Self.On_Location_Changed;
   end Filter_Changed;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Call_Stack_Fetch_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use type DAP.Clients.DAP_Client_Access;

      View : constant Call_Stack :=
        CS_MDI_Views.Retrieve_View
          (Get_Kernel (Context), Visible_Only => True);
   begin
      if View = null then
         return False;
      end if;

      declare
         Client : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
      begin
         if Client = null then
            return False;
         else
            return Client.Get_Stack_Trace.Can_Upload (Client);
         end if;
      end;
   end Filter_Matches_Primitive;

   --------------------
   -- Goto_Selection --
   --------------------

   procedure Goto_Location (Self : not null access Call_Stack_Record'Class) is
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
      Id     : Integer;
   begin
      if Client /= null then
         Self.Tree.Get_Selection.Get_Selected (Model, Iter);
         if Iter /= Null_Iter then
            Id := Integer'Value (Model.Get_String (Iter, Frame_Id_Column));
            Client.Get_Stack_Trace.Select_Frame (Id, Client);
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
      View : Call_Stack;
   begin
      if Pref = null
        or else Pref = Preference (Show_Frame_Number)
        or else Pref = Preference (Show_Name)
        or else Pref = Preference (Show_Location)
        or else Pref = Preference (Show_Address)
      then
         View := CS_MDI_Views.Retrieve_View (Kernel, Visible_Only => True);
         if View /= null then
            Update_Columns_Visibility (View);
            Update (View);
         end if;
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
      use type DAP.Clients.DAP_Client_Access;

      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant Call_Stack    :=
        Call_Stack (CS_MDI_Views.Retrieve_View (Kernel, Visible_Only => True));
      Client : DAP.Clients.DAP_Client_Access;
   begin
      if View /= null then
         Client := Get_Client (View);
         if Client /= null then
            Client.Get_Stack_Trace.Send_Request (Client);
         end if;
      end if;

      return Commands.Success;
   end Execute;

   -------------------------------
   -- Update_Columns_Visibility --
   -------------------------------

   procedure Update_Columns_Visibility
     (Self : not null access Call_Stack_Record'Class) is
   begin
      Set_Visible (Get_Column (Self.Tree, 0), Show_Frame_Number.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 1), Show_Name.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 2), Show_Location.Get_Pref);
      Set_Visible (Get_Column (Self.Tree, 3), Show_Address.Get_Pref);
   end Update_Columns_Visibility;

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
         Column.Add_Attribute
           (Text_Renderer, "markup", Index);
         Column.Add_Attribute
           (Text_Renderer, "foreground-rgba", Fg_Color_Column);
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

      Add_Column ("Num", Frame_Id_Column);
      Add_Column ("Name", Name_Column);
      Add_Column ("Location", Location_Column);
      Add_Column ("Address", Memory_Column);

      Set_Name (Widget.Tree, "Callstack tree");
      Widget.Tree.Get_Selection.Set_Mode (Selection_Single);
      Widget.Model := Widget.Tree.Model;

      Scrolled.Add (Widget.Tree);

      Update_Columns_Visibility (Widget);

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
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (View   : not null access Call_Stack_Record;
      Status : GPS.Debuggers.Debugger_State) is
   begin
      View.Update;
   end On_Status_Changed;

   -------------------------
   -- On_Location_Changed --
   -------------------------

   overriding procedure On_Location_Changed
     (Self : not null access Call_Stack_Record)
   is
      use type DAP.Clients.DAP_Client_Access;
      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Iter   : Gtk_Tree_Iter;
   begin
      Self.Tree.Get_Selection.Unselect_All;
      if Client = null then
         return;
      end if;

      if Client.Get_Stack_Trace.Get_Current_Frame_Id >= 0 then
         Iter := GUI_Utils.Find_Node
           (Model     => Self.Tree.Model,
            Name      => GNATCOLL.Utils.Image
              (Client.Get_Stack_Trace.Get_Current_Frame_Id, 1),
            Column    => Frame_Id_Column,
            Recursive => False);

         if Iter /= Null_Iter then
            Self.Tree.Get_Selection.Select_Iter
              (Self.Tree.Convert_To_Filter_Iter (Iter));
         end if;
      end if;
   end On_Location_Changed;

   ------------
   -- Update --
   ------------

   procedure Update
     (Kernel : GPS.Kernel.Kernel_Handle;
      Client : not null access DAP.Clients.DAP_Client'Class)
   is
      View : constant Call_Stack    :=
        Call_Stack (CS_MDI_Views.Retrieve_View (Kernel, Visible_Only => True));
   begin
      if View /= null
        and then Get_Client (View) = Client
      then
         View.Update;
      end if;
   end Update;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Call_Stack_Record) is
      use type DAP.Clients.DAP_Client_Access;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
      Status   : Debugger_Status_Kind;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path;
      Fg_Color : Gdk.RGBA.Gdk_RGBA;
   begin
      Clear (View.Model);

      if Client /= null then
         Status := View.Get_Client.Get_Status;

         if Status = Stopped then
            for Frame of Client.Get_Stack_Trace.Get_Trace loop
               Fg_Color :=
                 (if Frame.Location_Exists then
                     GPS.Kernel.Preferences.Default_Style.Get_Pref_Fg
                  else
                     Background
                       (GPS.Default_Styles.Editor_Code_Annotations_Style));

               View.Model.Append (Iter, Null_Iter);

               Set_All_And_Clear
                 (View.Model, Iter,
                  (Frame_Id_Column  => As_String
                       (GNATCOLL.Utils.Image (Frame.Id, 1)),
                   Name_Column      => As_String
                     (Escape_Text (To_String (Frame.Name))),
                   Location_Column  => As_String
                     (Escape_Text (+Full_Name (Frame.File) & ":" &
                        GNATCOLL.Utils.Image (Frame.Line, 1))),
                   Memory_Column    => As_String
                     (Escape_Text
                        ((if Frame.Address = Invalid_Address
                         then "<>"
                         else Address_To_String (Frame.Address)))),
                   Sourse_Column    => As_String (+Full_Name (Frame.File)),
                   Fg_Color_Column  => As_RGBA (Fg_Color)));
            end loop;

            View.Tree.Refilter;

            if Client.Get_Stack_Trace.Get_Current_Frame_Id /= -1 then
               Gtk_New
                 (Path,
                  GNATCOLL.Utils.Image
                    (Client.Get_Stack_Trace.Get_Current_Frame_Id, 1));
               View.Tree.Get_Selection.Select_Path (Path);
               Path_Free (Path);

            elsif View.Model.Get_Iter_First /= Null_Iter then
               View.Tree.Get_Selection.Select_Iter
                 (View.Tree.Convert_To_Filter_Iter
                    (View.Model.Get_Iter_First));
            end if;

         else
            --  The debugger is not stopped: clear the view and display
            --  a label according to the debugger's current status.

            View.Model.Append (Iter, Null_Iter);
            Set_And_Clear
              (View.Model, Iter, (Frame_Id_Column, Name_Column),
               (1 => As_String (""),
                2 => As_String (if Status = Running
                  then "Running..."
                  else "No data")));
         end if;
      end if;
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
        ("debug-callstack-show-name", True,
         Label => "Show Name");
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
