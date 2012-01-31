------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.Strings;           use GNAT.Strings;
with GNATCOLL.VFS;           use GNATCOLL.VFS;

with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with XML_Utils;              use XML_Utils;

with Gdk.Color;              use Gdk.Color;

with Gtk.Box;                use Gtk.Box;
with Gtk.Button;             use Gtk.Button;
with Gtk.Cell_Layout;        use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box;          use Gtk.Combo_Box;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.Image;              use Gtk.Image;
with Gtk.Label;              use Gtk.Label;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Style;              use Gtk.Style;
with Gtk.Table;              use Gtk.Table;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Dialogs;         use Gtkada.Dialogs;
with Gtkada.MDI;             use Gtkada.MDI;
with Collapsing_Pane;        use Collapsing_Pane;

with GPS.Intl;               use GPS.Intl;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Kernel.Remote;

with Remote;                 use Remote;
with Remote.Config_Dialog;   use Remote.Config_Dialog;
with Remote.Db;              use Remote.Db;
with Gexpect.Db;             use Gexpect, Gexpect.Db;
with Traces;                 use Traces;

package body Remote.View is

   Me : constant Debug_Handle := Create ("Remote");

   subtype Config_Servers is Server_Type range GPS_Server .. Debug_Server;

   type Sync_Buttons_Array is array (Config_Servers) of Gtk_Button;
   type Server_Combo_Array is array (Config_Servers) of Gtk_Combo_Box;

   type Remote_View_Record is new Gtk_Scrolled_Window_Record with record
      Main_Table         : Gtk.Table.Gtk_Table;
      Kernel             : GPS.Kernel.Kernel_Handle;
      Pane               : Collapsing_Pane.Collapsing_Pane;
      Servers_Combo      : Server_Combo_Array;
      To_Local_Buttons   : Sync_Buttons_Array;
      To_Remote_Buttons  : Sync_Buttons_Array;
      Check_Button       : Gtk_Button;
      Apply_Button       : Gtk_Button;
      Settings_Button    : Gtk_Button;
      Set_Default_Button : Gtk_Button;
      Normal_Style       : Gtk_Style;
      Modified_Style     : Gtk_Style;
      Connecting         : Boolean := False;
   end record;
   type Remote_View is access all Remote_View_Record'Class;

   procedure Gtk_New
     (View            : out Remote_View;
      Kernel          : Kernel_Handle;
      Use_Simple_View : Boolean := True);
   --  Create a new remote view associated with Manager.

   procedure Initialize
     (View            : access Remote_View_Record'Class;
      Kernel          : Kernel_Handle;
      Use_Simple_View : Boolean := True);
   --  Internal function for creating new widgets

   procedure Set_Servers
     (View : access Remote_View_Record'Class);
   --  Set the list of available servers

   function Check_Host (Nickname : String) return String;
   --  Check that the host can be contacted

   ---------------
   -- Callbacks --
   ---------------

   type Remote_Data is record
      View   : Remote_View;
      Server : Server_Type;
   end record;

   type Sync_Data is record
      View : Remote_View;
      From : Server_Type;
      To   : Server_Type;
   end record;

   procedure Setup (Data : Remote_Data; Id : Handler_Id);
   package View_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Gtk_Widget_Record, Remote_Data, Setup);

   procedure Setup (Data : Sync_Data; Id : Handler_Id);
   package Sync_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Gtk_Widget_Record, Sync_Data, Setup);

   type On_Server_Config_Hook is new Function_With_Args with record
      View : Remote_View;
   end record;
   overriding procedure Execute
     (Func   : On_Server_Config_Hook;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when server configuration changed

   type On_Server_List_Hook is new Function_No_Args with record
      View : Remote_View;
   end record;
   overriding procedure Execute
     (Func   : On_Server_List_Hook;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when server list changed

   procedure On_Combo_Changed
     (Combo : access Gtk_Widget_Record'Class;
      User  : Remote_Data);
   --  Called when one of the combo box's value changes

   procedure On_Check_Clicked
     (W    : access Gtk_Widget_Record'Class;
      User : Remote_Data);
   --  Called when the 'check' button is clicked

   procedure On_Connect_Clicked
     (W    : access Gtk_Widget_Record'Class;
      User : Remote_Data);
   --  Called when the 'Apply' button is clicked

   procedure On_Sync_Menu_Clicked
     (W    : access Gtk_Widget_Record'Class;
      User : Sync_Data);
   --  Called when an item of the sync menu is clicked

   procedure On_Config_List_Clicked
     (View : access Gtk_Widget_Record'Class;
      User : Remote_Data);
   --  Called when the "Settings" button is clicked

   procedure On_Set_Default_Clicked
     (View : access Gtk_Widget_Record'Class;
      User : Remote_Data);
   --  Called when the "Set default" button is clicked

   function Get_Selected
     (Combo : access Gtk_Combo_Box_Record'Class) return String;
   --  Utility function to retrieve the selected value from a combo box

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected
     (Combo : access Gtk_Combo_Box_Record'Class) return String
   is
      Iter : constant Gtk_Tree_Iter := Combo.Get_Active_Iter;
   begin
      if Iter = Null_Iter then
         return "<none>";
      else
         return Get_Model (Combo).Get_String (Iter, 0);
      end if;
   end Get_Selected;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View            : out Remote_View;
      Kernel          : Kernel_Handle;
      Use_Simple_View : Boolean := True) is
   begin
      View := new Remote_View_Record;
      Initialize (View, Kernel, Use_Simple_View);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View            : access Remote_View_Record'Class;
      Kernel          : Kernel_Handle;
      Use_Simple_View : Boolean := True)
   is
      Server_Label   : Gtk_Label;
      Simple_Table   : Gtk_Table;
      Full_Table     : Gtk_Table;
      Color          : Gdk_Color;
      Success        : Boolean;
      Hbox           : Gtk_Hbox;
      Buttons_Box    : Gtk_Hbox;
      To_Local_Img   : Gtk_Image;
      To_Remote_Img  : Gtk_Image;
      List           : Gtk_List_Store;
      Cell           : Gtk_Cell_Renderer_Text;

   begin
      Gtk.Scrolled_Window.Initialize (View);
      Set_Name (View, "remote_view");
      Set_Policy (View, Policy_Automatic, Policy_Automatic);
      Set_Shadow_Type (View, Shadow_None);
      Gtk_New (View.Main_Table, 3, 2, False);
      Add_With_Viewport (View, View.Main_Table);

      View.Kernel := Kernel;

      --  Server selection pane

      Gtk_New (View.Pane, "Servers assignment");
      Attach (View.Main_Table, View.Pane, 0, 2, 0, 1,
              Expand or Fill, 0);

      Gtk_New (Simple_Table, 1, 2, False);
      Set_Collapsed_Widget (View.Pane,
                            Simple_Table);
      Gtk_New (Full_Table, 6, 2, False);
      Set_Expanded_Widget (View.Pane,
                           Full_Table);

      if Use_Simple_View then
         Set_State (View.Pane, Collapsed);
      else
         Set_State (View.Pane, Expanded);
      end if;

      for S in Config_Servers'Range loop
         Gtk_New (List, (1 => Glib.GType_String));
         Gtk_New_With_Model (View.Servers_Combo (S), List);
         Gtk_New (Cell);
         Pack_Start
           (Implements_Cell_Layout.To_Interface (View.Servers_Combo (S)),
            Cell,
            True);
         Add_Attribute
           (Implements_Cell_Layout.To_Interface (View.Servers_Combo (S)),
            Cell, "text", 0);
         --  Set_Editable (Get_Entry (View.Servers_Combo (S)), False);
         --  Set_Width_Chars (Get_Entry (View.Servers_Combo (S)), 0);
         View_Callback.Connect
           (View.Servers_Combo (S), Signal_Changed, On_Combo_Changed'Access,
            (View => Remote_View (View), Server => S));

         case S is
            when GPS_Server =>
               Server_Label := null;
               Set_Name (View.Servers_Combo (S),
                         "combo_remote_server_all");
            when Build_Server =>
               Gtk_New (Server_Label, -("Build:"));
               Set_Name (View.Servers_Combo (S),
                         "combo_remote_server_build");
            when Debug_Server =>
               Gtk_New (Server_Label, -("Debug:"));
               Set_Name (View.Servers_Combo (S),
                         "combo_remote_server_debug");
            when Execution_Server =>
               Gtk_New (Server_Label, -("Execution:"));
               Set_Name (View.Servers_Combo (S),
                         "combo_remote_server_exec");
         end case;

         if Server_Label /= null then
            Set_Alignment (Server_Label, 0.0, 0.5);
         end if;

         Gtk_New
           (To_Local_Img,
            Pixbuf => Render_Icon (View, "gps-sync-to-local",
                                   Icon_Size_Button));
         Gtk_New (View.To_Local_Buttons (S));
         Add (View.To_Local_Buttons (S), To_Local_Img);
         Show (View.To_Local_Buttons (S));
         Set_Relief (View.To_Local_Buttons (S), Relief_None);
         Set_Border_Width (View.To_Local_Buttons (S), 0);
         Unset_Flags (View.To_Local_Buttons (S), Can_Focus or Can_Default);
         Set_Tooltip_Text
           (View.To_Local_Buttons (S),
            -("Synchronize all directories from the remote" &
              " server to the local machine"));

         Gtk_New
           (To_Remote_Img,
            Pixbuf => Render_Icon (View, "gps-sync-to-remote",
                                   Icon_Size_Button));
         Gtk_New (View.To_Remote_Buttons (S));
         Add (View.To_Remote_Buttons (S), To_Remote_Img);
         Show (View.To_Remote_Buttons (S));
         Set_Relief (View.To_Remote_Buttons (S), Relief_None);
         Set_Border_Width (View.To_Remote_Buttons (S), 0);
         Unset_Flags (View.To_Remote_Buttons (S), Can_Focus or Can_Default);
         Set_Tooltip_Text
           (View.To_Remote_Buttons (S),
            -("Synchronize all directories from the local" &
              " machine to the remote server"));

         if S /= GPS_Server then
            Sync_Callback.Connect
              (View.To_Local_Buttons (S), Signal_Clicked,
               On_Sync_Menu_Clicked'Access,
               (View => Remote_View (View), From => S, To => GPS_Server));
            Sync_Callback.Connect
              (View.To_Remote_Buttons (S), Signal_Clicked,
               On_Sync_Menu_Clicked'Access,
               (View => Remote_View (View), From => GPS_Server, To => S));
         else
            Sync_Callback.Connect
              (View.To_Local_Buttons (S), Signal_Clicked,
               On_Sync_Menu_Clicked'Access,
               (View => Remote_View (View),
                From => Build_Server,
                To   => GPS_Server));
            Sync_Callback.Connect
              (View.To_Remote_Buttons (S), Signal_Clicked,
               On_Sync_Menu_Clicked'Access,
               (View => Remote_View (View),
                From => GPS_Server,
                To   => Build_Server));
         end if;

         Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 0);
         Pack_Start (Hbox, View.To_Local_Buttons (S));
         Pack_Start (Hbox, View.To_Remote_Buttons (S));

         case S is
            when GPS_Server =>
               Attach (Simple_Table, View.Servers_Combo (S),
                       0, 1, 0, 1);
               Attach (Simple_Table, Hbox,
                       1, 2, 0, 1, 0, 0, 0, 0);
            when Build_Server =>
               Attach (Full_Table, Server_Label,
                       0, 2, 0, 1, Fill);
               Attach (Full_Table, View.Servers_Combo (S),
                       0, 1, 1, 2);
               Attach (Full_Table, Hbox,
                       1, 2, 1, 2, 0, 0, 0, 0);
            when Debug_Server =>
               Attach (Full_Table, Server_Label,
                       0, 2, 2, 3, Fill);
               Attach (Full_Table, View.Servers_Combo (S),
                       0, 1, 3, 4);
               Attach (Full_Table, Hbox,
                       1, 2, 3, 4, 0, 0, 0, 0);
            when Execution_Server =>
               Attach (Full_Table, Server_Label,
                       0, 2, 4, 5, Fill);
               Attach (Full_Table, View.Servers_Combo (S),
                       0, 1, 5, 6);
               Attach (Full_Table, Hbox,
                       1, 2, 5, 6, 0, 0, 0, 0);
         end case;
      end loop;

      Set_Tooltip_Text
        (View.Servers_Combo (GPS_Server),
         -("The remote server used to compile, debug and execute your " &
           "project."));
      Set_Tooltip_Text
        (View.Servers_Combo (Build_Server),
         -"The server used to perform builds and execute gnat tools");
      Set_Tooltip_Text
        (View.Servers_Combo (Debug_Server),
         -"The server used to launch the debugger");
      Set_Tooltip_Text
        (View.Servers_Combo (Execution_Server),
         -"The server used to execute the built executables");

      --  Styles
      Gtk_New (View.Normal_Style);
      Gtk_New (View.Modified_Style);
      Color := Parse ("red");
      Alloc_Color (Get_Default_Colormap, Color, Success => Success);
      for State in Gtk_State_Type loop
         Set_Text (View.Modified_Style, State, Color);
      end loop;

      --  Buttons

      Gtk_New_Hbox (Buttons_Box, Homogeneous => False, Spacing => 5);
      Attach (View.Main_Table, Buttons_Box, 0, 1, 1, 2,
              Xoptions => Fill or Expand,
              Yoptions => 0,
              Xpadding => 5,
              Ypadding => 0);

      Gtk_New (View.Check_Button, Label => -"Check");
      Set_Tooltip_Text
        (View.Check_Button,
         -"Check your configuration against current project");
      Set_Sensitive (View.Check_Button, False);
      Pack_Start (Buttons_Box, View.Check_Button, False, False);
      View_Callback.Connect
        (View.Check_Button, Signal_Clicked, On_Check_Clicked'Access,
         (View => Remote_View (View), Server => GPS_Server));

      Gtk_New (View.Apply_Button, Label => -"Apply");
      Set_Tooltip_Text
        (View.Apply_Button,
         -"Apply remote servers configuration");
      Set_Sensitive (View.Apply_Button, False);
      Pack_Start (Buttons_Box, View.Apply_Button, False, False);
      View_Callback.Connect
        (View.Apply_Button, Signal_Clicked, On_Connect_Clicked'Access,
         (View => Remote_View (View), Server => GPS_Server));

      Gtk_New (View.Set_Default_Button, -"Set default");
      Set_Tooltip_Text
        (View.Set_Default_Button,
         -"Set the servers assignment as default for the current project");
      Attach (View.Main_Table, View.Set_Default_Button,
              1, 2, 1, 2, 0, 0, 5, 0);
      View_Callback.Connect
        (View.Set_Default_Button, Signal_Clicked,
         On_Set_Default_Clicked'Access,
         (View => Remote_View (View), Server => GPS_Server));

      Gtk_New_Hbox (Buttons_Box, Homogeneous => False, Spacing => 5);
      Attach (View.Main_Table, Buttons_Box, 0, 2, 2, 3,
              Yoptions => 0,
              Xpadding => 5,
              Ypadding => 5);

      Gtk_New (View.Settings_Button, -"Servers settings");
      Set_Name (View.Settings_Button, -"remote_view_servers_settings");
      Set_Tooltip_Text
        (View.Settings_Button,
         -"Configure the list of available servers");
      Pack_Start (Buttons_Box, View.Settings_Button, False, False);
      View_Callback.Connect
        (View.Settings_Button, Signal_Clicked, On_Config_List_Clicked'Access,
         (View => Remote_View (View), Server => GPS_Server));

      Set_Servers (View);

      declare
         Hook_Func : constant Function_With_Args_Access :=
                       new On_Server_Config_Hook'
                         (Function_With_Args with View => Remote_View (View));
      begin
         Add_Hook (Kernel, GPS.Kernel.Remote.Server_Config_Changed_Hook,
                   Hook_Func, "remote_views module", Watch => GObject (View));
      end;

      declare
         Hook_Func : constant Function_No_Args_Access :=
                       new On_Server_List_Hook'
                         (Function_No_Args with View => Remote_View (View));
      begin
         Add_Hook (Kernel, Remote.Db.Server_List_Changed_Hook,
                   Hook_Func, "remote_views_module", Watch => GObject (View));
      end;
   end Initialize;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Module : GPS.Kernel.Modules.Module_ID;
      Node   : Node_Ptr;
      User   : Kernel_Handle) return MDI_Child
   is
      View     : Remote_View;
      Child    : GPS_MDI_Child;
      Mode     : Boolean;
      Mode_Str : constant String :=
                   Get_Attribute (Node, "simple_mode", "True");
   begin
      begin
         Mode := Boolean'Value (Mode_Str);
      exception
         when Constraint_Error =>
            Mode := True;
      end;

      Gtk_New (View, User, Mode);

      Gtk_New (Child, View,
               Default_Width => 215,
               Group         => Group_View,
               Module        => Module);
      Set_Title (Child, -"Remote", -"Remote");
      Put (Get_MDI (User), Child, Initial_Position => Position_Left);
      return MDI_Child (Child);
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      User        : GPS.Kernel.Kernel_Handle;
      Module_Name : String) return XML_Utils.Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Remote_View_Record'Class then
         N := new Node;
         N.Tag := new String'(Module_Name);

         if Get_State (Remote_View (Widget).Pane) = Collapsed then
            Set_Attribute (N, "simple_mode", "true");
         else
            Set_Attribute (N, "simple_mode", "false");
         end if;
         return N;
      end if;

      return null;
   end Save_Desktop;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Remote_Data; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data.View);
   end Setup;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Sync_Data; Id : Handler_Id) is
   begin
      Add_Watch (Id, Data.View);
   end Setup;

   -----------------
   -- Set_Servers --
   -----------------

   procedure Set_Servers (View : access Remote_View_Record'Class) is
      Machines   : constant GNAT.Strings.String_List := Get_Servers;

   begin
      for S in View.Servers_Combo'Range loop
         --  Set server for full view
         declare
            List       : constant Gtk_List_Store :=
                           Gtk_List_Store (Get_Model (View.Servers_Combo (S)));
            Iter       : Gtk_Tree_Iter :=
                           View.Servers_Combo (S).Get_Active_Iter;

            --  Save the selected server to select it back after list update
            Old_Server : constant String :=
                           Get_Selected (View.Servers_Combo (S));

         begin
            List.Clear;
            Iter := Null_Iter;

            List.Append (Iter);
            List.Set (Iter, 0, Display_Local_Nickname);

            View.Servers_Combo (S).Set_Active_Iter (Iter);

            for J in Machines'Range loop
               List.Append (Iter);
               List.Set (Iter, 0, Machines (J).all);

               if Old_Server = Machines (J).all then
                  View.Servers_Combo (S).Set_Active_Iter (Iter);
               end if;
            end loop;
         end;
      end loop;

      --  Set 'set default' button sensitivity

      Set_Sensitive (View.Set_Default_Button,
                     not GPS.Kernel.Remote.Is_Default_Remote_Setting);

      --  Update the view according to the modification in the servers list
      --  We just need to call it once for one of the distant servers

      On_Combo_Changed
        (View.Servers_Combo (Build_Server),
         (View => Remote_View (View), Server => Build_Server));
   end Set_Servers;

   -----------------------------------
   -- On_Server_Config_Changed_Hook --
   -----------------------------------

   overriding procedure Execute
     (Func   : On_Server_Config_Hook;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
   begin
      if not Func.View.Connecting then
         Set_Servers (Func.View);
      end if;
   end Execute;

   -------------------------
   -- On_Server_List_Hook --
   -------------------------

   overriding procedure Execute
     (Func   : On_Server_List_Hook;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      --  Check for deleted server

      for J in Server_Type'Range loop
         if Get_Nickname (J) /= Local_Nickname
           and then not Is_Configured (Get_Nickname (J))
         then
            GPS.Kernel.Remote.Assign
              (Kernel_Handle (Kernel), J, Local_Nickname, Reload_Prj => True);
         end if;
      end loop;

      Set_Servers (Func.View);
   end Execute;

   ----------------------
   -- On_Combo_Changed --
   ----------------------

   procedure On_Combo_Changed
     (Combo : access Gtk_Widget_Record'Class;
      User  : Remote_Data)
   is
      List     : Gtk_List_Store;
      Iter     : Gtk_Tree_Iter;
      Value    : constant String := Get_Selected (Gtk_Combo_Box (Combo));
      Txt_Set  : Boolean;
      Modified : Boolean;
      Remote   : Boolean;
      Advanced : Boolean;
      Adv_Str  : constant String := -"(Advanced configuration)";

   begin
      if Active (Me) then
         Trace (Me, "Combo for Server " & Server_Type'Image (User.Server) &
                " changed");
      end if;

      --  Take care of the 'advanced' state of the simple view: remove it if
      --  the value is present in the combo and wasn't selected.

      --  Selecting this value means only one thing: nothing has changed, so
      --  we can safely return in this specific case.
      List :=
        Gtk_List_Store (Get_Model (User.View.Servers_Combo (GPS_Server)));

      if List.Get_Iter_First /= Null_Iter
        and then List.Get_String (List.Get_Iter_First, 0) = Adv_Str
      then
         if Value = Adv_Str then
            return;
         else
            --  Remove in all cases, as this may be added later on in this
            --  procedure
            Iter := List.Get_Iter_First;
            List.Remove (Iter);
         end if;
      end if;

      --  Update the other combos according to the selection

      if User.Server = GPS_Server then
         for S in Distant_Server_Type'Range loop
            List := Gtk_List_Store (Get_Model (User.View.Servers_Combo (S)));
            Iter := List.Get_Iter_First;

            while Iter /= Null_Iter loop
               if List.Get_String (Iter, 0) = Value then
                  User.View.Servers_Combo (S).Set_Active_Iter (Iter);

                  exit;
               end if;

               List.Next (Iter);
            end loop;
         end loop;
      end if;

      --  Take care of the "advanced config" case

      Txt_Set := False;
      Advanced := False;

      if User.Server /= GPS_Server then
         Servers_Loop :
         for S1 in Distant_Server_Type'Range loop
            for S2 in Distant_Server_Type'Range loop
               if S1 /= S2
                 and then Get_Selected (User.View.Servers_Combo (S1)) /=
                 Get_Selected (User.View.Servers_Combo (S2))
               then
                  List :=
                    Gtk_List_Store
                      (User.View.Servers_Combo (GPS_Server).Get_Model);
                  Iter := List.Get_Iter_First;
                  Prepend (List, Iter);
                  List.Set (Iter, 0, Adv_Str);
                  User.View.Servers_Combo (GPS_Server).Set_Active_Iter (Iter);

                  Txt_Set := True;
                  Advanced := True;

                  exit Servers_Loop;
               end if;
            end loop;
         end loop Servers_Loop;

         --  No advanced config: make sure that the simple view matches the
         --  selected server in the advanced view

         if not Txt_Set then
            Iter := List.Get_Iter_First;

            while Iter /= Null_Iter loop
               if List.Get_String (Iter, 0) = Value then
                  User.View.Servers_Combo (GPS_Server).Set_Active_Iter (Iter);

                  exit;
               end if;

               List.Next (Iter);
            end loop;
         end if;
      end if;

      --  Now make sure that the modification states are properly set

      Modified := False;
      Remote   := False;
      Set_Sensitive (User.View.To_Local_Buttons (GPS_Server), False);
      Set_Sensitive (User.View.To_Remote_Buttons (GPS_Server), False);

      for S in Distant_Server_Type'Range loop
         --  Set modified states if needed
         if Get_Selected (User.View.Servers_Combo (S)) /=
           Get_Printable_Nickname (S)
         then
            Modified := True;
            Set_Style (User.View.Servers_Combo (S).Get_Child,
                       User.View.Modified_Style);
         else
            Set_Style (User.View.Servers_Combo (S).Get_Child,
                       User.View.Normal_Style);
         end if;

         --  Update buttons sensitity
         if Get_Selected (User.View.Servers_Combo (S)) /=
           Display_Local_Nickname
         then
            Remote := True;
            Set_Sensitive (User.View.To_Local_Buttons (S), True);
            Set_Sensitive (User.View.To_Remote_Buttons (S), True);

            if S = Build_Server and then not Advanced then
               Set_Sensitive (User.View.To_Local_Buttons (GPS_Server), True);
               Set_Sensitive (User.View.To_Remote_Buttons (GPS_Server), True);
            end if;

         else
            Set_Sensitive (User.View.To_Local_Buttons (S), False);
            Set_Sensitive (User.View.To_Remote_Buttons (S), False);
         end if;
      end loop;

      if Modified then
         Set_Style
           (User.View.Servers_Combo (GPS_Server).Get_Child,
            User.View.Modified_Style);
      else
         Set_Style
           (User.View.Servers_Combo (GPS_Server).Get_Child,
            User.View.Normal_Style);
      end if;

      Set_Sensitive (User.View.Check_Button, Remote and then Modified);
      Set_Sensitive (User.View.Apply_Button, Modified);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Combo_Changed;

   ----------------
   -- Check_Host --
   ----------------

   function Check_Host (Nickname : String) return String is
      Dir : GNATCOLL.VFS.Virtual_File;
   begin
      Dir := Get_Root (Get_Current_Dir (Nickname));

      if Is_Directory (Dir) then
         return "";
      else
         return "Could not establish communication with host " & Nickname;
      end if;

   exception
      when E : others =>
         return Ada.Exceptions.Exception_Message (E);
   end Check_Host;

   ----------------------
   -- On_Check_Clicked --
   ----------------------

   procedure On_Check_Clicked
     (W    : access Gtk_Widget_Record'Class;
      User : Remote_Data)
   is
      New_Server : constant String :=
                     Get_Selected (User.View.Servers_Combo (Build_Server));
      --  ??? We used to have a single assignment for Prj and Project below,
      --  but this caused a memory corruption (codegen bug ?), so work around
      --  it for now
      Prj        : constant GNATCOLL.VFS.Virtual_File :=
                     Get_Project (User.View.Kernel).Object_Dir;
      Reasons    : Ada.Strings.Unbounded.Unbounded_String;
      Failure    : Boolean := False;
      Ignore     : Message_Dialog_Buttons;
      pragma Unreferenced (W, Ignore);

   begin
      Push_State (User.View.Kernel, Busy);

      for S in Distant_Server_Type'Range loop
         declare
            Server_Name : constant String :=
                            Get_Selected (User.View.Servers_Combo (S));
            Already_Checked : Boolean := False;

         begin
            --  Catch potential Constraint_Error raised by 'Pred use
            begin
               for S2 in Distant_Server_Type'First .. Server_Type'Pred (S) loop
                  Already_Checked := Server_Name =
                    Get_Selected (User.View.Servers_Combo (S2));
                  exit when Already_Checked;
               end loop;

            exception
               when Constraint_Error =>
                  Already_Checked := False;
            end;

            if not Already_Checked
              and then Server_Name /= Display_Local_Nickname
              and then Server_Name /= Get_Nickname (S)
            then
               declare
                  Error_Msg : constant String := Check_Host (Server_Name);
               begin
                  if Error_Msg /= "" then
                     Failure := True;
                     Reasons := Reasons & "Check failed for server " &
                                Server_Name & ": " & Error_Msg & ASCII.LF;
                  end if;
               end;
            end if;
         end;
      end loop;

      begin
         --  In case of a remote build server: check that the project has an
         --  equivalent path on the remote server, or the paths are identical.
         if New_Server /= Display_Local_Nickname
           and then not Is_Regular_File (To_Remote (Prj, New_Server))
         then
            Failure := True;
            Reasons := Reasons & "Project " & Prj.Display_Full_Name &
              " has no equivalence on build server " & New_Server &
              ASCII.LF & ASCII.LF &
              "Please verify the Path translation setting in the 'Servers " &
              "settings' dialog, and in particular verify that " & ASCII.LF &
              Prj.Display_Dir_Name &
              " can be synchronized to the server '" &
              New_Server & "'" & ASCII.LF;
         end if;

      exception
         when E : Invalid_Process =>
            Failure := True;
            Reasons := Reasons & "Could not connect to host " &
              New_Server & ": " & Exception_Message (E) & ASCII.LF;
      end;

      Pop_State (User.View.Kernel);

      if not Failure then
         Ignore := Message_Dialog
           ("Remote configuration check has successfully completed",
            Buttons => Button_OK);
      else
         Ignore := Message_Dialog
           ("Remote configuration check has failed for the following reasons:"
            & ASCII.LF & To_String (Reasons),
            Dialog_Type => Error,
            Buttons     => Button_OK);
      end if;

   exception
      when E : others =>
         Ignore := Message_Dialog
           ("Remote configuration check has failed for the following reasons:"
            & ASCII.LF & "Exception received: " & ASCII.LF &
            Ada.Exceptions.Exception_Information (E),
            Dialog_Type => Error,
            Buttons     => Button_OK);
   end On_Check_Clicked;

   ------------------------
   -- On_Connect_Clicked --
   ------------------------

   procedure On_Connect_Clicked
     (W    : access Gtk_Widget_Record'Class;
      User : Remote_Data)
   is
      pragma Unreferenced (W);

   begin
      User.View.Connecting := True;

      for S in Distant_Server_Type'Range loop
         declare
            Server_Name : constant String :=
                            Get_Selected (User.View.Servers_Combo (S));
         begin
            if Server_Name /= Get_Printable_Nickname (S) then
               if Active (Me) then
                  Trace (Me, "Assign server " & Server_Type'Image (S) &
                         " to " & Server_Name);
               end if;

               if Server_Name = Display_Local_Nickname then
                  GPS.Kernel.Remote.Assign
                    (User.View.Kernel, S, Local_Nickname,
                     Reload_Prj => S = Build_Server);
               else
                  GPS.Kernel.Remote.Assign
                    (User.View.Kernel, S, Server_Name,
                     Reload_Prj => S = Build_Server);
               end if;
            end if;
         end;
      end loop;

      User.View.Connecting := False;
      Set_Servers (User.View);
   end On_Connect_Clicked;

   --------------------------
   -- On_Sync_Menu_Clicked --
   --------------------------

   procedure On_Sync_Menu_Clicked
     (W    : access Gtk_Widget_Record'Class;
      User : Sync_Data)
   is
      pragma Unreferenced (W);

      function Str_From_Server (S : Server_Type) return String;
      --  returns selected server's nickname

      ---------------------
      -- Str_From_Server --
      ---------------------

      function Str_From_Server (S : Server_Type) return String is
      begin
         case S is
            when GPS_Server =>
               return "";

            when others =>
               declare
                  Str : constant String :=
                          Get_Selected (User.View.Servers_Combo (S));
               begin
                  if Str = Local_Nickname then
                     return "";
                  else
                     return Str;
                  end if;
               end;
         end case;
      end Str_From_Server;

   begin
      if Str_From_Server (User.From) = ""
        and then Str_From_Server (User.To) = ""
      then
         return;
      end if;

      GPS.Kernel.Remote.Synchronize
        (User.View.Kernel,
         From          => Str_From_Server (User.From),
         To            => Str_From_Server (User.To),
         Blocking      => False,
         Print_Command => True,
         Print_Output  => True,
         Force         => True,
         Queue_Id      => "");
   end On_Sync_Menu_Clicked;

   ----------------------------
   -- On_Config_List_Clicked --
   ----------------------------

   procedure On_Config_List_Clicked
     (View : access Gtk_Widget_Record'Class;
      User : Remote_Data)
   is
      pragma Unreferenced (View);
      Build_Txt : constant String :=
                    Get_Selected (User.View.Servers_Combo (Build_Server));
   begin
      if Build_Txt /= Local_Nickname then
         Remote.Config_Dialog.Configure_Server_List
           (User.View.Kernel, Build_Txt);
      else
         Remote.Config_Dialog.Configure_Server_List
           (User.View.Kernel);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Config_List_Clicked;

   ----------------------------
   -- On_Set_Default_Clicked --
   ----------------------------

   procedure On_Set_Default_Clicked
     (View : access Gtk_Widget_Record'Class;
      User : Remote_Data)
   is
      pragma Unreferenced (View);
   begin
      GPS.Kernel.Remote.Set_Default_Remote_Settings;
      Set_Sensitive (User.View.Set_Default_Button, False);
   end On_Set_Default_Clicked;

   ----------------------
   -- Show_Remote_View --
   ----------------------

   procedure Show_Remote_View
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle;
      Module : GPS.Kernel.Modules.Module_ID)
   is
      pragma Unreferenced (Widget);
      Remote : Remote_View;
      Child  : GPS_MDI_Child;
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
                              (Get_MDI (Kernel), Remote_View_Record'Tag));
      if Child = null then
         Gtk_New (Remote, Kernel);
         Gtk_New (Child, Remote,
                  Default_Width => 215,
                  Group         => Group_View,
                  Module        => Module);
         Set_Title (Child, -"Remote", -"Remote");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Left);
      end if;

      Set_Focus_Child (Child);
      Raise_Child (Child);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Show_Remote_View;

end Remote.View;
