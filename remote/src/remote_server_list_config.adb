-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2006                            --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;     use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Xml_Int;               use Glib.Xml_Int;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Event_Box;              use Gtk.Event_Box;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Label;                  use Gtk.Label;
with Gtk.List;                   use Gtk.List;
with Gtk.List_Item;              use Gtk.List_Item;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Spin_Button;            use Gtk.Spin_Button;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Table;                  use Gtk.Table;
with Gtk.Text;                   use Gtk.Text;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Toggle_Button;          use Gtk.Toggle_Button;
with Gtk.Tooltips;               use Gtk.Tooltips;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtkada.Combo;               use Gtkada.Combo;
with Gtkada.Handlers;            use Gtkada.Handlers;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;

with GUI_Utils;                  use GUI_Utils;
with Traces;                     use Traces;
with VFS;                        use VFS;
with XML_Parsers;

package body Remote_Server_List_Config is

   ------------
   -- Module --
   ------------

   Me : constant Debug_Handle := Create ("Remote_Server_List_Config");

   type Server_List_Module_Record is new Module_ID_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Server_List_Module_ID is access all Server_List_Module_Record'Class;

   procedure Customize
     (Module : access Server_List_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   procedure Destroy (Module : in out Server_List_Module_Record);
   --  See doc for inherited subprogram

   Server_List_Module : Server_List_Module_ID;

   type Descriptor_Attribute is
     (System_Defined,
      User_Defined);

   ------------------------
   -- Machine_Descriptor --
   ------------------------

   type Machine_Descriptor_Record is
     new GNAT.Expect.TTY.Remote.Machine_Descriptor_Record
   with record
      Attribute : Descriptor_Attribute;
   end record;

   type Item_Record;
   type Item_Access is access all Item_Record;

   type Item_Record is record
      Desc : Machine_Descriptor;
      Next : Item_Access;
   end record;

   System_Machine_List : Item_Access := null;

   procedure Parse_Remote_Machine_Descriptor_Node
     (Kernel    : Kernel_Handle;
      Node      : Glib.Xml_Int.Node_Ptr;
      Attribute : Descriptor_Attribute);
   --  Parse a remote_machine_descriptor node

   procedure Load_Remote_Machine_List (Kernel : Kernel_Handle);
   --  Loads remote machines descriptors from .gps/remote.xml

   procedure Save_Remote_Machine_List (Kernel : Kernel_Handle);
   --  Save remote machines descriptors in .gps/remote.xml

   Name_Col     : constant Gint := 0;
   Modified_Col : constant Gint := 1;
   User_Def_Col : constant Gint := 2;

   type Server_List_Editor_Record is new Gtk_Dialog_Record with record
      Kernel                : Kernel_Handle;
      Machines              : Item_Access;
      Tree                  : Gtk_Tree_View;
      Add_Machine_Button    : Gtk_Button;
      Right_Table           : Gtk_Table;
      Nickname_Event        : Gtk_Event_Box;
      Nickname_Label        : Gtk_Label;
      Nickname_Entry        : Gtk_Entry;
      Network_Name_Entry    : Gtk_Entry;
      Remote_Access_Combo   : Gtkada_Combo;
      Remote_Shell_Combo    : Gtkada_Combo;
      Restore_Button        : Gtk_Button;
      Restoring             : Boolean := False;
      Advanced_Button       : Gtk_Toggle_Button;
      Advanced_Table        : Gtk_Table;
      User_Name_Entry       : Gtk_Entry;
      Max_Nb_Connected_Spin : Gtk_Spin_Button;
      Timeout_Spin          : Gtk_Spin_Button;
      Extra_Init_Cmds_Text  : Gtk_Text;
   end record;
   type Server_List_Editor is access all Server_List_Editor_Record'Class;

   procedure Gtk_New (Dialog : out Server_List_Editor;
                      Kernel : Kernel_Handle);
   --  Creates the server_list_editor dialog

   procedure Changed  (W : access Gtk_Widget_Record'Class);
   --  Called when one of the entries has changed

   procedure Selection_Changed (W : access Gtk_Widget_Record'Class);
   --  Called when the selected machine has changed.

   procedure Add_Machine_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the add_machine button is clicked.

   procedure Restore_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the restore button is clicked.

   procedure Advanced_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the advanced button is toggeled.

   ------------------------------------------
   -- Parse_Remote_Machine_Descriptor_Node --
   ------------------------------------------

   procedure Parse_Remote_Machine_Descriptor_Node
     (Kernel    : Kernel_Handle;
      Node      : Glib.Xml_Int.Node_Ptr;
      Attribute : Descriptor_Attribute)
   is
      Nickname           : constant String
        := Get_Attribute (Node, "nickname");
      Network_Name       : constant String
        := Get_Attribute (Node, "network_name");
      Remote_Access      : constant String
        := Get_Attribute (Node, "remote_access");
      Remote_Shell       : constant String
        := Get_Attribute (Node, "remote_shell");
      Field              : String_Ptr;
      Max_Nb_Connections : Natural;
      User_Name          : String_Access;
      Timeout            : Natural;
      Extra_Init_Cmds    : GNAT.OS_Lib.Argument_List_Access;
      Nb_Init_Cmds       : Natural;
      Child              : Node_Ptr;
      Cmd                : Node_Ptr;
      Desc               : Machine_Descriptor;
   begin

      if Nickname = "" then
         Console.Insert
           (Kernel, " XML Error: remote_machine_descriptor tags shall" &
            " have a nickname attribute",
            Add_LF => True, Mode => Error);
         return;
      end if;

      if Network_Name = "" then
         Console.Insert
           (Kernel, " XML Error: remote_machine_descriptor tags shall" &
            " have a network_name attribute",
            Add_LF => True, Mode => Error);
         return;
      end if;

      if Remote_Access = "" then
         Console.Insert
           (Kernel, " XML Error: remote_machine_descriptor tags shall" &
            " have a remote_access attribute",
            Add_LF => True, Mode => Error);
         return;
      end if;

      if Remote_Shell = "" then
         Console.Insert
           (Kernel, " XML Error: remote_machine_descriptor tags shall" &
            " have a remote_shell attribute",
            Add_LF => True, Mode => Error);
         return;
      end if;

      Field := Get_Field (Node, "max_nb_connections");

      if Field /= null then
         begin
            Max_Nb_Connections := Natural'Value (Field.all);
         exception
            when Constraint_Error =>
               Max_Nb_Connections := 3;
         end;
      else
         Max_Nb_Connections := 3;
      end if;

      Field := Get_Field (Node, "user_name");

      if Field /= null then
         User_Name := new String'(Field.all);
      else
         User_Name := new String'("");
      end if;

      Field := Get_Field (Node, "timeout");

      if Field /= null then
         begin
            Timeout := Natural'Value (Field.all);
         exception
            when Constraint_Error =>
               Timeout := 5000;
         end;
      else
         Timeout := 5000;
      end if;

      Extra_Init_Cmds := null;
      Nb_Init_Cmds := 0;
      Child := null;

      if Node.Child /= null then
         Child := Find_Tag (Node.Child, "extra_init_commands");
      end if;

      if Child /= null then
         Cmd          := Child.Child;
         while Cmd /= null loop
            Nb_Init_Cmds := Nb_Init_Cmds + 1;
            Cmd := Cmd.Next;
         end loop;
      end if;

      if Nb_Init_Cmds /= 0 then
         Extra_Init_Cmds := new Argument_List (1 .. Nb_Init_Cmds);
         Cmd := Child.Child;

         for J in Extra_Init_Cmds'Range loop
            Extra_Init_Cmds (J) := new String'(Cmd.Value.all);
            Cmd := Cmd.Next;
         end loop;
      end if;

      Desc := new Machine_Descriptor_Record'
        (Nickname            => new String'(Nickname),
         Network_Name        => new String'(Network_Name),
         Access_Name         => new String'(Remote_Access),
         Shell_Name          => new String'(Remote_Shell),
         Max_Nb_Connections  => Max_Nb_Connections,
         User_Name           => User_Name,
         Timeout             => Timeout,
         Extra_Init_Commands => Extra_Init_Cmds,
         Attribute           => Attribute,
         Ref                 => 0);

      --  Add this machine at GNAT.Expect.TTY.Remote level
      if not Is_Configured (Nickname)
        or else Machine_Descriptor_Record
          (Get_Machine_Descriptor (Nickname).all).Attribute /= User_Defined
      then
         Add_Machine_Descriptor (Desc);
      end if;

      --  If this is a predefined machine, save it
      if Attribute = System_Defined then
         Desc.Ref := Desc.Ref + 1;
         System_Machine_List :=
           new Item_Record'(Desc => Desc,
                            Next => System_Machine_List);
      end if;

      Run_Hook (Kernel, Server_List_Changed_Hook);
   end Parse_Remote_Machine_Descriptor_Node;

   ------------------------------
   -- Load_Remote_Machine_List --
   ------------------------------

   procedure Load_Remote_Machine_List (Kernel : Kernel_Handle)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "remote.xml";
      File, Child : Node_Ptr;
      Err : String_Access;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
         else
            Child := File.Child;

            while Child /= null loop
               if Child.Tag.all = "remote_machine_descriptor" then
                  Parse_Remote_Machine_Descriptor_Node
                    (Kernel, Child, User_Defined);
               end if;
               Child := Child.Next;
            end loop;
         end if;
      end if;
   end Load_Remote_Machine_List;

   ------------------------------
   -- Save_Remote_Machine_List --
   ------------------------------

   procedure Save_Remote_Machine_List (Kernel : Kernel_Handle)
   is
      Filename : constant String := Get_Home_Dir (Kernel) & "remote.xml";
      File, Item, Child, Cmd_Node : Node_Ptr;
      Desc : Machine_Descriptor;
      Nb_Desc : Natural;
   begin
      Trace (Me, "Saving " & Filename);

      File := new Node;
      File.Tag := new String'("remote_machines");

      Nb_Desc := Get_Nb_Machine_Descriptor;
      for J in 1 .. Nb_Desc loop
         Desc := Get_Machine_Descriptor (J);
         if Machine_Descriptor_Record (Desc.all).Attribute =
           User_Defined
         then
            Item := new Node;
            Item.Tag := new String'("remote_machine_descriptor");
            Set_Attribute (Item, "nickname", Desc.Nickname.all);
            Set_Attribute (Item, "network_name", Desc.Network_Name.all);
            Set_Attribute (Item, "remote_access", Desc.Access_Name.all);
            Set_Attribute (Item, "remote_shell", Desc.Shell_Name.all);
            Child := new Node;
            Child.Tag := new String'("timeout");
            Child.Value := new String'(Natural'Image (Desc.Timeout));
            Add_Child (Item, Child);
            Child := new Node;
            Child.Tag := new String'("max_nb_connections");
            Child.Value := new String'
              (Natural'Image (Desc.Max_Nb_Connections));
            Add_Child (Item, Child);

            if Desc.Extra_Init_Commands /= null then
               Child := new Node;
               Child.Tag := new String'("extra_init_commands");
               for J in Desc.Extra_Init_Commands'Range loop
                  Cmd_Node := new Node;
                  Cmd_Node.Tag := new String'("cmd");
                  Cmd_Node.Value :=
                    new String'(Desc.Extra_Init_Commands (J).all);
                  Add_Child (Child, Cmd_Node, True);
               end loop;
               Add_Child (Item, Child);
            end if;

            Add_Child (File, Item, True);
         end if;
      end loop;
      Print (File, Filename);
      Free (File);
   end Save_Remote_Machine_List;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Server_List_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);

   begin
      if Node.Tag.all = "remote_machine_descriptor" then
         Trace (Me, "Customize : 'remote_machine_descriptor'");
         Parse_Remote_Machine_Descriptor_Node
           (Module.Kernel, Node, System_Defined);
      end if;
   end Customize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Server_List_Module_Record) is
      pragma Unreferenced (Module);
   begin
      Server_List_Module := null;
      --  ???: save machines configuration in .gps/remote.xml
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      --  Register server list changed hook
      Register_Hook_No_Args
        (Kernel, Server_List_Changed_Hook);

      --  Load user specific machine list
      Load_Remote_Machine_List (Kernel_Handle (Kernel));

      --  Register the module
      Server_List_Module := new Server_List_Module_Record;
      Server_List_Module.Kernel := Kernel_Handle (Kernel);
      Register_Module
        (Server_List_Module, Kernel, "remote");
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Dialog : out Server_List_Editor;
                      Kernel : Kernel_Handle)
   is
      Nb_Machines : Natural;
      Tips        : Gtk_Tooltips;
      Main_Table  : Gtk_Table;
      Frame       : Gtk_Frame;
      Scrolled    : Gtk_Scrolled_Window;
      Model       : Gtk_Tree_Store;
      Iter        : Gtk_Tree_Iter := Null_Iter;
      Tmp         : Gtk_Widget;
      Label       : Gtk_Label;
      Item        : Gtk_List_Item;
      Empty_List  : Boolean := True;
      pragma Unreferenced (Tmp);
   begin
      Dialog := new Server_List_Editor_Record;
      Initialize (Dialog,
                  -"Server list configuration",
                  Get_Main_Window (Kernel),
                  Modal + Destroy_With_Parent);
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, 620, 400);
      Gtk_New (Tips);

      Dialog.Kernel := Kernel;

      Gtk_New (Main_Table, Rows => 2, Columns => 2, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Main_Table);

      Gtk_New (Frame);
      Attach (Main_Table, Frame, 0, 1, 0, 1);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Add (Frame, Scrolled);

      Dialog.Tree := Create_Tree_View
        (Column_Types       => (Guint (Name_Col)     => GType_String,
                                Guint (Modified_Col) => GType_Boolean,
                                Guint (User_Def_Col) => GType_Boolean),
         Column_Names       => (1 => new String'("Servers")),
         Show_Column_Titles => True,
         Selection_Mode     => Selection_Single,
         Sortable_Columns   => True,
         Initial_Sort_On    => 1,
         Hide_Expander      => False);
      Add (Scrolled, Dialog.Tree);

      Gtk_New (Dialog.Add_Machine_Button, -"Add a new machine");
      Attach (Main_Table, Dialog.Add_Machine_Button, 0, 1, 1, 2, 0, 0, 5, 5);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Attach (Main_Table, Scrolled, 1, 2, 0, 1);

      Gtk_New (Dialog.Right_Table, Rows => 6, Columns => 2,
               Homogeneous => False);

      Create_Blue_Label (Dialog.Nickname_Label,
                         Dialog.Nickname_Event);
      Attach (Dialog.Right_Table, Dialog.Nickname_Event, 0, 2, 0, 1,
              Fill or Expand, 0, 5, 5);

      Gtk_New (Label, -"Network name:");
      Attach (Dialog.Right_Table, Label, 0, 1, 1, 2,
              Fill or Expand, 0);
      Gtk_New (Dialog.Network_Name_Entry);
      Attach (Dialog.Right_Table, Dialog.Network_Name_Entry, 1, 2, 1, 2,
              Fill or Expand, 0);

      Gtk_New (Label, -"Remote access tool:");
      Attach (Dialog.Right_Table, Label, 0, 1, 2, 3,
              Fill or Expand, 0);
      Gtk_New (Dialog.Remote_Access_Combo);
      Set_Editable (Get_Entry (Dialog.Remote_Access_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Access_Combo, 1, 2, 2, 3,
              Fill or Expand, 0);

      for J in 1 .. Get_Nb_Remote_Access_Descriptor loop
         Gtk_New (Item, Locale_To_UTF8 (Get_Remote_Access_Name (J)));
         Add (Get_List (Dialog.Remote_Access_Combo), Item);
      end loop;
      Show_All (Get_List (Dialog.Remote_Access_Combo));

      Gtk_New (Label, -"Shell:");
      Attach (Dialog.Right_Table, Label, 0, 1, 3, 4,
              Fill or Expand, 0);
      Gtk_New (Dialog.Remote_Shell_Combo);
      Set_Editable (Get_Entry (Dialog.Remote_Shell_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Shell_Combo, 1, 2, 3, 4,
              Fill or Expand, 0);

      for J in 1 .. Get_Nb_Shell_Descriptor loop
         if Active (Me) then
            Trace (Me, "Add shell " & Get_Shell_Descriptor_Name (J));
         end if;
         Gtk_New (Item, Locale_To_UTF8 (Get_Shell_Descriptor_Name (J)));
         Add (Get_List (Dialog.Remote_Shell_Combo), Item);
      end loop;
      Show_All (Get_List (Dialog.Remote_Shell_Combo));

      Gtk_New (Dialog.Restore_Button, -"Restore default");
      Attach (Dialog.Right_Table, Dialog.Restore_Button, 0, 1, 4, 5,
              Fill or Expand, 0, 10, 10);

      Gtk_New (Dialog.Advanced_Button, -"Advanced >>");
      Set_Active (Dialog.Advanced_Button, False);
      Attach (Dialog.Right_Table, Dialog.Advanced_Button, 1, 2, 4, 5,
              Fill or Expand, 0, 10, 10);

      Gtk_New (Dialog.Advanced_Table,
               Rows => 4, Columns => 2, Homogeneous => False);
      Attach (Dialog.Right_Table, Dialog.Advanced_Table, 0, 2, 6, 7,
              0, 0);
      Set_Child_Visible (Dialog.Advanced_Table,
                         Get_Mode (Dialog.Advanced_Button));

      Gtk_New (Label, -"User name:");
      Attach (Dialog.Advanced_Table, Label, 0, 1, 0, 1,
              Fill or Expand, 0);
      Gtk_New (Dialog.User_Name_Entry);
      Attach (Dialog.Advanced_Table, Dialog.User_Name_Entry, 1, 2, 0, 1,
              Fill or Expand, 0);

      Gtk_New (Label, -"Timeout value (in s):");
      Attach (Dialog.Advanced_Table, Label, 0, 1, 1, 2,
              Fill or Expand, 0);
      Gtk_New (Dialog.Timeout_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Timeout_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Timeout_Spin, 1, 2, 1, 2,
              Fill or Expand, 0);

      Gtk_New (Label, -"Max number of connections:");
      Attach (Dialog.Advanced_Table, Label, 0, 1, 2, 3,
              Fill or Expand, 0);
      Gtk_New (Dialog.Max_Nb_Connected_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Max_Nb_Connected_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Max_Nb_Connected_Spin, 1, 2, 2, 3,
              Fill or Expand, 0);

      Gtk_New (Label, -"Extra init commands:");
      Attach (Dialog.Advanced_Table, Label, 0, 1, 3, 4,
              Fill or Expand, 0);
      Gtk_New (Dialog.Extra_Init_Cmds_Text);
      Set_Editable (Dialog.Extra_Init_Cmds_Text, True);
      Attach (Dialog.Advanced_Table, Dialog.Extra_Init_Cmds_Text, 1, 2, 3, 4,
              Fill or Expand, 0);

      Add_With_Viewport (Scrolled, Dialog.Right_Table);

      Widget_Callback.Object_Connect
        (Dialog.Network_Name_Entry, "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.User_Name_Entry, "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Access_Combo),
         "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Shell_Combo),
         "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Max_Nb_Connected_Spin, "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Timeout_Spin, "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Extra_Init_Cmds_Text, "changed", Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Selection (Dialog.Tree), "changed",
         Selection_Changed'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Add_Machine_Button, "clicked",
         Add_Machine_Clicked'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Restore_Button, "clicked",
         Restore_Clicked'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Advanced_Button, "toggled",
         Advanced_Clicked'Access,
         Dialog);

      Model := Gtk_Tree_Store (Get_Model (Dialog.Tree));

      Nb_Machines := Get_Nb_Machine_Descriptor;

      for J in 1 .. Nb_Machines loop
         Empty_List := False;
         Dialog.Machines := new Item_Record'
           (Desc => Get_Machine_Descriptor (J),
            Next => Dialog.Machines);
         Dialog.Machines.Desc.Ref := Dialog.Machines.Desc.Ref + 1;
         Append (Model, Iter, Null_Iter);
         Set (Model, Iter, Name_Col, Get_Nickname (J));
         Set (Model, Iter, Modified_Col, False);

         if Machine_Descriptor_Record (Dialog.Machines.Desc.all).Attribute
           = User_Defined then
            Set (Model, Iter, User_Def_Col, True);
         else
            Set (Model, Iter, User_Def_Col, False);
         end if;

         if J = 1 then
            Select_Iter (Get_Selection (Dialog.Tree),
                         Iter);
         end if;
      end loop;

      if Empty_List then
         Set_Child_Visible (Dialog.Right_Table, False);
      end if;

      Tmp := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Apply, Gtk_Response_Apply);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Enable (Tips);

      Show_All (Dialog);
   end Gtk_New;

   -------------
   -- Changed --
   -------------

   procedure Changed  (W : access Gtk_Widget_Record'Class)
   is
      Dialog    : Server_List_Editor_Record
        renames Server_List_Editor_Record (W.all);
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if not Dialog.Restoring then
         Get_Selected (Get_Selection (Dialog.Tree),
                       Model,
                       Iter);
         Trace (Me, "Changes detected for current selection");
         --  Set this iter as modified
         Set (Gtk_Tree_Store (Model), Iter, Modified_Col, True);
         --  User defined item
         Set (Gtk_Tree_Store (Model), Iter, User_Def_Col, True);
      else
         Trace (Me, "Changes not from user inputs");
      end if;
   end Changed;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (W : access Gtk_Widget_Record'Class)
   is
      procedure Save
        (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Dialog : in out Server_List_Editor_Record);
      --  Saves all modified item

      ----------
      -- Save --
      ----------

      procedure Save
        (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Dialog : in out Server_List_Editor_Record)
      is
         Item      : Item_Access;
         Init_Cmds : String_List_Access;
      begin
         if Get_Boolean (Model, Iter, Modified_Col) then
            declare
               Attribute : Descriptor_Attribute;
               Nickname  : constant String
                 := Get_String (Model, Iter, Name_Col);
            begin
               if Active (Me) then
                  Trace (Me, "Saving in dialog conf for " & Nickname);
               end if;

               --  Get attribute value.
               if Get_Boolean (Model, Iter, User_Def_Col) then
                  Trace (Me, "... user defined attribute");
                  Attribute := User_Defined;
               else
                  Trace (Me, "... system defined attribute");
                  Attribute := System_Defined;
               end if;

               Item := Dialog.Machines;
               while Item /= null loop
                  if Item.Desc.Nickname.all = Nickname then
                     exit;
                  end if;

                  Item := Item.Next;
               end loop;

               if Item /= null then
                  Unref (Item.Desc);

                  declare
                     Txt : constant String
                       := Get_Chars (Dialog.Extra_Init_Cmds_Text);
                     Nb_Lines : Natural := 0;
                     Last_LF  : Integer := -1;
                  begin
                     for J in Txt'Range loop
                        if Txt (J) = ASCII.LF then
                           Nb_Lines := Nb_Lines + 1;
                           Last_LF := J;
                        end if;
                     end loop;

                     if Last_LF /= Txt'Last and then Txt'Length > 0 then
                        Nb_Lines := Nb_Lines + 1;
                     end if;

                     Init_Cmds := new GNAT.OS_Lib.String_List (1 .. Nb_Lines);
                     Nb_Lines := Init_Cmds'First;
                     Last_LF  := Txt'First - 1;

                     for J in Txt'Range loop
                        if Txt (J) = ASCII.LF then
                           Init_Cmds (Nb_Lines) :=
                             new String'(Txt (Last_LF + 1 .. J - 1));
                           Nb_Lines := Nb_Lines + 1;
                           Last_LF := J;
                        end if;
                     end loop;

                     if Last_LF /= Txt'Last and then Txt'Length > 0 then
                        Init_Cmds (Nb_Lines) :=
                          new String'(Txt (Last_LF + 1 .. Txt'Last));
                     end if;
                  end;

                  Item.Desc := new Machine_Descriptor_Record'
                    (Nickname            => new String'(Nickname),
                     Network_Name        => new String'
                       (Get_Text (Dialog.Network_Name_Entry)),
                     Access_Name         => new String'
                       (Get_Text (Get_Entry (Dialog.Remote_Access_Combo))),
                     Shell_Name          => new String'
                       (Get_Text (Get_Entry (Dialog.Remote_Shell_Combo))),
                     User_Name           => new String'
                       (Get_Text (Dialog.User_Name_Entry)),
                     Timeout             =>
                       Integer (Get_Value_As_Int (Dialog.Timeout_Spin)) * 1000,
                     Max_Nb_Connections  => Integer
                       (Get_Value_As_Int (Dialog.Max_Nb_Connected_Spin)),
                     Extra_Init_Commands => Init_Cmds,
                     Attribute           => Attribute,
                     Ref                 => 1);
               end if;
            end;
            --  Set this iter as not modified
            Set (Gtk_Tree_Store (Model), Iter, Modified_Col, False);
         end if;
      end Save;

      Dialog    : Server_List_Editor_Record
        renames Server_List_Editor_Record (W.all);
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item      : Item_Access;
      Sys_Item  : Item_Access;
      Overriden : Boolean;
      Pos       : Gint;
   begin
      --  First save previous selection if needed
      Model := Get_Model (Dialog.Tree);
      Iter  := Get_Iter_First (Model);

      while Iter /= Null_Iter loop
         Save (Model, Iter, Dialog);
         Next (Model, Iter);
      end loop;

      --  Now reinit the dialog values
      Get_Selected (Get_Selection (Dialog.Tree),
                    Model,
                    Iter);

      if Iter /= Null_Iter then
         declare
            Nickname : constant String := Get_String (Model, Iter, Name_Col);
         begin
            if Active (Me) then
               Trace (Me, "Setting dialog values for new selection " &
                      Nickname);
            end if;
            Set_Text (Dialog.Nickname_Label, Nickname);
            Item := Dialog.Machines;

            while Item /= null loop
               exit when Item.Desc.Nickname.all = Nickname;
               Item := Item.Next;
            end loop;

            if Item = null then
               return;
            end if;

            Dialog.Restoring := True;
            Set_Text (Dialog.Network_Name_Entry,
                      Item.Desc.Network_Name.all);
            Set_Text (Dialog.User_Name_Entry,
                      Item.Desc.User_Name.all);
            Set_Text (Get_Entry (Dialog.Remote_Access_Combo),
                      Item.Desc.Access_Name.all);
            Set_Text (Get_Entry (Dialog.Remote_Shell_Combo),
                      Item.Desc.Shell_Name.all);
            Set_Value (Dialog.Timeout_Spin,
                       Gdouble (Item.Desc.Timeout) / 1000.0);
            Set_Value (Dialog.Max_Nb_Connected_Spin,
                       Gdouble (Item.Desc.Max_Nb_Connections));

            Delete_Text (Dialog.Extra_Init_Cmds_Text);
            Pos := 0;

            if Item.Desc.Extra_Init_Commands /= null then
               for J in Item.Desc.Extra_Init_Commands'Range loop
                  Insert_Text
                    (Dialog.Extra_Init_Cmds_Text,
                     Item.Desc.Extra_Init_Commands (J).all & ASCII.LF,
                     Pos);
               end loop;
            end if;

            Dialog.Restoring := False;

            --  If user defined, look for a system defined descriptor with
            --  same nickname. If found, propose to restore user defined
            --  value with default value.
            Overriden := False;
            if Get_Boolean (Model, Iter, User_Def_Col) then
               Sys_Item := System_Machine_List;
               while Sys_Item /= null loop
                  if Sys_Item.Desc.Nickname.all = Nickname then
                     Overriden := True;
                     exit;
                  end if;

                  Sys_Item := Sys_Item.Next;
               end loop;
            end if;
            Set_Child_Visible (Dialog.Restore_Button, Overriden);
         end;
      end if;
   end Selection_Changed;

   -------------------------
   -- Add_Machine_Clicked --
   -------------------------

   procedure Add_Machine_Clicked (W : access Gtk_Widget_Record'Class)
   is
      Dialog   : Server_List_Editor_Record
        renames Server_List_Editor_Record (W.all);
      Nickname : constant String := Query_User
        (Parent => Gtk_Window (W),
         Prompt => -"Please enter the new machine's nickname",
         Password_Mode => False);
      Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter := Null_Iter;
   begin

      if Nickname = "" then
         return;
      end if;

      Set_Child_Visible (Dialog.Right_Table, True);

      Dialog.Machines := new Item_Record'
        (Desc => new Machine_Descriptor_Record'
           (Nickname            => new String'(Nickname),
            Network_Name        => new String'(""),
            Access_Name         => new String'(""),
            Shell_Name          => new String'(""),
            User_Name           => new String'(""),
            Extra_Init_Commands => null,
            Timeout             => 3000,
            Max_Nb_Connections  => 3,
            Attribute           => User_Defined,
            Ref                 => 1),
         Next => Dialog.Machines);

      Model := Gtk_Tree_Store (Get_Model (Dialog.Tree));
      Append (Model, Iter, Null_Iter);
      Set (Model, Iter, Name_Col, Nickname);
      --  Set iter as not modified
      Set (Model, Iter, Modified_Col, False);
      --  User defined item
      Set (Model, Iter, User_Def_Col, True);

      Select_Iter (Get_Selection (Dialog.Tree),
                   Iter);
   end Add_Machine_Clicked;

   ---------------------
   -- Restore_Clicked --
   ---------------------

   procedure Restore_Clicked (W : access Gtk_Widget_Record'Class)
   is
      Dialog : Server_List_Editor_Record
        renames Server_List_Editor_Record (W.all);
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item   : Item_Access;
   begin
      Set_Child_Visible (Dialog.Restore_Button, False);
      Get_Selected (Get_Selection (Dialog.Tree),
                    Model,
                    Iter);

      if Iter /= Null_Iter then
         declare
            Current_Selection : constant String
              := Get_String (Model, Iter, Name_Col);
         begin
            if Active (Me) then
               Trace (Me, "Restoring " & Current_Selection);
            end if;

            Item := System_Machine_List;
            while Item /= null loop
               if Item.Desc.Nickname.all = Current_Selection then
                  exit;
               end if;

               Item := Item.Next;
            end loop;

            if Item /= null then
               Dialog.Restoring := True;
               --  Modified item
               Set (Gtk_Tree_Store (Model), Iter, Modified_Col, True);
               --  Not user defined item
               Set (Gtk_Tree_Store (Model), Iter, User_Def_Col, False);
               --  Set dialog values
               Set_Text (Dialog.Network_Name_Entry,
                         Item.Desc.Network_Name.all);
               Set_Text (Dialog.User_Name_Entry,
                         Item.Desc.User_Name.all);
               Set_Text (Get_Entry (Dialog.Remote_Access_Combo),
                         Item.Desc.Access_Name.all);
               Set_Text (Get_Entry (Dialog.Remote_Shell_Combo),
                         Item.Desc.Shell_Name.all);
               Set_Value (Dialog.Timeout_Spin,
                          Gdouble (Item.Desc.Timeout) / 1000.0);
               Set_Value (Dialog.Max_Nb_Connected_Spin,
                          Gdouble (Item.Desc.Max_Nb_Connections));
               Dialog.Restoring := False;
            end if;
         end;
      end if;
   end Restore_Clicked;

   ----------------------
   -- Advanced_Clicked --
   ----------------------

   procedure Advanced_Clicked (W : access Gtk_Widget_Record'Class)
   is
      Dialog    : Server_List_Editor_Record
        renames Server_List_Editor_Record (W.all);
   begin
      Set_Child_Visible (Dialog.Advanced_Table,
                         Get_Active (Dialog.Advanced_Button));
   end Advanced_Clicked;

   ---------------------------
   -- Configure_Server_List --
   ---------------------------

   procedure Configure_Server_List
     (Kernel : GPS.Kernel.Kernel_Handle)
   is
      Dialog : Server_List_Editor;
      Resp   : Gtk_Response_Type;
      Item   : Item_Access;
   begin
      Gtk_New (Dialog, Kernel);
      loop
         Resp := Run (Dialog);

         --  Apply changes
         if Resp = Gtk_Response_OK or Resp = Gtk_Response_Apply then
            --  First make sure the last edited machine is saved
            Selection_Changed (Dialog);
            --  For all config, apply in g-exttre
            Item := Dialog.Machines;
            --  Remove all machine descriptors
            Remove_All_Machine_Descriptors;
            while Item /= null loop
               Add_Machine_Descriptor (Item.Desc);
               Item := Item.Next;
            end loop;
            Run_Hook (Kernel, Server_List_Changed_Hook);
            Save_Remote_Machine_List (Kernel);
         end if;

         exit when Resp /= Gtk_Response_Apply;
      end loop;
      Destroy (Dialog);
   end Configure_Server_List;

end Remote_Server_List_Config;
