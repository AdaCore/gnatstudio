-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Tags;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
with GNAT.Expect.TTY.Remote;     use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regpat;                use GNAT.Regpat;

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Glist;
with Glib.Object;                use Glib.Object;
with Glib.Xml_Int;               use Glib.Xml_Int;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Check_Button;           use Gtk.Check_Button;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Event_Box;              use Gtk.Event_Box;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Image;                  use Gtk.Image;
with Gtk.Label;                  use Gtk.Label;
with Gtk.List;                   use Gtk.List;
with Gtk.List_Item;              use Gtk.List_Item;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Spin_Button;            use Gtk.Spin_Button;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Table;                  use Gtk.Table;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Tooltips;               use Gtk.Tooltips;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtkada;
with Gtkada.Combo;               use Gtkada.Combo;
with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Collapsing_Pane;            use Collapsing_Pane;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;

with Filesystem.Unix;            use Filesystem.Unix;
with Filesystem.Windows;         use Filesystem.Windows;
with GUI_Utils;                  use GUI_Utils;
with Interactive_Consoles;       use Interactive_Consoles;
with Projects;                   use Projects;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;
with VFS;                        use VFS;
with XML_Parsers;

package body GPS.Kernel.Remote is

   ------------
   -- Module --
   ------------

   Me : constant Debug_Handle := Create ("GPS.Kernel.Remote");

   type Remote_Module_Record is new Module_ID_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Remote_Module_ID is access all Remote_Module_Record'Class;

   procedure Customize
     (Module : access Remote_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   procedure Destroy (Module : in out Remote_Module_Record);
   --  See doc for inherited subprogram

   procedure Load_Remote_Config (Kernel : Kernel_Handle);
   --  Loads remote machines descriptors from .gps/remote.xml

   procedure Save_Remote_Config (Kernel : Kernel_Handle);
   --  Save remote machines descriptors in .gps/remote.xml

   Remote_Module : Remote_Module_ID;

   type Descriptor_Attribute is (System_Defined, User_Defined);
   --  ???

   ------------------
   -- Mirror_Paths --
   ------------------

   type Mirror_Path_Record;
   type Mirror_Path_Access is access all Mirror_Path_Record;
   type Mirror_Path_Record is record
      Local_Path  : String_Access;
      Remote_Path : String_Access;
      Need_Sync   : Boolean;
      Attribute   : Descriptor_Attribute;
      Next        : Mirror_Path_Access;
   end record;

   type Mirrors_List_Record;
   type Mirrors_List_Access is access all Mirrors_List_Record;
   type Mirrors_List_Record is record
      Nickname   : String_Access;
      Path_List  : Mirror_Path_Access;
      Next       : Mirrors_List_Access;
   end record;

   Main_Paths_Table : Mirrors_List_Access := null;
   --  ??? Global variable, should get rid of it

   function Deep_Copy (List : Mirror_Path_Access) return Mirror_Path_Access;
   function Deep_Copy (List : Mirrors_List_Access) return Mirrors_List_Access;
   --  Perform a deep copy of the list

   procedure Free (List : in out Mirror_Path_Access);
   procedure Free (List : in out Mirrors_List_Access);
   --  Free the mirror list.

   procedure Parse_Remote_Path_Node
     (Kernel    : Kernel_Handle;
      Node      : Glib.Xml_Int.Node_Ptr;
      Attribute : Descriptor_Attribute);
   --  Parse a remote_path node

   No_Path : constant String_Access := new String'("");
   --  ??? Global variable, should get rid of it

   ------------------------
   -- Machine_Descriptor --
   ------------------------

   type Machine_Descriptor_Record is
     new GNAT.Expect.TTY.Remote.Machine_Descriptor_Record
   with record
      Attribute  : Descriptor_Attribute;
      Rsync_Func : String_Access;
   end record;

   type Item_Record;
   type Item_Access is access all Item_Record;

   type Item_Record is record
      Desc : Machine_Descriptor;
      Next : Item_Access;
   end record;

   System_Machine_List : Item_Access := null;
   --  ??? Global variable, should get rid of it

   procedure Free (Item : in out Item_Access);
   --  Free memory associated with Item.

   procedure Parse_Remote_Machine_Descriptor_Node
     (Kernel    : Kernel_Handle;
      Node      : Glib.Xml_Int.Node_Ptr;
      Attribute : Descriptor_Attribute);
   --  Parse a remote_machine_descriptor node

   ------------------------
   -- Server list dialog --
   ------------------------

   Name_Col      : constant := 0;
   Modified_Col  : constant := 1;
   User_Def_Col  : constant := 2;

   Enter_Local_Path_String  : constant String := -"<enter local path here>";
   Enter_Remote_Path_String : constant String := -"<enter remote path here>";

   type Path_Row_Record is record
      Local_Entry          : Gtk_Entry;
      Remote_Entry         : Gtk_Entry;
      Need_Sync_Button     : Gtk_Check_Button;
      Remove_Button        : Gtk_Button;
   end record;
   type Path_Row is access all Path_Row_Record;
   --  This widget is the graphical representation of a mirror path.

   function Convert is new Ada.Unchecked_Conversion (System.Address, Path_Row);
   function Convert is new Ada.Unchecked_Conversion (Path_Row, System.Address);

   package Path_Row_List is new Glib.Glist.Generic_List (Path_Row);

   type Paths_Widget_Record is new Gtk_Frame_Record with record
      Table           : Gtk_Table;
      Add_Path_Button : Gtk_Button;
      List            : Path_Row_List.Glist;
      Nb_Rows         : Guint;
      On_Change_W     : Gtk_Widget;
      On_Change_P     : Widget_Callback.Simple_Handler;
   end record;
   type Paths_Widget is access all Paths_Widget_Record'Class;

   procedure Gtk_New
     (Widget      : out Paths_Widget;
      On_Change_W : Gtk_Widget;
      On_Change_P : Widget_Callback.Simple_Handler);

   procedure Set_Path_List
     (Widget    : Paths_Widget;
      Path_List : Mirror_Path_Access);
   --  Reset the widget and fills it with the path list

   function Get_Path_List
     (Widget : Paths_Widget;
      FS     : Filesystem_Record'Class) return Mirror_Path_Access;
   --  Retrieve the mirror path list represented by the widget.

   procedure Add_Path_Row
     (Widget      : Paths_Widget;
      Row_Number  : Guint;
      Path        : Mirror_Path_Access);
   --  Add a new Path row to the Mirror_Path_Widget

   procedure Remove_Path_Row
     (Widget : Paths_Widget;
      Row    : in out Path_Row);
   --  Remove the row from widget

   function Get_Path
     (Row : Path_Row;
      FS  : Filesystem_Record'Class) return Mirror_Path_Access;
   --  Retrieve the mirror path represented by the widget

   procedure On_Path_Grab_Focus (Widget : access Gtk_Widget_Record'Class);
   --  Called when a path with default string is clicked

   procedure On_Add_Path_Clicked (W : access Gtk_Widget_Record'Class);
   --  Add_Path button is clicked.

   type Remove_Row_Data is new Glib.Object.GObject_Record with record
      Widget : Paths_Widget;
      Row    : Path_Row;
   end record;
   type Remove_Row_Data_Access is access all Remove_Row_Data'Class;

   package Remove_Callback is new
     Gtk.Handlers.Callback (Remove_Row_Data);

   procedure On_Remove_Path_Clicked (W : access Remove_Row_Data'Class);
   --  One of the Remove_Path button is clicked.

   type Server_List_Editor_Record is new Gtk_Dialog_Record with record
      Kernel                : Kernel_Handle;
      Machines              : Item_Access;
      Machine_Tree          : Gtk_Tree_View;
      --  Machine config pannel
      Right_Table           : Gtk_Table;
      Nickname_Event        : Gtk_Event_Box;
      Nickname_Label        : Gtk_Label;
      Nickname_Entry        : Gtk_Entry;
      Network_Name_Entry    : Gtk_Entry;
      Remote_Access_Combo   : Gtkada_Combo;
      Remote_Shell_Combo    : Gtkada_Combo;
      Remote_Sync_Combo     : Gtkada_Combo;
      --  Advanced config panel
      Advanced_Pane         : Collapsing_Pane.Collapsing_Pane;
      Advanced_Table        : Gtk_Table;
      User_Name_Entry       : Gtk_Entry;
      Max_Nb_Connected_Spin : Gtk_Spin_Button;
      Timeout_Spin          : Gtk_Spin_Button;
      Init_Cmds_View        : Gtk_Text_View;
      --  Mirror Paths config pannel
      Paths_List            : Mirrors_List_Access;
      Paths_List_Widget     : Paths_Widget;
      --  Add/Remove/Restore buttons
      Add_Machine_Button    : Gtk_Button;
      Restore_Button        : Gtk_Button;
      Remove_Button         : Gtk_Button;
      Restoring             : Boolean := False;
      Added_Item            : Boolean := False;
      Select_Back           : Boolean := False;
   end record;
   type Server_List_Editor is access all Server_List_Editor_Record'Class;

   procedure Gtk_New
     (Dialog : out Server_List_Editor;
      Kernel : Kernel_Handle);
   --  Creates the server_list_editor dialog

   procedure On_Changed (W : access Gtk_Widget_Record'Class);
   --  Called when one of the entries has changed

   function Save
     (Dialog        : Server_List_Editor;
      Save_Selected : Boolean := False) return Boolean;
   --  Saves all modified item. Return True if succeed, false if missing
   --  mandatory values exist.
   --  If Save_Selected is set, then the selected item is saved. Else
   --  only unselected items are saved.

   procedure On_Selection_Changed (W : access Gtk_Widget_Record'Class);
   --  Called when the selected machine has changed.

   procedure On_Add_Machine_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the add_machine button is clicked.

   procedure On_Restore_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the restore button is clicked.

   procedure On_Remove_Clicked (W : access Gtk_Widget_Record'Class);
   --  Called when the remove button is clicked.

   -----------------------
   -- Server Assignment --
   -----------------------

   type Server_Config is record
      Is_Local : Boolean := True;
      --  Is_Local Tells if the server is the local machine or not
      Nickname : String_Ptr;
      --  Identifier of the server
   end record;

   type Servers_Config is array (Server_Type) of Server_Config;

   Servers : Servers_Config :=
               (others => (Is_Local => True, Nickname => new String'("")));
   --  Servers currently used. Default is the localhost.
   --  ??? Global variable, should get rid of it

   type Servers_Property is new Property_Record with record
      Servers : Servers_Config;
   end record;

   procedure Save
     (Property : access Servers_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr);

   procedure Load
     (Property : in out Servers_Property; From : Glib.Xml_Int.Node_Ptr);

   ----------------------------
   -- Project load utilities --
   ----------------------------

   type Reload_Callback_Data is record
      File   : VFS.Virtual_File;
      Kernel : Kernel_Handle;
   end record;

   package Reload_Timeout is new Gtk.Main.Timeout (Reload_Callback_Data);

   function Reload_Prj_Cb (Data : Reload_Callback_Data) return Boolean;
   --  Callback used to reload the project when build_server changed

   ---------------------
   -- Utility methods --
   ---------------------

   procedure Simple_Free is new Ada.Unchecked_Deallocation
     (Object => Argument_List, Name => Argument_List_Access);
   --  Frees the pointer without freeing internal strings

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Project_Changing
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when project is about to change

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   function From_Callback_Data_Server_Config_Changed_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (List : Mirror_Path_Access) return Mirror_Path_Access
   is
      Dest, Item_Src, Item_Dest : Mirror_Path_Access;
   begin
      Dest := null;
      Item_Src := List;

      while Item_Src /= null loop
         if Item_Dest /= null then
            Item_Dest.Next := new Mirror_Path_Record'
              (Local_Path  => new String'(Item_Src.Local_Path.all),
               Remote_Path => new String'(Item_Src.Remote_Path.all),
               Need_Sync   => Item_Src.Need_Sync,
               Attribute   => Item_Src.Attribute,
               Next        => null);
            Item_Dest := Item_Dest.Next;
         else
            Dest := new Mirror_Path_Record'
              (Local_Path  => new String'(Item_Src.Local_Path.all),
               Remote_Path => new String'(Item_Src.Remote_Path.all),
               Need_Sync   => Item_Src.Need_Sync,
               Attribute   => Item_Src.Attribute,
               Next        => null);
            Item_Dest := Dest;
         end if;

         Item_Src := Item_Src.Next;
      end loop;

      return Dest;
   end Deep_Copy;

   function Deep_Copy
     (List : Mirrors_List_Access) return Mirrors_List_Access
   is
      Dest, Item_Src, Item_Dest : Mirrors_List_Access;
   begin
      Dest := null;
      Item_Src := List;

      while Item_Src /= null loop
         if Item_Dest /= null then
            Item_Dest.Next := new Mirrors_List_Record'
              (Nickname  => new String'(Item_Src.Nickname.all),
               Path_List => Deep_Copy (Item_Src.Path_List),
               Next      => null);
            Item_Dest := Item_Dest.Next;
         else
            Dest := new Mirrors_List_Record'
              (Nickname  => new String'(Item_Src.Nickname.all),
               Path_List => Deep_Copy (Item_Src.Path_List),
               Next      => null);
            Item_Dest := Dest;
         end if;

         Item_Src := Item_Src.Next;
      end loop;

      return Dest;
   end Deep_Copy;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Mirror_Path_Access) is
      Next_Item : Mirror_Path_Access;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Mirror_Path_Record, Mirror_Path_Access);

   begin
      while List /= null loop
         Next_Item := List.Next;
         Free (List.Local_Path);
         Free (List.Remote_Path);
         Unchecked_Free (List);
         List := Next_Item;
      end loop;
   end Free;

   procedure Free (List : in out Mirrors_List_Access) is
      Next_Item : Mirrors_List_Access;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Mirrors_List_Record, Mirrors_List_Access);

   begin
      while List /= null loop
         Next_Item := List.Next;
         Free (List.Nickname);
         Free (List.Path_List);
         Unchecked_Free (List);
         List := Next_Item;
      end loop;
   end Free;

   procedure Free (Item : in out Item_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Item_Record, Item_Access);
   begin
      Unref (Item.Desc);
      Internal (Item);
   end Free;

   ------------------------------------------
   -- Parse_Remote_Machine_Descriptor_Node --
   ------------------------------------------

   procedure Parse_Remote_Machine_Descriptor_Node
     (Kernel    : Kernel_Handle;
      Node      : Glib.Xml_Int.Node_Ptr;
      Attribute : Descriptor_Attribute)
   is
      Nickname           : constant String := Get_Attribute (Node, "nickname");
      Network_Name       : constant String :=
                             Get_Attribute (Node, "network_name");
      Remote_Access      : constant String :=
                             Get_Attribute (Node, "remote_access");
      Remote_Shell       : constant String :=
                             Get_Attribute (Node, "remote_shell");
      Remote_Sync        : constant String :=
                             Get_Attribute (Node, "remote_sync", "rsync");
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
           (Kernel,
            -("XML Error: remote_machine_descriptor tags missing" &
              " a nickname attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      if Network_Name = "" then
         Console.Insert
           (Kernel,
            -("XML Error: remote_machine_descriptor tags missing" &
              " a network_name attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      if Remote_Access = "" then
         Console.Insert
           (Kernel,
            -("XML Error: remote_machine_descriptor tags missing" &
              " a remote_access attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      if Remote_Shell = "" then
         Console.Insert
           (Kernel,
            -("XML Error: remote_machine_descriptor tags missing" &
              " a remote_shell attribute"),
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
         Cmd := Child.Child;

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
         Rsync_Func          => new String'(Remote_Sync),
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

   ----------------------------
   -- Parse_Remote_Path_Node --
   ----------------------------

   procedure Parse_Remote_Path_Node
     (Kernel    : Kernel_Handle;
      Node      : Glib.Xml_Int.Node_Ptr;
      Attribute : Descriptor_Attribute)
   is
      pragma Unreferenced (Kernel);
      Nickname   : constant String := Get_Attribute (Node, "server_name");
      Paths_Item : Mirrors_List_Access;

   begin
      Paths_Item := Main_Paths_Table;

      while Paths_Item /= null loop
         exit when Paths_Item.Nickname.all = Nickname;
         Paths_Item := Paths_Item.Next;
      end loop;

      if Paths_Item = null then
         Main_Paths_Table := new Mirrors_List_Record'
           (Nickname  => new String'(Nickname),
            Path_List => null,
            Next      => Main_Paths_Table);
         Paths_Item := Main_Paths_Table;
      end if;

      declare
         Child : Node_Ptr := Node.Child;
      begin
         while Child /= null loop
            declare
               Local_Path  : constant String :=
                               Get_Attribute (Child, "local_path");
               Remote_Path : constant String :=
                               Get_Attribute (Child, "remote_path");
               Need_Sync   : constant String :=
                               Get_Attribute (Child, "need_sync", "False");

            begin
               Paths_Item.Path_List := new Mirror_Path_Record'
                 (Local_Path => new String'(Local_Path),
                  Remote_Path => new String'(Remote_Path),
                  Need_Sync   => Boolean'Value (Need_Sync),
                  Attribute   => Attribute,
                  Next        => Paths_Item.Path_List);
            end;

            Child := Child.Next;
         end loop;
      end;
   end Parse_Remote_Path_Node;

   ------------------------------
   -- Load_Remote_Machine_List --
   ------------------------------

   procedure Load_Remote_Config (Kernel : Kernel_Handle) is
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
               elsif Child.Tag.all = "remote_path_config" then
                  Parse_Remote_Path_Node
                    (Kernel, Child, User_Defined);
               end if;

               Child := Child.Next;
            end loop;
         end if;
      end if;
   end Load_Remote_Config;

   ------------------------------
   -- Save_Remote_Machine_List --
   ------------------------------

   procedure Save_Remote_Config (Kernel : Kernel_Handle) is
      Filename   : constant String := Get_Home_Dir (Kernel) & "remote.xml";
      File, Item, Child, Cmd_Node : Node_Ptr;
      Desc       : Machine_Descriptor;
      Nb_Desc    : Natural;
      M_Path     : Mirror_Path_Access;
      Paths_Item : Mirrors_List_Access;

   begin
      Trace (Me, "Saving " & Filename);

      File := new Node;
      File.Tag := new String'("remote_config");

      Nb_Desc := Get_Nb_Machine_Descriptor;

      for J in 1 .. Nb_Desc loop
         Desc := Get_Machine_Descriptor (J);

         if Machine_Descriptor_Record (Desc.all).Attribute = User_Defined then
            Item := new Node;
            Item.Tag := new String'("remote_machine_descriptor");
            Set_Attribute
              (Item, "remote_sync",
               Machine_Descriptor_Record (Desc.all).Rsync_Func.all);
            Set_Attribute (Item, "remote_shell", Desc.Shell_Name.all);
            Set_Attribute (Item, "remote_access", Desc.Access_Name.all);
            Set_Attribute (Item, "network_name", Desc.Network_Name.all);
            Set_Attribute (Item, "nickname", Desc.Nickname.all);
            Child := new Node;
            Child.Tag := new String'("user_name");
            Child.Value := new String'(Desc.User_Name.all);
            Add_Child (Item, Child);
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

         --  Save remote paths list

         Paths_Item := Main_Paths_Table;

         while Paths_Item /= null loop
            exit when Paths_Item.Nickname.all = Desc.Nickname.all;
            Paths_Item := Paths_Item.Next;
         end loop;

         if Paths_Item /= null then
            M_Path := Paths_Item.Path_List;
         else
            M_Path := null;
         end if;

         Item := new Glib.Xml_Int.Node;
         Item.Tag := new String'("remote_path_config");
         Set_Attribute (Item, "server_name", Desc.Nickname.all);

         while M_Path /= null loop
            if M_Path.Attribute = User_Defined then
               Child := new Glib.Xml_Int.Node;
               Child.Tag := new String'("mirror_path");
               Set_Attribute
                 (Child, "need_sync", Boolean'Image (M_Path.Need_Sync));
               Set_Attribute
                 (Child, "remote_path", M_Path.Remote_Path.all);
               Set_Attribute
                 (Child, "local_path", M_Path.Local_Path.all);
               Add_Child (Item, Child);
            end if;

            M_Path := M_Path.Next;
         end loop;

         Add_Child (File, Item, True);
      end loop;

      Print (File, Filename);
      Trace (Me, Filename & " saved");
      Free (File);
   end Save_Remote_Config;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Paths_Widget;
      On_Change_W : Gtk_Widget;
      On_Change_P : Widget_Callback.Simple_Handler)
   is
      Pix    : Gtk_Image;
      Label  : Gtk_Label;
   begin
      Widget := new Paths_Widget_Record;
      Gtk.Frame.Initialize (Widget, -"Path Translations");

      Widget.On_Change_W := On_Change_W;
      Widget.On_Change_P := On_Change_P;

      Gtk_New (Widget.Table, 1, 4, False);
      Add (Widget, Widget.Table);

      Gtk_New (Label, -"Local Path");
      Attach (Widget.Table, Label, 0, 1, 0, 1, Expand or Fill, 0);
      Gtk_New (Label, -"Remote Path");
      Attach (Widget.Table, Label, 1, 2, 0, 1, Expand or Fill, 0);
      Gtk_New (Label, -"keep sync");
      Attach (Widget.Table, Label, 2, 3, 0, 1, Expand or Fill, 0);

      Gtk_New (Widget.Add_Path_Button);
      Gtk_New (Pix, Stock_Add, Icon_Size_Menu);
      Add (Widget.Add_Path_Button, Pix);
      Attach (Widget.Table, Widget.Add_Path_Button, 3, 4, 1, 2, 0, 0);

      Widget.List := Path_Row_List.Null_List;
      Widget_Callback.Object_Connect
        (Widget.Add_Path_Button, "clicked",
         On_Add_Path_Clicked'Access, Widget);
   end Gtk_New;

   -------------------
   -- Set_Path_List --
   -------------------

   procedure Set_Path_List
     (Widget    : Paths_Widget;
      Path_List : Mirror_Path_Access)
   is
      Path   : Mirror_Path_Access;
      Row    : Path_Row;
      use type Path_Row_List.Glist;

   begin
      loop
         exit when Widget.List = Path_Row_List.Null_List;
         Row := Path_Row_List.Get_Data (Widget.List);
         Remove_Path_Row (Widget, Row);
      end loop;

      Ref (Widget.Add_Path_Button);
      Remove (Widget.Table, Widget.Add_Path_Button);

      Resize (Widget.Table, 1, 4);

      Widget.Nb_Rows := 1;
      Path := Path_List;

      while Path /= null loop
         Add_Path_Row (Widget, Widget.Nb_Rows, Path);
         Widget.Nb_Rows := Widget.Nb_Rows + 1;
         Path := Path.Next;
      end loop;

      Attach (Widget.Table, Widget.Add_Path_Button,
              3, 4, Widget.Nb_Rows, Widget.Nb_Rows + 1, 0, 0);
      Show_All (Widget.Table);
      Unref (Widget.Add_Path_Button);
   end Set_Path_List;

   -------------------
   -- Get_Path_List --
   -------------------

   function Get_Path_List
     (Widget : Paths_Widget;
      FS     : Filesystem_Record'Class) return Mirror_Path_Access
   is
      List : Path_Row_List.Glist;
      Row  : Path_Row;
      Path : Mirror_Path_Access;
      Item : Mirror_Path_Access;
      use type Path_Row_List.Glist;

   begin
      List := Widget.List;

      while List /= Path_Row_List.Null_List loop
         Row := Path_Row_List.Get_Data (List);

         if Path = null then
            Path := Get_Path (Row, FS);
            Item := Path;
         else
            Item.Next := Get_Path (Row, FS);

            if Item.Next /= null then
               Item := Item.Next;
            end if;
         end if;

         List := Path_Row_List.Next (List);
      end loop;

      return Path;
   end Get_Path_List;

   ------------------
   -- Add_Path_Row --
   ------------------

   procedure Add_Path_Row
     (Widget      : Paths_Widget;
      Row_Number  : Guint;
      Path        : Mirror_Path_Access)
   is
      Pix : Gtk_Image;
      Row : Path_Row;
      Data : Remove_Row_Data_Access;
   begin
      Row := new Path_Row_Record;

      Gtk_New (Row.Local_Entry);
      Set_Width_Chars (Row.Local_Entry, 15);
      Attach (Widget.Table, Row.Local_Entry, 0, 1, Row_Number, Row_Number + 1,
              Fill or Expand, 0, 0, 2);

      Gtk_New (Row.Remote_Entry);
      Set_Width_Chars (Row.Remote_Entry, 15);
      Attach (Widget.Table, Row.Remote_Entry, 1, 2, Row_Number, Row_Number + 1,
              Fill or Expand, 0, 0, 2);

      Gtk_New (Row.Need_Sync_Button);
      Attach
        (Widget.Table, Row.Need_Sync_Button, 2, 3, Row_Number, Row_Number + 1,
         0, 0, 0, 2);

      Gtk_New (Row.Remove_Button);
      Gtk_New (Pix, Stock_Remove, Icon_Size_Menu);
      Add (Row.Remove_Button, Pix);
      Attach
        (Widget.Table, Row.Remove_Button, 3, 4, Row_Number, Row_Number + 1,
         0, 0, 0, 2);
      Data := new Remove_Row_Data;
      Data.Widget := Widget;
      Data.Row    := Row;
      Remove_Callback.Object_Connect
        (Row.Remove_Button, "clicked", On_Remove_Path_Clicked'Access, Data);

      Path_Row_List.Append (Widget.List, Row);

      if Path = null then
         Set_Text (Row.Local_Entry, Enter_Local_Path_String);
         Set_Text (Row.Remote_Entry, Enter_Remote_Path_String);
         Set_Active (Row.Need_Sync_Button, False);
         Widget_Callback.Object_Connect
           (Row.Local_Entry, "grab_focus",
            On_Path_Grab_Focus'Access, Row.Local_Entry);
         Widget_Callback.Object_Connect
           (Row.Remote_Entry, "grab_focus",
            On_Path_Grab_Focus'Access, Row.Remote_Entry);

      else
         Set_Text (Row.Local_Entry, Path.Local_Path.all);
         Set_Text (Row.Remote_Entry, Path.Remote_Path.all);
         Set_Active (Row.Need_Sync_Button, Path.Need_Sync);
      end if;

      Widget_Callback.Object_Connect
        (Row.Local_Entry, "changed", Widget.On_Change_P, Widget.On_Change_W);
      Widget_Callback.Object_Connect
        (Row.Remote_Entry, "changed", Widget.On_Change_P, Widget.On_Change_W);
      Widget_Callback.Object_Connect
        (Row.Need_Sync_Button, "toggled",
         Widget.On_Change_P, Widget.On_Change_W);
      Show_All (Widget.Table);
   end Add_Path_Row;

   ------------------------
   -- On_Path_Grab_Focus --
   ------------------------

   procedure On_Path_Grab_Focus (Widget : access Gtk_Widget_Record'Class) is
      Gentry : constant Gtk_Entry := Gtk_Entry (Widget);
      Str    : constant String := Get_Text (Gentry);
   begin
      if Str = Enter_Local_Path_String
        or else Str = Enter_Remote_Path_String
      then
         Set_Text (Gentry, "");
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Path_Grab_Focus;

   ---------------------
   -- Remove_Path_Row --
   ---------------------

   procedure Remove_Path_Row
     (Widget : Paths_Widget;
      Row    : in out Path_Row)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Path_Row_Record, Path_Row);
   begin
      Remove (Widget.Table, Row.Local_Entry);
      Remove (Widget.Table, Row.Remote_Entry);
      Remove (Widget.Table, Row.Need_Sync_Button);
      Remove (Widget.Table, Row.Remove_Button);
      Path_Row_List.Remove (Widget.List, Row);
      Free (Row);
   end Remove_Path_Row;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Row : Path_Row;
      FS  : Filesystem_Record'Class) return Mirror_Path_Access
   is
      Item   : Mirror_Path_Access;
      Local  : constant String := Get_Text (Row.Local_Entry);
      Remote : constant String := Get_Text (Row.Remote_Entry);

   begin
      --  ??? get remote machine from the widget to be able to get remote FS

      if Local /= Enter_Local_Path_String
        and then Local /= ""
        and then Is_Absolute_Path (Get_Local_Filesystem, Local)
        and then Remote /= Enter_Remote_Path_String
        and then Remote /= ""
      then
         --  ??? Attribute item need to be determined

         Item := new Mirror_Path_Record'
           (Local_Path => new String'
                            (Ensure_Directory (Get_Local_Filesystem, Local)),
            Remote_Path => new String'
                            (Ensure_Directory (FS, Remote)),
            Need_Sync   => Get_Active (Row.Need_Sync_Button),
            Attribute   => User_Defined,
            Next        => null);

         return Item;
      end if;

      return null;
   end Get_Path;

   ----------------------
   -- Add_Path_Clicked --
   ----------------------

   procedure On_Add_Path_Clicked (W : access Gtk_Widget_Record'Class) is
      Widget : Paths_Widget_Record renames Paths_Widget_Record (W.all);
   begin
      Ref (Widget.Add_Path_Button);
      Remove (Widget.Table, Widget.Add_Path_Button);

      Add_Path_Row (Paths_Widget (W), Widget.Nb_Rows, null);
      Widget.Nb_Rows := Widget.Nb_Rows + 1;

      Attach (Widget.Table, Widget.Add_Path_Button,
              3, 4, Widget.Nb_Rows, Widget.Nb_Rows + 1, 0, 0);
      Show_All (Widget.Table);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Add_Path_Clicked;

   -------------------------
   -- Remove_Path_Clicked --
   -------------------------

   procedure On_Remove_Path_Clicked (W : access Remove_Row_Data'Class) is
   begin
      Remove_Path_Row (W.Widget, W.Row);
      W.Widget.On_Change_P (W.Widget.On_Change_W);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Remove_Path_Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog : out Server_List_Editor;
      Kernel : Kernel_Handle)
   is
      Nb_Machines : Natural;
      Tips        : Gtk_Tooltips;
      Main_Table  : Gtk_Table;
      Frame       : Gtk_Frame;
      Scrolled    : Gtk_Scrolled_Window;
      Model       : Gtk_Tree_Store;
      Iter        : Gtk_Tree_Iter;
      Tmp         : Gtk_Widget;
      Label       : Gtk_Label;
      Item        : Gtk_List_Item;
      Empty_List  : Boolean := True;
      Line_Nb     : Guint;
      VBox        : Gtk_Vbox;
      pragma Unreferenced (Tmp);

   begin
      Dialog := new Server_List_Editor_Record;
      Initialize
        (Dialog,
         -"Servers configuration",
         Get_Main_Window (Kernel),
         Modal + Destroy_With_Parent);
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, 670, 450);
      Gtk_New (Tips);

      Dialog.Kernel := Kernel;

      Gtk_New (Main_Table, Rows => 3, Columns => 1, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Main_Table);

      Gtk_New (Frame);
      Attach (Main_Table, Frame, 0, 1, 0, 1);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scrolled);

      Dialog.Machine_Tree := Create_Tree_View
        (Column_Types       => (Name_Col     => GType_String,
                                Modified_Col => GType_Boolean,
                                User_Def_Col => GType_Boolean),
         Column_Names       => (1 => new String'("Servers")),
         Show_Column_Titles => True,
         Selection_Mode     => Selection_Single,
         Sortable_Columns   => True,
         Initial_Sort_On    => 1,
         Hide_Expander      => False);
      Add (Scrolled, Dialog.Machine_Tree);

      --  Machine configuration

      Gtk_New_Vbox (VBox, Homogeneous => False);
      Attach (Main_Table, VBox, 1, 2, 0, 1);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Never, Policy_Automatic);
      Pack_Start (VBox, Scrolled);

      Gtk_New (Dialog.Right_Table, Rows => 7, Columns => 2,
               Homogeneous => False);
      Add_With_Viewport (Scrolled, Dialog.Right_Table);

      Line_Nb := 0;
      Create_Blue_Label (Dialog.Nickname_Label,
                         Dialog.Nickname_Event);
      Attach (Dialog.Right_Table, Dialog.Nickname_Event,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 5, 5);

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup
        (Label, "<span foreground=""red"">*</span>" & (-" Network name:"));
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Network_Name_Entry);
      Attach (Dialog.Right_Table, Dialog.Network_Name_Entry,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup
        (Label,
         "<span foreground=""red"">*</span>" & (-" Remote access tool:"));
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Remote_Access_Combo);
      Set_Editable (Get_Entry (Dialog.Remote_Access_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Access_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);

      for J in 1 .. Get_Nb_Remote_Access_Descriptor loop
         Gtk_New (Item, Locale_To_UTF8 (Get_Remote_Access_Name (J)));
         Add (Get_List (Dialog.Remote_Access_Combo), Item);
      end loop;

      Show_All (Get_List (Dialog.Remote_Access_Combo));

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup (Label, "<span foreground=""red"">*</span>" & (-" Shell:"));
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Remote_Shell_Combo);
      Set_Editable (Get_Entry (Dialog.Remote_Shell_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Shell_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);

      for J in 1 .. Get_Nb_Shell_Descriptor loop
         Gtk_New (Item, Locale_To_UTF8 (Get_Shell_Descriptor_Name (J)));
         Add (Get_List (Dialog.Remote_Shell_Combo), Item);
      end loop;
      Show_All (Get_List (Dialog.Remote_Shell_Combo));

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label, -"Sync tool:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label,
              0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Remote_Sync_Combo);
      Set_Editable (Get_Entry (Dialog.Remote_Sync_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Sync_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);

      declare
         Rsync_List : constant GNAT.OS_Lib.String_List :=
           Get_Hook_Func_List (Kernel, Rsync_Action_Hook);
      begin
         for J in Rsync_List'Range loop
            Gtk_New (Item, Locale_To_UTF8 (Rsync_List (J).all));
            Add (Get_List (Dialog.Remote_Sync_Combo), Item);
         end loop;
         Show_All (Get_List (Dialog.Remote_Sync_Combo));
      end;

      Line_Nb := Line_Nb + 1;
      Gtk_New (Dialog.Advanced_Pane, -"Advanced configuration");
      Set_State (Dialog.Advanced_Pane, Collapsed);
      Attach (Dialog.Right_Table, Dialog.Advanced_Pane,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10, 10);

      Gtk_New (Dialog.Advanced_Table,
               Rows => 4, Columns => 2, Homogeneous => False);
      Set_Expanded_Widget (Dialog.Advanced_Pane, Dialog.Advanced_Table);

      Gtk_New (Label, -"User name:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 0, 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.User_Name_Entry);
      Attach (Dialog.Advanced_Table, Dialog.User_Name_Entry, 1, 2, 0, 1,
              Fill or Expand, 0);

      Gtk_New (Label, -"Timeout value (in s):");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 1, 2,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Timeout_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Timeout_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Timeout_Spin, 1, 2, 1, 2,
              Fill or Expand, 0);

      Gtk_New (Label, -"Max number of connections:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 2, 3,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Max_Nb_Connected_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Max_Nb_Connected_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Max_Nb_Connected_Spin, 1, 2, 2, 3,
              Fill or Expand, 0);

      Gtk_New (Label, -"Extra init commands:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 3, 4,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Init_Cmds_View);
      Set_Wrap_Mode (Dialog.Init_Cmds_View, Wrap_Char);
      Set_Left_Margin (Dialog.Init_Cmds_View, 10);
      Set_Indent (Dialog.Init_Cmds_View, -10);
      Set_Pixels_Below_Lines (Dialog.Init_Cmds_View, 3);
      Attach (Dialog.Advanced_Table, Dialog.Init_Cmds_View, 1, 2, 3, 4,
              Fill or Expand);

      --  Remote paths configuration
      Line_Nb := Line_Nb + 1;
      Gtk_New
        (Dialog.Paths_List_Widget, Gtk_Widget (Dialog), On_Changed'Access);
      Attach (Dialog.Right_Table, Dialog.Paths_List_Widget,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10, 0);

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label);
      Set_Markup
        (Label,
         "<span style=""italic"">" &
         (-" Fields marked by an asterisk (") &
         ("<span foreground=""red"">*</span>") &
         (-") are mandatory") & "</span>");
      Set_Alignment (Label, 0.0, 0.5);
      Pack_End (VBox, Label, False, False, Padding => 5);

      --  Add/Restore/Remove buttons
      Gtk_New_Vbox (VBox, Homogeneous => False);
      Attach (Main_Table, VBox, 2, 3, 0, 1,
              Fill or Expand, Expand);
      Gtk_New (Dialog.Add_Machine_Button, -"Add server");
      Pack_Start (VBox, Dialog.Add_Machine_Button, False, False);
      Gtk_New (Dialog.Restore_Button, -"Remove local changes");
      Pack_Start (VBox, Dialog.Restore_Button, False, False);
      Gtk_New (Dialog.Remove_Button, -"Remove server");
      Pack_Start (VBox, Dialog.Remove_Button, False, False);
      Set_Sensitive (Dialog.Restore_Button, False);
      Set_Sensitive (Dialog.Remove_Button, False);

      --  Callbacks connections

      Widget_Callback.Object_Connect
        (Dialog.Network_Name_Entry, "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.User_Name_Entry, "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Access_Combo),
         "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Shell_Combo),
         "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Sync_Combo),
         "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Max_Nb_Connected_Spin, "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Timeout_Spin, "changed", On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Buffer (Dialog.Init_Cmds_View), "changed",
         On_Changed'Access, Dialog);
      Widget_Callback.Object_Connect
        (Get_Selection (Dialog.Machine_Tree), "changed",
         On_Selection_Changed'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Add_Machine_Button, "clicked",
         On_Add_Machine_Clicked'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Restore_Button, "clicked",
         On_Restore_Clicked'Access,
         Dialog);
      Widget_Callback.Object_Connect
        (Dialog.Remove_Button, "clicked",
         On_Remove_Clicked'Access,
         Dialog);

      --  Copy paths table

      Dialog.Paths_List := Deep_Copy (Main_Paths_Table);

      --  Fill the tree with already configured machines

      Model := Gtk_Tree_Store (Get_Model (Dialog.Machine_Tree));
      Iter := Null_Iter;
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
           = User_Defined
         then
            Set (Model, Iter, User_Def_Col, True);
         else
            Set (Model, Iter, User_Def_Col, False);
         end if;

         if J = 1 then
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
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

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (W : access Gtk_Widget_Record'Class) is
      Dialog    : Server_List_Editor_Record
                    renames Server_List_Editor_Record (W.all);
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if not Dialog.Restoring then
         Get_Selected (Get_Selection (Dialog.Machine_Tree), Model, Iter);
         Trace (Me, "Changes detected for current selection");

         --  Set this iter as modified
         Set (Gtk_Tree_Store (Model), Iter, Modified_Col, True);

         --  User defined item
         Set (Gtk_Tree_Store (Model), Iter, User_Def_Col, True);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Changed;

   ----------
   -- Save --
   ----------

   function Save
     (Dialog        : Server_List_Editor;
      Save_Selected : Boolean := False) return Boolean
   is
      function Get_Command_List
        (View : Gtk_Text_View) return Argument_List;
      --  Retrieve the commands from the gtk_text_view

      function Check_Fields
        (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
         Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
         Dialog : Server_List_Editor) return Boolean;
      --  Check that the last selected machine has correctly been entered

      ----------------------
      -- Get_Command_List --
      ----------------------

      function Get_Command_List
        (View : Gtk_Text_View) return Argument_List
      is
         I_Start : Gtk_Text_Iter;
         I_End   : Gtk_Text_Iter;
         Buffer  : constant Gtk_Text_Buffer := Get_Buffer (View);

      begin
         Get_Start_Iter (Buffer, I_Start);
         Get_End_Iter (Buffer, I_End);

         declare
            Str     : constant String := Get_Text (Buffer, I_Start, I_End);
            Idx     : Natural := Str'First;
            Idx_End : Natural := Str'Last;
            N_Lines : Natural;

         begin
            Skip_Blanks (Str, Idx);
            Skip_Blanks (Str, Idx_End, -1);
            N_Lines := Lines_Count (Str (Idx .. Idx_End));

            declare
               Substr : constant String := Str (Idx .. Idx_End);
               List   : GNAT.OS_Lib.String_List (1 .. N_Lines);
            begin
               Idx := Substr'First;

               for J in List'Range loop
                  Idx_End := Line_End (Substr, Idx);
                  List (J) := new String'(Substr (Idx .. Idx_End));
                  Idx := Next_Line (Substr, Idx);
               end loop;

               return List;
            end;
         end;
      end Get_Command_List;

      ------------------
      -- Check_Fields --
      ------------------

      function Check_Fields
        (Model          : Gtk.Tree_Store.Gtk_Tree_Store;
         Iter           : Gtk.Tree_Model.Gtk_Tree_Iter;
         Dialog         : Server_List_Editor) return Boolean
      is
         Nickname  : constant String := Get_String (Model, Iter, Name_Col);
         Has_Network_Name : Boolean;
         Has_Access_Name  : Boolean;
         Has_Shell_Name   : Boolean;
         Error_Str        : Ada.Strings.Unbounded.Unbounded_String;
         Ret              : Message_Dialog_Buttons;
         pragma Unreferenced (Ret);
         use type Ada.Strings.Unbounded.Unbounded_String;

      begin
         Has_Network_Name := Get_Text (Dialog.Network_Name_Entry) /= "";
         Has_Access_Name  := Get_Text
           (Get_Entry (Dialog.Remote_Access_Combo)) /= "";
         Has_Shell_Name   :=
           Get_Text (Get_Entry (Dialog.Remote_Shell_Combo)) /= "";

         if not Has_Network_Name
           or else not Has_Access_Name
           or else not Has_Shell_Name
         then
            Error_Str := Ada.Strings.Unbounded.To_Unbounded_String
              (-"The following items are missing for server ") &
               Nickname & ":";

            if not Has_Network_Name then
               Error_Str := Error_Str & ASCII.LF & (-"- Network name");
            end if;

            if not Has_Access_Name then
               Error_Str := Error_Str & ASCII.LF & (-"- Remote access");
            end if;

            if not Has_Shell_Name then
               Error_Str := Error_Str & ASCII.LF & (-"- Shell");
            end if;

            Ret := Message_Dialog
              (Ada.Strings.Unbounded.To_String (Error_Str),
               Dialog_Type => Error,
               Buttons     => Button_OK,
               Parent      => Gtk_Window (Dialog));

            return False;
         end if;

         return True;
      end Check_Fields;

      Model      : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item       : Item_Access;
      Path_Item  : Mirrors_List_Access;
      Attribute  : Descriptor_Attribute;
      Modified   : Boolean;

   begin
      Trace (Me, "Save");
      Model := Gtk_Tree_Store (Get_Model (Dialog.Machine_Tree));
      Iter  := Get_Iter_First (Model);

      Modified := False;

      while Iter /= Null_Iter loop
         --  Do not check or save a selected item !

         if Get_Boolean (Model, Iter, Modified_Col)
           and then (Save_Selected or else
                     not Iter_Is_Selected (Get_Selection (Dialog.Machine_Tree),
                                           Iter))
         then
            Modified := True;
            exit;
         end if;

         Next (Model, Iter);
      end loop;

      --  Nothing modified. Just return

      if not Modified then
         return True;
      end if;

      if not Check_Fields (Model, Iter, Dialog) then
         Trace (Me, "Setting back selection to uncomplete machine");
         Dialog.Select_Back := True;
         Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
         return False;
      end if;

      Set (Model, Iter, Modified_Col, False);

      declare
         Nickname : constant String := Get_String (Model, Iter, Name_Col);
      begin
         Trace (Me, "Save machine " & Nickname);

         --  Get attribute value

         if Get_Boolean (Model, Iter, User_Def_Col) then
            Trace (Me, "Internal save user defined machine");
            Attribute := User_Defined;
         else
            Trace (Me, "Internal save system defined machine");
            Attribute := System_Defined;
         end if;

         Item := Dialog.Machines;

         while Item /= null loop
            exit when Item.Desc.Nickname.all = Nickname;
            Item := Item.Next;
         end loop;

         if Item /= null then
            --  Free replaced descriptor
            Unref (Item.Desc);

            Item.Desc := new Machine_Descriptor_Record'
              (Nickname            => new String'(Nickname),
               Network_Name        => new String'
                 (Get_Text (Dialog.Network_Name_Entry)),
               Access_Name         => new String'
                 (Get_Text (Get_Entry (Dialog.Remote_Access_Combo))),
               Shell_Name          => new String'
                 (Get_Text (Get_Entry (Dialog.Remote_Shell_Combo))),
               Rsync_Func          => new String'
                 (Get_Text (Get_Entry (Dialog.Remote_Sync_Combo))),
               User_Name           => new String'
                 (Get_Text (Dialog.User_Name_Entry)),
               Timeout             =>
                 Integer (Get_Value_As_Int (Dialog.Timeout_Spin)) * 1000,
               Max_Nb_Connections  => Integer
                 (Get_Value_As_Int (Dialog.Max_Nb_Connected_Spin)),
               Extra_Init_Commands => new Argument_List'
                 (Get_Command_List (Dialog.Init_Cmds_View)),
               Attribute           => Attribute,
               Ref                 => 1);
         end if;

         --  Now save the paths

         Trace (Me, "Internal save paths");

         Path_Item := Dialog.Paths_List;
         while Path_Item /= null loop
            exit when Path_Item.Nickname.all = Nickname;
            Path_Item := Path_Item.Next;
         end loop;

         if Path_Item = null then
            Dialog.Paths_List := new Mirrors_List_Record'
              (Nickname  => new String'(Nickname),
               Path_List => null,
               Next      => Dialog.Paths_List);
            Path_Item := Dialog.Paths_List;
         end if;

         Free (Path_Item.Path_List);

            Path_Item.Path_List := Get_Path_List
              (Dialog.Paths_List_Widget,
               Get_Filesystem_From_Shell
                 (Get_Text (Get_Entry (Dialog.Remote_Shell_Combo))));
      end;

      return True;
   end Save;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure On_Selection_Changed (W : access Gtk_Widget_Record'Class) is
      Dialog    : Server_List_Editor_Record
                    renames Server_List_Editor_Record (W.all);
      Model     : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item      : Item_Access;
      Sys_Item  : Item_Access;
      Overriden : Boolean;
      User_Only : Boolean;
      Path_Item : Mirrors_List_Access;

   begin
      if Dialog.Select_Back then
         --  Do not change dialog values
         Dialog.Select_Back := False;

         return;
      end if;

      --  If we just added a machine, do not perform any save

      if not Dialog.Added_Item and then not Save (Server_List_Editor (W)) then
         return;
      end if;

      Dialog.Added_Item := False;

      --  Now reinit the dialog values

      Get_Selected
        (Get_Selection (Dialog.Machine_Tree), Gtk_Tree_Model (Model), Iter);

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
            Set_Text
              (Get_Entry (Dialog.Remote_Sync_Combo),
               Machine_Descriptor_Record (Item.Desc.all).Rsync_Func.all);
            Set_Value (Dialog.Timeout_Spin,
                       Gdouble (Item.Desc.Timeout) / 1000.0);
            Set_Value (Dialog.Max_Nb_Connected_Spin,
                       Gdouble (Item.Desc.Max_Nb_Connections));

            Set_Text (Get_Buffer (Dialog.Init_Cmds_View), "");

            if Item.Desc.Extra_Init_Commands /= null then
               for J in Item.Desc.Extra_Init_Commands'Range loop
                  Insert_At_Cursor
                    (Get_Buffer (Dialog.Init_Cmds_View),
                     Item.Desc.Extra_Init_Commands (J).all & ASCII.LF);
               end loop;
            end if;

            Dialog.Restoring := False;

            --  If user defined, look for a system defined descriptor with
            --  same nickname. If found, propose to restore user defined
            --  value with default value.

            Overriden := False;
            User_Only := False;

            if Get_Boolean (Model, Iter, User_Def_Col) then
               Sys_Item := System_Machine_List;
               User_Only := True;

               while Sys_Item /= null loop
                  if Sys_Item.Desc.Nickname.all = Nickname then
                     Overriden := True;
                     User_Only := False;
                     exit;
                  end if;

                  Sys_Item := Sys_Item.Next;
               end loop;
            end if;

            Set_Sensitive (Dialog.Restore_Button, Overriden);
            Set_Sensitive (Dialog.Remove_Button, User_Only);

            --  Now fill the path list

            Path_Item := Dialog.Paths_List;

            while Path_Item /= null loop
               exit when Path_Item.Nickname.all = Nickname;

               Path_Item := Path_Item.Next;
            end loop;

            Set_Path_List (Dialog.Paths_List_Widget, Path_Item.Path_List);
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Selection_Changed;

   -------------------------
   -- Add_Machine_Clicked --
   -------------------------

   procedure On_Add_Machine_Clicked (W : access Gtk_Widget_Record'Class) is
      Dialog   : Server_List_Editor_Record
                   renames Server_List_Editor_Record (W.all);
      Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter := Null_Iter;

   begin
      --  First save the machines

      if Save (Server_List_Editor (W), True) then
         declare
            Nickname : constant String := Query_User
              (Parent => Gtk_Window (W),
               Prompt => -"Please enter the new machine's nickname",
               Password_Mode => False);

         begin
            if Nickname = "" then
               return;
            end if;

            --  ??? Check that nickname does not already exist

            Set_Child_Visible (Dialog.Right_Table, True);

            Dialog.Machines := new Item_Record'
              (Desc => new Machine_Descriptor_Record'
                 (Nickname            => new String'(Nickname),
                  Network_Name        => new String'(""),
                  Access_Name         => new String'(""),
                  Shell_Name          => new String'(""),
                  Rsync_Func          => new String'("rsync"),
                  User_Name           => new String'(""),
                  Extra_Init_Commands => null,
                  Timeout             => 10000,
                  Max_Nb_Connections  => 3,
                  Attribute           => User_Defined,
                  Ref                 => 1),
               Next => Dialog.Machines);
            Dialog.Paths_List := new Mirrors_List_Record'
              (Nickname  => new String'(Nickname),
               Path_List => null,
               Next      => Dialog.Paths_List);

            Model := Gtk_Tree_Store (Get_Model (Dialog.Machine_Tree));
            Append (Model, Iter, Null_Iter);
            Set (Model, Iter, Name_Col, Nickname);

            --  Set iter as modified
            Set (Model, Iter, Modified_Col, True);

            --  User defined item
            Set (Model, Iter, User_Def_Col, True);

            --  Tell the Selection_Changed callback that a new item
            --  has been added.
            Dialog.Added_Item := True;

            --  Select this newly created machine
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Add_Machine_Clicked;

   ---------------------
   -- Restore_Clicked --
   ---------------------

   procedure On_Restore_Clicked (W : access Gtk_Widget_Record'Class) is
      Dialog : Server_List_Editor_Record
                 renames Server_List_Editor_Record (W.all);
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item   : Item_Access;

   begin
      Set_Sensitive (Dialog.Restore_Button, False);
      Get_Selected (Get_Selection (Dialog.Machine_Tree), Model, Iter);

      if Iter /= Null_Iter then
         declare
            Current_Selection : constant String :=
                                  Get_String (Model, Iter, Name_Col);
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

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Restore_Clicked;

   --------------------
   -- Remove_Clicked --
   --------------------

   procedure On_Remove_Clicked (W : access Gtk_Widget_Record'Class) is
      Dialog    : Server_List_Editor_Record
                    renames Server_List_Editor_Record (W.all);
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item      : Item_Access;
      Prev      : Item_Access;
      Ret       : Message_Dialog_Buttons;

   begin
      Get_Selected (Get_Selection (Dialog.Machine_Tree), Model, Iter);

      if Iter /= Null_Iter then
         declare
            Current_Selection : constant String :=
                                  Get_String (Model, Iter, Name_Col);
         begin
            Ret := Message_Dialog
              ((-"Are you sure you want to remove server ") &
               Current_Selection & " ?",
               Dialog_Type => Confirmation,
               Buttons     => Button_OK or Button_Cancel,
               Title       => "Server removal confirmation");

            if Ret = Button_Cancel then
               return;
            end if;

            if Active (Me) then
               Trace (Me, "Removing " & Current_Selection);
            end if;

            Item := Dialog.Machines;
            Prev := null;

            while Item /= null loop
               if Item.Desc.Nickname.all = Current_Selection then
                  if Prev = null then
                     Dialog.Machines := Item.Next;
                  else
                     Prev.Next := Item.Next;
                  end if;

                  Free (Item);
                  exit;
               end if;

               Prev := Item;
               Item := Item.Next;
            end loop;

            if Dialog.Machines = null then
               Set_Child_Visible (Dialog.Right_Table, False);
               Set_Sensitive (Dialog.Restore_Button, False);
               Set_Sensitive (Dialog.Remove_Button, False);
            end if;

            Remove (Gtk_Tree_Store (Model), Iter);
            Iter := Get_Iter_First (Model);

            if Iter /= Null_Iter then
               Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Remove_Clicked;

   ---------------------------
   -- Configure_Server_List --
   ---------------------------

   procedure Configure_Server_List
     (Kernel : GPS.Kernel.Kernel_Handle)
   is
      Dialog : Server_List_Editor;
      Resp   : Gtk_Response_Type;
      Item   : Item_Access;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Item_Record, Item_Access);

   begin
      Gtk_New (Dialog, Kernel);

      loop
         Resp := Run (Dialog);

         --  Apply changes

         if Resp = Gtk_Response_OK or Resp = Gtk_Response_Apply then
            --  First make sure the last edited machine is saved

            if Save (Dialog, True) then
               --  For all config, apply in g-exttre

               Item := Dialog.Machines;
               Remove_All_Machine_Descriptors;

               while Item /= null loop
                  Add_Machine_Descriptor (Item.Desc);
                  Item := Item.Next;
               end loop;

               --  Save mirror paths
               Free (Main_Paths_Table);

               --  Duplicate it instead of just copy it, for easier handling
               --  of cancel/apply cases
               Main_Paths_Table := Deep_Copy (Dialog.Paths_List);

               Save_Remote_Config (Kernel);
               Run_Hook (Kernel, Server_List_Changed_Hook);
            end if;
         end if;

         exit when Resp /= Gtk_Response_Apply;
      end loop;

      --  Destroy duplicated machine list
      while Dialog.Machines /= null loop
         Item := Dialog.Machines.Next;
         Unref (Dialog.Machines.Desc);
         Unchecked_Free (Dialog.Machines);
         Dialog.Machines := Item;
      end loop;

      --  Destroy duplicated path table
      Free (Dialog.Paths_List);

      --  Destroy the widget
      Destroy (Dialog);
   end Configure_Server_List;

   ----------------------------------
   -- From_Callback_Data_Sync_Hook --
   ----------------------------------

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class is
   begin
      declare
         Tool_Name    : constant String  := Nth_Arg (Data, 2);
         Src_Name     : constant String  := Nth_Arg (Data, 3);
         Dest_Name    : constant String  := Nth_Arg (Data, 4);
         Queue_Id     : constant String  := Nth_Arg (Data, 5);
         Src_Path     : constant String  := Nth_Arg (Data, 6);
         Dest_Path    : constant String  := Nth_Arg (Data, 7);
         Sync_Deleted : constant Boolean := Nth_Arg (Data, 8);
         Synchronous  : constant Boolean := Nth_Arg (Data, 9);

      begin
         return Rsync_Hooks_Args'
           (Hooks_Data with
            Tool_Name_Length => Tool_Name'Length,
            Src_Name_Length  => Src_Name'Length,
            Dest_Name_Length => Dest_Name'Length,
            Queue_Id_Length  => Queue_Id'Length,
            Src_Path_Length  => Src_Path'Length,
            Dest_Path_Length => Dest_Path'Length,
            Tool_Name        => Tool_Name,
            Src_Name         => Src_Name,
            Dest_Name        => Dest_Name,
            Queue_Id         => Queue_Id,
            Src_Path         => Src_Path,
            Dest_Path        => Dest_Path,
            Sync_Deleted     => Sync_Deleted,
            Synchronous      => Synchronous);
      end;
   end From_Callback_Data_Sync_Hook;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Rsync_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
        new Callback_Data'Class'(Create (Script, 9));
   begin
      Set_Nth_Arg (D.all, 1, Hook_Name);
      Set_Nth_Arg (D.all, 2, Data.Tool_Name);
      Set_Nth_Arg (D.all, 3, Data.Src_Name);
      Set_Nth_Arg (D.all, 4, Data.Dest_Name);
      Set_Nth_Arg (D.all, 5, Data.Queue_Id);
      Set_Nth_Arg (D.all, 6, Data.Src_Path);
      Set_Nth_Arg (D.all, 7, Data.Dest_Path);
      Set_Nth_Arg (D.all, 8, Data.Sync_Deleted);
      Set_Nth_Arg (D.all, 9, Data.Synchronous);
      return D;
   end Create_Callback_Data;

   ----------------------------------------------
   -- From_Callback_Server_Config_Changed_Hook --
   ----------------------------------------------

   function From_Callback_Data_Server_Config_Changed_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Server : Server_Type;
   begin
      Server  := Server_Type'Value (String'(Nth_Arg (Data, 2)));

      declare
         Nickname  : constant String := Nth_Arg (Data, 3);
      begin
         return Server_Config_Changed_Hooks_Args'
           (Hooks_Data with
            Nickname_Length => Nickname'Length,
            Server          => Server,
            Nickname        => Nickname);
      end;
   end From_Callback_Data_Server_Config_Changed_Hook;

   ---------------------------
   -- Create_Callbackc_Data --
   ---------------------------

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Server_Config_Changed_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
        new Callback_Data'Class'(Create (Script, 3));
   begin
      Set_Nth_Arg (D.all, 1, Hook_Name);
      Set_Nth_Arg (D.all, 2, Server_Type'Image (Data.Server));
      Set_Nth_Arg (D.all, 3, Data.Nickname);
      return D;
   end Create_Callback_Data;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Unix_FS    : Unix_Filesystem_Record;
      Windows_FS : Windows_Filesystem_Record;
   begin
      --  Register synchronisation hook
      Register_Hook_Data_Type
        (Kernel, Rsync_Hook_Type,
         Args_Creator => From_Callback_Data_Sync_Hook'Access);
      Register_Hook_Return_Boolean
        (Kernel, Rsync_Action_Hook, Rsync_Hook_Type);

      --  Register server config changed hook
      Register_Hook_Data_Type
        (Kernel, Server_Config_Changed_Hook_Type,
         Args_Creator => From_Callback_Data_Server_Config_Changed_Hook'Access);
      Register_Hook_No_Return
        (Kernel, Server_Config_Changed_Hook, Server_Config_Changed_Hook_Type);

      --  Register server list changed hook
      Register_Hook_No_Args
        (Kernel, Server_List_Changed_Hook);

      --  Load user specific machine list
      Load_Remote_Config (Kernel_Handle (Kernel));

      --  Register the module
      Remote_Module := new Remote_Module_Record;
      Remote_Module.Kernel := Kernel_Handle (Kernel);
      Register_Module
        (Remote_Module, Kernel, "remote");

      Initialize_Module (Unix_FS);
      Initialize_Module (Windows_FS);

      --  Connect to project_changing hook
      Add_Hook (Kernel, Project_Changing_Hook,
                Wrapper (On_Project_Changing'Access), "gps.kernel.remote");
   end Register_Module;

   ----------
   -- Save --
   ----------

   procedure Save
     (Property : access Servers_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr)
   is
      Srv : Node_Ptr;
   begin
      Trace (Me, "Saving remote property");

      for J in Remote_Server_Type'Range loop
         Srv := new Glib.Xml_Int.Node;
         Srv.Tag := new String'(Server_Type'Image (J));
         Srv.Value := new String'(Property.Servers (J).Nickname.all);
         Add_Child (Node, Srv);
      end loop;
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load
     (Property : in out Servers_Property; From : Glib.Xml_Int.Node_Ptr)
   is
      Srv :  Node_Ptr;
   begin
      Trace (Me, "Loading remote property");

      if From.Child /= null then
         for J in Remote_Server_Type'Range loop
            Srv := Find_Tag (From.Child, Server_Type'Image (J));

            if Srv /= null
              and then Is_Configured (Srv.Value.all)
              and then Srv.Value.all /= ""
            then
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Srv.Value.all));
            else
               Property.Servers (J) :=
                 (Is_Local => True, Nickname => new String'(""));
            end if;
         end loop;

         Property.Servers (GPS_Server) :=
           (Is_Local => True,
            Nickname => new String'(""));

      else
         for J in Server_Type'Range loop
            Property.Servers (J) :=
              (Is_Local => True,
               Nickname => new String'(""));
         end loop;
      end if;
   end Load;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changing
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D          : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Property   : Servers_Property;
      Prop       : Property_Access;
      Success    : Boolean;
      Local_File : VFS.Virtual_File;

   begin
      --  Get local file equivalence for project

      if not Is_Local (D.File) then
         Local_File :=
           Create (To_Local (Full_Name (D.File).all, Get_Host (D.File)));
      else
         Local_File := D.File;
      end if;

      --  Get servers associated with this file

      Trace (Me, "Loading servers_config property for file " &
             Full_Name (Local_File).all);
      Get_Property
        (Property, Local_File,
         Name => "servers_config", Found => Success);

      if not Success then
         Trace (Me, "Property servers_config does not exist. Create it");

         if not Is_Local (D.File) then
            for J in Remote_Server_Type'Range loop
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Get_Host (D.File)));
            end loop;

         else
            for J in Remote_Server_Type'Range loop
               Property.Servers (J) := (Is_Local => True,
                                        Nickname => new String'(""));
            end loop;
         end if;

         Prop := new Servers_Property'(Property);
         Set_Property (Local_File, "servers_config", Prop, Persistent => True);
      end if;

      for J in Remote_Server_Type'Range loop
         Glib.Free (Servers (J).Nickname);
         Servers (J) :=
           (Is_Local => Property.Servers (J).Is_Local,
            Nickname => new String'(Property.Servers (J).Nickname.all));
      end loop;

      --  If Project is loaded from a distant host, force build_server as
      --  distant host.

      if not Is_Local (D.File)
        and then Get_Host (D.File) /= Servers (Build_Server).Nickname.all
      then
         Trace (Me, "Assign build server: project loaded from remote host");
         Assign (Kernel_Handle (Kernel),
                 Build_Server,
                 Get_Host (D.File),
                 Local_File,
                 Reload_Prj => False);
      end if;

      if not Is_Local (Build_Server) then
         Trace (Me, "Start synchronization of build_server");
         --  Use a 2-way synchronization: the project might be a local project
         --  that will be compiled on a remote machine (in this case, the
         --  project needs to be copied on this remote machine), or a remote
         --  project is loaded locally (in this case, the remote project needs
         --  to be copied locally).
         --  Note that the Sync_Deleted option has to be set to false, in order
         --  to be non destructive. Only the most recent files are copied, no
         --  deletion is performed.
         Synchronize
           (Kernel_Handle (Kernel), GPS_Server, Build_Server, "", False);
         Synchronize
           (Kernel_Handle (Kernel), Build_Server, GPS_Server, "", False);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Project_Changing;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Remote_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Child                     : Node_Ptr;
      Name                      : String_Ptr;
      Start_Command             : String_Ptr;
      Start_Command_Common_Args : String_List_Access;
      Start_Command_User_Args   : String_List_Access;
      User_Prompt_Ptrn          : String_Ptr;
      Password_Prompt_Ptrn      : String_Ptr;
      Passphrase_Prompt_Ptrn    : String_Ptr;
      Extra_Ptrn_Length         : Natural;
      Use_Cr_Lf                 : Boolean;

   begin
      if Node.Tag.all = "remote_machine_descriptor" then
         Trace (Me, "Customize: 'remote_machine_descriptor'");
         Parse_Remote_Machine_Descriptor_Node
           (Module.Kernel, Node, System_Defined);

      elsif Node.Tag.all = "remote_path_config" then
         Trace (Me, "Customize: 'remote_path_config'");
         Parse_Remote_Path_Node
           (Module.Kernel, Node, System_Defined);

      elsif Node.Tag.all = "remote_connection_config" then
         Trace (Me, "Initialize_Remote_Config: 'remote_connection_config'");

         Name := new String'(Get_Attribute (Node, "name"));

         if Name.all = "" then
            Console.Insert
              (Module.Kernel,
               -("XML Error: remote_connection_config tag is missing a " &
                 "name attribute"),
               Add_LF => True, Mode => Error);
            return;
         end if;

         Start_Command := Get_Field (Node, "start_command");

         if Start_Command = null then
            Console.Insert
              (Module.Kernel,
               -("XML Error: remote_connection_config is missing a " &
               "start_command field"),
               Add_LF => True, Mode => Error);
            return;
         end if;

         Start_Command_Common_Args := Argument_String_To_List
           (Get_Field (Node, "start_command_common_args").all);
         Start_Command_User_Args := Argument_String_To_List
           (Get_Field (Node, "start_command_user_args").all);
         --  If null, the default value will be used in Add_Remote_Access_Desc.
         User_Prompt_Ptrn       := Get_Field (Node, "user_prompt_ptrn");
         Password_Prompt_Ptrn   := Get_Field (Node, "password_prompt_ptrn");
         Passphrase_Prompt_Ptrn := Get_Field (Node, "passphrase_prompt_ptrn");

         declare
            Use_Cr_Lf_String_Access : constant String_Ptr :=
                                      Get_Field (Node, "use_cr_lf");
         begin
            if Use_Cr_Lf_String_Access = null then
               Use_Cr_Lf := False;
            else
               Use_Cr_Lf := Boolean'Value (Use_Cr_Lf_String_Access.all);
            end if;

         exception
            when others =>
               Use_Cr_Lf := False;
         end;

         Child := Node.Child;
         Extra_Ptrn_Length := 0;

         while Child /= null loop
            if Child.Tag.all = "extra_ptrn" then
               Extra_Ptrn_Length := Extra_Ptrn_Length + 1;
            end if;

            Child := Child.Next;
         end loop;

         declare
            Extra_Ptrns : Extra_Prompts (1 .. Extra_Ptrn_Length);
            Auto_Answer : Boolean;
            Str_Access  : String_Ptr;

         begin
            Child := Node.Child;
            Extra_Ptrn_Length := 0;

            while Child /= null loop
               if Child.Tag.all = "extra_ptrn" then
                  Extra_Ptrn_Length := Extra_Ptrn_Length + 1;
                  Auto_Answer := Boolean'Value
                    (Get_Attribute (Child, "auto_answer", "true"));
                  Str_Access := Child.Value;

                  if Auto_Answer then
                     Extra_Ptrns (Extra_Ptrn_Length) :=
                       (Auto_Answer => True,
                        Ptrn        => new Pattern_Matcher'(Compile
                          (Str_Access.all, Single_Line or Multiple_Lines)),
                        Answer      => new String'(Get_Attribute
                          (Child, "answer", "")));
                  else
                     Extra_Ptrns (Extra_Ptrn_Length) :=
                       (Auto_Answer => False,
                        Ptrn        => new Pattern_Matcher'(Compile
                          (Str_Access.all, Single_Line or Multiple_Lines)),
                        Question    => new String'(Get_Attribute
                          (Child, "question", "")));
                  end if;
               end if;

               Child := Child.Next;
            end loop;

            Add_Remote_Access_Descriptor
              (Name                      => Name.all,
               Start_Command             => Start_Command.all,
               Start_Command_Common_Args => Start_Command_Common_Args.all,
               Start_Command_User_Args   => Start_Command_User_Args.all,
               User_Prompt_Ptrn          => User_Prompt_Ptrn,
               Password_Prompt_Ptrn      => Password_Prompt_Ptrn,
               Passphrase_Prompt_Ptrn    => Passphrase_Prompt_Ptrn,
               Extra_Prompt_Array        => Extra_Ptrns,
               Use_Cr_Lf                 => Use_Cr_Lf);
         end;

         Glib.Xml_Int.Free (Name);
      end if;
   end Customize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Remote_Module_Record) is
      pragma Unreferenced (Module);
   begin
      Remote_Module := null;
   end Destroy;

   ------------------------
   -- To_Remote_Possible --
   ------------------------

   function To_Remote_Possible
     (Path : String;
      To   : String) return Boolean
   is
      Mirror         : Mirror_Path_Access;
      Path_List_Item : Mirrors_List_Access;

   begin
      if To = "" then
         return True;
      end if;

      --  Search for mirror path in 'To' config

      Path_List_Item := Main_Paths_Table;
      Mirror := null;

      while Path_List_Item /= null loop
         if Path_List_Item.Nickname.all = To then
            Mirror := Path_List_Item.Path_List;
            exit;
         end if;

         Path_List_Item := Path_List_Item.Next;
      end loop;

      while Mirror /= null loop
         if Is_Subtree (Get_Local_Filesystem, Mirror.Local_Path.all, Path) then
            return True;
         end if;

         Mirror := Mirror.Next;
      end loop;

      return False;
   end To_Remote_Possible;

   -----------------------
   -- To_Local_Possible --
   -----------------------

   function To_Local_Possible
     (Path : String;
      From : String) return Boolean
   is
      Mirror         : Mirror_Path_Access;
      Path_List_Item : Mirrors_List_Access;

   begin
      if From = "" then
         return True;
      end if;

      --  Search for mirror path in 'From' config

      Path_List_Item := Main_Paths_Table;
      Mirror := null;

      while Path_List_Item /= null loop
         if Path_List_Item.Nickname.all = From then
            Mirror := Path_List_Item.Path_List;
            exit;
         end if;

         Path_List_Item := Path_List_Item.Next;
      end loop;

      while Mirror /= null loop
         if Is_Subtree
              (Get_Filesystem (From), Mirror.Remote_Path.all, Path)
         then
            return True;
         end if;

         Mirror := Mirror.Next;
      end loop;

      return False;
   end To_Local_Possible;

   ---------------
   -- To_Remote --
   ---------------

   function To_Remote
     (Path       : String;
      To         : Server_Type;
      Unix_Style : Boolean := False) return String is
   begin
      return To_Remote (Path, Get_Nickname (To), Unix_Style);
   end To_Remote;

   ---------------
   -- To_Remote --
   ---------------

   function To_Remote
     (Path       : String;
      To         : String;
      Unix_Style : Boolean := False) return String
   is
      Path_From      : String_Access;
      Path_To        : String_Access;
      Mirror         : Mirror_Path_Access;
      Path_List_Item : Mirrors_List_Access;

   begin
      --  If From and To are the same machine (and no unix path translation is
      --  needed), just return Path

      if To = "" then
         if Unix_Style then
            return To_Unix (Get_Local_Filesystem, Path);
         else
            return Path;
         end if;
      end if;

      if Active (Me) then
         Trace (Me, "To_Remote: " & Path & " to server " & To);
      end if;

      --  Search for mirror path in 'To' config

      Path_List_Item := Main_Paths_Table;
      Mirror := null;

      while Path_List_Item /= null loop
         if Path_List_Item.Nickname.all = To then
            Mirror := Path_List_Item.Path_List;
            exit;
         end if;

         Path_List_Item := Path_List_Item.Next;
      end loop;

      while Mirror /= null loop
         if Is_Subtree (Get_Local_Filesystem, Mirror.Local_Path.all, Path) then
            Path_From := Mirror.Local_Path;
            Path_To   := Mirror.Remote_Path;
            exit;
         end if;

         Mirror := Mirror.Next;
      end loop;

      if Path_From = null or Path_To = null then
         --  Not configured mirror path

         Path_From := No_Path;
         Path_To := No_Path;
      end if;

      --  At this point, we have the from and to moint points. Let's translate
      --  the path

      declare
         To_Filesystem : Filesystem_Record'Class := Get_Filesystem (To);
         U_Path     : constant String := To_Unix (Get_Local_Filesystem, Path);
         --  The input path in unix style
         U_Frompath : constant String := To_Unix (Get_Local_Filesystem,
                                                  Path_From.all);
         --  The local root dir, in unix style
         U_Subpath  : constant String :=
           U_Path (U_Path'First + U_Frompath'Length .. U_Path'Last);

      begin
         if Unix_Style then
            if Active (Me) then
               Trace (Me, "result: '" &
                      To_Unix (To_Filesystem, Path_To.all) & U_Subpath &
                      "'");
            end if;

            return To_Unix (To_Filesystem, Path_To.all) & U_Subpath;

         else
            if Active (Me) then
               Trace (Me, "local filesystem " &
                      Ada.Tags.External_Tag (Get_Local_Filesystem'Tag));
               Trace (Me, "from_path: " & Path_From.all);
               Trace (Me, "unix form: " & U_Frompath);
               Trace (Me, "result: '" &
                      Concat (To_Filesystem,
                              Path_To.all,
                              From_Unix (To_Filesystem, U_Subpath)) &
                      "'");
            end if;

            return Concat (To_Filesystem,
                           Path_To.all,
                           From_Unix (To_Filesystem, U_Subpath));
         end if;
      end;
   end To_Remote;

   --------------
   -- To_Local --
   --------------

   function To_Local (Path : String; From : Server_Type) return String is
   begin
      return To_Local (Path, Servers (From).Nickname.all);
   end To_Local;

   --------------
   -- To_Local --
   --------------

   function To_Local (Path : String; From : String) return String is
      Path_From       : String_Access;
      Path_To         : String_Access;
      Mirror          : Mirror_Path_Access;
      Path_List_Item  : Mirrors_List_Access;

   begin
      --  If From and To are the same machine, just return Path

      if From = "" then
         return Path;
      end if;

      if Active (Me) then
         Trace (Me, "To_Local: " & Path & " on server " & From);
      end if;

      --  Search for mirror path in 'From' config

      Path_List_Item := Main_Paths_Table;

      while Path_List_Item /= null loop
         if Path_List_Item.Nickname.all = From then
            Mirror := Path_List_Item.Path_List;
            exit;
         end if;

         Path_List_Item := Path_List_Item.Next;
      end loop;

      while Mirror /= null loop
         if Is_Subtree
           (Get_Filesystem (From), Mirror.Remote_Path.all, Path)
         then
            Path_To   := Mirror.Local_Path;
            Path_From := Mirror.Remote_Path;
            exit;
         end if;

         Mirror := Mirror.Next;
      end loop;

      if Path_From = null or Path_To = null then
         --  Not configured mirror path. Try to convert all paths
         Path_From := No_Path;
         Path_To   := No_Path;
      end if;

      if Active (Me) then
         Trace (Me, "Path_From: " & Path_From.all);
         Trace (Me, "Path_To: " & Path_To.all);
      end if;

      --  At this point, we have the from and to moint points. Let's translate
      --  the path

      declare
         FS         : Filesystem_Record'Class := Get_Filesystem (From);
         U_Path     : constant String := To_Unix (FS, Path);
         --  The input path in unix style
         U_Frompath : constant String := To_Unix (FS, Path_From.all);
         --  The local root dir, in unix style
         U_Subpath  : constant String :=
           U_Path (U_Path'First + U_Frompath'Length .. U_Path'Last);

      begin
         return Concat
           (Get_Local_Filesystem,
            Path_To.all,
            From_Unix (Get_Local_Filesystem, U_Subpath));
      end;
   end To_Local;

   ------------------
   -- To_Unix_Path --
   ------------------

   function To_Unix_Path
     (Path       : String;
      Server     : Server_Type;
      Use_Cygwin : Boolean := False) return String is
   begin
      return To_Unix (Get_Filesystem (Server), Path, Use_Cygwin);
   end To_Unix_Path;

   -------------------
   -- Reload_Prj_Cb --
   -------------------

   function Reload_Prj_Cb (Data : Reload_Callback_Data) return Boolean is
   begin
      Trace (Me, "Reloading the project");
      Load_Project (Data.Kernel, Data.File);
      return False;
   end Reload_Prj_Cb;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Kernel     : Kernel_Handle;
      Server     : Server_Type;
      Nickname   : String;
      Prj_File   : VFS.Virtual_File := VFS.No_File;
      Reload_Prj : Boolean := False)
   is
      Data        : aliased Server_Config_Changed_Hooks_Args :=
                     (Hooks_Data with
                       Nickname_Length => Nickname'Length,
                       Server          => Server,
                       Nickname        => Nickname);
      Timeout     : constant Guint32 := 50;
      Id          : Timeout_Handler_Id;
      Load_Data   : Reload_Callback_Data;
      Property    : Servers_Property;
      Prop        : Property_Access;
      Found       : Boolean;
      The_File    : VFS.Virtual_File;
      pragma Unreferenced (Id);

   begin
      Glib.Free (Servers (Server).Nickname);
      if Nickname /= "" and Nickname /= Local_Nickname then
         Servers (Server) := (Is_Local => False,
                              Nickname => new String'(Nickname));
      else
         Servers (Server) := (Is_Local => True,
                              Nickname => new String'(""));
      end if;

      --  Update the project's property

      if Prj_File /= VFS.No_File then
         The_File := Prj_File;
      else
         The_File := Project_Path (Get_Project (Kernel));
      end if;

      Get_Property (Property, The_File, "servers_config", Found);

      if Found then
         Trace (Me, "Saving servers_config property");
         Glib.Free (Property.Servers (Server).Nickname);
         Property.Servers (Server) :=
           (Is_Local => Servers (Server).Is_Local,
            Nickname => new String'(Servers (Server).Nickname.all));
         Remove_Property (The_File, "servers_config");

         Prop := new Servers_Property'(Property);
         Set_Property (The_File,
                       "servers_config",
                       Prop,
                       Persistent => True);
      end if;

      --  Reload project if Build_Server has been assigned

      if Server = Build_Server and then Reload_Prj then
         Load_Data.Kernel := Kernel;
         Load_Data.File := Project_Path (Get_Project (Kernel),
                                         Get_Nickname (Build_Server));
         if Get_Host (Load_Data.File) /= Get_Nickname (Build_Server) then
            Insert (Kernel,
                    -"Error: the project " & Full_Name (Load_Data.File).all &
                    (-" has no path equivalence on remote machine ") &
                    Get_Nickname (Build_Server));
         else
            Id := Reload_Timeout.Add (Timeout, Reload_Prj_Cb'Access,
                                      Load_Data);
         end if;
      end if;

      if Active (Me) then
         Trace (Me, "run server_changed hook for " &
                Server_Type'Image (Server) & " => " & Data.Nickname);
      end if;

      Run_Hook (Kernel, Server_Config_Changed_Hook, Data'Unchecked_Access);
   end Assign;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (Server : Server_Type) return String is
   begin
      return Servers (Server).Nickname.all;
   end Get_Nickname;

   ----------------------------
   -- Get_Printable_Nickname --
   ----------------------------

   function Get_Printable_Nickname (Server : Server_Type) return String is
      Nickname : constant String := Get_Nickname (Server);
   begin
      if Nickname = "" then
         return Local_Nickname;
      else
         return Nickname;
      end if;
   end Get_Printable_Nickname;

   ----------------------
   -- Get_Network_Name --
   ----------------------

   function Get_Network_Name (Server : Server_Type) return String is
   begin
      if Is_Local (Server) then
         return "localhost";
      else
         return Get_Network_Name (Get_Nickname (Server));
      end if;
   end Get_Network_Name;

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (Server : Server_Type) return Filesystem_Record'Class is
   begin
      if Is_Local (Server) then
         return Get_Local_Filesystem;
      else
         return Get_Filesystem (Get_Nickname (Server));
      end if;
   end Get_Filesystem;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Server : Server_Type) return Boolean is
   begin
      return Servers (Server).Is_Local;
   end Is_Local;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize
     (Kernel       : Kernel_Handle;
      From         : Server_Type;
      To           : Server_Type;
      Queue_Id     : String;
      Sync_Deleted : Boolean)
   is
      Server         : Server_Type;
      Mirror         : Mirror_Path_Access;
      From_Path      : String_Access;
      To_Path        : String_Access;
      Path_List_Item : Mirrors_List_Access;
      Machine        : Machine_Descriptor_Record;

   begin
      Trace (Me, "Synchronizing paths");

      if not Is_Local (From) then
         Server := From;
      elsif not Is_Local (To) then
         Server := To;
      else
         --  Both servers local. No need to synchronize

         return;
      end if;

      --  Search for mirror paths in 'To' config
      Path_List_Item := Main_Paths_Table;
      Mirror := null;

      while Path_List_Item /= null loop
         if Path_List_Item.Nickname.all = Servers (Server).Nickname.all then
            Mirror := Path_List_Item.Path_List;
            exit;
         end if;

         Path_List_Item := Path_List_Item.Next;
      end loop;

      while Mirror /= null loop
         if Mirror.Need_Sync then
            if Is_Local (From) then
               From_Path := Mirror.Local_Path;
               To_Path   := Mirror.Remote_Path;
               Machine   := Machine_Descriptor_Record
                 (Get_Machine_Descriptor (Servers (To).Nickname.all).all);

            else
               From_Path := Mirror.Remote_Path;
               To_Path   := Mirror.Local_Path;
               Machine   := Machine_Descriptor_Record
                 (Get_Machine_Descriptor (Servers (From).Nickname.all).all);
            end if;

            declare
               From_Name : constant String := Get_Nickname (From);
               To_Name   : constant String := Get_Nickname (To);
               Data      : aliased Rsync_Hooks_Args :=
                 (Hooks_Data with
                   Tool_Name_Length => Machine.Rsync_Func.all'Length,
                   Src_Name_Length  => From_Name'Length,
                   Dest_Name_Length => To_Name'Length,
                   Queue_Id_Length  => Queue_Id'Length,
                   Src_Path_Length  => From_Path'Length,
                   Dest_Path_Length => To_Path'Length,
                   Tool_Name        => Machine.Rsync_Func.all,
                   Src_Name         => From_Name,
                   Dest_Name        => To_Name,
                   Queue_Id         => Queue_Id,
                   Src_Path         => From_Path.all,
                   Dest_Path        => To_Path.all,
                   Sync_Deleted     => Sync_Deleted,
                   Synchronous      => Queue_Id = "");

            begin
               Trace (Me, "run sync hook for " & Data.Src_Path);

               if not Run_Hook_Until_Success
                 (Kernel, Rsync_Action_Hook, Data'Unchecked_Access)
               then
                  Trace (Me, "No remote sync was registered");
               end if;
            end;
         end if;

         Mirror := Mirror.Next;
      end loop;
   end Synchronize;

   --------------
   -- On_Error --
   --------------

   procedure On_Error
     (Manager : access Default_Error_Display_Record;
      Message : String) is
   begin
      if Manager.Kernel /= null then
         Insert (Manager.Kernel, Message, Mode => Error);
      end if;
   end On_Error;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Kernel           : Kernel_Handle := null;
      Arguments        : GNAT.OS_Lib.Argument_List;
      Server           : Server_Type;
      Pd               : out GNAT.Expect.Process_Descriptor_Access;
      Success          : out Boolean;
      Use_Ext_Terminal : Boolean := False;
      Console          : Interactive_Consoles.Interactive_Console := null;
      Show_Command     : Boolean := True;
      Directory        : String := "";
      Error_Manager    : Error_Display := null)
   is
      Exec                  : String_Access;
      Old_Dir               : String_Access;
      Args                  : Argument_List_Access;
      L_Args                : Argument_List_Access := null;
      Default_Error_Manager : aliased Default_Error_Display_Record;
      In_Use_Error_Manager  : Error_Display;

      function Check_Exec (Exec : String) return String_Access;
      --  Check that executable is on the path, and return the full path if
      --  found, return null otherwise.

      ----------------
      -- Check_Exec --
      ----------------

      function Check_Exec (Exec : String) return String_Access is
         Full_Exec : String_Access;
         Norm_Exec : String_Access;
      begin
         Full_Exec := Locate_Exec_On_Path (Exec);

         if Full_Exec = null then
            On_Error
              (In_Use_Error_Manager,
               -"Could not locate executable on path: " & Exec);
            return null;
         end if;

         --  use Normalize_Pathname to prevent relative paths like
         --  ./my_prog. See F322-010 concerning problem with gnatls in this
         --  particular case.

         Norm_Exec := new String'
           (Normalize_Pathname (Full_Exec.all, Resolve_Links => False));
         Free (Full_Exec);
         return Norm_Exec;
      end Check_Exec;

      Main_Window : Gtk.Window.Gtk_Window := null;

   begin
      Success := False;

      --  Set the error display manager

      if Error_Manager = null then
         Default_Error_Manager.Kernel := Kernel;
         In_Use_Error_Manager := Default_Error_Manager'Unchecked_Access;
      else
         In_Use_Error_Manager := Error_Manager;
      end if;

      if Kernel /= null then
         Main_Window := Get_Main_Window (Kernel);
      end if;

      --  First verify the executable to be launched

      if Is_Local (Server) then
         Exec := Check_Exec (Arguments (Arguments'First).all);

         if Exec = null then
            return;
         end if;

         Args := new Argument_List'
           ((1 => Exec) &
            Clone (Arguments (Arguments'First + 1 .. Arguments'Last)));
      else
         Args := new Argument_List'(Clone (Arguments));
      end if;

      if Console /= null
        and then Show_Command
      then
         if Is_Local (Server) then
            Insert (Console,
                    Argument_List_To_String (Arguments),
                    Add_LF => True);
         else
            Insert (Console,
                    Get_Nickname (Server) & "> " &
                    Argument_List_To_String (Arguments),
                    Add_LF => True);
         end if;
      end if;

      if Is_Local (Server) then
         Pd := new GNAT.Expect.TTY.TTY_Process_Descriptor;

         --  If using an external terminal, use Execute_Command preference
         --  ??? incompatible with gnat.expect.tty.remote...

         if Use_Ext_Terminal then
            declare
               Tmp_Args_1 : Argument_List_Access := Args;
               Tmp_Args_2 : Argument_List_Access;
            begin
               Tmp_Args_2 := Argument_String_To_List
                 (Get_Pref (GPS.Kernel.Preferences.Execute_Command));
               Args := new Argument_List'(Tmp_Args_2.all & Tmp_Args_1.all);
               Simple_Free (Tmp_Args_1);
               Simple_Free (Tmp_Args_2);
            end;
         end if;

         if Directory /= "" then
            Old_Dir := new String'(Get_Current_Dir);
            Change_Dir (Directory);
         end if;

         if Active (Me) then
            Trace (Me, "Spawning " & Argument_List_To_String (Args.all));
         end if;

         --  Set buffer_size to 0 for dynamically allocated buffer (prevents
         --  possible overflow)

         if L_Args /= null then
            Non_Blocking_Spawn
              (Pd.all,
               L_Args (L_Args'First).all,
               L_Args (L_Args'First + 1 .. L_Args'Last) &
               Args.all,
               Buffer_Size => 0,
               Err_To_Out  => True);
            Free (L_Args);

         else
            Non_Blocking_Spawn
              (Pd.all,
               Args (Args'First).all,
               Args (Args'First + 1 .. Args'Last),
               Buffer_Size => 0,
               Err_To_Out  => True);
         end if;

         if Directory /= "" then
            Change_Dir (Old_Dir.all);
            Free (Old_Dir);
         end if;

      else
         if Active (Me) then
            Trace
              (Me, "Remote Spawning " & Argument_List_To_String (Args.all));
         end if;

         if Directory = "" then
            Old_Dir := new String'(To_Remote (Get_Current_Dir, Server));
         else
            Old_Dir := new String'(Directory);
         end if;

         --  Set buffer_size to 0 for dynamically allocated buffer
         --  (prevents possible overflow)

         Remote_Spawn
           (Pd,
            Target_Nickname     => Servers (Server).Nickname.all,
            Args                => Args.all,
            Execution_Directory => Old_Dir.all,
            Err_To_Out          => True,
            Main_Window         => Main_Window);
         Free (Old_Dir);
      end if;

      Success := True;

      Free (Args);

   exception
      when E : Invalid_Process | Process_Died =>
         Success := False;
         On_Error
           (In_Use_Error_Manager,
            -"Invalid command (" & Ada.Exceptions.Exception_Message (E) & ")");
   end Spawn;

end GPS.Kernel.Remote;
