-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.Strings;

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
with Gtk.Paned;                  use Gtk.Paned;
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
with Gtkada.File_Selector;       use Gtkada.File_Selector;
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
with Filesystem.Queries;         use Filesystem.Queries;
with GUI_Utils;                  use GUI_Utils;
with Interactive_Consoles;       use Interactive_Consoles;
with Projects;                   use Projects;
with Remote.Path.Translator;     use Remote.Path, Remote.Path.Translator;
with Shell_Descriptors;          use Shell_Descriptors;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;
with VFS;                        use VFS;
with XML_Parsers;

with Remote_Descriptors;         use Remote_Descriptors;
with Connection_Debuggers;       use Connection_Debuggers;
with Machine_Descriptors;        use Machine_Descriptors;

package body GPS.Kernel.Remote is

   ------------
   -- Module --
   ------------

   Me : constant Debug_Handle := Create ("GPS.Kernel.Remote");

   Invalid_Path : exception;

   type Remote_Module_Record is new Module_ID_Record with record
      Kernel            : Kernel_Handle;
      Project_Reloading : Boolean := False;
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
   --  Tell where the descriptor comes from: system wide setup of user defined
   --  setup

   --------------------------
   -- Connection debugging --
   --------------------------

   type Connection_Debug is new Connection_Debugger_Record with record
      Kernel  : Kernel_Handle;
      Title   : String_Access;
      Console : Interactive_Console;
   end record;

   function Create (Kernel : Kernel_Handle;
                    Title  : String) return Connection_Debugger;
   --  Create a Connection_Debug object

   procedure Print (Dbg  : access Connection_Debug;
                    Str  : String;
                    Mode : Mode_Type);
   --  Display Str in the connection debugger

   function Create (Kernel : Kernel_Handle;
                    Title  : String) return Connection_Debugger is
   begin
      return new Connection_Debug'
        (Kernel  => Kernel,
         Title   => new String'(Title),
         Console => null);
   end Create;

   procedure Print (Dbg  : access Connection_Debug;
                    Str  : String;
                    Mode : Mode_Type) is
      Console : Interactive_Console;
   begin
      Console :=
        Create_Interactive_Console (Dbg.Kernel, Dbg.Title.all);

      case Mode is
         when Input =>
            Insert (Console, Str, Add_LF => True, Highlight => True);
         when Output =>
            Insert (Console, Str, Add_LF => False, Highlight => False);
      end case;
   end Print;

   ------------------------
   -- Machine_Descriptor --
   ------------------------

   type Machine_Descriptor_Record is
     new Machine_Descriptors.Machine_Descriptor_Record
   with record
      Attribute  : Descriptor_Attribute;
      Rsync_Func : GNAT.Strings.String_Access;
      Applied    : Boolean;
      --  tells if the machine configuration has been applied.
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

   procedure Parse_Remote_Path_Node (Node : Glib.Xml_Int.Node_Ptr);
   --  Parse a remote_path node

   ------------------------
   -- Server list dialog --
   ------------------------

   Name_Col     : constant := 0;
   Modified_Col : constant := 1;
   User_Def_Col : constant := 2;

   Enter_Local_Path_String  : constant String := -"<enter local path here>";
   Enter_Remote_Path_String : constant String := -"<enter remote path here>";

   Synchronisation_String : constant array (Synchronisation_Type)
   of String_Access
     := (Never          => new String'("Never"),
         Once_To_Local  => new String'("Once to local"),
         Once_To_Remote => new String'("Once to remote"),
         Always         => new String'("Always"));

   type Path_Row_Record is record
      Local_Entry          : Gtk_Entry;
      Local_Browse_Button  : Gtk_Button;
      Local_Frame          : Gtk_Frame;
      Local_Hbox           : Gtk_Hbox;
      Remote_Entry         : Gtk_Entry;
      Remote_Browse_Button : Gtk_Button;
      Remote_Frame         : Gtk_Frame;
      Remote_Hbox          : Gtk_Hbox;
      Sync_Combo           : Gtkada_Combo;
      Remove_Button        : Gtk_Button;
      Cursor               : Mirror_List.Cursor;
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
      M_List          : Mirror_List_Access;
      Nb_Rows         : Guint;
      Dialog          : Gtk_Dialog;
   end record;
   type Paths_Widget is access all Paths_Widget_Record'Class;

   procedure Gtk_New
     (Widget      : out Paths_Widget;
      Dialog      : Gtk_Dialog);

   procedure Set_Path_List
     (Widget : Paths_Widget;
      List   : Mirror_List_Access);
   --  Reset the widget and fills it with the path list

   procedure Save_Tentative_Path_List
     (Widget : Paths_Widget;
      FS     : Filesystem_Record'Class);
   --  Retrieve the mirror path list represented by the widget.

   procedure Add_Path_Row
     (Widget      : Paths_Widget;
      Row_Number  : Guint;
      Cursor      : Mirror_List.Cursor);
   --  Add a new Path row to the Mirror_Path_Widget

   procedure Remove_Path_Row
     (Widget : Paths_Widget;
      Row    : in out Path_Row);
   --  Remove the row from widget

   procedure Update_Path
     (Row  : Path_Row;
      FS   : Filesystem_Record'Class;
      Path : in out Mirror_Path);
   --  Retrieve the mirror path represented by the widget

   procedure On_Path_Grab_Focus (Widget : access Gtk_Widget_Record'Class);
   --  Called when a path with default string is clicked

   procedure On_Add_Path_Clicked (W : access Gtk_Widget_Record'Class);
   --  Add_Path button is clicked.

   type Path_Cb_Data is new Glib.Object.GObject_Record with record
      Widget : Paths_Widget;
      Row    : Path_Row;
   end record;
   type Path_Cb_Data_Access is access all Path_Cb_Data'Class;

   package Path_Callback is new
     Gtk.Handlers.Callback (Path_Cb_Data);

   procedure On_Remove_Path_Clicked (W : access Path_Cb_Data'Class);
   --  One of the Remove_Path button is clicked.

   procedure On_Browse_Local (Widget : access Path_Cb_Data'Class);
   --  Select a local directory

   procedure On_Browse_Remote (Widget : access Path_Cb_Data'Class);
   --  Select a remote directory

   type Server_List_Editor_Record is new Gtk_Dialog_Record with record
      Kernel                : Kernel_Handle;
      Selected_Machine      : Item_Access;
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
      Debug_Button          : Gtk_Check_Button;
      --  Mirror Paths config pannel
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
     (Dialog         : out Server_List_Editor;
      Kernel         : Kernel_Handle;
      Default_Server : String);
   --  Creates the server_list_editor dialog

   procedure On_Changed (W                 : access Gtk_Widget_Record'Class;
                         Connection_Params : Boolean);
   --  Called when one of the entries has changed
   --  Connection_Params tells if connection configuration changed. If set, the
   --   machine cannot be browsed until 'Apply' is called.

   package Widget_Boolean_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Boolean);

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
      Nickname : String_Access;
      --  Identifier of the server
   end record;
   type Servers_Config is array (Distant_Server_Type) of Server_Config;

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

   ----------
   -- Free --
   ----------

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
      Debug_Console      : constant String :=
                             Get_Attribute (Node, "debug_console", "false");
      Field              : Glib.String_Ptr;
      Max_Nb_Connections : Natural;
      User_Name          : String_Access;
      Timeout            : Natural;
      Extra_Init_Cmds    : GNAT.OS_Lib.Argument_List_Access;
      Nb_Init_Cmds       : Natural;
      Child              : Node_Ptr;
      Cmd                : Node_Ptr;
      Desc               : Machine_Descriptor;
      Dbg                : Connection_Debugger;

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

      if Debug_Console = "true" then
         Dbg := Create (Kernel, Nickname & " session");
      else
         Dbg := null;
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
         Applied             => True,
         Ref                 => 0,
         Dbg                 => Dbg);

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
     (Node      : Glib.Xml_Int.Node_Ptr)
   is
      Nickname : constant String := Get_Attribute (Node, "server_name");
      List     : constant Mirror_List_Access := Get_List (Nickname);
      Path     : Mirror_Path;
      Child    : Node_Ptr := Node.Child;

   begin
      while Child /= null loop
         declare
            Local_Path  : constant String :=
                            Get_Attribute (Child, "local_path");
            Remote_Path : constant String :=
                            Get_Attribute (Child, "remote_path");
            Sync_Str    : constant String :=
                            Get_Attribute (Child, "sync", "never");
            Sync        : Synchronisation_Type;
         begin

            --  Retrieve Sync value from string.
            begin
               Sync := Synchronisation_Type'Value (Sync_Str);
            exception
               when Constraint_Error =>
                  Sync := Never;
            end;

            Path.Init (Local_Path      => Local_Path,
                       Remote_Path     => Remote_Path,
                       Synchronisation => Sync);
            List.Append (Path);
         end;

         Child := Child.Next;
      end loop;
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
                  Parse_Remote_Path_Node (Child);
               end if;

               Child := Child.Next;
            end loop;
         end if;
      end if;
   end Load_Remote_Config;

   ------------------------
   -- Save_Remote_Config --
   ------------------------

   procedure Save_Remote_Config (Kernel : Kernel_Handle) is
      Filename   : constant String := Get_Home_Dir (Kernel) & "remote.xml";
      File, Item, Child, Cmd_Node : Node_Ptr;
      Desc       : Machine_Descriptor;
      Nb_Desc    : Natural;

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
            if Desc.Dbg = null then
               Set_Attribute (Item, "debug_console", "false");
            else
               Set_Attribute (Item, "debug_console", "true");
            end if;

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
         if not Get_List (Desc.Nickname.all).Is_Empty then
            Item := new Glib.Xml_Int.Node;
            Item.Tag := new String'("remote_path_config");
            Set_Attribute (Item, "server_name", Desc.Nickname.all);

            declare
               List   : constant Mirror_List_Access :=
                          Get_List (Desc.Nickname.all);
               Cursor : Mirror_List.Cursor;
               Path   : Mirror_Path;
            begin
               Cursor := Mirror_List.First (List.all);

               while Mirror_List.Has_Element (Cursor) loop
                  Path := Mirror_List.Element (Cursor);

                  if Path /= Null_Path then
                     Child := new Glib.Xml_Int.Node;
                     Child.Tag := new String'("mirror_path");
                     Set_Attribute
                       (Child, "sync",
                        Synchronisation_Type'Image (Path.Get_Synchronisation));
                     Set_Attribute
                       (Child, "remote_path", Path.Get_Remote_Path);
                     Set_Attribute (Child, "local_path", Path.Get_Local_Path);
                     Add_Child (Item, Child, True);
                  end if;

                  Mirror_List.Next (Cursor);
               end loop;
            end;

            Add_Child (File, Item, True);
         end if;
      end loop;

      Print (File, Filename);
      Trace (Me, Filename & " saved");
      Free (File);
   end Save_Remote_Config;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget      : out Paths_Widget;
      Dialog      : Gtk_Dialog)
   is
      Pix    : Gtk_Image;
      Label  : Gtk_Label;
   begin
      Widget := new Paths_Widget_Record;
      Gtk.Frame.Initialize (Widget, -"Path Translations");

      Widget.Dialog      := Dialog;

      Gtk_New (Widget.Table, 1, 6, False);
      Add (Widget, Widget.Table);

      Gtk_New (Label, -"Local Path");
      Attach (Widget.Table, Label, 0, 1, 0, 1, Expand or Fill, 0);
      Gtk_New (Label, -"Remote Path");
      Attach (Widget.Table, Label, 1, 2, 0, 1, Expand or Fill, 0);
      Gtk_New (Label, -"Sync");
      Attach (Widget.Table, Label, 2, 3, 0, 1, 0, 0);

      Gtk_New (Widget.Add_Path_Button);
      Set_Name (Widget.Add_Path_Button, "add path button");
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
     (Widget : Paths_Widget;
      List   : Mirror_List_Access)
   is
      Cursor : Mirror_List.Cursor;
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
      Widget.M_List := List;
      Cursor := List.First;

      while Mirror_List.Has_Element (Cursor) loop
         Add_Path_Row (Widget, Widget.Nb_Rows, Cursor);
         Widget.Nb_Rows := Widget.Nb_Rows + 1;
         Mirror_List.Next (Cursor);
      end loop;

      Attach (Widget.Table, Widget.Add_Path_Button,
              3, 4, Widget.Nb_Rows, Widget.Nb_Rows + 1, 0, 0);
      Show_All (Widget.Table);
      Unref (Widget.Add_Path_Button);
   end Set_Path_List;

   ------------------------------
   -- Save_Tentative_Path_List --
   ------------------------------

   procedure Save_Tentative_Path_List
     (Widget : Paths_Widget;
      FS     : Filesystem_Record'Class)
   is
      List   : Path_Row_List.Glist;
      Row    : Path_Row;
      use type Path_Row_List.Glist;

      procedure Update (P : in out Mirror_Path);
      --  Update the Mirror path with values in the widget

      procedure Update (P : in out Mirror_Path) is
      begin
         Update_Path (Path_Row_List.Get_Data (List), FS, P);
      end Update;

   begin
      List := Widget.List;

      while List /= Path_Row_List.Null_List loop
         Row := Path_Row_List.Get_Data (List);
         Widget.M_List.Update_Element (Row.Cursor, Update'Access);
         List := Path_Row_List.Next (List);
      end loop;
   end Save_Tentative_Path_List;

   ------------------
   -- Add_Path_Row --
   ------------------

   procedure Add_Path_Row
     (Widget      : Paths_Widget;
      Row_Number  : Guint;
      Cursor      : Mirror_List.Cursor)
   is
      Path : Mirror_Path := Mirror_List.Element (Cursor);
      Pix  : Gtk_Image;
      Row  : Path_Row;
      Data : Path_Cb_Data_Access;
      Item : Gtk_List_Item;
      Tips : Gtk_Tooltips;
   begin
      Row := new Path_Row_Record;
      Row.Cursor := Cursor;
      Tips := Get_Tooltips (Server_List_Editor (Widget.Dialog).Kernel);

      Gtk_New (Row.Local_Frame);
      Attach (Widget.Table, Row.Local_Frame, 0, 1, Row_Number, Row_Number + 1,
              Fill or Expand or Shrink, 0, 0, 2);

      Gtk_New_Hbox (Row.Local_Hbox, Spacing => 0);
      Add (Row.Local_Frame, Row.Local_Hbox);

      Gtk_New (Row.Local_Entry);
      Set_Name (Row.Local_Entry,
                "local path entry" & Guint'Image (Row_Number));
      Set_Width_Chars (Row.Local_Entry, 16);
      Pack_Start (Row.Local_Hbox, Row.Local_Entry, True, True);
      Set_Tip
        (Tips, Row.Local_Entry,
         -("Enter here the local path"));

      Gtk_New (Row.Local_Browse_Button);
      Gtk_New (Pix, Stock_Open, Icon_Size_Menu);
      Add (Row.Local_Browse_Button, Pix);
      Set_Relief (Row.Local_Browse_Button, Relief_None);
      Set_Border_Width (Row.Local_Browse_Button, 0);
      Unset_Flags (Row.Local_Browse_Button, Can_Focus or Can_Default);
      Pack_Start (Row.Local_Hbox, Row.Local_Browse_Button, False, False);
      Set_Tip
        (Tips, Row.Local_Browse_Button,
         -"Use this button to select a local path with a file explorer");

      Gtk_New (Row.Remote_Frame);
      Attach (Widget.Table, Row.Remote_Frame, 1, 2, Row_Number, Row_Number + 1,
              Fill or Expand or Shrink, 0, 0, 2);

      Gtk_New_Hbox (Row.Remote_Hbox, Spacing => 0);
      Add (Row.Remote_Frame, Row.Remote_Hbox);

      Gtk_New (Row.Remote_Entry);
      Set_Name (Row.Remote_Entry,
                "remote path entry" & Guint'Image (Row_Number));
      Set_Width_Chars (Row.Remote_Entry, 16);
      Pack_Start (Row.Remote_Hbox, Row.Remote_Entry, True, True);
      Set_Tip
        (Tips, Row.Remote_Entry,
         -("Enter here the remote path"));

      Gtk_New (Row.Remote_Browse_Button);
      Gtk_New (Pix, Stock_Open, Icon_Size_Menu);
      Add (Row.Remote_Browse_Button, Pix);
      Set_Relief (Row.Remote_Browse_Button, Relief_None);
      Set_Border_Width (Row.Remote_Browse_Button, 0);
      Unset_Flags (Row.Remote_Browse_Button, Can_Focus or Can_Default);
      Pack_Start (Row.Remote_Hbox, Row.Remote_Browse_Button, False, False);
      Set_Tip
        (Tips, Row.Remote_Browse_Button,
         -("Use this button to select a remote path with a file explorer. " &
           "Note that the machine configuration shall be properly set and " &
           "applied"));

      Gtk_New (Row.Sync_Combo);
      Set_Text (Get_Entry (Row.Sync_Combo),
                Synchronisation_String (Never).all);

      for I in Synchronisation_Type'Range loop
         Gtk_New (Item, Synchronisation_String (I).all);
         Add (Get_List (Row.Sync_Combo), Item);
         Show_All (Get_List (Row.Sync_Combo));
      end loop;

      Set_Value_In_List (Row.Sync_Combo);
      Set_Width_Chars (Get_Entry (Row.Sync_Combo), 14);
      Attach (Widget.Table, Row.Sync_Combo, 2, 3, Row_Number, Row_Number + 1,
              0, 0, 0, 2);
      Set_Tip
        (Tips, Get_Entry (Row.Sync_Combo),
         -("Four kind of paths synchronisation can be set for each defined " &
           "path:" & ASCII.LF &
           "* None: no synchronisation is required from GPS, the paths " &
           "are shared using an OS mechanism like NFS." & ASCII.LF &
           "* Always: the paths are kept synchronised by GPS before and " &
           "after every remote action." &
           ASCII.LF &
           "* Once to local/Once to remote: project's dependencies. They are" &
           " synchronized once when a remote project is loaded or when a " &
           "local project is set remote. They can be still manually " &
           "synchronized using the Remote View"));

      Gtk_New (Row.Remove_Button);
      Gtk_New (Pix, Stock_Remove, Icon_Size_Menu);
      Add (Row.Remove_Button, Pix);
      Attach
        (Widget.Table, Row.Remove_Button, 3, 4, Row_Number, Row_Number + 1,
         0, 0, 0, 2);

      Path_Row_List.Append (Widget.List, Row);

      declare
         Local  : constant String := Path.Get_Local_Path (True);
         Remote : constant String := Path.Get_Remote_Path (True);
      begin
         if Local = "" then
            Set_Text (Row.Local_Entry, Enter_Local_Path_String);
            Widget_Callback.Object_Connect
              (Row.Local_Entry, "grab_focus",
               On_Path_Grab_Focus'Access, Row.Local_Entry);
         else
            Set_Text (Row.Local_Entry, Path.Get_Local_Path (True));
         end if;

         if Remote = "" then
            Set_Text (Row.Remote_Entry, Enter_Remote_Path_String);
            Widget_Callback.Object_Connect
              (Row.Remote_Entry, "grab_focus",
               On_Path_Grab_Focus'Access, Row.Remote_Entry);
         else
            Set_Text (Row.Remote_Entry, Path.Get_Remote_Path (True));
         end if;

         Set_Text
           (Get_Entry (Row.Sync_Combo),
            Synchronisation_String (Path.Get_Synchronisation (True)).all);
      end;

      Data := new Path_Cb_Data;
      Data.Widget := Widget;
      Data.Row    := Row;

      Widget_Boolean_Callback.Object_Connect
        (Row.Local_Entry, "changed", On_Changed'Access, Widget.Dialog, False);
      Path_Callback.Object_Connect
        (Row.Local_Browse_Button, "clicked", On_Browse_Local'Access, Data);
      Widget_Boolean_Callback.Object_Connect
        (Row.Remote_Entry, "changed", On_Changed'Access, Widget.Dialog, False);
      Path_Callback.Object_Connect
        (Row.Remote_Browse_Button, "clicked", On_Browse_Remote'Access, Data);
      Widget_Boolean_Callback.Object_Connect
        (Row.Sync_Combo, "changed", On_Changed'Access, Widget.Dialog, False);
      Path_Callback.Object_Connect
        (Row.Remove_Button, "clicked", On_Remove_Path_Clicked'Access, Data);
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
      Remove (Widget.Table, Row.Local_Frame);
      Remove (Widget.Table, Row.Remote_Frame);
      Remove (Widget.Table, Row.Sync_Combo);
      Remove (Widget.Table, Row.Remove_Button);
      Path_Row_List.Remove (Widget.List, Row);
      Free (Row);
   end Remove_Path_Row;

   -----------------
   -- Update_Path --
   -----------------

   procedure Update_Path
     (Row  : Path_Row;
      FS   : Filesystem_Record'Class;
      Path : in out Mirror_Path)
   is
      Local  : constant String := Get_Text (Row.Local_Entry);
      Remote : constant String := Get_Text (Row.Remote_Entry);
      Sync   : Synchronisation_Type := Never;
      Dead   : Message_Dialog_Buttons;
      pragma Unreferenced (Dead);

   begin
      if Local = Enter_Local_Path_String
        or else Local = ""
      then
         Dead := Message_Dialog (-"Please enter a valid local path",
                                 Error, Button_OK);
         raise Invalid_Path;
      end if;

      if Remote = Enter_Remote_Path_String
        or else Remote = ""
      then
         Dead := Message_Dialog (-"Please enter a valid remote path",
                                 Error, Button_OK);
         raise Invalid_Path;
      end if;

      if not Is_Absolute_Path (Get_Local_Filesystem, Local) then
         Dead := Message_Dialog
           (-"Local path " & Local & (-" needs to be an absolute path"),
            Error, Button_OK);
         raise Invalid_Path;
      end if;

      if not Is_Absolute_Path (FS, Remote) then
         Dead := Message_Dialog
           (-"Remote path " & Remote & (-" needs to be an absolute path"),
            Error, Button_OK);
         raise Invalid_Path;
      end if;

      for J in Synchronisation_Type'Range loop
         if Get_Text (Get_Entry (Row.Sync_Combo)) =
           Synchronisation_String (J).all
         then
            Sync := J;
            exit;
         end if;
      end loop;

      if Active (Me) then
         Trace (Me, "Set tentative : " & Local & " - " & Remote & " - " &
                Synchronisation_Type'Image (Sync));
      end if;

      Path.Set_Tentative_Local_Path
        (Get_Local_Filesystem.Ensure_Directory  (Local));
      Path.Set_Tentative_Remote_Path
        (FS.Ensure_Directory (Remote));
      Path.Set_Tentative_Synchronisation (Sync);
   end Update_Path;

   -------------------------
   -- On_Add_Path_Clicked --
   -------------------------

   procedure On_Add_Path_Clicked (W : access Gtk_Widget_Record'Class)
   is
      Widget : Paths_Widget_Record renames Paths_Widget_Record (W.all);
      Path   : Mirror_Path := Null_Path;
   begin
      Ref (Widget.Add_Path_Button);
      Remove (Widget.Table, Widget.Add_Path_Button);

      Widget.M_List.Append (Path);
      Add_Path_Row (Paths_Widget (W), Widget.Nb_Rows, Widget.M_List.Last);
      Widget.Nb_Rows := Widget.Nb_Rows + 1;

      Attach (Widget.Table, Widget.Add_Path_Button,
              3, 4, Widget.Nb_Rows, Widget.Nb_Rows + 1, 0, 0);
      Show_All (Widget.Table);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Add_Path_Clicked;

   ----------------------------
   -- On_Remove_Path_Clicked --
   ----------------------------

   procedure On_Remove_Path_Clicked (W : access Path_Cb_Data'Class) is
   begin
      Mirror_List.Update_Element
        (W.Widget.M_List.all, W.Row.Cursor, Set_Deleted_State'Access);
      Remove_Path_Row (W.Widget, W.Row);
      On_Changed (W.Widget.Dialog, False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Remove_Path_Clicked;

   ---------------------
   -- On_Browse_Local --
   ---------------------

   procedure On_Browse_Local (Widget : access Path_Cb_Data'Class) is
      Current_Dir : constant String :=
                      Get_Text (Widget.Row.Local_Entry);
      Start_Dir   : Virtual_File := No_File;
   begin
      if Current_Dir /= Enter_Local_Path_String then
         Start_Dir := Create (Current_Dir);

         if not Is_Directory (Start_Dir) then
            Start_Dir := No_File;
         end if;
      end if;

      declare
         Dir : constant VFS.Virtual_File :=
                 Select_Directory
                   (Base_Directory => Start_Dir,
                    Parent         => Gtk_Window (Widget.Widget.Dialog));
      begin
         if Dir /= No_File then
            Set_Text (Widget.Row.Local_Entry, Full_Name (Dir).all);
            On_Changed (Widget.Widget.Dialog, False);
         end if;
      end;
   end On_Browse_Local;

   ----------------------
   -- On_Browse_Remote --
   ----------------------

   procedure On_Browse_Remote (Widget : access Path_Cb_Data'Class) is
      Current_Dir  : constant String :=
                       Get_Text (Widget.Row.Remote_Entry);
      Start_Dir    : Virtual_File := No_File;
      Dialog       : constant Server_List_Editor :=
                       Server_List_Editor (Widget.Widget.Dialog);
      Gtk_Resp     : Message_Dialog_Buttons;
      pragma Unreferenced (Gtk_Resp);
   begin
      if Dialog.Selected_Machine = null then
         --  Should never happend... however, still preferable to catch
         --  this case !
         Trace (Me, "Dialog.Selected_Machine null while calling " &
                "On_Browse_Remote. This should never happend !");

         return;
      end if;

      if not Machine_Descriptor_Record
        (Dialog.Selected_Machine.Desc.all).Applied
      then
         Gtk_Resp := Message_Dialog
           (-"Cannot browse the selected server until Apply button is pressed",
            Dialog_Type => Error,
            Buttons     => Button_OK);
         return;
      end if;

      --  Check connection before browsing the remote host
      Start_Dir := Get_Root
        (Create (Dialog.Selected_Machine.Desc.Nickname.all, ""));

      if not Is_Directory (Start_Dir) then
         Gtk_Resp := Message_Dialog
           (-"Could not establish communication with selected host",
            Dialog_Type => Error,
            Buttons     => Button_OK);
         return;
      end if;

      --  Determine Start directory
      if Current_Dir /= Enter_Local_Path_String then
         Start_Dir := Create
           (Dialog.Selected_Machine.Desc.Nickname.all, Current_Dir);

         if not Is_Directory (Start_Dir) then
            Start_Dir := No_File;
         end if;
      end if;

      if Start_Dir = No_File then
         Start_Dir :=
           Get_Root (Create (Dialog.Selected_Machine.Desc.Nickname.all, ""));
      end if;

      declare
         Dir : constant VFS.Virtual_File :=
                 Select_Directory
                   (Base_Directory => Start_Dir,
                    Parent         => Gtk_Window (Widget.Widget.Dialog));
      begin
         if Dir /= No_File then
            Set_Text (Widget.Row.Remote_Entry, Full_Name (Dir).all);
            On_Changed (Widget.Widget.Dialog, False);
         end if;
      end;
   end On_Browse_Remote;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog         : out Server_List_Editor;
      Kernel         : Kernel_Handle;
      Default_Server : String)
   is
      Nb_Machines : Natural;
      Tips        : Gtk_Tooltips;
      Main_Table  : Gtk_Paned;
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
      Event       : Gtk_Event_Box;
      pragma Unreferenced (Tmp);

   begin
      Dialog := new Server_List_Editor_Record;
      Initialize
        (Dialog,
         -"Servers configuration",
         Get_Main_Window (Kernel),
         Modal + Destroy_With_Parent);
      Set_Position (Dialog, Win_Pos_Center_On_Parent);
      Set_Default_Size (Dialog, -1, 400);
      Tips := Get_Tooltips (Kernel);

      Dialog.Kernel := Kernel;

      Gtk_New_Hpaned (Main_Table);
      Pack_Start (Get_Vbox (Dialog), Main_Table);

      Gtk_New_Vbox (VBox, Homogeneous => False);
      Pack1 (Main_Table, VBox, Resize => False, Shrink => False);

      Gtk_New (Frame);
      Pack_Start (VBox, Frame, Expand => True, Fill => True);

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
      Set_Name (Dialog.Machine_Tree, "machine tree");
      Add (Scrolled, Dialog.Machine_Tree);

      --  Add/Restore/Remove buttons
      Gtk_New (Dialog.Add_Machine_Button, -"Add server");
      Set_Tip (Tips, Dialog.Add_Machine_Button,
               -"Add a new server in the servers list");
      Pack_Start (VBox, Dialog.Add_Machine_Button, False, False);
      Gtk_New (Dialog.Restore_Button, -"Remove local changes");
      Set_Tip (Tips, Dialog.Restore_Button,
               -("Reinitialize the selected server's parameters to their " &
                 "default values"));
      Pack_Start (VBox, Dialog.Restore_Button, False, False);
      Gtk_New (Dialog.Remove_Button, -"Remove server");
      Set_Tip (Tips, Dialog.Remove_Button,
               -"Remove the selected server from the servers list");
      Pack_Start (VBox, Dialog.Remove_Button, False, False);
      Set_Sensitive (Dialog.Restore_Button, False);
      Set_Sensitive (Dialog.Remove_Button, False);

      --  Machine configuration

      Gtk_New_Vbox (VBox, Homogeneous => False);
      Pack2 (Main_Table, VBox, Resize => True, Shrink => False);

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
      Set_Name (Dialog.Network_Name_Entry, "network name entry");
      Attach (Dialog.Right_Table, Dialog.Network_Name_Entry,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tip
        (Tips, Dialog.Network_Name_Entry,
         -("The network name is the name used to connect to this server via " &
           "your network. It can be either an IP address, a host name of " &
           "your local network, or a fully qualified network name."));

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
      Set_Name (Dialog.Remote_Access_Combo, "remote access combo");
      Set_Editable (Get_Entry (Dialog.Remote_Access_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Access_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tip
        (Tips, Get_Entry (Dialog.Remote_Access_Combo),
         -("The remote access tool is the tool used to connect to this " &
           "server."));

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
      Set_Name (Dialog.Remote_Shell_Combo, "remote shell combo");
      Set_Editable (Get_Entry (Dialog.Remote_Shell_Combo), False);
      Attach (Dialog.Right_Table, Dialog.Remote_Shell_Combo,
              1, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0);
      Set_Tip
        (Tips, Get_Entry (Dialog.Remote_Shell_Combo),
         -"The shell tells GPS what shell runs on the remote server.");

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
      Set_Tip
        (Tips, Get_Entry (Dialog.Remote_Sync_Combo),
         -("The sync tool is used to synchronize remote and local " &
           "filesystems, if these are not shared filesystems."));

      declare
         Rsync_List : constant GNAT.Strings.String_List :=
           Get_Hook_Func_List (Kernel, Rsync_Action_Hook);
      begin
         for J in Rsync_List'Range loop
            Gtk_New (Item, Locale_To_UTF8 (Rsync_List (J).all));
            Add (Get_List (Dialog.Remote_Sync_Combo), Item);
         end loop;
         Show_All (Get_List (Dialog.Remote_Sync_Combo));
      end;

      Line_Nb := Line_Nb + 1;
      Gtk_New (Label, -"Extra init commands:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Right_Table, Label, 0, 1, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Init_Cmds_View);
      Set_Wrap_Mode (Dialog.Init_Cmds_View, Wrap_Char);
      Set_Left_Margin (Dialog.Init_Cmds_View, 10);
      Set_Indent (Dialog.Init_Cmds_View, -10);
      Set_Pixels_Below_Lines (Dialog.Init_Cmds_View, 3);
      Gtk_New (Event);
      Add (Event, Dialog.Init_Cmds_View);
      Attach (Dialog.Right_Table, Event, 1, 2,
              Line_Nb, Line_Nb + 1, Fill or Expand, 0);
      Set_Tip
        (Tips, Event,
         -("The Extra Init Commands field represents initialization commands" &
           " sent to the server upon connection: when GPS connects to your " &
           "remote machine, the chosen shell is launched, and your default " &
           "initialization files are read (e.g. .bashrc file for the bash " &
           "shell). Then GPS sends these extra init commands, allowing you " &
           "for example to specify a compilation toolchain."));

      Line_Nb := Line_Nb + 1;
      Gtk_New (Dialog.Advanced_Pane, -"Advanced configuration");
      Set_State (Dialog.Advanced_Pane, Collapsed);
      Attach (Dialog.Right_Table, Dialog.Advanced_Pane,
              0, 2, Line_Nb, Line_Nb + 1,
              Fill or Expand, 0, 10, 10);

      Gtk_New (Dialog.Advanced_Table,
               Rows => 4, Columns => 2, Homogeneous => False);
      Set_Expanded_Widget (Dialog.Advanced_Pane, Dialog.Advanced_Table);
      --  ??? The following uses Gtk_Expander instead of Collapsing_Pane
--        Gtk_New (Frame);
--        Attach (Dialog.Right_Table, Frame,
--                0, 2, Line_Nb, Line_Nb + 1,
--                Fill or Expand, 0, 10, 10);
--
--        Gtk_New (Dialog.Advanced_Pane, -"Advanced configuration");
--        Set_Expanded (Dialog.Advanced_Pane, False);
--        Add (Frame, Dialog.Advanced_Pane);
--
--        Gtk_New (Dialog.Advanced_Table,
--                 Rows => 4, Columns => 2, Homogeneous => False);
--        Add (Dialog.Advanced_Pane, Dialog.Advanced_Table);

      Gtk_New (Label, -"User name:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 0, 1,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.User_Name_Entry);
      Attach (Dialog.Advanced_Table, Dialog.User_Name_Entry, 1, 2, 0, 1,
              Fill or Expand, 0);
      Set_Tip
        (Tips, Dialog.User_Name_Entry,
         -("The user name specifies the name used to connect to the server. " &
           "If unspecified, the remote access tool will most of the time " &
           "use your current login name. If not, and a user name is " &
           "requested, gps will prompt you for a user name when requested.)"));

      Gtk_New (Label, -"Timeout value (in s):");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 1, 2,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Timeout_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Timeout_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Timeout_Spin, 1, 2, 1, 2,
              Fill or Expand, 0);
      Set_Tip
        (Tips, Dialog.Timeout_Spin,
         -("The timeout value is used to determine if a connection to a " &
           "remote host is dead. All elementary operations performed on the " &
           "remote host (i.e. operations that are normally almost immediate " &
           "to perform) will use this timeout value. By default, this value " &
           "is set to 10s. If you have a very slow network connection or a " &
           "very overloaded server, set this timeout to a higher value."));

      Gtk_New (Label, -"Max number of connections:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 2, 3,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Max_Nb_Connected_Spin, 1.0, 50.0, 1.0);
      Set_Digits (Dialog.Max_Nb_Connected_Spin, 0);
      Attach (Dialog.Advanced_Table, Dialog.Max_Nb_Connected_Spin, 1, 2, 2, 3,
              Fill or Expand, 0);
      Set_Tip
        (Tips, Dialog.Max_Nb_Connected_Spin,
         -("The maximum number of connections determines the maximum number " &
           "of simultaneous connections GPS is allowed to perform to this " &
           "server. In fact, if you want to compile, debug and execute at " &
           "the same time on the machine, GPS will need more that one " &
           "connection to do this. The default value is 3."));

      Gtk_New (Label, -"Debug console:");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Dialog.Advanced_Table, Label, 0, 1, 3, 4,
              Fill or Expand, 0, 10);
      Gtk_New (Dialog.Debug_Button);
      Attach (Dialog.Advanced_Table, Dialog.Debug_Button, 1, 2, 3, 4, 0, 0);
      Set_Tip
        (Tips, Dialog.Debug_Button,
         -("The Debug console allow you to easily debug a remote connection." &
           " If checked, it will open a console reporting all exchanges " &
           "between GPS and the selected server."));

      --  Remote paths configuration
      Line_Nb := Line_Nb + 1;
      Gtk_New
        (Dialog.Paths_List_Widget, Gtk_Dialog (Dialog));
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

      --  Callbacks connections

      Widget_Boolean_Callback.Object_Connect
        (Dialog.Network_Name_Entry, "changed", On_Changed'Access,
         Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.User_Name_Entry, "changed", On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Access_Combo),
         "changed", On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Shell_Combo),
         "changed", On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Get_Entry (Dialog.Remote_Sync_Combo),
         "changed", On_Changed'Access, Dialog, False);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Max_Nb_Connected_Spin, "changed", On_Changed'Access, Dialog,
         False);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Timeout_Spin, "changed", On_Changed'Access, Dialog, True);
      Widget_Boolean_Callback.Object_Connect
        (Get_Buffer (Dialog.Init_Cmds_View), "changed",
         On_Changed'Access, Dialog, False);
      Widget_Boolean_Callback.Object_Connect
        (Dialog.Debug_Button, "clicked", On_Changed'Access, Dialog, False);
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

         if J = 1 or else Get_Nickname (J) = Default_Server then
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
         end if;
      end loop;

      if Empty_List then
         Set_Child_Visible (Dialog.Right_Table, False);
      end if;

      Tmp := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Tmp := Add_Button (Dialog, Stock_Apply, Gtk_Response_Apply);
      Tmp := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);
   end Gtk_New;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (W                 : access Gtk_Widget_Record'Class;
                         Connection_Params : Boolean)
   is
      Dialog    : Server_List_Editor_Record
                    renames Server_List_Editor_Record (W.all);
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if not Dialog.Restoring then
         Get_Selected (Get_Selection (Dialog.Machine_Tree), Model, Iter);

         --  Set this iter as modified
         Set (Gtk_Tree_Store (Model), Iter, Modified_Col, True);

         if Connection_Params and then Dialog.Selected_Machine /= null then
            Machine_Descriptor_Record
              (Dialog.Selected_Machine.Desc.all).Applied := False;
         end if;

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

            if Idx_End < Idx then
               N_Lines := 0;
            else
               N_Lines := Lines_Count (Str (Idx .. Idx_End));
            end if;

            declare
               Substr : constant String := Str (Idx .. Idx_End);
               List   : GNAT.Strings.String_List (1 .. N_Lines);
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

      Model       : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item        : Item_Access;
      Attribute   : Descriptor_Attribute;
      Modified    : Boolean;
      Dbg         : Connection_Debugger;
      Is_Selected : Boolean;

   begin
      Trace (Me, "Save");
      Model := Gtk_Tree_Store (Get_Model (Dialog.Machine_Tree));
      Iter  := Get_Iter_First (Model);

      Modified := False;

      while Iter /= Null_Iter loop
         --  Do not check or save a selected item !

         if Get_Boolean (Model, Iter, Modified_Col)
           and then (not Save_Selected or else
                     Iter_Is_Selected (Get_Selection (Dialog.Machine_Tree),
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

      Is_Selected := Iter_Is_Selected (Get_Selection (Dialog.Machine_Tree),
                                       Iter);

      if not Check_Fields (Model, Iter, Dialog) then
         Trace (Me, "Setting back selection to uncomplete machine");

         if not Is_Selected then
            Dialog.Select_Back := True;
            Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
         end if;

         return False;
      end if;

      declare
         Nickname : constant String := Get_String (Model, Iter, Name_Col);
         Applied  : Boolean;
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
            Applied := Machine_Descriptor_Record (Item.Desc.all).Applied;

            --  Free replaced descriptor
            Unref (Item.Desc);

            if Get_Active (Dialog.Debug_Button) then
               Dbg := Create (Dialog.Kernel, Nickname & " session");
            else
               Dbg := null;
            end if;

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
               Applied             => Applied,
               Ref                 => 1,
               Dbg                 => Dbg);
         end if;

         --  Now save the paths

         Trace (Me, "Internal save paths");

         begin
            Save_Tentative_Path_List
              (Dialog.Paths_List_Widget,
               Get_Filesystem_From_Shell
                 (Get_Text (Get_Entry (Dialog.Remote_Shell_Combo))));
         exception
            when Invalid_Path =>
               Trace (Me, "Invalid path detected, selecting back " & Nickname);

               if not Is_Selected then
                  Dialog.Select_Back := True;
                  Select_Iter (Get_Selection (Dialog.Machine_Tree), Iter);
               end if;

               return False;
         end;
      end;

      --  Machine saved, set the Modified_Col to False

      Set (Model, Iter, Modified_Col, False);

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

   begin
      Trace (Me, "on selection changed");

      if Dialog.Select_Back then
         --  Do not change dialog values
         Trace (Me, "select change: selecting back");
         Dialog.Select_Back := False;
         return;
      end if;

      --  If we just added a machine, do not perform any save

      if not Dialog.Added_Item
        and then not Save (Server_List_Editor (W))
      then

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

            Dialog.Selected_Machine := Item;

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
            Set_Active (Dialog.Debug_Button, Item.Desc.Dbg /= null);

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

            Set_Path_List (Dialog.Paths_List_Widget, Get_List (Nickname));
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
                  Applied             => False,
                  Ref                 => 1,
                  Dbg                 => null),
               Next => Dialog.Machines);

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
               Machine_Descriptor_Record
                 (Dialog.Selected_Machine.Desc.all).Applied := False;

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
               Set_Active (Dialog.Debug_Button, Item.Desc.Dbg /= null);
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
     (Kernel         : GPS.Kernel.Kernel_Handle;
      Default_Server : String := "")
   is
      Dialog  : Server_List_Editor;
      Resp    : Gtk_Response_Type;
      Item    : Item_Access;
      Updated : Boolean;
      N       : Natural;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Item_Record, Item_Access);

   begin
      Gtk_New (Dialog, Kernel, Default_Server);

      loop
         Resp := Run (Dialog);

         --  Apply changes

         if Resp = Gtk_Response_OK or Resp = Gtk_Response_Apply then
            --  First make sure the last edited machine is saved

            if Save (Dialog, True) then
               --  For all config, apply in g-exttre

               --  Update already set machines
               N := 1;

               while N <= Get_Nb_Machine_Descriptor loop
                  declare
                     Desc     : Machine_Descriptor :=
                                  Get_Machine_Descriptor (N);
                     Nickname : constant String :=
                                  Desc.Nickname.all;
                  begin
                     Item := Dialog.Machines;
                     Updated := False;

                     while Item /= null loop
                        if Item.Desc.Nickname.all = Nickname then
                           Machine_Descriptor_Record (Item.Desc.all).Applied :=
                             True;
                           Add_Machine_Descriptor (Item.Desc);
                           Updated := True;

                           exit;
                        end if;

                        Item := Item.Next;
                     end loop;

                     --  Not found in dialog: has been removed
                     if not Updated then
                        Remove_Machine_Descriptor (Desc);
                     else
                        N := N + 1;
                     end if;
                  end;
               end loop;

               --  Set new machines
               Item := Dialog.Machines;

               while Item /= null loop
                  if not Machine_Descriptor_Record (Item.Desc.all).Applied then
                     Machine_Descriptor_Record (Item.Desc.all).Applied := True;
                     Add_Machine_Descriptor (Item.Desc);
                  end if;

                  Item := Item.Next;
               end loop;

               --  Apply mirror paths
               for N in 1 .. Get_Nb_Machine_Descriptor loop

                  declare
                     Nickname : constant String :=
                                  Get_Machine_Descriptor (N).Nickname.all;
                     List     : Mirror_List_Access;
                     Cursor   : Mirror_List.Cursor;
                     Next     : Mirror_List.Cursor;
                     Path     : Mirror_Path;
                     Modified : Boolean;
                  begin
                     Trace (Me, "Apply path for machine " & Nickname);
                     Modified := False;
                     List     := Get_List (Nickname);
                     Cursor   := List.First;

                     while Mirror_List.Has_Element (Cursor) loop
                        Path := Mirror_List.Element (Cursor);

                        if Get_Deleted_State (Path) then
                           Trace (Me, "Delete element");
                           Modified := True;
                           Next := Mirror_List.Next (Cursor);
                           List.Delete (Cursor);
                           Cursor := Next;
                        else
                           if Is_Modified (Path) then
                              Trace (Me, "Update element");
                              Modified := True;
                              List.Update_Element (Cursor, Apply'Access);
                           else
                              Trace (Me, "Element not modified");
                           end if;
                           Mirror_List.Next (Cursor);
                        end if;
                     end loop;

                     if Modified then
                        Invalidate_Predefined_Paths_Cache (Kernel, Nickname);
                     end if;
                  end;
               end loop;

               Save_Remote_Config (Kernel);
               Run_Hook (Kernel, Server_List_Changed_Hook);

               exit when Resp = Gtk_Response_OK;
            end if;

         else
            --  Cancel clicked

            --  Revert mirror paths
            for N in 1 .. Get_Nb_Machine_Descriptor loop

               declare
                  Nickname : constant String :=
                               Get_Machine_Descriptor (N).Nickname.all;
                  List     : Mirror_List_Access;
                  Cursor   : Mirror_List.Cursor;
                  Next     : Mirror_List.Cursor;
               begin
                  List := Get_List (Nickname);
                  Cursor := List.First;

                  while Mirror_List.Has_Element (Cursor) loop
                     List.Update_Element (Cursor, Cancel'Access);

                     if Mirror_List.Element (Cursor) = Null_Path then
                        Next := Mirror_List.Next (Cursor);
                        List.Delete (Cursor);
                        Cursor := Next;
                     else
                        Mirror_List.Next (Cursor);
                     end if;
                  end loop;
               end;
            end loop;

            exit; --  Exit loop when Cancel is clicked
         end if;
      end loop;

      --  Destroy duplicated machine list
      while Dialog.Machines /= null loop
         Item := Dialog.Machines.Next;
         Unref (Dialog.Machines.Desc);
         Unchecked_Free (Dialog.Machines);
         Dialog.Machines := Item;
      end loop;

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
         Synchronous  : constant Boolean := Nth_Arg (Data, 8);
         Print_Output : constant Boolean := Nth_Arg (Data, 9);

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
            Synchronous      => Synchronous,
            Print_Output     => Print_Output);
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
      Set_Nth_Arg (D.all, 8, Data.Synchronous);
      Set_Nth_Arg (D.all, 9, Data.Print_Output);
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
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

      --  Register build server connected hook
      Register_Hook_No_Args (Kernel, Build_Server_Connected_Hook);

      --  Register server list changed hook
      Register_Hook_No_Args
        (Kernel, Server_List_Changed_Hook);

      --  Load user specific machine list
      Load_Remote_Config (Kernel_Handle (Kernel));

      --  Register the module
      Remote_Module := new Remote_Module_Record;
      Remote_Module.Kernel := Kernel_Handle (Kernel);
      Remote_Module.Project_Reloading := False;
      Register_Module
        (Remote_Module, Kernel, "remote");

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

      for J in Property.Servers'Range loop
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
         for J in Property.Servers'Range loop
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

      else
         for J in Property.Servers'Range loop
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
      --  This module reloaded the project: remote config is OK
      if Remote_Module.Project_Reloading then
         return;
      end if;

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

      --  If no previous property exist, create it
      if not Success then
         Trace (Me, "Property servers_config does not exist. Create it");

         if not Is_Local (D.File) then
            for J in Property.Servers'Range loop
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Get_Host (D.File)));
            end loop;

            --  Set the property for loaded project
            Prop := new Servers_Property'(Property);
            Set_Property (Local_File, "servers_config", Prop,
                          Persistent => True);

         else
            for J in Property.Servers'Range loop
               Property.Servers (J) := (Is_Local => True,
                                        Nickname => new String'(""));
            end loop;
         end if;
      end if;

      --  Assign servers following property values

      for J in Property.Servers'Range loop
         --  If current server is not local, and we're assigning the local
         --  server.
         if not Is_Local (J) and then
           (Property.Servers (J).Nickname.all = Local_Nickname
            or else Property.Servers (J).Nickname.all = "")
         then
            declare
               Hook_Data : aliased Server_Config_Changed_Hooks_Args :=
                 (Hooks_Data with
                  Nickname_Length => Local_Nickname'Length,
                  Server          => J,
                  Nickname        => Local_Nickname);
            begin
               Assign (J, "");
               Run_Hook (Kernel, Server_Config_Changed_Hook,
                         Hook_Data'Unchecked_Access);
            end;

         elsif Property.Servers (J).Nickname.all /= Get_Nickname (J) then
            declare
               Nickname  : constant String :=
                             Property.Servers (J).Nickname.all;
               Hook_Data : aliased Server_Config_Changed_Hooks_Args :=
                             (Hooks_Data with
                              Nickname_Length => Nickname'Length,
                              Server          => J,
                              Nickname        => Nickname);

            begin
               Assign (J, Nickname);
               Run_Hook (Kernel, Server_Config_Changed_Hook,
                         Hook_Data'Unchecked_Access);
            end;
         end if;
      end loop;

      --  If Project is loaded from a distant host, force build_server as
      --  distant host.

      if not Is_Local (D.File)
        and then Get_Host (D.File) /= Get_Nickname (Build_Server)
      then
         Trace (Me, "Assign build server: project loaded from remote host");
         Assign (Kernel_Handle (Kernel),
                 Build_Server,
                 Get_Host (D.File),
                 Local_File,
                 Reload_Prj => False);
      end if;

      --  If project is loaded from distant host then synchronize all dirs to
      --  local machine

      if not Is_Local (D.File) then
         Trace (Me, "Start synchronization of build_server");
         Synchronize
           (Kernel_Handle (Kernel), Build_Server, GPS_Server,
            Blocking       => True,
            Print_Output   => False,
            Sync_Once_Dirs => True);
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
      pragma Unreferenced (Level);
      Child                     : Node_Ptr;
      Name                      : Glib.String_Ptr;
      Start_Command             : Glib.String_Ptr;
      Start_Command_Common_Args : String_List_Access;
      Start_Command_User_Args   : String_List_Access;
      User_Prompt_Ptrn          : Glib.String_Ptr;
      Password_Prompt_Ptrn      : Glib.String_Ptr;
      Passphrase_Prompt_Ptrn    : Glib.String_Ptr;
      Extra_Ptrn_Length         : Natural;
      Use_Cr_Lf                 : Boolean;
      Use_Pipes                 : Boolean;

   begin
      if Node.Tag.all = "remote_machine_descriptor" then
         Trace (Me, "Customize: 'remote_machine_descriptor'");
         Parse_Remote_Machine_Descriptor_Node
           (Module.Kernel, Node, System_Defined);

      elsif Node.Tag.all = "remote_path_config" then
         Trace (Me, "Customize: 'remote_path_config'");
         Parse_Remote_Path_Node (Node);
         --  ??? Should set attrib to System_Defined

      elsif Node.Tag.all = "remote_shell_config" then
         Trace (Me, "Customize: 'remote_shell_config'");

         declare
            Shell_Name             : constant String
                                      := Get_Attribute (Node, "name", "");
            Shell_Cmd              : Glib.String_Ptr;
            Default_Generic_Prompt : aliased String
                                      := "^[^\n]*[#$%>] *$";
            Generic_Prompt         : Glib.String_Ptr;
            GPS_Prompt             : Glib.String_Ptr;
            FS_Str                 : Glib.String_Ptr;
            Windows_FS             : aliased Windows_Filesystem_Record;
            Unix_FS                : aliased Unix_Filesystem_Record;
            FS                     : Filesystem_Access;
            Init_Cmds_Child        : Node_Ptr;
            Exit_Cmds_Child        : Node_Ptr;
            Nb_Init_Cmds           : Natural;
            Nb_Exit_Cmds           : Natural;
            Cd_Cmd                 : Glib.String_Ptr;
            Get_Status_Cmd         : Glib.String_Ptr;
            Get_Status_Ptrn        : Glib.String_Ptr;

         begin
            if Shell_Name = "" then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'name' attribute in remote_shell_config",
                  Mode => Error);
               return;
            end if;

            Shell_Cmd := Get_Field (Node, "start_command");
            if Shell_Cmd = null then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'start_command' child in remote_shell_config",
                  Mode => Error);
               return;
            end if;

            Generic_Prompt := Get_Field (Node, "generic_prompt");
            if Generic_Prompt = null then
               Generic_Prompt := Default_Generic_Prompt'Unchecked_Access;
            end if;

            GPS_Prompt := Get_Field (Node, "gps_prompt");
            if GPS_Prompt = null then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'gps_prompt' child in remote_shell_config",
                  Mode => Error);
               return;
            end if;

            FS_Str := Get_Field (Node, "filesystem");
            if FS_Str = null then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'filesystem' child in remote_shell_config",
                  Mode => Error);
               return;
            end if;
            if FS_Str.all = "windows" then
               FS := Windows_FS'Unchecked_Access;
            elsif FS_Str.all = "unix" then
               FS := Unix_FS'Unchecked_Access;
            else
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": 'filesystem' child has " & FS_Str.all &
                  " value. Only 'windows' or 'unix' values are supported",
                  Mode => Error);
               return;
            end if;

            Init_Cmds_Child := Find_Tag (Node.Child, "init_commands");

            if Init_Cmds_Child /= null then
               Child := Init_Cmds_Child.Child;
            else
               Child := null;
            end if;

            Nb_Init_Cmds := 0;

            while Child /= null loop
               Child := Child.Next;
               Nb_Init_Cmds := Nb_Init_Cmds + 1;
            end loop;

            Exit_Cmds_Child := Find_Tag (Node.Child, "exit_commands");

            if Exit_Cmds_Child /= null then
               Child := Exit_Cmds_Child.Child;
            else
               Child := null;
            end if;

            Nb_Exit_Cmds := 0;

            while Child /= null loop
               Child := Child.Next;
               Nb_Exit_Cmds := Nb_Exit_Cmds + 1;
            end loop;

            Cd_Cmd := Get_Field (Node, "cd_command");
            if Cd_Cmd = null then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'cd_command' child in remote_shell_config",
                  Mode => Error);
               return;
            end if;

            Get_Status_Cmd := Get_Field (Node, "get_status_command");
            if Get_Status_Cmd = null then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'get_status_command' child in " &
                  "remote_shell_config",
                  Mode => Error);
               return;
            end if;

            Get_Status_Ptrn := Get_Field (Node, "get_status_ptrn");
            if Get_Status_Ptrn = null then
               Console.Insert
                 (Module.Kernel, "XML Error in " & Full_Name (File).all &
                  ": missing 'get_status_ptrn' child in remote_shell_config",
                  Mode => Error);
               return;
            end if;

            declare
               Init_Cmds : GNAT.Strings.String_List (1 .. Nb_Init_Cmds);
               Exit_Cmds : GNAT.Strings.String_List (1 .. Nb_Exit_Cmds);
               Idx       : Natural;
            begin
               Child := Init_Cmds_Child.Child;
               Idx := Init_Cmds'First;

               while Child /= null loop
                  Init_Cmds (Idx) := new String'(Child.Value.all);
                  Idx := Idx + 1;
                  Child := Child.Next;
               end loop;

               Child := Exit_Cmds_Child.Child;
               Idx   := Exit_Cmds'First;

               while Child /= null loop
                  Exit_Cmds (Idx) := new String'(Child.Value.all);
                  Idx := Idx + 1;
                  Child := Child.Next;
               end loop;

               Add_Shell_Descriptor
                 (Shell_Name,
                  Shell_Cmd.all,
                  Generic_Prompt.all,
                  GPS_Prompt.all,
                  FS.all,
                  Init_Cmds,
                  Exit_Cmds,
                  Cd_Cmd.all,
                  Get_Status_Cmd.all,
                  Get_Status_Ptrn.all);
            end;
         end;

      elsif Node.Tag.all = "remote_connection_config" then
         Trace (Me, "Initialize_Remote_Config: 'remote_connection_config'");

         Name := new String'(Get_Attribute (Node, "name"));

         if Name.all = "" then
            Console.Insert
              (Module.Kernel,
               -("XML Error: remote_connection_config tag is missing a " &
                 "name attribute in " & Full_Name (File).all),
               Add_LF => True, Mode => Error);
            return;
         end if;

         Child := Find_Tag (Node.Child, "start_command");

         if Child /= null then
            Use_Pipes :=
              Boolean'Value (Get_Attribute (Child, "use_pipes", "false"));
            Start_Command := Child.Value;
         else
            Start_Command := null;
         end if;

         if Start_Command = null then
            Console.Insert
              (Module.Kernel,
               -("XML Error: remote_connection_config is missing a " &
                 "start_command field in " & Full_Name (File).all),
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
            Use_Cr_Lf_String_Access : constant Glib.String_Ptr :=
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
            Str_Access  : Glib.String_Ptr;

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
               User_Prompt_Ptrn          => String_Access (User_Prompt_Ptrn),
               Password_Prompt_Ptrn      =>
                 String_Access (Password_Prompt_Ptrn),
               Passphrase_Prompt_Ptrn    =>
                 String_Access (Passphrase_Prompt_Ptrn),
               Extra_Prompt_Array        => Extra_Ptrns,
               Use_Cr_Lf                 => Use_Cr_Lf,
               Use_Pipes                 => Use_Pipes);
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

   -------------------
   -- Reload_Prj_Cb --
   -------------------

   function Reload_Prj_Cb (Data : Reload_Callback_Data) return Boolean is
   begin
      Trace (Me, "Reloading the project");
      Remote_Module.Project_Reloading := True;
      Load_Project (Data.Kernel, Project_Path (Get_Project (Data.Kernel)));
      Remote_Module.Project_Reloading := False;
      return False;
   end Reload_Prj_Cb;

   -------------------------------
   -- Is_Default_Remote_Setting --
   -------------------------------

   function Is_Default_Remote_Setting return Boolean is
      The_File : VFS.Virtual_File;
      Property : Servers_Property;
      Found    : Boolean;

   begin
      if Status (Get_Project (Remote_Module.Kernel)) /= From_File then
         return True;
      end if;

      The_File := Project_Path (Get_Project (Remote_Module.Kernel));
      Get_Property (Property, The_File, "servers_config", Found);

      if not Found then
         --  Check that all servers are local
         for J in Distant_Server_Type'Range loop
            if not Is_Local (J) then
               Trace
                 (Me, "server " & Server_Type'Image (J) & " not local");
               return False;
            end if;
         end loop;

      else
         for J in Distant_Server_Type'Range loop
            if  Property.Servers (J).Nickname.all /= Get_Printable_Nickname (J)
              and then Property.Servers (J).Nickname.all /= Get_Nickname (J)
            then
               Trace
                 (Me, "server " & Server_Type'Image (J) &
                  " is changed from property");
               return False;
            end if;
         end loop;
      end if;

      return True;
   end Is_Default_Remote_Setting;

   ---------------------------------
   -- Set_Default_Remote_Settings --
   ---------------------------------

   procedure Set_Default_Remote_Settings is
      The_File : VFS.Virtual_File;
      Property : Servers_Property;
      Prop     : Property_Access;
      Found    : Boolean;
      Set_Prop : Boolean;

   begin
      if Status (Get_Project (Remote_Module.Kernel)) /= From_File then
         --  ??? do we want to be able to set a default config for the default
         --  project ?
         return;
      end if;

      The_File := Project_Path (Get_Project (Remote_Module.Kernel));
      Get_Property (Property, The_File, "servers_config", Found);

      if Found then
         Remove_Property (The_File, "servers_config");
      end if;

      Set_Prop := False;
      for J in Distant_Server_Type'Range loop
         if not Is_Local (J) then
            Set_Prop := True;
            exit;
         end if;
      end loop;

      if not Set_Prop then
         --  No need to save: default setting is 'all servers set to local'
         return;
      end if;

      for J in Property.Servers'Range loop
         if Is_Local (J) then
            Property.Servers (J) :=
              (Is_Local => True, Nickname => new String'(""));
         else
            Property.Servers (J) :=
              (Is_Local => False, Nickname => new String'(Get_Nickname (J)));
         end if;
      end loop;

      Prop := new Servers_Property'(Property);
      Set_Property (The_File, "servers_config", Prop, Persistent => True);
   end Set_Default_Remote_Settings;

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
      Data      : aliased Server_Config_Changed_Hooks_Args :=
                    (Hooks_Data with
                     Nickname_Length => Nickname'Length,
                     Server          => Server,
                     Nickname        => Nickname);
      Timeout   : constant Guint32 := 50;
      Id        : Timeout_Handler_Id;
      Load_Data : Reload_Callback_Data;
      pragma Unreferenced (Id);

   begin
      if Nickname = "" then
         --  Force to local nickname so that hook's data is correct.
         Assign (Kernel, Server, Local_Nickname, Prj_File, Reload_Prj);
         return;
      end if;

      if Get_Nickname (Server) = Nickname
        or else Get_Printable_Nickname (Server) = Nickname
      then
         return;
      end if;

      Assign (Server, Nickname);

      --  Reload project if Build_Server has been assigned

      if Server = Build_Server and then Reload_Prj then
         Load_Data.Kernel := Kernel;
         Load_Data.File :=
           Create
             (Get_Nickname (Build_Server),
              To_Remote
                (Project_Path (Get_Project (Kernel)).Full_Name (True).all,
                 Build_Server));

         if Get_Host (Load_Data.File) /= Get_Nickname (Build_Server) then
            Insert (Kernel,
                    -"Error: the project " & Full_Name (Load_Data.File).all &
                    (-" has no path equivalence on remote machine ") &
                    Get_Nickname (Build_Server));
         else
            Trace (Me, "Asking project reload");
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

   Id : Natural := 0;
   function Get_New_Queue_Id return String;
   --  Returns a new unique queue id

   ----------------------
   -- Get_New_Queue_Id --
   ----------------------

   function Get_New_Queue_Id return String is
      Str_Id : constant String := Natural'Image (Id);
   begin
      Id := Id + 1;
      return "gps-kernel-remote-sync" & Str_Id;
   end Get_New_Queue_Id;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize
     (Kernel         : Kernel_Handle;
      From           : String;
      To             : String;
      Blocking       : Boolean;
      Print_Output   : Boolean;
      Sync_Once_Dirs : Boolean;
      Queue_Id       : String  := "")
   is
      List           : Mirror_List_Access;
      Cursor         : Mirror_List.Cursor;
      Path           : Mirror_Path;
      From_Path      : Ada.Strings.Unbounded.Unbounded_String;
      To_Path        : Ada.Strings.Unbounded.Unbounded_String;
      Machine        : Machine_Descriptor_Record;
      Need_Sync      : Boolean;

      function Get_Queue_Id return String;
      --  Get a new queue id if needed

      function Get_Queue_Id return String is
      begin
         if Queue_Id /= "" then
            return Queue_Id;
         elsif not Blocking then
            return Get_New_Queue_Id;
         else
            return "";
         end if;
      end Get_Queue_Id;

      The_Queue_Id : constant String := Get_Queue_Id;
   begin
      --  Make sure that local nickname is empty string, not Local_Nickname
      if From = Local_Nickname then
         Synchronize
           (Kernel, "", To, Blocking, Print_Output, Sync_Once_Dirs, Queue_Id);
         return;

      elsif To = Local_Nickname then
         Synchronize
           (Kernel, From, "", Blocking, Print_Output, Sync_Once_Dirs,
            Queue_Id);
         return;
      end if;

      if To = "" then
         Trace (Me, "Synchronizing paths from " & From);
         List := Get_List (From);
      else
         Trace (Me, "Synchronizing paths to " & To);
         List := Get_List (To);
      end if;

      Cursor := List.First;

      while Mirror_List.Has_Element (Cursor) loop
         Path := Mirror_List.Element (Cursor);

         --  Determine if mirror path need to be synchronised
         case Path.Get_Synchronisation is
            when Never =>
               Need_Sync := False;

            when Once_To_Local =>
               if To = "" then
                  Need_Sync := Sync_Once_Dirs;
               else
                  Need_Sync := False;
               end if;

            when Once_To_Remote =>
               if To /= "" then
                  Need_Sync := Sync_Once_Dirs;
               else
                  Need_Sync := False;
               end if;

            when Always =>
               Need_Sync := True;
         end case;

         if Need_Sync then
            if From = "" then
               From_Path := Ada.Strings.Unbounded.To_Unbounded_String
                   (Path.Get_Local_Path);
               To_Path   := Ada.Strings.Unbounded.To_Unbounded_String
                   (Path.Get_Remote_Path);
               Machine   := Machine_Descriptor_Record
                 (Get_Machine_Descriptor (To).all);

            else
               From_Path := Ada.Strings.Unbounded.To_Unbounded_String
                 (Path.Get_Remote_Path);
               To_Path   := Ada.Strings.Unbounded.To_Unbounded_String
                 (Path.Get_Local_Path);
               Machine   := Machine_Descriptor_Record
                 (Get_Machine_Descriptor (From).all);
            end if;

            declare
               Data : aliased Rsync_Hooks_Args :=
                 (Hooks_Data with
                  Tool_Name_Length => Machine.Rsync_Func.all'Length,
                  Src_Name_Length  => From'Length,
                  Dest_Name_Length => To'Length,
                  Queue_Id_Length  => The_Queue_Id'Length,
                  Src_Path_Length  => Ada.Strings.Unbounded.Length (From_Path),
                  Dest_Path_Length => Ada.Strings.Unbounded.Length (To_Path),
                  Tool_Name        => Machine.Rsync_Func.all,
                  Src_Name         => From,
                  Dest_Name        => To,
                  Queue_Id         => The_Queue_Id,
                  Src_Path         =>
                    Ada.Strings.Unbounded.To_String (From_Path),
                  Dest_Path        =>
                    Ada.Strings.Unbounded.To_String (To_Path),
                  Synchronous      => Blocking,
                  Print_Output     => Print_Output);

            begin
               Trace (Me, "run sync hook for " & Data.Src_Path);

               if not Run_Hook_Until_Success
                 (Kernel, Rsync_Action_Hook, Data'Unchecked_Access)
               then
                  GPS.Kernel.Console.Insert
                    (Kernel,
                     Machine.Rsync_Func.all & (-" failure: ") &
                     (-"Directories ") &
                     Ada.Strings.Unbounded.To_String (From_Path) &
                     (-" and ") &
                     Ada.Strings.Unbounded.To_String (To_Path) &
                     (-" are not synchronized properly. ") &
                     (-"Please verify your network configuration"),
                     Mode => Error);

                  Trace (Me, "No remote sync was registered or errors during" &
                         " calls");
                  return;
               end if;

            exception
               when E : others =>
                  Trace (Exception_Handle, Exception_Information (E));
            end;
         end if;

         Mirror_List.Next (Cursor);
      end loop;
   end Synchronize;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize
     (Kernel         : Kernel_Handle;
      From           : Server_Type;
      To             : Server_Type;
      Blocking       : Boolean;
      Print_Output   : Boolean;
      Sync_Once_Dirs : Boolean;
      Queue_Id       : String  := "") is
   begin
      Synchronize
        (Kernel         => Kernel,
         From           => Get_Nickname (From),
         To             => Get_Nickname (To),
         Blocking       => Blocking,
         Print_Output   => Print_Output,
         Sync_Once_Dirs => Sync_Once_Dirs,
         Queue_Id       => Queue_Id);
   end Synchronize;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Kernel           : Kernel_Handle;
      Arguments        : GNAT.OS_Lib.Argument_List;
      Server           : Server_Type;
      Pd               : out GNAT.Expect.Process_Descriptor_Access;
      Success          : out Boolean;
      Use_Ext_Terminal : Boolean := False;
      Console          : Interactive_Consoles.Interactive_Console := null;
      Show_Command     : Boolean := True;
      Directory        : String := "";
      Use_Pipes        : Boolean := True)
   is
      Exec                  : String_Access;
      Old_Dir               : String_Access;
      Args                  : Argument_List_Access;
      L_Args                : Argument_List_Access := null;

      function Check_Exec (Exec : String) return String_Access;
      --  Check that executable is on the path, and return the full path if
      --  found, return null otherwise.

      procedure On_New_Connection (Server_Name : String);
      --  Executed when a new connection is performed.

      ----------------
      -- Check_Exec --
      ----------------

      function Check_Exec (Exec : String) return String_Access is
         Full_Exec : String_Access;
         Norm_Exec : String_Access;
      begin
         Full_Exec := Locate_Exec_On_Path (Exec);

         if Full_Exec = null then
            Insert
              (Kernel,
               -"Could not locate executable on path: " & Exec,
               Mode => Error);
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

      -----------------------
      -- On_New_Connection --
      -----------------------

      procedure On_New_Connection (Server_Name : String) is
      begin
         if Server_Name = Get_Nickname (Build_Server) then
            Run_Hook (Kernel, Build_Server_Connected_Hook);
         end if;
      end On_New_Connection;

   begin
      Success := False;

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
         Set_Use_Pipes (TTY_Process_Descriptor (Pd.all), Use_Pipes);

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
         begin
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

         exception
            when Invalid_Process =>
               raise Invalid_Process with "not an executable";
         end;

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
            Target_Nickname     => Get_Nickname (Server),
            Args                => Args.all,
            Execution_Directory => Old_Dir.all,
            Err_To_Out          => True,
            On_New_Connection   => On_New_Connection'Access);
         Free (Old_Dir);
      end if;

      Success := True;

      Free (Args);

   exception
      when E : Invalid_Process | Process_Died =>
         Success := False;
         Insert
           (Kernel,
            -"Error while trying to execute " & Args (Args'First).all & ": " &
            Ada.Exceptions.Exception_Message (E),
            Mode => Error);
      when E : Invalid_Nickname =>
         Success := False;
         Insert
           (Kernel,
            -"Remote configuration error: " &
            Ada.Exceptions.Exception_Message (E),
            Mode => Error);
   end Spawn;

end GPS.Kernel.Remote;
