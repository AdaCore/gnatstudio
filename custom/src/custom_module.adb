-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib;                      use Glib;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Icon_Factory;          use Gtk.Icon_Factory;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Handlers;              use Gtk.Handlers;
with Glib.Object;               use Glib.Object;

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with System.Assertions;         use System.Assertions;

with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Glide_Intl;                use Glide_Intl;
with Projects;                  use Projects;

with GUI_Utils;                 use GUI_Utils;
with File_Utils;                use File_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Language;                  use Language;
with Language.Custom;           use Language.Custom;
with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Commands.Custom;           use Commands.Custom;

with VFS;                       use VFS;

with Custom_Combos;             use Custom_Combos;
with Expect_Interface;          use Expect_Interface;

package body Custom_Module is

   package Action_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Action_Record_Access);

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Called when a new customization in parsed

   procedure Contextual_Handler
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Handles requests to display contextual menus

   procedure Register_Contextual_Menu
     (Kernel     : access Kernel_Handle_Record'Class;
      Menu_Title : String;
      Action     : Action_Record_Access);
   --  Register a new contextual menu

   procedure Contextual_Action
     (Kernel : access GObject_Record'Class; Action : Action_Record_Access);
   --  Execute action

   -----------------------
   -- Contextual_Action --
   -----------------------

   procedure Contextual_Action
     (Kernel : access GObject_Record'Class; Action : Action_Record_Access) is
   begin
      Launch_Background_Command
        (Kernel          => Kernel_Handle (Kernel),
         Command         => Action.Command,
         Active          => True,
         Show_Bar        => False,
         Destroy_On_Exit => False);
   end Contextual_Action;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel     : access Kernel_Handle_Record'Class;
      Menu_Title : String;
      Action     : Action_Record_Access)
   is
      pragma Unreferenced (Kernel);
   begin
      Custom_Module_ID.Contextual := new Contextual_Menu_Record'
        (Title  => new String'(Menu_Title),
         Action => Action,
         Next   => Custom_Module_ID.Contextual);
   end Register_Contextual_Menu;

   ------------------------
   -- Contextual_Handler --
   ------------------------

   procedure Contextual_Handler
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Contextual : Contextual_Menu_Access := Custom_Module_ID.Contextual;
      Item : Gtk_Menu_Item;
   begin
      while Contextual /= null loop
         if Filter_Matches
           (Contextual.Action.Filter, Selection_Context_Access (Context),
            Get_Kernel (Context))
         then
            Item := Find_Or_Create_Menu_Tree
              (Menu_Bar     => null,
               Menu         => Gtk_Menu (Menu),
               Path         => Contextual.Title.all,
               Accelerators => Get_Default_Accelerators (Get_Kernel (Context)),
               Allow_Create => True);

            if Item /= null then
               Action_Callback.Object_Connect
                 (Item, "activate",
                  Action_Callback.To_Marshaller (Contextual_Action'Access),
                  User_Data   => Contextual.Action,
                  Slot_Object => Get_Kernel (Context));
            end if;
         end if;

         Contextual := Contextual.Next;
      end loop;
   end Contextual_Handler;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level);

      Handler      : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));

      procedure Add_Child
        (Parent_Path  : String;
         Current_Node : Node_Ptr);
      --  Add a menuitem or submenu to the Parent_Path, according to
      --  what Current_Node contains.

      procedure Parse_Action_Node (Node : Node_Ptr);
      procedure Parse_Contextual_Node (Node : Node_Ptr);
      procedure Parse_Button_Node (Node : Node_Ptr);
      procedure Parse_Entry_Node (Node : Node_Ptr);
      procedure Parse_Tool_Node (Node : Node_Ptr);
      procedure Parse_Stock_Node (Node : Node_Ptr);
      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : UTF8_String);
      procedure Parse_Submenu_Node
        (Node : Node_Ptr; Parent_Path : UTF8_String);
      function Parse_Filter_Node (Node : Node_Ptr) return Action_Filter;
      --  Parse the various nodes: <action>, <shell>, ...

      ---------------------------
      -- Parse_Contextual_Node --
      ---------------------------

      procedure Parse_Contextual_Node (Node : Node_Ptr) is
         Action  : constant String := Get_Attribute (Node, "action");
         Child   : Node_Ptr;
         Title   : String_Access;
         Command : Action_Record_Access;
      begin
         if Action = "" then
            Insert (Kernel,
                    -"<contextual> nodes must have an action attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         Title := new String'(Action);

         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);

            else
               Insert
                 (Kernel,
                  -"Invalid child node for <contextual>: " & Child.Tag.all,
                  Mode => Error);
               Free (Title);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         Command := Lookup_Action (Kernel, Action);
         if Command.Command = null then
            Insert (Kernel,
                    -"Command not found when creating contextual menu: "
                    & Action,
                    Mode => Error);
            Free (Title);
            raise Assert_Failure;
         end if;

         Register_Contextual_Menu
           (Kernel,
            Menu_Title => Title.all,
            Action     => Command);

         Free (Title);
      end Parse_Contextual_Node;

      ---------------------
      -- Parse_Tool_Node --
      ---------------------

      procedure Parse_Tool_Node (Node : Node_Ptr) is
         Name  : constant String := Get_Attribute (Node, "name");
         Pack  : constant String :=
           Get_Attribute (Node, "package", Projects.Ide_Package);
         Index : constant String :=
           To_Lower (Get_Attribute (Node, "index", Name));
         Attribute : constant String :=
           Get_Attribute (Node, "attribute", "default_switches");
         Switches : String_Access;
         N     : Node_Ptr := Node.Child;
      begin
         if Name = "" then
            Insert (Kernel,
                    -"Invalid <tool> node, it must have a name attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         while N /= null loop
            if N.Tag.all = "initial-cmd-line" then
               Free (Switches);
               Switches := new String'(N.Value.all);
            elsif N.Tag.all = "language"
              or else N.Tag.all = "switches"
            then
               --  Handled in prj_editor module
               null;
            else
               Insert (Kernel,
                       -"Unsupport child tag for <tool>: " & N.Tag.all,
                       Mode => Error);
            end if;

            N := N.Next;
         end loop;

         Register_Tool
           (Kernel,
            Tool_Name => Name,
            Tool      => (Project_Package   => new String'(Pack),
                          Project_Attribute => new String'(Attribute),
                          Project_Index     => new String'(Index),
                          Initial_Cmd_Line  => Switches));
      end Parse_Tool_Node;

      -----------------------
      -- Parse_Filter_Node --
      -----------------------

      function Parse_Filter_Node (Node : Node_Ptr) return Action_Filter is
         Filter, Filter_Tmp : Action_Filter;
         Child  : Node_Ptr;
      begin
         if Node.Tag.all = "filter" then
            declare
               Lang    : constant String  := Get_Attribute (Node, "language");
               Shell   : constant String  := Get_Attribute (Node, "shell_cmd");
               Shell_Lang : constant String :=
                 Get_Attribute (Node, "shell_lang", GPS_Shell_Name);
               Module  : constant String := Get_Attribute (Node, "module");
               Id      : constant String := Get_Attribute (Node, "id");
            begin
               if Id /= "" then
                  Filter := Lookup_Filter (Kernel, Id);
                  if Filter = null then
                     Insert
                       (Kernel,
                          -"Unknown action filter " & Id,
                        Mode => Error);
                     raise Assert_Failure;
                  end if;
               else
                  Filter := Action_Filter
                    (Create
                       (Language   => Lang,
                        Shell      => Shell,
                        Shell_Lang => Shell_Lang,
                        Module     => Module));
                  Set_Error_Message (Filter, Get_Attribute (Node, "error"));
               end if;
            end;

         else
            Child := Node.Child;
            while Child /= null loop
               if Child.Tag.all = "filter"
                 or else Child.Tag.all = "filter_and"
                 or else Child.Tag.all = "filter_or"
               then
                  Filter_Tmp := Parse_Filter_Node (Child);

                  if Filter = null then
                     Filter := Filter_Tmp;
                  elsif Node.Tag.all = "filter_and" then
                     Filter := Action_Filter (Filter and Filter_Tmp);
                  elsif Node.Tag.all = "filter_or" then
                     Filter := Action_Filter (Filter or Filter_Tmp);
                  end if;
               end if;

               Child := Child.Next;
            end loop;

            Set_Error_Message (Filter, Get_Attribute (Node, "error"));
         end if;

         return Filter;
      end Parse_Filter_Node;

      -----------------------
      -- Parse_Action_Node --
      -----------------------

      procedure Parse_Action_Node (Node : Node_Ptr) is
         Name    : constant String := Get_Attribute (Node, "name");
         Child   : Node_Ptr;
         Command : Custom_Command_Access;
         Description : String_Access := new String'("");
         Filter_A    : Action_Filter;
         Implicit_Filter : Action_Filter;
      begin
         if Name = "" then
            Insert
              (Kernel,
               -("<action> tags in customization files must have"
                 & " a name attribute"),
               Mode => Error);
            raise Assert_Failure;  --  So that the name of the file is shown
         end if;

         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "shell"
              or else To_Lower (Child.Tag.all) = "external"
              or else To_Lower (Child.Tag.all) = "on-failure"
            then
               --  Handled directly by Commands.Custom
               null;

            elsif Child.Tag.all = "filter"
              or else Child.Tag.all = "filter_and"
              or else Child.Tag.all = "filter_or"
            then
               if Filter_A /= null then
                  Filter_A :=
                    Action_Filter (Filter_A or Parse_Filter_Node (Child));
               else
                  Filter_A := Parse_Filter_Node (Child);
               end if;

            elsif To_Lower (Child.Tag.all) = "description" then
               Free (Description);
               Description := new String'(Child.Value.all);
            else
               Insert
                 (Kernel,
                  -"Invalid child node for <action> tag",
                  Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         Create (Command,
                 Name           => Name,
                 Kernel         => Kernel_Handle (Kernel),
                 Command        => Node.Child,
                 Default_Output => Get_Attribute
                   (Node, "output", Console_Output),
                 Show_Command => To_Lower
                   (Get_Attribute (Node, "show-command", "true")) = "true");

         Implicit_Filter := Create_Filter (Node.Child);

         if Implicit_Filter /= null then
            if Filter_A /= null then
               declare
                  Error : constant String := Get_Error_Message (Filter_A);
               begin
                  Filter_A := Action_Filter (Filter_A and Implicit_Filter);
                  Set_Error_Message (Filter_A, Error);
               end;
            else
               Filter_A := Implicit_Filter;
            end if;
         end if;

         Register_Action
           (Kernel,
            Name        => Name,
            Command     => Command,
            Description => Description.all,
            Filter      => Filter_A);
         Free (Description);
      end Parse_Action_Node;

      -----------------------
      -- Parse_Button_Node --
      -----------------------

      procedure Parse_Button_Node (Node : Node_Ptr) is
         Action  : constant String := Get_Attribute (Node, "action");
         Child   : Node_Ptr;
         Title   : String_Access := new String'("");
         Pixmap  : String_Access := new String'("");
         Image   : Gtk_Image;
         Command : Action_Record_Access;

      begin
         if Action = "" then
            Insert (Kernel, -"<button> nodes must have an action attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         Child := Node.Child;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);
            elsif To_Lower (Child.Tag.all) = "pixmap" then
               Free (Pixmap);
               Pixmap := new String'(Child.Value.all);
            else
               Insert
                 (Kernel, -"Invalid child node for <button> tag",
                  Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         if Title.all /= "" then
            if Pixmap.all /= ""
              and then Is_Regular_File (Pixmap.all)
            then
               Gtk_New (Image, Pixmap.all);
            end if;

            Command := Lookup_Action (Kernel, Action);

            if Command /= null and then Command.Command /= null then
               Register_Button
                 (Kernel,
                  Title.all,
                  Command_Access (Command.Command),
                  Image);
            end if;

         else
            Append_Space (Get_Toolbar (Kernel));
         end if;

         Free (Title);
         Free (Pixmap);
      end Parse_Button_Node;

      ----------------------
      -- Parse_Entry_Node --
      ----------------------

      procedure Parse_Entry_Node (Node : Node_Ptr) is
         Child   : Node_Ptr;
         Id      : constant String := Get_Attribute (Node, "id", "");
      begin
         if Id = "" then
            Insert
              (Kernel,
               -"<entry> nodes must have a non-empty ""id"" attribute",
               Mode => Error);

            return;
         end if;

         Child := Node.Child;

         --  Determine the title of the combo and register it.

         if Child /=
           null and then To_Lower (Child.Tag.all) = "label"
         then
            Register_Combo (Kernel, Child.Value.all, Id);
         else
            Register_Combo (Kernel, "", Id);
         end if;

         --  Determine the default action for this entry

         declare
            Action  : constant String := Get_Attribute (Node, "on-changed");
            Command : Action_Record_Access;
         begin
            if Action /= "" then
               Command := Lookup_Action (Kernel, Action);

               if Command = null then
                  Insert
                    (Kernel,
                     -"Action not registered: " & Action,
                     Mode => Error);
               else
                  Set_Combo_Changed_Action (Kernel, Id, Command);
               end if;
            end if;
         end;

         --  Parse the child nodes

         Child := Child.Next;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "choice" then
               --  ??? Need to implement "default" attribute.

               Add_Combo_Entry
                 (Kernel,
                  Id,
                  Child.Value.all,
                  Lookup_Action
                    (Kernel, Get_Attribute (Child, "on-selected")));

            elsif To_Lower (Child.Tag.all) = "label" then
               Insert
                 (Kernel,
                  -"<label> node must be the first child in an <entry> node",
                  Mode => Error);
            else
               Insert
                 (Kernel,
                  -"Invalid child node for <entry> tag: " & Child.Tag.all,
                  Mode => Error);
            end if;

            Child := Child.Next;
         end loop;
      end Parse_Entry_Node;

      ------------------------
      -- Parse_Submenu_Node --
      ------------------------

      procedure Parse_Submenu_Node
        (Node : Node_Ptr; Parent_Path : UTF8_String)
      is
         Child : Node_Ptr := Node.Child;
         Title : String_Access := new String'("");
         Before : constant String := Get_Attribute (Node, "before");
         After  : constant String := Get_Attribute (Node, "after");
      begin
         --  First look for the title of the submenu
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               if Title.all /= "" then
                  Insert (Kernel,
                          -"Only one <title> node allowed in <submenu>",
                          Mode => Error);
                  raise Assert_Failure;
               end if;

               Free (Title);
               Title := new String'(Child.Value.all);
            end if;
            Child := Child.Next;
         end loop;

         Child := Node.Child;

         --  If specific locations are specified, create the menu now

         if Before /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path =>
                 Name_As_Directory (Parent_Path, UNIX) & Title.all,
               Item        => null,
               Ref_Item    => Before,
               Add_Before  => True);
         elsif After /= "" then
            Register_Menu
              (Kernel      => Kernel,
               Parent_Path =>
                 Name_As_Directory (Parent_Path, UNIX) & Title.all,
               Item        => null,
               Ref_Item    => After,
               Add_Before  => False);
         end if;

         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               null; --  Already handled
            elsif To_Lower (Child.Tag.all) = "submenu" then
               Parse_Submenu_Node
                 (Child, Name_As_Directory (Parent_Path, UNIX) & Title.all);
            elsif To_Lower (Child.Tag.all) = "menu" then
               Parse_Menu_Node
                 (Child, Name_As_Directory (Parent_Path, UNIX) & Title.all);
            elsif To_Lower (Child.Tag.all) = "menu_item"
              or else To_Lower (Child.Tag.all) = "toolbar_item"
            then
               Insert
                 (Kernel,
                  -("<menu_item> and <toolbar_item> are no longer"
                    & " supported. Please use the program"
                    & " gps2custom-1.3 to convert to the new format."),
                  Mode => Error);
               raise Assert_Failure;
            else
               Insert (Kernel,
                       -"Invalid child node for <submenu>: "
                       & Child.Tag.all,
                       Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         Free (Title);
      end Parse_Submenu_Node;

      ---------------------
      -- Parse_Menu_Node --
      ---------------------

      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : UTF8_String) is
         Action : constant String := Get_Attribute (Node, "action");
         Before : constant String := Get_Attribute (Node, "before");
         After  : constant String := Get_Attribute (Node, "after");
         Child  : Node_Ptr;
         Title  : String_Access := new String'("");
         Item   : Gtk_Menu_Item;
         Command : Action_Record_Access;
      begin
         Child := Node.Child;
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "title" then
               Free (Title);
               Title := new String'(Child.Value.all);
            else
               Insert
                 (Kernel, -"Invalid child node for <menu> tag", Mode => Error);
               raise Assert_Failure;
            end if;

            Child := Child.Next;
         end loop;

         --  Special case to allow separators
         if Action = ""
           and then Title.all /= ""
         then
            Insert (Kernel, -"<menu> nodes must have an action attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         if Title.all = "" then
            Gtk_New (Item);
            Register_Menu (Kernel, Parent_Path, Item);
         else
            Command := Lookup_Action (Kernel, Action);
            if Command.Command /= null then

               if Before /= "" then
                  Register_Menu
                    (Kernel,
                     Dir_Name
                       (Name_As_Directory (Parent_Path, UNIX) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command,
                     Ref_Item    => Before);

               elsif After /= "" then
                  Register_Menu
                    (Kernel,
                     Dir_Name
                       (Name_As_Directory (Parent_Path, UNIX) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command,
                     Ref_Item    => After,
                     Add_Before  => False);

               else
                  Register_Menu
                    (Kernel,
                     Dir_Name
                       (Name_As_Directory (Parent_Path, UNIX) & Title.all),
                     Text        => Base_Name (Title.all),
                     Stock_Image => "",
                     Callback    => null,
                     Action      => Command);
               end if;
            else
               Insert
                 (Kernel, -"Command not found for creating menu: "
                  & Action, Mode => Error);
            end if;
         end if;
         Free (Title);
      end Parse_Menu_Node;

      ----------------------
      -- Parse_Stock_Node --
      ----------------------

      procedure Parse_Stock_Node (Node : Node_Ptr) is
         Child   : Node_Ptr := Node.Child;
         Factory : constant Gtk_Icon_Factory := Get_Icon_Factory (Kernel);
         Source  : Gtk_Icon_Source;
         Set     : Gtk_Icon_Set;
      begin
         while Child /= null loop
            if To_Lower (Child.Tag.all) = "icon" then
               declare
                  Id       : constant String := Get_Attribute (Child, "id");
                  Filename : constant String := Get_Attribute (Child, "file");
                  Pic_File : VFS.Virtual_File;
               begin
                  if Id = "" then
                     Insert
                       (Kernel,
                        -"No id specified for stock icon.",
                        Mode => Error);

                  elsif Filename = "" then
                     Insert
                       (Kernel,
                        -"No file specified for stock icon " & Id,
                        Mode => Error);

                  else
                     if Is_Absolute_Path (Filename) then
                        Pic_File := Create (Filename);
                     else
                        if File = VFS.No_File then
                           Pic_File := Create (Get_Current_Dir & Filename);
                        else
                           Pic_File := Create (Dir_Name (File).all & Filename);
                        end if;
                     end if;

                     if Is_Regular_File (Pic_File) then
                        Set    := Gtk_New;
                        Source := Gtk_New;
                        Set_Filename (Source, Full_Name (Pic_File).all);

                        Add_Source (Set, Source);
                        Add (Factory, Id, Set);

                        Free (Source);
                     else
                        Insert
                          (Kernel,
                           -"Error when creating stock icon " & Id
                           & (-". File not found: ")
                           & Full_Name (Pic_File).all);
                     end if;
                  end if;
               end;
            end if;

            Child := Child.Next;
         end loop;
      end Parse_Stock_Node;

      ---------------
      -- Add_Child --
      ---------------

      procedure Add_Child (Parent_Path : String; Current_Node : Node_Ptr) is
         Lang  : Custom_Language_Access;
      begin
         if Current_Node = null
           or else Current_Node.Tag = null
         then
            return;
         end if;

         if To_Lower (Current_Node.Tag.all) = "language" then
            --  ??? Lang is never freed
            Lang := new Language.Custom.Custom_Language;
            Initialize (Lang, Handler, Kernel, Current_Node);

         elsif Current_Node.Tag.all = "menu" then
            Parse_Menu_Node (Current_Node, Parent_Path);

         elsif To_Lower (Current_Node.Tag.all) = "submenu" then
            Parse_Submenu_Node (Current_Node, Parent_Path);

         elsif To_Lower (Current_Node.Tag.all) = "action" then
            Parse_Action_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "contextual" then
            Parse_Contextual_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "filter"
           or else To_Lower (Current_Node.Tag.all) = "filter_and"
           or else To_Lower (Current_Node.Tag.all) = "filter_or"
         then
            declare
               Name   : constant String :=
                 Get_Attribute (Current_Node, "name");
               Filter : Action_Filter;
            begin
               if Name = "" then
                  Insert
                    (Kernel,
                     -("<filter>, <filter_and> and <filter_or> tags must have"
                       & " a name attribute when defined outside of <action>"),
                     Mode => Error);
               else
                  Filter := Parse_Filter_Node (Current_Node);
                  Register_Filter (Kernel, Filter, Name);
               end if;
            end;

         elsif To_Lower (Current_Node.Tag.all) = "button" then
            Parse_Button_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "tool" then
            Parse_Tool_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "stock" then
            Parse_Stock_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "entry" then
            Parse_Entry_Node (Current_Node);

         end if;
      end Add_Child;

      N : Node_Ptr := Node;
   begin
      while N /= null loop
         Add_Child ("/", N);
         N := N.Next;
      end loop;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar_Class       : constant Class_Type := New_Class
        (Kernel, "Toolbar", "Interface to commands related to the toolbar.");
      Process_Class       : constant Class_Type := New_Class
        (Kernel, "Process", "Interface to expect-related commands");
   begin
      Custom_Module_ID := new Custom_Module_ID_Record;
      Register_Module
        (Module                  => Module_ID (Custom_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => "Custom",
         Contextual_Menu_Handler => Contextual_Handler'Access,
         Priority                => Low_Priority,
         Customization_Handler   => Customize'Access);

      Custom_Module_ID.Kernel := Kernel_Handle (Kernel);

      Register_Command
        (Kernel,
         Command => "spawn",
         Params  => "(command, regexp, on_match_action, on_exit_action)",
         Minimum_Args => 4,
         Maximum_Args => 4,
         Return_Value => "string",
         Description  =>
         -("Spawn specified command. If regexp is not-empty and"
           & " on_match_action is specified, launch on_match_action when"
           & " regexp is found in the process output. If on_exit_action is"
           & " specified, execute it when the process terminates."
           & " Return the ID of the spawned process."
           & ASCII.LF & ASCII.LF
           & "on_match_action is an action with the parameters:" & ASCII.LF
           & "  $1 = the ID of the process" & ASCII.LF
           & "  $2 = the string which matched the regexp" & ASCII.LF
           & "  $3 = the string since the last match" & ASCII.LF
           & ASCII.LF
           & "on_exit_action is an action with the parameters" & ASCII.LF
           & "  $1 = the ID of the process" & ASCII.LF
           & "  $2 = the exit status" & ASCII.LF
           & "  $3 = the entire output of the process" & ASCII.LF
           & "" & ASCII.LF),
         Class         => Process_Class,
         Static_Method => True,
         Handler       => Custom_Spawn_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "send",
         Params        => "(id, command, add_lf[=TRUE])",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Return_Value  => "(null)",
         Description   =>
         -("Send a line of text to process controlled by GPS."),
         Class         => Process_Class,
         Static_Method => True,
         Handler       => Custom_Spawn_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "interrupt",
         Params        => "(id)",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Return_Value  => "(null)",
         Description   =>
         -("Interrupt a process controlled by GPS."),
         Class         => Process_Class,
         Static_Method => True,
         Handler       => Custom_Spawn_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "kill",
         Params        => "(id)",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Return_Value  => "(null)",
         Description   =>
         -("Terminate a process controlled by GPS."),
         Class         => Process_Class,
         Static_Method => True,
         Handler       => Custom_Spawn_Handler'Access);

      Register_Command
        (Kernel,
         Command      => "expect",
         Params       => "(id, regexp, expect_action)",
         Minimum_Args => 3,
         Maximum_Args => 3,
         Return_Value => "(null)",
         Description  =>
         -("Listen to the output of specified process for regexp. When the"
           & " regexp matches the output, execute the associated action, and"
           & " stop listening for that specific regexp."
           & ASCII.LF & ASCII.LF
           & "expect_action is an action with the parameters:" & ASCII.LF
           & "  $1 = the ID of the process" & ASCII.LF
           & "  $2 = the string which matched the regexp" & ASCII.LF
           & "  $3 = the string since the last match" & ASCII.LF),
         Class         => Process_Class,
         Static_Method => True,
         Handler       => Custom_Spawn_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "entry_add",
         Params        => "(entry_label, choice_label, [action])",
         Minimum_Args  => 2,
         Maximum_Args  => 3,
         Return_Value  => "(null)",
         Description   =>
         -("Add a choice to specified entry, action will be executed whenever"
           & " this choice is selected"),
         Class         => Toolbar_Class,
         Static_Method => True,
         Handler       => Custom_Entry_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "entry_remove",
         Params        => "(entry_label, choice_label)",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Return_Value  => "(null)",
         Description   => -"Remove a choice from specified entry",
         Class         => Toolbar_Class,
         Static_Method => True,
         Handler       => Custom_Entry_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "entry_clear",
         Params        => "(entry_label)",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Return_Value  => "(null)",
         Description   => -"Remove all choices from specified entry",
         Class         => Toolbar_Class,
         Static_Method => True,
         Handler       => Custom_Entry_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "entry_get_text",
         Params        => "(entry_label)",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Return_Value  => "(string)",
         Description   => -"Return the current selection in specified entry",
         Class         => Toolbar_Class,
         Static_Method => True,
         Handler       => Custom_Entry_Handler'Access);

      Register_Command
        (Kernel,
         Command       => "entry_set_text",
         Params        => "(entry_label, text)",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Return_Value  => "(null)",
         Description   => -"Set the current selection in specified entry",
         Class         => Toolbar_Class,
         Static_Method => True,
         Handler       => Custom_Entry_Handler'Access);
   end Register_Module;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Action_Record) is
   begin
      Free (X.Command);
      Unchecked_Free (X.Pattern);
      Free (X.Unmatched_Output);
      Free (X.Processed_Output);
   end Free;

   procedure Free (X : in out Custom_Action_Access) is
   begin
      if X /= null then
         Free (X.all);
         Unchecked_Free (X);
      end if;
   end Free;

   procedure Free (X : in out Expect_Filter) is
   begin
      Unchecked_Free (X.Pattern);
   end Free;

   procedure Free (X : in out Entry_Action_Record) is
   begin
      Free (X.Label);
   end Free;

   procedure Free (X : in out GPS_Combo_Access) is
   begin
      if X /= null then
         Free (X.Label);
         Entry_List.Free (X.Entries);

         Unchecked_Free (X);
      end if;
   end Free;

   ----------
   -- Hash --
   ----------

   function Hash (K : Integer) return Header_Num is
   begin
      return Header_Num ((K mod Integer (Header_Num'Last)) + 1);
   end Hash;

end Custom_Module;
