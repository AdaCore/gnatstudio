-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Gtk.Image;                 use Gtk.Image;
with Gtk.Toolbar;               use Gtk.Toolbar;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with System.Assertions;         use System.Assertions;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Intl;                use Glide_Intl;
with Projects;                  use Projects;

with Language;                  use Language;
with Language.Custom;           use Language.Custom;
with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Src_Info.Dummy;            use Src_Info.Dummy;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Commands.Custom;           use Commands.Custom;

package body Custom_Module is

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  Called when a new customization in parsed

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Kernel : access Kernel_Handle_Record'Class;
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
      procedure Parse_Button_Node (Node : Node_Ptr);
      procedure Parse_Tool_Node (Node : Node_Ptr);
      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : UTF8_String);
      --  Parse the various nodes: <action>, <shell>, ...

      ---------------------
      -- Parse_Tool_Node --
      ---------------------

      procedure Parse_Tool_Node (Node : Node_Ptr) is
         Name  : constant String := Get_Attribute (Node, "name");
         Pack  : constant String :=
           Get_Attribute (Node, "package", Projects.Ide_Package);
         Index : constant String := Get_Attribute (Node, "index", Name);
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
            if N.Tag.all = "default-switches" then
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
                          Project_Attribute => new String'("default_switches"),
                          Project_Index     => new String'(Index),
                          Default_Switches  => null));
      end Parse_Tool_Node;

      -----------------------
      -- Parse_Action_Node --
      -----------------------

      procedure Parse_Action_Node (Node : Node_Ptr) is
         Name    : constant String := Get_Attribute (Node, "name");
         Child   : Node_Ptr;
         Command : Custom_Command_Access;
         Description : String_Access := new String'("");
         Context     : String_Access;
         Context_A   : Action_Context;
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
            then
               --  Handled directly by Commands.Custom
               null;

            elsif To_Lower (Child.Tag.all) = "context" then
               Free (Context);
               Context := new String'(Child.Value.all);
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

         if Context /= null then
            Context_A := Lookup_Context (Kernel, Context.all);
            if Context_A = null then
               Insert
                 (Kernel,
                  -"Unknown action context " & Context.all,
                  Mode => Error);
               raise Assert_Failure;
            end if;
         end if;

         Create (Command, Kernel_Handle (Kernel), Node.Child,
                 Default_Output => Get_Attribute
                   (Node, "output", Console_Output));

         Register_Action
           (Kernel,
            Name        => Name,
            Command     => Command,
            Description => Description.all,
            Context     => Context_A);
         Free (Description);
         Free (Context);
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
         Command : Action_Record;

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

            if Command.Command /= null then
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
         Command : Action_Record;
      begin
         if Action = "" then
            Insert (Kernel, -"<menu> nodes must have an action attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

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

         if Title.all = "" then
            Gtk_New (Item);
            Register_Menu (Kernel, Parent_Path, Item);
         else
            Command := Lookup_Action (Kernel, Action);
            if Command.Command /= null then

               if Before /= "" then
                  Register_Menu
                    (Kernel,
                     Parent_Path,
                     Text        => Title.all,
                     Stock_Image => "",
                     Callback    => null,
                     Command     => Command_Access (Command.Command),
                     Ref_Item    => Before);

               elsif After /= "" then
                  Register_Menu
                    (Kernel,
                     Parent_Path,
                     Text        => Title.all,
                     Stock_Image => "",
                     Callback    => null,
                     Command     => Command_Access (Command.Command),
                     Ref_Item    => Before,
                     Add_Before  => False);

               else
                  Register_Menu
                    (Kernel,
                     Parent_Path,
                     Text        => Title.all,
                     Stock_Image => "",
                     Callback    => null,
                     Command     => Command_Access (Command.Command));
               end if;
            end if;
         end if;
         Free (Title);
      end Parse_Menu_Node;

      ---------------
      -- Add_Child --
      ---------------

      procedure Add_Child (Parent_Path : String; Current_Node : Node_Ptr) is
         Child : Node_Ptr;
         Lang  : Custom_Language_Access;
         Title : String_Access;
      begin
         if Current_Node = null
           or else Current_Node.Tag = null
         then
            return;
         end if;

         if To_Lower (Current_Node.Tag.all) = "language" then
            --  ??? Lang is never freed
            Lang := new Language.Custom.Custom_Language;
            Initialize (Lang, Current_Node);
            Register_Language
              (Handler, Get_Name (Lang), Language_Access (Lang));
            Add_Language_Info
              (Handler, Get_Name (Lang),
               LI                  => Dummy_Handler.all'Access,
               Default_Spec_Suffix => Get_Spec_Suffix (Lang),
               Default_Body_Suffix => Get_Body_Suffix (Lang));

         elsif To_Lower (Current_Node.Tag.all) = "submenu" then
            Child := Current_Node.Child;
            Title := new String'("");
            while Child /= null loop
               if To_Lower (Child.Tag.all) = "title" then
                  Free (Title);
                  Title := new String'(Child.Value.all);
               elsif To_Lower (Child.Tag.all) = "submenu" then
                  Add_Child (Parent_Path & '/' & Title.all, Child);
               elsif To_Lower (Child.Tag.all) = "menu" then
                  Parse_Menu_Node (Child, Parent_Path & '/' & Title.all);
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

         elsif To_Lower (Current_Node.Tag.all) = "action" then
            Parse_Action_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "button" then
            Parse_Button_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "tool" then
            Parse_Tool_Node (Current_Node);

         end if;
      end Add_Child;

      N : Node_Ptr := Node;
   begin
      while N /= null loop
         Add_Child ("", N);
         N := N.Next;
      end loop;
   end Customize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Custom_Module_ID   : Glide_Kernel.Module_ID;
   begin
      Register_Module
        (Module                  => Custom_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => "Custom",
         Priority                => Low_Priority,
         Customization_Handler   => Customize'Access);
   end Register_Module;

end Custom_Module;
