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
with Glib.Convert;              use Glib.Convert;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gtk.Enums;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with System.Assertions;         use System.Assertions;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Intl;                use Glide_Intl;

with Language;                  use Language;
with Language.Custom;           use Language.Custom;
with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Src_Info.Dummy;            use Src_Info.Dummy;

with Traces;                    use Traces;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Commands.Custom;           use Commands.Custom;


package body Custom_Module is

   Me : constant Debug_Handle := Create ("Custom_Module");

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Custom_File  : constant String := Get_Home_Dir (Kernel) & "custom";
      Local_Custom : constant String :=
        Normalize_Pathname (Get_Home_Dir (Kernel) & "customize/");
      Sys_Custom   : constant String :=
        Normalize_Pathname (Get_System_Dir (Kernel) & "share/gps/customize/");
      Handler      : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
      Button         : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
      Custom_Parsed : Boolean := False;
      Success       : Boolean;

      procedure Add_Child
        (Parent_Path  : String;
         Current_Node : Node_Ptr);
      --  Add a menuitem or submenu to the Parent_Path, according to
      --  what Current_Node contains.

      procedure Parse_Action_Node (Node : Node_Ptr);
      function Parse_Shell_Node
        (Node : Node_Ptr) return Custom_Command_Access;
      function Parse_External_Node
        (Node : Node_Ptr) return Custom_Command_Access;
      procedure Parse_Key_Node (Node : Node_Ptr);
      procedure Parse_Button_Node (Node : Node_Ptr);
      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : String);
      --  Parse the various nodes: <action>, <shell>, ...

      procedure Parse_Custom_Dir (Directory : String);
      --  Parse all custom files located in Directory

      ----------------------
      -- Parse_Shell_Node --
      ----------------------

      function Parse_Shell_Node
        (Node : Node_Ptr) return Custom_Command_Access
      is
         Lang    : constant String := Get_Attribute (Node, "lang");
         Script  : Scripting_Language;
         Command : Custom_Command_Access;
      begin
         if Lang = "" then
            Script := Lookup_Scripting_Language (Kernel, GPS_Shell_Name);
         else
            Script := Lookup_Scripting_Language (Kernel, Lang);
         end if;

         if Script = null then
            Insert (Kernel,
                    -"Invalid language specified for <shell> node: " & Lang,
                    Mode => Error);
            raise Assert_Failure;
         end if;

         Create (Command, Kernel_Handle (Kernel), Node.Value.all, Script);
         return Command;
      end Parse_Shell_Node;

      -------------------------
      -- Parse_External_Node --
      -------------------------

      function Parse_External_Node
        (Node : Node_Ptr) return Custom_Command_Access
      is
         Command : Custom_Command_Access;
      begin
         Create (Command, Kernel_Handle (Kernel), Node.Value.all, null);
         return Command;
      end Parse_External_Node;

      -----------------------
      -- Parse_Action_Node --
      -----------------------

      procedure Parse_Action_Node (Node : Node_Ptr) is
         Name    : constant String := Get_Attribute (Node, "name");
         Child   : Node_Ptr;
         Command, C : Custom_Command_Access;
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
            if To_Lower (Child.Tag.all) = "shell" then
               C := Parse_Shell_Node (Child);
               if Command = null then
                  Command := C;
               else
                  Add_Consequence_Action (Command, C);
               end if;

            elsif To_Lower (Child.Tag.all) = "external" then
               C := Parse_External_Node (Child);
               if Command = null then
                  Command := C;
               else
                  Add_Consequence_Action (Command, C);
               end if;

            elsif To_Lower (Child.Tag.all) = "context" then
               Free (Context);
               Context := new String'(Child.Value.all);
            elsif To_Lower (Child.Tag.all) = "description" then
               Free (Description);
               Description := new String'(Child.Value.all);
            else
               if Command /= null then
                  Destroy (Command_Access (Command));
               end if;

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

         Register_Action
           (Kernel,
            Name        => Name,
            Command     => Command,
            Description => Description.all,
            Context     => Context_A);
         Free (Description);
         Free (Context);
      end Parse_Action_Node;

      --------------------
      -- Parse_Key_Node --
      --------------------

      procedure Parse_Key_Node (Node : Node_Ptr) is
         Action : constant String := Get_Attribute (Node, "action");
      begin
         if Action = "" then
            Insert (Kernel, -"<key> nodes must have an action attribute",
                    Mode => Error);
            raise Assert_Failure;
         end if;

         if Node.Value = null then
            Insert (Kernel,
                    -"Invalid key binding for action " & Action,
                    Mode => Error);
            raise Assert_Failure;
         end if;

         if Node.Child /= null then
            Insert
              (Kernel, -"Invalid child node for <key> tag", Mode => Error);
            raise Assert_Failure;
         end if;

         Bind_Default_Key
           (Get_Key_Handler (Kernel),
            Action      => Action,
            Default_Key => Node.Value.all);
      end Parse_Key_Node;

      -----------------------
      -- Parse_Button_Node --
      -----------------------

      procedure Parse_Button_Node (Node : Node_Ptr) is
         Action : constant String := Get_Attribute (Node, "action");
         Child  : Node_Ptr;
         Title  : String_Access := new String'("");
         Pixmap : String_Access := new String'("");
         Image  : Gtk_Image;
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

         if Title.all = "" then
            if Pixmap.all /= ""
              and then Is_Regular_File (Pixmap.all)
            then
               Gtk_New (Image, Pixmap.all);
            end if;

            Command := Lookup_Action (Kernel, Action);
            if Command.Command /= null then
               Register_Button
                 (Kernel,
                  Locale_To_UTF8 (Title.all),
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

      procedure Parse_Menu_Node (Node : Node_Ptr; Parent_Path : String) is
         Action : constant String := Get_Attribute (Node, "action");
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
            Register_Menu (Kernel, Locale_To_UTF8 (Parent_Path), Item);
         else
            Command := Lookup_Action (Kernel, Action);
            if Command.Command /= null then
               Register_Menu
                 (Kernel,
                  Locale_To_UTF8 (Parent_Path),
                  Locale_To_UTF8 (Title.all),
                  "",
                  null,
                  Command_Access (Command.Command));
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
                       & " gps_convert_custom to convert to the new format."),
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

         elsif To_Lower (Current_Node.Tag.all) = "key" then
            Parse_Key_Node (Current_Node);

         elsif To_Lower (Current_Node.Tag.all) = "button" then
            Parse_Button_Node (Current_Node);
         end if;
      end Add_Child;

      ----------------------
      -- Parse_Custom_Dir --
      ----------------------

      procedure Parse_Custom_Dir (Directory : String) is
         File      : String (1 .. 1024);
         Last      : Natural;
         Dir       : Dir_Type;
         Node      : Node_Ptr;
         File_Node : Node_Ptr;

      begin
         if Is_Directory (Directory) then
            Open (Dir, Directory);
            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               declare
                  F : constant String := Directory & File (1 .. Last);
               begin
                  if Is_Regular_File (F) then
                     Trace (Me, "Loading " & F);
                     File_Node := Parse (F);

                     if File_Node = null then
                        Console.Insert
                          (Kernel, -"Syntax error in custom file " & F,
                           Mode => Error);
                     else
                        Node := File_Node.Child;
                        Custom_Parsed := True;

                        while Node /= null loop
                           Add_Child ("", Node);
                           Node := Node.Next;
                        end loop;

                        Free (File_Node);
                     end if;
                  end if;

               exception
                  when Assert_Failure =>
                     Console.Insert
                       (Kernel, -"Could not parse custom file " & F,
                        Mode => Error);
               end;
            end loop;

            Close (Dir);
         end if;

      exception
         when Directory_Error =>
            null;
      end Parse_Custom_Dir;

   begin
      if Is_Regular_File (Custom_File) then
         Rename_File (Custom_File, Local_Custom & "custom", Success);

         if Success then
            Button := Message_Dialog
              ((-"Moved file ") & Custom_File & ASCII.LF &
               (-"to directory ") & Local_Custom,
               Information, Button_OK,
               Justification => Gtk.Enums.Justify_Left);
         end if;
      end if;

      --  Load the system custom directory first, so that its contents can
      --  be overriden locally by the user
      Parse_Custom_Dir (Sys_Custom);
      Parse_Custom_Dir (Local_Custom);

      if Custom_Parsed then
         Register_Module
           (Module                  => Custom_Module_ID,
            Kernel                  => Kernel,
            Module_Name             => Custom_Module_Name,
            Priority                => Low_Priority,
            Contextual_Menu_Handler => null);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Register_Module;

end Custom_Module;
