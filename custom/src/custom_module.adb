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
with Glide_Intl;                use Glide_Intl;

with Language;                  use Language;
with Language.Custom;           use Language.Custom;
with Language_Handlers;         use Language_Handlers;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Src_Info.Dummy;            use Src_Info.Dummy;

with Traces;                    use Traces;
with Commands;                  use Commands;
with Commands.Custom;           use Commands.Custom;

with Ada.Unchecked_Deallocation;

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

      procedure Parse_Custom_Dir (Directory : String);
      --  Parse all custom files located in Directory

      procedure Add_Child
        (Parent_Path  : String;
         Current_Node : Node_Ptr)
      is
         Current_Child : Node_Ptr;
         Current_Child_Child : Node_Ptr;
         Command        : Command_Access := null;
         Current_Title  : String_Access;
         Current_Pixmap : String_Access;
         Item           : Gtk_Menu_Item;
         Image          : Gtk_Image;
         Menuitem       : Boolean := False;
         Lang           : Custom_Language_Access;

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
            Current_Child := Current_Node.Child;

            if To_Lower (Current_Child.Tag.all) = "title" then
               Current_Title := new String'
                 (Parent_Path & "/" & Current_Child.Value.all);

               Current_Child_Child := Current_Child.Next;

               while Current_Child_Child /= null loop
                  Add_Child (Current_Title.all, Current_Child_Child);
                  Current_Child_Child := Current_Child_Child.Next;
               end loop;
            end if;

         elsif To_Lower (Current_Node.Tag.all) = "menu_item"
           or else To_Lower (Current_Node.Tag.all) = "toolbar_item"
         then
            if To_Lower (Current_Node.Tag.all) = "menu_item" then
               Menuitem := True;
            end if;

            Current_Child := Current_Node.Child;

            while Current_Child /= null loop
               if To_Lower (Current_Child.Tag.all) = "title" then
                  if Current_Title /= null then
                     Free (Current_Title);
                  end if;

                  Current_Title := new String'(Current_Child.Value.all);
               end if;

               if To_Lower (Current_Child.Tag.all) = "pixmap" then
                  if Current_Pixmap /= null then
                     Free (Current_Pixmap);
                  end if;

                  Current_Pixmap := new String'(Current_Child.Value.all);
               end if;

               if To_Lower (Current_Child.Tag.all) = "action"
                 or else To_Lower (Current_Child.Tag.all) = "gps_action"
               then
                  declare
                     Args : Argument_List_Access :=
                       Argument_String_To_List (Current_Child.Value.all);
                     A : Argument_List_Access;
                     C : Custom_Command_Access;
                     GPS_Command : Boolean := True;

                     procedure Free_Array is new Ada.Unchecked_Deallocation
                       (Object => String_List, Name => String_List_Access);
                  begin
                     if Args'Length > 1 then
                        A := new Argument_List'
                          (Args (Args'First + 1 .. Args'Last));
                     end if;

                     if To_Lower (Current_Child.Tag.all) = "action" then
                        GPS_Command := False;
                     end if;

                     --  ??? This command is never freed anywhere
                     --  ??? Neither is A
                     Create
                       (C,
                        Kernel_Handle (Kernel),
                        Args (Args'First).all,
                        A,
                        GPS_Command);

                     Free (Args (Args'First));
                     Free_Array (Args);

                     if Command = null then
                        Command := Command_Access (C);
                     else
                        Add_Consequence_Action (Command, Command_Access (C));
                     end if;
                  end;
               end if;

               Current_Child := Current_Child.Next;
            end loop;

            if Current_Title /= null and then Current_Title.all /= "" then
               if Menuitem then
                  Register_Menu
                    (Kernel,
                     Locale_To_UTF8 (Parent_Path),
                     Locale_To_UTF8 (Current_Title.all),
                     "",
                     null,
                     Command);
               else
                  if Current_Pixmap /= null
                    and then Is_Regular_File (Current_Pixmap.all)
                  then
                     Gtk_New (Image, Current_Pixmap.all);
                  end if;

                  Register_Button
                    (Kernel,
                     Locale_To_UTF8 (Current_Title.all),
                     Command,
                     Image);
               end if;

            elsif Menuitem then
               Gtk_New (Item);
               Register_Menu (Kernel, Locale_To_UTF8 (Parent_Path), Item);
            else
               Append_Space (Get_Toolbar (Kernel));
            end if;
         end if;

         Free (Current_Title);
         Free (Current_Pixmap);
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

      Parse_Custom_Dir (Local_Custom);
      Parse_Custom_Dir (Sys_Custom);

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
