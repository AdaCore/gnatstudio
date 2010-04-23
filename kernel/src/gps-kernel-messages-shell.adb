-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2010, AdaCore                       --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with GPS.Kernel.Actions; use GPS.Kernel.Actions;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;

with Commands;    use Commands;
with Basic_Types; use Basic_Types;
with GPS.Editors; use GPS.Editors;

with Commands.Interactive;

with GPS.Kernel.Messages.Markup; use GPS.Kernel.Messages.Markup;

package body GPS.Kernel.Messages.Shell is

   Class         : constant String := "Message";
   Message_Class : Class_Type;

   Action_Cst     : aliased constant String := "action";
   Subprogram_Cst : aliased constant String := "subprogram";
   Tooltip_Cst    : aliased constant String := "tooltip";
   Image_Cst      : aliased constant String := "image";
   Category_Cst   : aliased constant String := "category";
   Line_Cst       : aliased constant String := "line";
   Column_Cst     : aliased constant String := "column";
   File_Cst       : aliased constant String := "file";
   Text_Cst       : aliased constant String := "text";
   Flags_Cst      : aliased constant String := "flags";

   type Message_Property_Record is new Instance_Property_Record with record
      Message : Message_Access;
   end record;
   type Message_Property_Access is access all Message_Property_Record'Class;

   type Subprogram_Command_Record is new Root_Command with record
      Sub  : Subprogram_Type;
      Inst : Class_Instance;
   end record;
   type Subprogram_Command is access all Subprogram_Command_Record'Class;

   overriding function Execute
     (Command : access Subprogram_Command_Record) return Command_Return_Type;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Data
     (Instance : Class_Instance;
      Message  : Message_Access);
   --  Set data in Instance to Message

   function Get_Message (Instance : Class_Instance) return Message_Access;
   --  Return Message stored in Instance

   procedure Message_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Message commands

   procedure Accessors
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the simple Message commands which simply access the fields
   --  of a message or run parameterless commands

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Subprogram_Command_Record) return Command_Return_Type
   is
      C : Callback_Data'Class :=
        Create (Get_Script (Command.Sub.all), Arguments_Count => 1);
      Tmp : Boolean;
   begin
      Set_Nth_Arg (C, 1, Command.Inst);
      Tmp := Execute (Command.Sub, C);
      Free (C);

      if Tmp then
         return Success;
      else
         return Failure;
      end if;
   end Execute;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Message  : Message_Access) is
   begin
      Set_Data (Instance, Class,
                Message_Property_Record'
                  (Message => Message));
   end Set_Data;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Instance : Class_Instance) return Message_Access is
      Prop : Message_Property_Access;
   begin
      if Instance /= No_Class_Instance then
         Prop := Message_Property_Access
           (Instance_Property'(Get_Data (Instance, Class)));

         if Prop /= null then
            return Prop.Message;
         end if;
      end if;

      return null;
   end Get_Message;

   ---------------
   -- Accessors --
   ---------------

   procedure Accessors
     (Data : in out Callback_Data'Class; Command : String)
   is
      Message : constant Message_Access := Get_Message
        (Nth_Arg (Data, 1, Message_Class));
   begin
      if Command = "get_line" then
         Set_Return_Value (Data, Message.Get_Line);

      elsif Command = "get_column" then
         Set_Return_Value (Data, Integer (Message.Get_Column));

      elsif Command = "get_file" then
         Set_Return_Value
           (Data, Create_File (Get_Script (Data), Message.Get_File));

      elsif Command = "get_category" then
         Set_Return_Value (Data, Message.Get_Category);

      elsif Command = "get_text" then
         Set_Return_Value (Data, To_String (Message.Get_Text));

      elsif Command = "remove" then
         Message.Remove;
      end if;

   end Accessors;

   -----------------------------
   -- Message_Command_Handler --
   -----------------------------

   procedure Message_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Message_Inst : Class_Instance;
      Kernel       : constant Kernel_Handle := Get_Kernel (Data);
      Container    : constant Messages_Container_Access :=
        Get_Messages_Container (Kernel);
   begin
      if Command = Constructor_Method then
         Name_Parameters
           (Data,
            (1 => Category_Cst'Access,
             2 => File_Cst'Access,
             3 => Line_Cst'Access,
             4 => Column_Cst'Access,
             5 => Text_Cst'Access,
             6 => Flags_Cst'Access));

         declare
            Category : constant String := Nth_Arg (Data, 2);
            File     : constant Virtual_File :=
              Get_Data (Nth_Arg
                        (Data, 3, Get_File_Class (Kernel),
                   Default => No_Class_Instance, Allow_Null => False));
            Line     : constant Natural := Nth_Arg (Data, 4);
            Column   : constant Natural := Nth_Arg (Data, 5);
            Text     : constant String := Nth_Arg (Data, 6);
            Flags    : constant Integer := Nth_Arg (Data, 7, 0);
            Message  : constant Markup_Message_Access :=
              Create_Markup_Message
                (Container => Container,
                 Category  => Category,
                 File      => File,
                 Line      => Line,
                 Column    => Visible_Column_Type (Column),
                 Text      => Text,
                 Weight    => 0, --  ??? what is Weight?
                 Flags     => From_Int (Flags));
         begin
            Message_Inst := Nth_Arg (Data, 1, Message_Class);
            Set_Data (Message_Inst, Message_Access (Message));
         end;

      elsif Command = "set_action" then
         Name_Parameters
           (Data,
            (1 => Action_Cst'Access,
             2 => Image_Cst'Access,
             3 => Tooltip_Cst'Access));

         declare
            Action_Str   : constant String := Nth_Arg (Data, 2, "");
            Image_Str    : constant String := Nth_Arg (Data, 3);
            Tooltip_Str  : constant String := Nth_Arg (Data, 4, "");
            Action       : Action_Item;
            The_Action   : Action_Record_Access;
            Command      : Command_Access := null;

            use type Commands.Interactive.Interactive_Command_Access;
         begin
            if Action_Str /= "" then
               The_Action := Lookup_Action (Kernel, Action_Str);

               if The_Action = null
                 or else The_Action.Command = null
               then
                  Set_Error_Msg (Data, "Could not find action for "
                                 & Action_Str);
               else
                  Command := Command_Access (The_Action.Command);
               end if;
            end if;

            Action := new Line_Information_Record'
              (Text         => null,
               Tooltip_Text => new String'(Tooltip_Str),
               Image        => Render_Icon
                 (Widget   => Gtk_Widget (Get_Main_Window (Kernel)),
                  Stock_Id => Image_Str,
                  Size     => Icon_Size_Menu),
               Associated_Command => Command);

            Get_Message (Nth_Arg (Data, 1, Message_Class)).Set_Action (Action);
         end;

      elsif Command = "set_subprogram" then
         Name_Parameters
           (Data,
            (1 => Subprogram_Cst'Access,
             2 => Image_Cst'Access,
             3 => Tooltip_Cst'Access));

         declare
            Image_Str    : constant String := Nth_Arg (Data, 3);
            Tooltip_Str  : constant String := Nth_Arg (Data, 4, "");
            Command      : Subprogram_Command;
            Action       : Action_Item;
         begin
            Command := new Subprogram_Command_Record;
            Command.Sub  := Nth_Arg (Data, 2);
            Command.Inst := Nth_Arg (Data, 1, Message_Class);

            Action := new Line_Information_Record'
              (Text         => null,
               Tooltip_Text => new String'(Tooltip_Str),
               Image        => Render_Icon
                 (Widget   => Gtk_Widget (Get_Main_Window (Kernel)),
                  Stock_Id => Image_Str,
                  Size     => Icon_Size_Menu),
               Associated_Command => Command_Access (Command));

            Get_Message (Nth_Arg (Data, 1, Message_Class)).Set_Action (Action);
         end;

      elsif Command = "list" then
         Name_Parameters
           (Data,
            (1 => Category_Cst'Access,
             2 => File_Cst'Access));

         Set_Return_Value_As_List (Data);

         declare
            Cat       : constant String := Nth_Arg (Data, 1, "");
            File_Inst : constant Class_Instance :=
              Nth_Arg
                (Data, 2, Get_File_Class (Kernel),
                 Default => No_Class_Instance, Allow_Null => True);

            procedure Add_Messages_For_Category_File
              (C : Unbounded_String;
               F : Virtual_File);
            --  Add to the return list messages for category C and file F.

            procedure Add_Messages_For_Category (C : Unbounded_String);
            --  Add to the return list all the messages in C for the given
            --  file.

            ------------------------------------
            -- Add_Messages_For_Category_File --
            ------------------------------------

            procedure Add_Messages_For_Category_File
              (C : Unbounded_String;
               F : Virtual_File)
            is
               Messages : constant Message_Array :=
                 Get_Messages (Container, C, F);
            begin
               for J in Messages'Range loop
                  Message_Inst := New_Instance
                    (Get_Script (Data), Message_Class);
                  Set_Data (Message_Inst, Messages (J));
                  Set_Return_Value (Data, Message_Inst);
               end loop;
            end Add_Messages_For_Category_File;

            -------------------------------
            -- Add_Messages_For_Category --
            -------------------------------

            procedure Add_Messages_For_Category (C : Unbounded_String) is
            begin
               if File_Inst = No_Class_Instance then
                  declare
                     Files : constant Virtual_File_Array :=
                       Get_Files (Container, C);
                  begin
                     for J in Files'Range loop
                        Add_Messages_For_Category_File (C, Files (J));
                     end loop;
                  end;
               else
                  Add_Messages_For_Category_File (C, Get_Data (File_Inst));
               end if;
            end Add_Messages_For_Category;

         begin
            if Cat = "" then
               declare
                  Categories : constant Unbounded_String_Array :=
                    Get_Categories (Container);
               begin
                  for J in Categories'Range loop
                     Add_Messages_For_Category (Categories (J));
                  end loop;
               end;
            else
               Add_Messages_For_Category (To_Unbounded_String (Cat));
            end if;
         end;
      end if;
   end Message_Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Message_Class := New_Class (Kernel, Class);

      Register_Command
        (Kernel, Constructor_Method, 5, 6, Message_Command_Handler'Access,
         Message_Class, False);

      Register_Command
        (Kernel, "list", 0, 2, Message_Command_Handler'Access,
         Message_Class, True);

      Register_Command
        (Kernel, "get_file", 0, 0, Accessors'Access,
         Message_Class);

      Register_Command
        (Kernel, "get_line", 0, 0, Accessors'Access,
         Message_Class);

      Register_Command
        (Kernel, "get_text", 0, 0, Accessors'Access,
         Message_Class);

      Register_Command
        (Kernel, "get_column", 0, 0, Accessors'Access,
         Message_Class);

      Register_Command
        (Kernel, "get_category", 0, 0, Accessors'Access,
         Message_Class);

      Register_Command
        (Kernel, "remove", 0, 0, Accessors'Access,
         Message_Class);

      Register_Command
        (Kernel, "set_action", 1, 3, Message_Command_Handler'Access,
         Message_Class);

      Register_Command
        (Kernel, "set_subprogram", 1, 3, Message_Command_Handler'Access,
         Message_Class);

   end Register_Commands;

end GPS.Kernel.Messages.Shell;
