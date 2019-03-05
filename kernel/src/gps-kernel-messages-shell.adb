------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Basic_Types;                    use Basic_Types;
with Commands;                       use Commands;
with GNATCOLL.Scripts;               use GNATCOLL.Scripts;
with GPS.Editors.Line_Information;   use GPS.Editors.Line_Information;
with GPS.Editors;                    use GPS.Editors;
with GPS.Kernel.Actions;             use GPS.Kernel.Actions;
with GPS.Kernel.Messages.Markup;     use GPS.Kernel.Messages.Markup;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Scripts;             use GPS.Kernel.Scripts;
with GPS.Kernel.Style_Manager.Shell; use GPS.Kernel.Style_Manager.Shell;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Widget;                     use Gtk.Widget;

package body GPS.Kernel.Messages.Shell is

   Class         : constant String := "Message";
   Message_Class : Class_Type;

   Action_Cst        : aliased constant String := "action";
   Subprogram_Cst    : aliased constant String := "subprogram";
   Tooltip_Cst       : aliased constant String := "tooltip";
   Image_Cst         : aliased constant String := "image";
   Category_Cst      : aliased constant String := "category";
   File_Cst          : aliased constant String := "file";
   Hint_Cst          : aliased constant String := "hint";

   type Message_Property_Record is new Instance_Property_Record with record
      Message : Message_Reference;
   end record;
   type Message_Property_Access is access all Message_Property_Record'Class;

   type Subprogram_Command_Record is new Root_Command with record
      Sub  : Subprogram_Type;
      Inst : Class_Instance;
   end record;
   type Subprogram_Command is access all Subprogram_Command_Record'Class;

   overriding function Execute
     (Command : access Subprogram_Command_Record) return Command_Return_Type;

   overriding procedure Primitive_Free (X : in out Subprogram_Command_Record);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Message (Instance : Class_Instance) return Message_Access;
   --  Return Message stored in Instance.
   --  Return null if the message is no longer valid.

   procedure Message_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Message commands

   procedure Accessors
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the simple Message commands which simply access the fields
   --  of a message or run parameterless commands

   -----------------------------
   -- Create_Message_Instance --
   -----------------------------

   function Create_Message_Instance
     (Script  : not null access Scripting_Language_Record'Class;
      Message : not null Message_Access) return Class_Instance is
   begin
      return Result : constant Class_Instance :=
        New_Instance (Script, Message_Class)
      do
         Set_Data (Result, Message);
      end return;
   end Create_Message_Instance;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : GNATCOLL.Scripts.Class_Instance)
      return GPS.Kernel.Messages.Message_Access
   is
      Data : Instance_Property;

   begin
      if Instance /= No_Class_Instance then
         Data := Get_Data (Instance, Class);
      end if;

      if Data = null then
         return null;

      else
         return Message_Property_Record (Data.all).Message.Message;
      end if;
   end Get_Data;

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

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
      (X : in out Subprogram_Command_Record) is
   begin
      Free (X.Sub);
      Root_Command (X).Primitive_Free;
   end Primitive_Free;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Message  : Message_Access) is
   begin
      Set_Data
        (Instance,
         Class,
         Message_Property_Record'(Message => Create (Message)));
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
            return Prop.Message.Message;
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
      Inst    : constant Class_Instance := Nth_Arg (Data, 1, Message_Class);
      Message : constant Message_Access := Get_Message (Inst);
   begin
      if Message = null then
         return;
      end if;

      if Command = "get_line" then
         Set_Return_Value (Data, Message.Get_Line);

      elsif Command = "get_column" then
         Set_Return_Value (Data, Integer (Message.Get_Column));

      elsif Command = "get_file" then
         Set_Return_Value
           (Data, Create_File (Get_Script (Data), Message.Get_File));

      elsif Command = "get_mark" then
         Set_Return_Value
           (Data, Message.Get_Editor_Mark.Create_Instance (Get_Script (Data)));

      elsif Command = "get_category" then
         Set_Return_Value (Data, Message.Get_Category);

      elsif Command = "get_flags" then
         Set_Return_Value (Data, To_Int (Message.Get_Flags));

      elsif Command = "get_text" then
         Set_Return_Value (Data, To_String (Message.Get_Text));

      elsif Command = "remove" then
         Message.Remove;

      elsif Command = "set_style" then
         declare
            Length     : Integer := Nth_Arg (Data, 3, -1);
         begin
            if Length < 0 then
               --  Reuse the previous length of the message
               Length := Integer (Message.Get_Highlighting_Length);
            end if;

            Message.Set_Highlighting
              (Get_Style (Nth_Arg (Data, 2)), Highlight_Length (Length));
         end;
      elsif Command = "execute_action" then
         declare
            Action  : constant Action_Item := Message.Get_Action;
            Success : Command_Return_Type;
            pragma Unreferenced (Success);
         begin
            if Action /= null
              and then Action.Associated_Command /= null
            then
               Success := Execute (Action.Associated_Command);
            end if;
         end;
      end if;
   end Accessors;

   -----------------------------
   -- Message_Command_Handler --
   -----------------------------

   procedure Message_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Message_Inst : Class_Instance;
      Message      : Message_Access;
      Kernel       : constant Kernel_Handle := Get_Kernel (Data);
      Container    : constant Messages_Container_Access :=
        Get_Messages_Container (Kernel);

   begin
      if Command = Constructor_Method then

         declare
            Category   : constant String := Nth_Arg (Data, 2);
            File       : constant Virtual_File :=
              Get_Data (Nth_Arg
                        (Data, 3, Get_File_Class (Kernel),
                         Default => No_Class_Instance, Allow_Null => False));
            Line       : constant Natural := Nth_Arg (Data, 4);
            Column     : constant Natural := Nth_Arg (Data, 5);
            Text       : constant String := Nth_Arg (Data, 6);
            Flags      : constant Message_Flags :=
              (Editor_Side => Nth_Arg (Data, 7, True),
               Editor_Line => False,
               Locations   => Nth_Arg (Data, 8, True));
            Auto_First : constant Boolean := Nth_Arg (Data, 9, True);
            Importance : constant Natural := Nth_Arg (Data, 10, 1);
            Message    : constant Markup_Message_Access :=
              Create_Markup_Message
                (Container                => Container,
                 Category                 => Category,
                 File                     => File,
                 Line                     => Line,
                 Column                   => Visible_Column_Type (Column),
                 Text                     => Text,
                 Importance               =>
                   Message_Importance_Type'Val (Importance),
                 Flags                    => Flags,
                 Allow_Auto_Jump_To_First => Auto_First);

         begin
            Message_Inst := Nth_Arg (Data, 1, Message_Class);
            Set_Data (Message_Inst, Message_Access (Message));
         end;

      elsif Command = "create_nested_message" then
         declare
            Parent  : constant Message_Access :=
              Get_Message (Nth_Arg (Data, 1, Message_Class));
            File    : constant Virtual_File :=
              Get_Data (Nth_Arg
                        (Data, 2, Get_File_Class (Kernel),
                         Default => No_Class_Instance, Allow_Null => False));
            Line    : constant Natural := Nth_Arg (Data, 3);
            Column  : constant Natural := Nth_Arg (Data, 4);
            Text    : constant String := Nth_Arg (Data, 5);
            Message : constant Simple_Message_Access :=
              Create_Simple_Message
                (Parent => Parent,
                 File   => File,
                 Line   => Line,
                 Column => GNATCOLL.Xref.Visible_Column (Column),
                 Text   => Text,
                 Flags  => Parent.Get_Flags);

         begin
            Data.Set_Return_Value
              (Create_Message_Instance
                 (Data.Get_Script, Message_Access (Message)));
         end;

      elsif Command = "set_action" then
         Name_Parameters
           (Data,
            (1 => Action_Cst'Access,
             2 => Image_Cst'Access,
             3 => Tooltip_Cst'Access));

         declare
            Action_Str  : constant String := Nth_Arg (Data, 2, "");
            Image_Str   : constant String := Nth_Arg (Data, 3);
            Tooltip_Str : constant String := Nth_Arg (Data, 4, "");
            Action      : Action_Item;
            The_Action  : Action_Access;
            Command     : Command_Access := null;

         begin
            Message := Get_Message (Nth_Arg (Data, 1, Message_Class));

            if Message = null then
               return;
            end if;

            if Action_Str /= "" then
               The_Action := Lookup_Action (Kernel, Action_Str);

               if The_Action = null then
                  Set_Error_Msg (Data, "Could not find action for "
                                 & Action_Str);
               else
                  Command := Command_Access (Get_Command (The_Action));
               end if;
            end if;

            Action := new Line_Information_Record'
              (Text               => Null_Unbounded_String,
               Tooltip_Text       => To_Unbounded_String (Tooltip_Str),
               Image              => To_Unbounded_String (Image_Str),
               Message            => <>,
               Associated_Command => Command);

            Message.Set_Action (Action);
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
            Message := Get_Message (Nth_Arg (Data, 1, Message_Class));
            if Message = null then
               return;
            end if;

            Command := new Subprogram_Command_Record;
            Command.Sub  := Nth_Arg (Data, 2);
            Command.Inst := Nth_Arg (Data, 1, Message_Class);

            Action := new Line_Information_Record'
              (Text               => Null_Unbounded_String,
               Tooltip_Text       => To_Unbounded_String (Tooltip_Str),
               Image              => To_Unbounded_String (Image_Str),
               Message            => <>,
               Associated_Command => Command_Access (Command));

            Message.Set_Action (Action);
         end;
      elsif Command = "cancel_subprogram" then
         Message := Get_Message (Nth_Arg (Data, 1, Message_Class));
         Message.Cancel_Action;

      elsif Command = "set_sort_order_hint" then
         Name_Parameters
           (Data,
            (1 => Category_Cst'Access,
             2 => Hint_Cst'Access));

         declare
            Category : constant String := Nth_Arg (Data, 1);
            Hint     : constant Sort_Order_Hint :=
              Sort_Order_Hint'Value (Nth_Arg (Data, 2));

         begin
            Container.Set_Sort_Order_Hint (Category, Hint);
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
                  Set_Return_Value
                    (Data,
                     Create_Message_Instance
                       (Get_Script (Data), Messages (J)));
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

      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Params =>
           (Param ("category"),
            Param ("file"),
            Param ("line"),
            Param ("column"),
            Param ("text"),
            Param ("show_on_editor_side", Optional => True),
            Param ("show_in_locations", Optional => True),
            Param ("allow_auto_jump_to_first",  Optional => True),
            Param ("importance",  Optional => True)),
         Handler => Message_Command_Handler'Access,
         Class => Message_Class);

      Kernel.Scripts.Register_Command
        (Command => "create_nested_message",
         Params  =>
           (Param ("file"),
            Param ("line"),
            Param ("column"),
            Param ("text")),
         Handler => Message_Command_Handler'Access,
         Class   => Message_Class);

      Register_Command
        (Kernel, Destructor_Method, 0, 0, Accessors'Access,
         Message_Class, False);

      Register_Command
        (Kernel, "list", 0, 2, Message_Command_Handler'Access,
         Message_Class, True);

      Register_Command
        (Kernel, "set_sort_order_hint", 2, 2, Message_Command_Handler'Access,
         Message_Class, True);

      Register_Command
        (Kernel, "get_file", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "get_mark", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "get_line", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "get_text", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "get_column", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "get_category", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "get_flags", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "remove", 0, 0, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "set_action", 1, 3, Message_Command_Handler'Access,
         Message_Class);

      Register_Command
        (Kernel, "set_subprogram", 1, 3, Message_Command_Handler'Access,
         Message_Class);

      Kernel.Scripts.Register_Command
        (Command       => "cancel_subprogram",
         Handler       => Message_Command_Handler'Access,
         Class         => Message_Class);

      Register_Command
        (Kernel, "set_style", 1, 2, Accessors'Access, Message_Class);

      Register_Command
        (Kernel, "execute_action", 0, 0, Accessors'Access, Message_Class);

   end Register_Commands;

end GPS.Kernel.Messages.Shell;
