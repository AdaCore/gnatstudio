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

with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with Basic_Types; use Basic_Types;

package body GPS.Kernel.Messages.Simple.Shell is

   Class         : constant String := "Message";
   Message_Class : Class_Type;

   type Message_Property_Record is new Instance_Property_Record with record
      Message : Message_Access;
   end record;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Message  : Message_Access);
   --  Set data in Instance to Message

   procedure Message_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Message commands

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Message  : Message_Access) is
   begin
      Set_Data (Instance, "message",
                Message_Property_Record'
                  (Message => Message));
   end Set_Data;

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
         declare
            Category : constant String := Nth_Arg (Data, 2);
            File     : constant Virtual_File :=
              Get_Data (Nth_Arg
                        (Data, 3, Get_File_Class (Kernel),
                   Default => No_Class_Instance, Allow_Null => False));
            Line     : constant Natural := Nth_Arg (Data, 4);
            Column   : constant Natural := Nth_Arg (Data, 5);
            Text     : constant String := Nth_Arg (Data, 6);
            Flags    : constant Integer := Nth_Arg (Data, 7);
            Message  : constant Simple_Message_Access :=
              Create_Simple_Message
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
      elsif Command = "list" then
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
        (Kernel, Constructor_Method, 6, 6, Message_Command_Handler'Access,
         Message_Class, False);

      Register_Command
        (Kernel, "list", 0, 2, Message_Command_Handler'Access,
         Message_Class, True);

      --  ??? To be implemented
--        Register_Command
--          (Kernel, "get_file", 0, 0, Message_Command_Handler'Access,
--           Message_Class);
--
--        Register_Command
--          (Kernel, "get_line", 0, 0, Message_Command_Handler'Access,
--           Message_Class);
--
--        Register_Command
--          (Kernel, "get_column", 0, 0, Message_Command_Handler'Access,
--           Message_Class);
--
--        Register_Command
--          (Kernel, "get_category", 0, 0, Message_Command_Handler'Access,
--           Message_Class);
--
--        Register_Command
--          (Kernel, "get_action", 0, 0, Message_Command_Handler'Access,
--           Message_Class);
--
--        Register_Command
--          (Kernel, "set_action", 3, 3, Message_Command_Handler'Access,
--           Message_Class);
--
   end Register_Commands;

end GPS.Kernel.Messages.Simple.Shell;
