------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018-2019, AdaCore                   --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Messages;       use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Shell; use GPS.Kernel.Messages.Shell;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.Xref;             use GNATCOLL.Xref;
with GNAThub.Filters_Views;     use GNAThub.Filters_Views;
with GNAThub.Module;            use GNAThub.Module;
with GNAThub.Messages;          use GNAThub.Messages;
with GNAThub.Loader.External;

package body GNAThub.Module.Shell is

   Analysis_Class_Name : constant String := "Analysis";
   Analysis_Class      : Class_Type;

   Analysis_Tool_Class_Name : constant String := "AnalysisTool";
   Analysis_Tool_Class      : Class_Type;

   type Tool_Property_Record is new Instance_Property_Record with record
      Tool : Tool_Access;
   end record;
   type Tool_Property_Access is access all Tool_Property_Record'Class;
   --  Used to map tools with their associated Python instances

   procedure Set_Data
     (Instance : Class_Instance;
      Tool     : Tool_Access);

   function Get_Tool (Instance : Class_Instance) return Tool_Access;

   procedure Analysis_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String);

   procedure Analysis_Tool_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String);

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Tool     : Tool_Access) is
   begin
      Set_Data
        (Instance,
         Analysis_Tool_Class_Name,
         Tool_Property_Record'(Tool => (Tool)));
   end Set_Data;

   --------------
   -- Get_Tool --
   --------------

   function Get_Tool (Instance : Class_Instance) return Tool_Access is
      Prop : Tool_Property_Access;
   begin
      if Instance /= No_Class_Instance then
         Prop := Tool_Property_Access
           (Instance_Property'(Get_Data (Instance, Analysis_Tool_Class_Name)));

         if Prop /= null then
            return Prop.Tool;
         end if;
      end if;

      return null;
   end Get_Tool;

   ------------------------------
   -- Analysis_Commands_Handler --
   ------------------------------

   procedure Analysis_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
   begin
      if Command = "display_report" then
         declare
            Tool_Inst : constant Class_Instance := Data.Nth_Arg
              (1, Allow_Null => True);
            Tool       : constant Tool_Access := Get_Tool (Tool_Inst);
         begin
            GNAThub.Module.Module.Display_Data;

            if Tool /= null then
               --  If a tool is specified, select only this tool in the Filters
               --  view.

               for Registered_Tool of GNAThub.Module.Module.Tools loop
                  Set_Tool_Selection
                    (Kernel   => GNAThub.Module.Module.Kernel,
                     Tool     => Registered_Tool,
                     Selected => Registered_Tool.Name = Tool.Name);
               end loop;
            end if;
         end;
      end if;
   end Analysis_Commands_Handler;

   -----------------------------------
   -- Analysis_Tool_Commands_Handler --
   -----------------------------------

   procedure Analysis_Tool_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      GNAThub_Module : GNAThub_Module_Id renames GNAThub.Module.Module;
      Tool_Inst      : constant Class_Instance := Data.Nth_Arg (1);
      Kernel         : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = Constructor_Method then
         declare
            Name : constant Unbounded_String := Data.Nth_Arg (2);
            Tool : constant Tool_Access := GNAThub_Module.Get_Or_Create_Tool
              (Name);
         begin
            Set_Data (Tool_Inst, Tool);
         end;

      elsif Command = "add_rule" then
         declare
            Tool       : constant Tool_Access := Get_Tool (Tool_Inst);
            Name       : constant Unbounded_String := Data.Nth_Arg (2);
            Identifier : constant Unbounded_String := Data.Nth_Arg (3);
            Rule       : Rule_Access with Unreferenced;
         begin

            Rule := GNAThub_Module.Get_Or_Create_Rule
              (Tool       => Tool,
               Name       => Name,
               Identifier => Identifier);
         end;

      elsif Command = "create_message" then
         declare
            Tool       : constant Tool_Access := Get_Tool (Tool_Inst);
            Container  : constant Messages_Container_Access :=
              GNAThub_Module.Kernel.Get_Messages_Container;
            Category   : constant String := Nth_Arg (Data, 2);
            File       : constant Virtual_File :=
              Get_Data (Nth_Arg
                        (Data, 3, Get_File_Class (Kernel),
                         Default => No_Class_Instance, Allow_Null => False));
            Line       : constant Natural := Nth_Arg (Data, 4);
            Column     : constant Natural := Nth_Arg (Data, 5);
            Text       : constant Unbounded_String := Nth_Arg (Data, 6);
            Importance : constant Natural := Nth_Arg (Data, 7);
            Rule_ID    : constant Unbounded_String := Data.Nth_Arg (8);
            Rule       : constant Rule_Access :=
              GNAThub_Module.Get_Or_Create_Rule
                (Tool       => Tool,
                 Name       => To_Unbounded_String ("unknown"),
                 Identifier => Rule_ID);
            Message    : constant GNAThub_Message_Access :=
              new GNAThub_Message;
         begin

            GNAThub.Messages.Initialize
              (Self          => Message,
               Container     => Container,
               Severity      => GNAThub_Module.Get_Severity
                 (Message_Importance_Type'Val (Importance)),
               Rule          => Rule,
               Text          => Text,
               File          => File,
               Line          => Line,
               Column        => Visible_Column (Column),
               Category      => Category);

            GNAThub_Module.Ext_Loader.all.Add_External_Message (Message);
            Data.Set_Return_Value
              (Create_Message_Instance
                 (Data.Get_Script, Message_Access (Message)));
         end;
      end if;
   end Analysis_Tool_Commands_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Analysis_Class := Kernel.Scripts.New_Class (Analysis_Class_Name);
      Analysis_Tool_Class := Kernel.Scripts.New_Class
        (Analysis_Tool_Class_Name);

      Kernel.Scripts.Register_Command
        (Command       => "display_report",
         Params        => (1 => Param ("tool", Optional => True)),
         Handler       => Analysis_Commands_Handler'Access,
         Class         => Analysis_Class,
         Static_Method => True);

      Kernel.Scripts.Register_Command
        (Command       => Constructor_Method,
         Handler       => Analysis_Tool_Commands_Handler'Access,
         Class         => Analysis_Tool_Class,
         Params        => (1 => Param ("name")));

      Kernel.Scripts.Register_Command
        (Command       => "add_rule",
         Handler       => Analysis_Tool_Commands_Handler'Access,
         Class         => Analysis_Tool_Class,
         Params        => (1 => Param ("name"),
                           2 => Param ("id")));

      Kernel.Scripts.Register_Command
        (Command       => "create_message",
         Handler       => Analysis_Tool_Commands_Handler'Access,
         Class         => Analysis_Tool_Class,
         Params        => (1 => Param ("category"),
                           2 => Param ("file"),
                           3 => Param ("line"),
                           4 => Param ("column"),
                           5 => Param ("text"),
                           6 => Param ("importance"),
                           7 => Param ("rule_id")));
   end Register_Commands;

end GNAThub.Module.Shell;
