------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with GNATCOLL.Scripts;   use GNATCOLL.Scripts;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Tools_Output;    use GPS.Kernel.Tools_Output;

package body Custom_Tools_Output is

   Tools_Output_Handler_Class_Name : constant String := "ToolsOutputHandler";

   On_Parse_Cst       : aliased constant String := "on_parse_stdout";
   On_EOF_Cst         : aliased constant String := "on_eof_stdout";
   On_Parse_Error_Cst : aliased constant String := "on_parse_stderr";
   On_EOF_Error_Cst   : aliased constant String := "on_eof_stderr";
   Priority_Cst       : aliased constant String := "priority";

   Handler_Constructor_Args : constant Cst_Argument_List :=
     (2  => On_Parse_Cst'Access,
      3  => On_EOF_Cst'Access,
      4  => On_Parse_Error_Cst'Access,
      5  => On_EOF_Error_Cst'Access);

   Register_Args : constant Cst_Argument_List :=
     (2  => Priority_Cst'Access);

   type Tools_Output_Property is new Instance_Property_Record with record
      Inst           : Class_Instance;
      On_Parse       : Subprogram_Type;
      On_EOF         : Subprogram_Type;
      On_Parse_Error : Subprogram_Type;
      On_EOF_Error   : Subprogram_Type;
   end record;

   type Tools_Output_Property_Access is access all Tools_Output_Property;

   procedure Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handle the custom timeout commands

   type Custom_Parser is new Tools_Output_Parser with record
      Object : Tools_Output_Property;
   end record;

   overriding procedure Parse_Standard_Output
     (Self : not null access Custom_Parser;
      Item : String);
   --  Parse a piece of an output passed as Item.

   overriding procedure Parse_Standard_Error
     (Self : not null access Custom_Parser;
      Item : String);
   --  Parse a piece of an stderr passed as Item.

   overriding procedure End_Of_Stream
     (Self : not null access Custom_Parser);
   --  Process end of streams (both output and error).

   type Custom_Parser_Fabric is
     new GPS.Kernel.Tools_Output.Output_Parser_Fabric with record
      Object : Tools_Output_Property;
   end record;

   overriding function Create
     (Self  : access Custom_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access;
   --  Create new parser to write on given Console.

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Custom_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      pragma Unreferenced (Self);
   begin
      return new Custom_Parser (Child);
   end Create;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self : not null access Custom_Parser)
   is
      C : Callback_Data'Class := Create
        (Get_Script (Self.Object.Inst), Arguments_Count => 0);
   begin
      if Self.Object.On_EOF /= null then
         declare
            Text : constant String := Execute (Self.Object.On_EOF, C);
         begin
            if Text /= "" then
               Self.Child.Parse_Standard_Output (Text);
            end if;
         end;
      end if;

      if Self.Object.On_EOF_Error /= null then
         declare
            Text : constant String := Execute (Self.Object.On_EOF_Error, C);
         begin
            if Text /= "" then
               Self.Child.Parse_Standard_Error (Text);
            end if;
         end;
      end if;

      Self.Child.End_Of_Stream;
      Free (C);
   end End_Of_Stream;

   -------------
   -- Handler --
   -------------

   procedure Handler
     (Data    : in out Callback_Data'Class; Command : String) is
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Handler_Constructor_Args);

         declare
            Inst            : constant Class_Instance :=
                                Nth_Arg
                                  (Data, 1,
                                   New_Class
                                     (Get_Repository (Data),
                                      Tools_Output_Handler_Class_Name));

            Property        : Tools_Output_Property;

         begin
            Property := (Inst           => Inst,
                         On_Parse       => Nth_Arg (Data, 2, null),
                         On_EOF         => Nth_Arg (Data, 3, null),
                         On_Parse_Error => Nth_Arg (Data, 2, null),
                         On_EOF_Error   => Nth_Arg (Data, 3, null));

            Set_Data (Inst, Tools_Output_Handler_Class_Name, Property);
         end;
      elsif Command = "register_tools_output_handler" then
         Name_Parameters (Data, Register_Args);

         declare
            Inst            : constant Class_Instance :=
                                Nth_Arg
                                  (Data, 1,
                                   New_Class
                                     (Get_Repository (Data),
                                      Tools_Output_Handler_Class_Name),
                                   Allow_Null => True);

            Priority        : constant Integer := Nth_Arg (Data, 2);

            Property        : Tools_Output_Property_Access;
         begin
            if Inst = No_Class_Instance then
               Set_Error_Msg
                 (Data, -"handler object must be initialized");
               return;
            end if;

            Property := Tools_Output_Property_Access
              (GNATCOLL.Scripts.Get_Data
                 (Inst, Tools_Output_Handler_Class_Name));

            Register_Output_Parser
              (new Custom_Parser_Fabric'(Object => Property.all),
               Parser_Priority (Priority));
         end;
      end if;
   end Handler;

   --------------------------
   -- Parse_Standard_Error --
   --------------------------

   overriding procedure Parse_Standard_Error
     (Self : not null access Custom_Parser;
      Item : String)
   is
      C : Callback_Data'Class := Create
        (Get_Script (Self.Object.Inst), Arguments_Count => 1);
   begin
      if Self.Object.On_Parse_Error /= null then
         Set_Nth_Arg (C, 1, Item);

         declare
            Text : constant String := Execute (Self.Object.On_Parse_Error, C);
         begin
            Self.Child.Parse_Standard_Error (Text);
         end;
      else
         Self.Child.Parse_Standard_Error (Item);
      end if;

      Free (C);
   end Parse_Standard_Error;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self : not null access Custom_Parser;
      Item : String)
   is
      C : Callback_Data'Class := Create
        (Get_Script (Self.Object.Inst), Arguments_Count => 1);
   begin
      if Self.Object.On_Parse /= null then
         Set_Nth_Arg (C, 1, Item);

         declare
            Text : constant String := Execute (Self.Object.On_Parse, C);
         begin
            Self.Child.Parse_Standard_Output (Text);
         end;
      else
         Self.Child.Parse_Standard_Output (Item);
      end if;

      Free (C);
   end Parse_Standard_Output;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Handler_Class : constant Class_Type :=
        New_Class (Kernel, Tools_Output_Handler_Class_Name);
   begin
      Register_Command
        (Kernel,
         Constructor_Method,
         Minimum_Args  => 1,
         Maximum_Args  => Handler_Constructor_Args'Last,
         Class         => Handler_Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel,
         "register_tools_output_handler",
         Minimum_Args  => 2,
         Maximum_Args  => Register_Args'Last,
         Class         => Handler_Class,
         Handler       => Handler'Access,
         Static_Method => True);
   end Register_Commands;

end Custom_Tools_Output;
