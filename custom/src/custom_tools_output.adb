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

with GNATCOLL.Any_Types;         use GNATCOLL.Any_Types;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;

with Commands; use Commands;

with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Tools_Output;           use GPS.Tools_Output;
with String_List_Utils;

package body Custom_Tools_Output is

   Tools_Output_Handler_Class_Name : constant String := "OutputParserWrapper";

   On_Stdout_Cst : aliased constant String := "on_stdout";
   On_Exit_Cst   : aliased constant String := "on_exit";
   On_Stderr_Cst : aliased constant String := "on_stderr";
   Text_Cst      : aliased constant String := "text";
   Status_Cst    : aliased constant String := "status";

   On_Text_Params : constant Cst_Argument_List := (1 => Text_Cst'Access);
   On_Exit_Params : constant Cst_Argument_List := (1 => Status_Cst'Access);

   type Tools_Output_Property is new Instance_Property_Record with record
      Child : Tools_Output_Parser_Access;
   end record;

   type Tools_Output_Property_Access is access all Tools_Output_Property;

   procedure Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handle the custom output parser commands

   type Custom_Parser is new Tools_Output_Parser with record
      Inst : Class_Instance;
   end record;

   overriding procedure Parse_Standard_Output
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : Command_Access);
   --  Parse a piece of an output passed as Item.

   overriding procedure Parse_Standard_Error
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : Command_Access);
   --  Parse a piece of an stderr passed as Item.

   overriding procedure End_Of_Stream
     (Self    : not null access Custom_Parser;
      Status  : Integer;
      Command : Command_Access);
   --  Process end of streams (both output and error).

   type Python_Parser_Fabric is new External_Parser_Fabric with record
      Kernel : Kernel_Handle;
   end record;

   overriding procedure Create_External_Parsers
     (Self        : access Python_Parser_Fabric;
      Parser_List : in out String_List_Utils.String_List.List_Node;
      Child       : in out Tools_Output_Parser_Access;
      Found       : out Boolean);

   -----------------------------
   -- Create_External_Parsers --
   -----------------------------

   overriding procedure Create_External_Parsers
     (Self        : access Python_Parser_Fabric;
      Parser_List : in out String_List_Utils.String_List.List_Node;
      Child       : in out Tools_Output_Parser_Access;
      Found       : out Boolean)
   is
      use String_List_Utils.String_List;

      Create_Parser : constant String := "tool_output.create_parser";

      Inst   : Class_Instance;
      Script : constant Scripting_Language :=
        Lookup_Scripting_Language (Get_Scripts (Self.Kernel), "Python");
   begin
      --  Try to create new python parser
      declare
         Args : Callback_Data'Class :=
           Create (Script, 1 + Boolean'Pos (Child /= null));
      begin
         Set_Nth_Arg (Args, 1, Data (Parser_List));

         if Child /= null then
            --  Create wrapper around Child and pass to python function
            declare
               Class    : constant Class_Type :=
                 New_Class (Self.Kernel, Tools_Output_Handler_Class_Name);
               Instance : constant Class_Instance :=
                 New_Instance (Script, Class);
               Property : constant Tools_Output_Property := (Child => Child);
            begin
               Set_Data (Instance, Tools_Output_Handler_Class_Name, Property);
               Set_Nth_Arg (Args, 2, Instance);
            end;
         end if;

         Execute_Command (Args, Create_Parser);

         Inst := Args.Return_Value;
      end;

      if Inst = No_Class_Instance then
         Found := False;
         return;
      end if;

      Parser_List := Next (Parser_List);

      while Parser_List /= Null_Node loop
         declare
            Args : Callback_Data'Class := Create (Script, 2);
         begin
            Set_Nth_Arg (Args, 1, Data (Parser_List));
            Set_Nth_Arg (Args, 2, Inst);

            Execute_Command (Args, Create_Parser);

            exit when Args.Return_Value = No_Class_Instance;

            Inst := Args.Return_Value;
            Parser_List := Next (Parser_List);
         end;
      end loop;

      Found := True;
      Child := new Custom_Parser'(Child => null, Inst => Inst);
   end Create_External_Parsers;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Custom_Parser;
      Status  : Integer;
      Command : Command_Access)
   is
      pragma Unreferenced (Command);
      Args : Callback_Data'Class := Create
        (Get_Script (Self.Inst), Arguments_Count => 1);
      Proc : Subprogram_Type;
   begin
      Proc := Get_Method (Self.Inst, On_Exit_Cst);
      Set_Nth_Arg (Args, 1, Status);

      declare
         Ignore : constant Any_Type := Proc.Execute (Args);
         pragma Unreferenced (Ignore);
      begin
         Free (Proc);
         Free (Args);
      end;
   end End_Of_Stream;

   -------------
   -- Handler --
   -------------

   procedure Handler
     (Data    : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Class  : constant Class_Type := New_Class
        (Kernel, Tools_Output_Handler_Class_Name);
   begin
      if Command = On_Stdout_Cst then
         Name_Parameters (Data, On_Text_Params);

         declare
            Inst     : constant Class_Instance := Nth_Arg (Data, 1, Class);
            Text     : constant String := Nth_Arg (Data, 2);
            Property : constant Tools_Output_Property_Access :=
              Tools_Output_Property_Access
                (Get_Data (Inst, Tools_Output_Handler_Class_Name));
         begin
            Property.Child.Parse_Standard_Output (Text, null);
         end;
      elsif Command = On_Stderr_Cst then
         Name_Parameters (Data, On_Text_Params);

         declare
            Inst     : constant Class_Instance := Nth_Arg (Data, 1, Class);
            Text     : constant String := Nth_Arg (Data, 2);
            Property : constant Tools_Output_Property_Access :=
              Tools_Output_Property_Access
                (Get_Data (Inst, Tools_Output_Handler_Class_Name));
         begin
            Property.Child.Parse_Standard_Error (Text, null);
         end;
      elsif Command = On_Exit_Cst then
         Name_Parameters (Data, On_Exit_Params);

         declare
            Inst     : constant Class_Instance := Nth_Arg (Data, 1, Class);
            Status   : constant Integer := Nth_Arg (Data, 2, 0);
            Property : constant Tools_Output_Property_Access :=
              Tools_Output_Property_Access
                (Get_Data (Inst, Tools_Output_Handler_Class_Name));
         begin
            Property.Child.End_Of_Stream (Status, null);
         end;
      end if;
   end Handler;

   --------------------------
   -- Parse_Standard_Error --
   --------------------------

   overriding procedure Parse_Standard_Error
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : Command_Access)
   is
      pragma Unreferenced (Command);
      Args : Callback_Data'Class := Create
        (Get_Script (Self.Inst), Arguments_Count => 1);
      Proc : Subprogram_Type;
   begin
      Proc := Get_Method (Self.Inst, On_Stderr_Cst);
      Set_Nth_Arg (Args, 1, Item);

      declare
         Ignore : constant Any_Type := Proc.Execute (Args);
         pragma Unreferenced (Ignore);
      begin
         Free (Proc);
         Free (Args);
      end;
   end Parse_Standard_Error;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : Command_Access)
   is
      pragma Unreferenced (Command);
      Args : Callback_Data'Class := Create
        (Get_Script (Self.Inst), Arguments_Count => 1);
      Proc : Subprogram_Type;
   begin
      Proc := Get_Method (Self.Inst, On_Stdout_Cst);
      Set_Nth_Arg (Args, 1, Item);

      declare
         Ignore : constant Any_Type := Proc.Execute (Args);
         pragma Unreferenced (Ignore);
      begin
         Free (Proc);
         Free (Args);
      end;
   end Parse_Standard_Output;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Fabric : constant External_Parser_Fabric_Access :=
        new Python_Parser_Fabric'(Kernel => Kernel_Handle (Kernel));
      Class : constant Class_Type :=
        New_Class (Kernel, Tools_Output_Handler_Class_Name);
   begin
      Register_Command
        (Kernel,
         Constructor_Method,
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel,
         On_Stdout_Cst,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel,
         On_Stderr_Cst,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel,
         On_Exit_Cst,
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => Class,
         Handler       => Handler'Access);

      Set_External_Parser_Fabric (Fabric);
   end Register_Commands;

end Custom_Tools_Output;
