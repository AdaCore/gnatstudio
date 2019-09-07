------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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
with Commands;                   use Commands;
with GPS.Scripts;                use GPS.Scripts;
with GPS.Scripts.Commands;       use GPS.Scripts.Commands;
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
      Inst   : Class_Instance;
      Kernel : Core_Kernel;
   end record;

   overriding procedure Parse_Standard_Output
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : access Root_Command'Class);
   --  Parse a piece of an output passed as Item.

   overriding procedure Parse_Standard_Error
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : access Root_Command'Class);
   --  Parse a piece of an stderr passed as Item.

   overriding procedure End_Of_Stream
     (Self    : not null access Custom_Parser;
      Status  : Integer;
      Command : access Root_Command'Class);
   --  Process end of streams (both output and error).

   type Python_Parser_Fabric is new External_Parser_Fabric with record
      Kernel : Core_Kernel;
   end record;

   overriding procedure Create_External_Parsers
     (Self        : access Python_Parser_Fabric;
      Parser_List : in out String_List_Utils.String_List.Cursor;
      Child       : in out Tools_Output_Parser_Access;
      Found       : out Boolean);

   -----------------------------
   -- Create_External_Parsers --
   -----------------------------

   overriding procedure Create_External_Parsers
     (Self        : access Python_Parser_Fabric;
      Parser_List : in out String_List_Utils.String_List.Cursor;
      Child       : in out Tools_Output_Parser_Access;
      Found       : out Boolean)
   is
      use String_List_Utils.String_List;

      Create_Parser : constant String := "tool_output.create_parser";

      Inst   : Class_Instance;
      Script : constant Scripting_Language :=
        Lookup_Scripting_Language (Self.Kernel.Scripts, "Python");
   begin
      --  Try to create new python parser
      declare
         Args : Callback_Data'Class :=
           Create (Script, 1 + Boolean'Pos (Child /= null));
      begin
         Set_Nth_Arg (Args, 1, Element (Parser_List));

         if Child /= null then
            --  Create wrapper around Child and pass to python function
            declare
               Class    : constant Class_Type := New_Class
                 (Self.Kernel.Scripts, Tools_Output_Handler_Class_Name);
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

      Next (Parser_List);

      while Has_Element (Parser_List) loop
         declare
            Args : Callback_Data'Class := Create (Script, 2);
         begin
            Set_Nth_Arg (Args, 1, Element (Parser_List));
            Set_Nth_Arg (Args, 2, Inst);

            Execute_Command (Args, Create_Parser);

            exit when Args.Return_Value = No_Class_Instance;

            Inst := Args.Return_Value;
            Next (Parser_List);
         end;
      end loop;

      Found := True;
      Child := new Custom_Parser'
        (Child => null, Inst => Inst, Kernel => Self.Kernel);
   end Create_External_Parsers;

   -------------------
   -- End_Of_Stream --
   -------------------

   overriding procedure End_Of_Stream
     (Self    : not null access Custom_Parser;
      Status  : Integer;
      Command : access Root_Command'Class)
   is
      Inst : Class_Instance := No_Class_Instance;
      Scheduled : Scheduled_Command_Access;
   begin
      --  Unless we have finalized the scripts module already
      if Self.Kernel.Scripts /= null then
         Scheduled := Scheduled_Command_Access
            (Self.Kernel.Get_Scheduled_Command (Command));
         if Scheduled /= null then
            Inst := Scheduled.Get_Instance (Get_Script (Self.Inst));
         end if;

         declare
            Proc : Subprogram_Type := Get_Method (Self.Inst, On_Exit_Cst);
            Args : Callback_Data'Class := Create
              (Get_Script (Self.Inst), Arguments_Count => 2);
         begin
            Set_Nth_Arg (Args, 1, Status);
            Set_Nth_Arg (Args, 2, Inst);

            declare
               Dummy : constant Any_Type := Proc.Execute (Args);
            begin
               Free (Proc);
               Free (Args);
            end;
         end;
      end if;

      if Self.Child /= null then
         Self.Child.End_Of_Stream (Status, Command);
      end if;
   end End_Of_Stream;

   -------------
   -- Handler --
   -------------

   procedure Handler
     (Data    : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Core_Kernel := Get_Kernel (Data);
      Class  : constant Class_Type := New_Class
        (Kernel.Scripts, Tools_Output_Handler_Class_Name);
      Arg_3  : constant Scheduled_Command_Access :=
         Get_Command (Data, 3, Allow_Null => True);
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
            Property.Child.Parse_Standard_Output
              (Text, Command_Access (Arg_3));
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
            Property.Child.Parse_Standard_Error (Text, Command_Access (Arg_3));
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
            Property.Child.End_Of_Stream (Status, Command_Access (Arg_3));
         end;
      end if;
   end Handler;

   --------------------------
   -- Parse_Standard_Error --
   --------------------------

   overriding procedure Parse_Standard_Error
     (Self    : not null access Custom_Parser;
      Item    : String;
      Command : access Root_Command'Class)
   is
      Args : Callback_Data'Class := Create
        (Get_Script (Self.Inst), Arguments_Count => 2);
      Proc : Subprogram_Type;
      Inst : Class_Instance := No_Class_Instance;
      Scheduled : constant Scheduled_Command_Access :=
        Scheduled_Command_Access (Self.Kernel.Get_Scheduled_Command (Command));
   begin
      if Scheduled /= null then
         Inst := Scheduled.Get_Instance (Get_Script (Self.Inst));
      end if;

      Proc := Get_Method (Self.Inst, On_Stderr_Cst);
      Set_Nth_Arg (Args, 1, Item);
      Set_Nth_Arg (Args, 2, Inst);

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
      Command : access Root_Command'Class)
   is
      Args : Callback_Data'Class := Create
        (Get_Script (Self.Inst), Arguments_Count => 2);
      Proc : Subprogram_Type;
      Inst : Class_Instance := No_Class_Instance;
      Scheduled : constant Scheduled_Command_Access :=
        Scheduled_Command_Access (Self.Kernel.Get_Scheduled_Command (Command));
   begin
      if Scheduled /= null then
         Inst := Scheduled.Get_Instance (Get_Script (Self.Inst));
      end if;

      Proc := Get_Method (Self.Inst, On_Stdout_Cst);
      Set_Nth_Arg (Args, 1, Item);
      Set_Nth_Arg (Args, 2, Inst);

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

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class) is
      Fabric : constant External_Parser_Fabric_Access :=
        new Python_Parser_Fabric'(Kernel => Core_Kernel (Kernel));
      Class : constant Class_Type :=
        New_Class (Kernel.Scripts, Tools_Output_Handler_Class_Name);
   begin
      Register_Command
        (Kernel.Scripts,
         Constructor_Method,
         Minimum_Args  => 0,
         Maximum_Args  => 1,
         Class         => Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel.Scripts,
         On_Stdout_Cst,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel.Scripts,
         On_Stderr_Cst,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Class,
         Handler       => Handler'Access);
      Register_Command
        (Kernel.Scripts,
         On_Exit_Cst,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Class,
         Handler       => Handler'Access);

      Set_External_Parser_Fabric (Fabric);
   end Register_Commands;

end Custom_Tools_Output;
