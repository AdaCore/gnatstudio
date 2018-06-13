------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with Language.Debugger;

package Debugger.LLDB is

   type LLDB_Debugger is new Debugger.Debugger_Root with private;
   type LLDB_Debugger_Access is access all LLDB_Debugger'Class;

   overriding procedure Spawn
     (Debugger        : access LLDB_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Debugger_Num    : Natural;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "");

   overriding procedure Initialize (Debugger : access LLDB_Debugger);

   overriding procedure Connect_To_Target
     (Debugger : access LLDB_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Is_Connected_To_Target
     (Debugger : access LLDB_Debugger) return Boolean;

   overriding procedure Send
     (Debugger        : access LLDB_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Send_And_Get_Clean_Output
     (Debugger : access LLDB_Debugger;
      Cmd      : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden)
      return String;

   overriding function Highlighting_Pattern
     (Debugger : access LLDB_Debugger)
      return GNAT.Regpat.Pattern_Matcher;

   overriding procedure Close (Debugger : access LLDB_Debugger);

--     overriding procedure Set_Language
--       (Debugger     : access LLDB_Debugger;
--        The_Language : access Language.Language_Root'Class);

--     overriding function Get_Language
--       (Debugger : access LLDB_Debugger;
--        Lang     : String := "") return Language.Language_Access;

   overriding procedure Detect_Language (Debugger : access LLDB_Debugger);

   overriding function Get_Uniq_Id
     (Debugger : access LLDB_Debugger; Entity : String) return String;

   overriding procedure Wait_Prompt (Debugger : access LLDB_Debugger);

   overriding function Wait_Prompt
     (Debugger : access LLDB_Debugger;
      Timeout  : Integer) return Boolean;

   overriding procedure Display_Prompt (Debugger : access LLDB_Debugger);

   overriding function Type_Of
     (Debugger : access LLDB_Debugger;
      Entity   : String) return String;

   function Get_Type
     (Debugger : access LLDB_Debugger;
      Entity   : String) return String;

   overriding function Get_Type_Info
     (Debugger  : access LLDB_Debugger;
      Entity    : String;
      Default   : String) return String;

   overriding function Info_Locals
     (Debugger : access LLDB_Debugger) return String;

   overriding function Info_Args
     (Debugger : access LLDB_Debugger) return String;

   overriding function Value_Of
     (Debugger : access LLDB_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String;

   overriding function Print_Value_Cmd
     (Debugger : access LLDB_Debugger;
      Entity   : String) return String;

   overriding procedure Change_Directory
     (Debugger    : access LLDB_Debugger;
      Dir         : GNATCOLL.VFS.Virtual_File;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Found_File_Name
     (Debugger    : access LLDB_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type);

   overriding procedure Found_Frame_Info
     (Debugger : access LLDB_Debugger;
      Str      : String;
      Frame    : out Unbounded_String;
      Message  : out Frame_Info_Type);

--     overriding function Find_File
--       (Debugger : access LLDB_Debugger; File_Name : String) return String;

   overriding procedure Set_Executable
     (Debugger   : access LLDB_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Get_Executable
     (Debugger : access LLDB_Debugger)
      return GNATCOLL.VFS.Virtual_File;

   overriding procedure Load_Core_File
     (Debugger : access LLDB_Debugger;
      Core     : GNATCOLL.VFS.Virtual_File;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Add_Symbols
     (Debugger : access LLDB_Debugger;
      Module   : GNATCOLL.VFS.Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Load_Executable
     (Debugger   : access LLDB_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Run
     (Debugger  : access LLDB_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Start
     (Debugger : access LLDB_Debugger;
      Arguments : String := "";
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Attach_Process
     (Debugger : access LLDB_Debugger;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Detach_Process
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Kill_Process
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Into
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Over
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Into_Instruction
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Over_Instruction
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Continue
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Continue_Until_Location
     (Debugger : access LLDB_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Line_Contains_Code
     (Debugger   : not null access LLDB_Debugger;
      Dummy_File : GNATCOLL.VFS.Virtual_File;
      Dummy_Line : Editable_Line_Type)
      return Boolean
   is
     (True);

   overriding procedure Interrupt (Debugger : access LLDB_Debugger);

   overriding function Command_Kind
     (Debugger : access LLDB_Debugger;
      Command  : String) return Command_Category;

   overriding function Breakpoints_Changed
     (Debugger : access LLDB_Debugger;
      Command  : String) return Boolean;

   overriding procedure Stack_Down
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Stack_Up
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Stack_Frame
     (Debugger : access LLDB_Debugger;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Finish
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Backtrace
     (Debugger : access LLDB_Debugger;
      From     : Integer;
      To       : Integer;
      Value    : out Backtrace_Vector);

   overriding function Current_Frame
     (Debugger : access LLDB_Debugger)
      return Integer;

   overriding procedure Configure_Backtrace
     (Self                 : not null access LLDB_Debugger;
      Show_Id              : Boolean := True;
      Show_PC              : Boolean := True;
      Show_Subprogram_Name : Boolean := True;
      Show_Parameters      : Boolean := True;
      Show_Location        : Boolean := True);

   overriding function Break_Subprogram
     (Debugger  : access LLDB_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Source
     (Debugger  : access LLDB_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Exception
     (Debugger  : access LLDB_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding function Catch_Assertions
     (Debugger  : access LLDB_Debugger;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Address
     (Debugger   : access LLDB_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Regexp
     (Debugger   : access LLDB_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding procedure Enable_Breakpoint
     (Debugger : access LLDB_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Remove_Breakpoint
     (Debugger : access LLDB_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Remove_Breakpoint_At
     (Debugger : not null access LLDB_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure List_Breakpoints
     (Debugger  : not null access LLDB_Debugger;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector);

   overriding function Get_Last_Breakpoint_Id
     (Debugger  : access LLDB_Debugger)
      return GVD.Types.Breakpoint_Identifier;

   overriding procedure Set_Breakpoint_Condition
     (Debugger  : access LLDB_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Set_Breakpoint_Command
     (Debugger : access LLDB_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Set_Breakpoint_Ignore_Count
     (Debugger : access LLDB_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Set_Scope_Action
     (Debugger : access LLDB_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Watch
     (Debugger  : access LLDB_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
      return GVD.Types.Breakpoint_Identifier;

   overriding function List_Exceptions
     (Debugger : access LLDB_Debugger) return GVD.Types.Exception_Array;

   overriding procedure Task_Switch
     (Debugger : access LLDB_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Thread_Switch
     (Debugger : access LLDB_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure PD_Switch
     (Debugger : access LLDB_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Info_Tasks
     (Debugger : access LLDB_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   overriding procedure Info_Threads
     (Debugger : access LLDB_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   overriding procedure Info_PD
     (Debugger : access LLDB_Debugger;
      Info     : out PD_Information_Array;
      Len      : out Natural);

   overriding procedure Get_Machine_Code
     (Debugger        : access LLDB_Debugger;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type;
      Code            : out Disassemble_Elements;
      Start_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address     : GVD.Types.Address_Type := GVD.Types.Invalid_Address);

   overriding procedure Get_Machine_Code
     (Debugger : access LLDB_Debugger;
      File     : String;
      From     : Natural;
      To       : Natural;
      Code     : out Disassemble_Elements);

   overriding procedure Get_Line_Address
     (Debugger        : access LLDB_Debugger;
      Line            : Natural;
      File            : GNATCOLL.VFS.Virtual_File;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type);

   overriding function Get_Register_Names
     (Debugger : access LLDB_Debugger)
      return GVD.Types.Strings_Vectors.Vector;

   overriding function Get_Registers_Values
     (Debugger : access LLDB_Debugger;
      Names    : GVD.Types.Strings_Vectors.Vector;
      Format   : GVD.Types.Registers_Format)
      return GVD.Types.Strings_Vectors.Vector;

   overriding procedure Set_Register
     (Debugger : access LLDB_Debugger;
      Name     : String;
      Value    : String);

   overriding function Get_Memory
     (Debugger : access LLDB_Debugger;
      Size     : Integer;
      Address  : String) return Memory_Dump_Access;

   overriding procedure Put_Memory_Byte
     (Debugger : access LLDB_Debugger;
      Address  : String;
      Byte     : String);

   overriding function Get_Variable_Address
     (Debugger  : access LLDB_Debugger;
      Variable  : String) return String;

   overriding function Get_Endian_Type
     (Debugger : access LLDB_Debugger) return Endian_Type;

   overriding function Complete
     (Debugger  : access LLDB_Debugger;
      Beginning : String) return GNAT.Strings.String_List;

   overriding procedure Open_Processes (Debugger : access LLDB_Debugger);

   overriding procedure Next_Process
     (Debugger : access LLDB_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean);

   overriding procedure Close_Processes (Debugger : access LLDB_Debugger);

   overriding procedure Set_TTY
     (Debugger : access LLDB_Debugger; TTY : String);

   overriding procedure Filter_Output
     (Debugger : access LLDB_Debugger;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String);

   overriding function Is_Quit_Command
     (Debugger : access LLDB_Debugger;
      Command : String) return Boolean;

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Entity     : String;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive;
      Parent     : GVD.Variables.Types.GVD_Type_Holder);

private

   type LLDB_Debugger is new Debugger.Debugger_Root with record
      Executable              : GNATCOLL.VFS.Virtual_File;
      Executable_Args         : GNAT.Strings.String_Access;

      Target_Connected        : Boolean         := False;
      --  Whether we have connected to a target.

      Debuggee_Pid            : Integer          := 0;
      Is_Running              : Boolean          := False;
      Current_Command_Kind    : Command_Category := Misc_Command;
      Breakpoints_Changed     : Boolean          := False;
      Register_Names          : GVD.Types.Strings_Vectors.Vector;
   end record;

   procedure Prepare_Target_For_Send
     (Debugger : access LLDB_Debugger;
      Cmd      : String);

end Debugger.LLDB;
