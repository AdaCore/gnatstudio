------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

--  This is the implementation of Debugger for the GNU Debugger (Gdb).
--  See debugger.ads for complete documentation on this package.

with GNAT.Strings;
with Debugger;
with GNAT.Regpat;
with GVD.Types;
with GNATCOLL.VFS;
with GNATCOLL.Tribooleans; use GNATCOLL.Tribooleans;

package Debugger.Base_Gdb.Gdb_CLI is

   type Gdb_Debugger is new Debugger.Base_Gdb.Base_Gdb_Debugger with private;

   overriding procedure Spawn
     (Debugger        : access Gdb_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "");

   overriding procedure Initialize (Debugger : access Gdb_Debugger);

   overriding procedure Close (Debugger : access Gdb_Debugger);

   overriding procedure Connect_To_Target
     (Debugger : access Gdb_Debugger;
      Target   : String;
      Protocol : String;
      Force    : Boolean := False;
      Mode     : GVD.Types.Invisible_Command := GVD.Types.Hidden);

   overriding function Is_Connected_To_Target
     (Debugger : access Gdb_Debugger) return Boolean;

   overriding procedure Wait_Prompt (Debugger : access Gdb_Debugger);

   overriding function Wait_Prompt
     (Debugger : access Gdb_Debugger;
      Timeout  : Integer) return Boolean;

   overriding function Highlighting_Pattern
     (Debugger : access Gdb_Debugger) return GNAT.Regpat.Pattern_Matcher;

   overriding procedure Display_Prompt (Debugger : access Gdb_Debugger);

   overriding procedure Change_Directory
     (Debugger    : access Gdb_Debugger;
      Dir         : GNATCOLL.VFS.Virtual_File;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Detect_Language (Debugger : access Gdb_Debugger);

   overriding procedure Found_File_Name
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      Name        : out Unbounded_String;
      Line        : out Natural;
      Addr        : out GVD.Types.Address_Type);

   overriding procedure Found_Frame_Info
     (Debugger : access Gdb_Debugger;
      Str      : String;
      Frame    : out Unbounded_String;
      Message  : out Frame_Info_Type);

   overriding function Source_Files_List
     (Debugger : access Gdb_Debugger) return GNAT.Strings.String_List;

   overriding function Find_File
     (Debugger : access Gdb_Debugger; File_Name : String) return String;

   overriding function Type_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   overriding function Get_Type_Info
     (Debugger  : access Gdb_Debugger;
      Entity    : String;
      Default   : String) return String;

   overriding function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String;

   overriding function Print_Value_Cmd
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   overriding function Info_Locals
     (Debugger : access Gdb_Debugger) return String;

   overriding function Info_Args
     (Debugger : access Gdb_Debugger) return String;

   overriding function Info_Registers
     (Debugger : access Gdb_Debugger) return String;

   overriding function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   overriding procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Get_Executable
     (Debugger : access Gdb_Debugger) return GNATCOLL.VFS.Virtual_File;

   overriding procedure Load_Core_File
     (Debugger : access Gdb_Debugger;
      Core     : GNATCOLL.VFS.Virtual_File;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Add_Symbols
     (Debugger : access Gdb_Debugger;
      Module   : GNATCOLL.VFS.Virtual_File;
      Address  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Load_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Run
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Start
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Attach_Process
     (Debugger : access Gdb_Debugger;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Detach_Process
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Kill_Process
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Into
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Over
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Into_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Over_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Continue
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Current_Frame
     (Debugger : access Gdb_Debugger)
      return Integer;

   overriding procedure Interrupt (Debugger : access Gdb_Debugger);

   overriding function Command_Kind
     (Debugger : access Gdb_Debugger;
      Command  : String) return Command_Category;

   overriding function Breakpoints_Changed
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean;

   overriding procedure Stack_Down
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Stack_Up
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Stack_Frame
     (Debugger : access Gdb_Debugger;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Break_Subprogram
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
     return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Source
     (Debugger  : access Gdb_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
     return GVD.Types.Breakpoint_Identifier;

   overriding procedure Remove_Breakpoint_At
     (Debugger : not null access Gdb_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Editable_Line_Type;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
     return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Address
     (Debugger   : access Gdb_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
     return GVD.Types.Breakpoint_Identifier;

   overriding function Break_Regexp
     (Debugger   : access Gdb_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden)
     return GVD.Types.Breakpoint_Identifier;

   overriding procedure Enable_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Remove_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Get_Last_Breakpoint_Id
     (Debugger  : access Gdb_Debugger)
      return GVD.Types.Breakpoint_Identifier;

   overriding procedure Set_Breakpoint_Condition
     (Debugger  : access Gdb_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Set_Breakpoint_Command
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Set_Scope_Action
     (Debugger : access Gdb_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Watch
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden)
     return GVD.Types.Breakpoint_Identifier;

   overriding procedure Finish
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Vector);

   overriding procedure Configure_Backtrace
     (Self                 : not null access Gdb_Debugger;
      Show_Id              : Boolean := True;
      Show_PC              : Boolean := True;
      Show_Subprogram_Name : Boolean := True;
      Show_Parameters      : Boolean := True;
      Show_Location        : Boolean := True);

   overriding procedure Task_Switch
     (Debugger : access Gdb_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Thread_Switch
     (Debugger : access Gdb_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure PD_Switch
     (Debugger : access Gdb_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Info_Tasks
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   overriding procedure Info_Threads
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   overriding procedure Info_PD
     (Debugger : access Gdb_Debugger;
      Info     : out PD_Information_Array;
      Len      : out Natural);

   overriding procedure Set_VxWorks_Version
     (Debugger : access Gdb_Debugger; Force : Boolean := False);

   overriding function VxWorks_Version
     (Debugger : access Gdb_Debugger) return GVD.Types.VxWorks_Version_Type;

   overriding procedure List_Breakpoints
     (Debugger  : not null access Gdb_Debugger;
      Kernel    : not null access Kernel_Handle_Record'Class;
      List      : out Breakpoint_Vectors.Vector);

   overriding function List_Exceptions
     (Debugger : access Gdb_Debugger) return GVD.Types.Exception_Array;

   overriding procedure Get_Machine_Code
     (Debugger      : access Gdb_Debugger;
      Range_Start   : out GVD.Types.Address_Type;
      Range_End     : out GVD.Types.Address_Type;
      Code          : out Disassemble_Elements;
      Start_Address : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address);

   overriding procedure Get_Machine_Code
     (Debugger : access Gdb_Debugger;
      File     : String;
      From     : Natural;
      To       : Natural;
      Code     : out Disassemble_Elements);

   overriding procedure Get_Line_Address
     (Debugger        : access Gdb_Debugger;
      Line            : Natural;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type);

   overriding function Get_Memory
     (Debugger : access Gdb_Debugger;
      Size     : Integer;
      Address  : String) return Memory_Dump_Access;

   overriding procedure Put_Memory_Byte
     (Debugger : access Gdb_Debugger;
      Address  : String;
      Byte     : String);

   overriding function Get_Variable_Address
     (Debugger  : access Gdb_Debugger;
      Variable  : String) return String;

   overriding function Get_Endian_Type
     (Debugger : access Gdb_Debugger) return Endian_Type;

   overriding function Complete
     (Debugger  : access Gdb_Debugger;
      Beginning : String) return GNAT.Strings.String_List;

   overriding procedure Open_Processes (Debugger : access Gdb_Debugger);

   overriding procedure Next_Process
     (Debugger : access Gdb_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean);

   overriding procedure Close_Processes (Debugger : access Gdb_Debugger);

   overriding function Support_TTY
     (Debugger : access Gdb_Debugger) return Boolean;

   overriding procedure Set_TTY (Debugger : access Gdb_Debugger; TTY : String);

   overriding procedure Filter_Output
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type;
      Str      : String;
      Result   : out Unbounded_String);

   overriding function Is_Quit_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean;

   overriding function Get_Register_Names
     (Debugger : access Gdb_Debugger)
      return GVD.Types.Strings_Vectors.Vector;

   overriding function Get_Registers_Values
     (Debugger : access Gdb_Debugger;
      Names    : GVD.Types.Strings_Vectors.Vector;
      Format   : GVD.Types.Registers_Format)
      return GVD.Types.Strings_Vectors.Vector;

private

   overriding function Send_And_Get_Clean_Output
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden)
      return String;

   overriding procedure Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden);

   type Gdb_Debugger is new Debugger.Base_Gdb.Base_Gdb_Debugger with record
      WTX_List         : GNAT.Strings.String_Access;
      WTX_Index        : Natural;
      VxWorks_Version  : GVD.Types.VxWorks_Version_Type := GVD.Types.Vx_None;
      Default_Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Default_Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Has_Symbol_List  : Integer := -1;
      Has_Start_Cmd    : GNATCOLL.Tribooleans.Triboolean := Indeterminate;

      Use_Catch_For_Exceptions : GNATCOLL.Tribooleans.Triboolean :=
        GNATCOLL.Tribooleans.Indeterminate;
      --  Whether we should use "catch" or "break" to set a breakpoint on
      --  exceptions. This is initialized the first time we set a breakpoint
      --  on exception

      Current_Frame_Num : Integer := -1;
   end record;

end Debugger.Base_Gdb.Gdb_CLI;
