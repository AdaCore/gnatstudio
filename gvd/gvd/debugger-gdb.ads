-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2006                       --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This is the implementation of Debugger for the GNU Debugger (Gdb).
--  See debugger.ads for complete documentation on this package.

with GNAT.Strings;
with Debugger;
with GNAT.Regpat;
with Gtk.Window;
with Language.Debugger;
with GVD.Types;
with VFS;

package Debugger.Gdb is

   type Gdb_Debugger is new Debugger.Debugger_Root with private;

   procedure Spawn
     (Debugger        : access Gdb_Debugger;
      Executable      : VFS.Virtual_File := VFS.No_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "");

   procedure Initialize (Debugger : access Gdb_Debugger);

   procedure Close (Debugger : access Gdb_Debugger);

   procedure Connect_To_Target
     (Debugger : access Gdb_Debugger;
      Target   : String;
      Protocol : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Wait_Prompt (Debugger : access Gdb_Debugger);

   function Wait_Prompt
     (Debugger : access Gdb_Debugger;
      Timeout  : Integer) return Boolean;

   function Highlighting_Pattern
     (Debugger : access Gdb_Debugger) return GNAT.Regpat.Pattern_Matcher;

   procedure Display_Prompt (Debugger : access Gdb_Debugger);

   procedure Change_Directory
     (Debugger    : access Gdb_Debugger;
      Dir         : String;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Detect_Language (Debugger : access Gdb_Debugger);

   procedure Found_File_Name
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural;
      Addr_First  : out Natural;
      Addr_Last   : out Natural);

   procedure Found_Frame_Info
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      First, Last : out Natural;
      Message     : out Frame_Info_Type);

   function Source_Files_List
     (Debugger : access Gdb_Debugger) return GNAT.Strings.String_List;

   function Find_File
     (Debugger : access Gdb_Debugger; File_Name : String) return String;

   function Type_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   function Get_Type_Info
     (Debugger  : access Gdb_Debugger;
      Entity    : String;
      Default   : String) return String;

   function Value_Of
     (Debugger : access Gdb_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String;

   function Print_Value_Cmd
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   function Info_Locals (Debugger : access Gdb_Debugger) return String;

   function Info_Args (Debugger : access Gdb_Debugger) return String;

   function Info_Registers (Debugger : access Gdb_Debugger) return String;

   function Get_Uniq_Id
     (Debugger : access Gdb_Debugger;
      Entity   : String) return String;

   procedure Set_Executable
     (Debugger   : access Gdb_Debugger;
      Executable : VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   function Get_Executable
     (Debugger : access Gdb_Debugger) return VFS.Virtual_File;

   procedure Load_Core_File
     (Debugger : access Gdb_Debugger;
      Core     : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Add_Symbols
     (Debugger : access Gdb_Debugger;
      Module   : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Run
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Start
     (Debugger  : access Gdb_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Attach_Process
     (Debugger : access Gdb_Debugger;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Detach_Process
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Kill_Process
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Step_Into
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Step_Over
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Step_Into_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Step_Over_Instruction
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Continue
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Interrupt (Debugger : access Gdb_Debugger);

   function Is_Execution_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean;

   function Is_Context_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean;

   function Is_Load_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean;

   function Is_Break_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean;

   procedure Stack_Down
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Stack_Up
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Stack_Frame
     (Debugger : access Gdb_Debugger;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Break_Subprogram
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Break_Source
     (Debugger  : access Gdb_Debugger;
      File      : VFS.Virtual_File;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Break_Address
     (Debugger   : access Gdb_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Break_Regexp
     (Debugger   : access Gdb_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Enable_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Remove_Breakpoint
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   function Get_Last_Breakpoint_Id
     (Debugger  : access Gdb_Debugger)
      return GVD.Types.Breakpoint_Identifier;

   procedure Set_Breakpoint_Condition
     (Debugger  : access Gdb_Debugger;
      Num       : GVD.Types.Breakpoint_Identifier;
      Condition : String;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Set_Breakpoint_Command
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Commands : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Set_Breakpoint_Ignore_Count
     (Debugger : access Gdb_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Count    : Integer;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Set_Scope_Action
     (Debugger : access Gdb_Debugger;
      Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Num      : GVD.Types.Breakpoint_Identifier := 0;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Watch
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Finish
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural);

   procedure Task_Switch
     (Debugger : access Gdb_Debugger;
      Task_Num : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Thread_Switch
     (Debugger : access Gdb_Debugger;
      Thread   : Natural;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure PD_Switch
     (Debugger : access Gdb_Debugger;
      PD       : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   procedure Info_Tasks
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   procedure Info_Threads
     (Debugger : access Gdb_Debugger;
      Info     : out Thread_Information_Array;
      Len      : out Natural);

   procedure Info_PD
     (Debugger : access Gdb_Debugger;
      Info     : out PD_Information_Array;
      Len      : out Natural);

   procedure Info_WTX
     (Debugger : access Gdb_Debugger;
      Version  : out Natural);

   procedure Lines_With_Code
     (Debugger : access Gdb_Debugger;
      File     : VFS.Virtual_File;
      Result   : out Boolean;
      Lines    : out Line_Array);

   function List_Breakpoints
     (Debugger : access Gdb_Debugger) return GVD.Types.Breakpoint_Array;

   function List_Exceptions
     (Debugger : access Gdb_Debugger) return GVD.Types.Exception_Array;

   procedure Get_Machine_Code
     (Debugger      : access Gdb_Debugger;
      Range_Start   : out GVD.Types.Address_Type;
      Range_End     : out GVD.Types.Address_Type;
      Code          : out GNAT.Strings.String_Access;
      Start_Address : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address);

   procedure Get_Line_Address
     (Debugger        : access Gdb_Debugger;
      Line            : Natural;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type);

   function Get_Memory
     (Debugger : access Gdb_Debugger;
      Size     : in Integer;
      Address  : in String) return String;

   procedure Put_Memory_Byte
     (Debugger : access Gdb_Debugger;
      Address  : in String;
      Byte     : in String);

   function Get_Variable_Address
     (Debugger  : access Gdb_Debugger;
      Variable  : in String) return String;

   function Get_Endian_Type
     (Debugger : access Gdb_Debugger) return Endian_Type;

   function Complete
     (Debugger  : access Gdb_Debugger;
      Beginning : String) return GNAT.Strings.String_List;

   procedure Open_Processes (Debugger : access Gdb_Debugger);

   procedure Next_Process
     (Debugger : access Gdb_Debugger;
      Info     : out GVD.Proc_Utils.Process_Info;
      Success  : out Boolean);

   procedure Close_Processes (Debugger : access Gdb_Debugger);

   function Support_TTY (Debugger : access Gdb_Debugger) return Boolean;

   procedure Set_TTY (Debugger : access Gdb_Debugger; TTY : String);

private

   function Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Mode            : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String;

   procedure Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden);

   type Gdb_Debugger is new Debugger.Debugger_Root with record
      Executable       : VFS.Virtual_File;
      Executable_Args  : GNAT.Strings.String_Access;
      Stored_Language  : GNAT.Strings.String_Access;
      WTX_List         : GNAT.Strings.String_Access;
      WTX_Index        : Natural;
      WTX_Version      : Integer := -1;
      Endian           : Endian_Type := Unknown_Endian;
      Default_Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Default_Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Debuggee_Pid     : Integer := 0;
      Has_Symbol_List  : Integer := -1;
      Has_Start_Cmd    : Integer := -1;
   end record;

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive;
      Parent     : Items.Generic_Type_Access);
   --  Internal function used to parse the value.
   --  The parameters are the same as for Parse_Value, plus Parent that is
   --  the item that contains Result.
   --  Parent should be null for the top-level item.

end Debugger.Gdb;
