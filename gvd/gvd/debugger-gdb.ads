------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Gtk.Window;
with Language.Debugger;
with GVD.Types;
with GNATCOLL.VFS;

package Debugger.Gdb is

   type Gdb_Debugger is new Debugger.Debugger_Root with private;

   overriding procedure Spawn
     (Debugger        : access Gdb_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "");

   overriding procedure Initialize (Debugger : access Gdb_Debugger);

   overriding procedure Close (Debugger : access Gdb_Debugger);

   overriding procedure Connect_To_Target
     (Debugger : access Gdb_Debugger;
      Target   : String;
      Protocol : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

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
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural;
      Addr_First  : out Natural;
      Addr_Last   : out Natural);

   overriding procedure Found_Frame_Info
     (Debugger    : access Gdb_Debugger;
      Str         : String;
      First, Last : out Natural;
      Message     : out Frame_Info_Type);

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

   overriding procedure Interrupt (Debugger : access Gdb_Debugger);

   overriding function Is_Execution_Command
     (Debugger : access Gdb_Debugger;
      Command : String) return Boolean;

   overriding function Is_Context_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean;

   overriding function Is_Load_Command
     (Debugger : access Gdb_Debugger;
      Command  : String) return Boolean;

   overriding function Is_Break_Command
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

   overriding procedure Break_Subprogram
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Source
     (Debugger  : access Gdb_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Exception
     (Debugger  : access Gdb_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Address
     (Debugger   : access Gdb_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Regexp
     (Debugger   : access Gdb_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

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

   overriding procedure Watch
     (Debugger  : access Gdb_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Finish
     (Debugger : access Gdb_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Backtrace
     (Debugger : access Gdb_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural);

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

   overriding procedure Lines_With_Code
     (Debugger : access Gdb_Debugger;
      File     : GNATCOLL.VFS.Virtual_File;
      Result   : out Boolean;
      Lines    : out Line_Array);

   overriding function List_Breakpoints
     (Debugger : access Gdb_Debugger) return GVD.Types.Breakpoint_Array;

   overriding function List_Exceptions
     (Debugger : access Gdb_Debugger) return GVD.Types.Exception_Array;

   overriding procedure Get_Machine_Code
     (Debugger      : access Gdb_Debugger;
      Range_Start   : out GVD.Types.Address_Type;
      Range_End     : out GVD.Types.Address_Type;
      Code          : out GNAT.Strings.String_Access;
      Start_Address : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address);

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

private

   overriding function Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Mode            : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String;

   overriding procedure Send
     (Debugger        : access Gdb_Debugger;
      Cmd             : String;
      Empty_Buffer    : Boolean := True;
      Wait_For_Prompt : Boolean := True;
      Force_Send      : Boolean := False;
      Mode            : GVD.Types.Command_Type := GVD.Types.Hidden);

   type Remote_GDB_Mode is (Native, Cross, VxWorks);
   --  Indicates the type of remote access.
   --  This controls the behavior of the debugger when doing file load
   --  operations.
   --  Here are the commands that are launched:
   --
   --   Native  :  "file"
   --   Cross   :  "file" -> "target" * -> "load"
   --   VxWorks :            "target" * -> "load"
   --
   --  * Note: "target" is only launched if the debugger is not already
   --    connected to a target.

   ----------------------
   -- Version handling --
   ----------------------

   type Version_Number is record
      Major : Natural;
      Minor : Natural;
   end record;

   Unknown_Version : constant Version_Number := (Major => 0, Minor => 0);

   type Gdb_Debugger is new Debugger.Debugger_Root with record
      Executable       : GNATCOLL.VFS.Virtual_File;
      Executable_Args  : GNAT.Strings.String_Access;
      Stored_Language  : GNAT.Strings.String_Access;
      WTX_List         : GNAT.Strings.String_Access;
      WTX_Index        : Natural;
      GDB_Version      : Version_Number := Unknown_Version;
      VxWorks_Version  : GVD.Types.VxWorks_Version_Type := GVD.Types.Vx_None;
      Endian           : Endian_Type := Unknown_Endian;
      Default_Scope    : GVD.Types.Scope_Type := GVD.Types.No_Scope;
      Default_Action   : GVD.Types.Action_Type := GVD.Types.No_Action;
      Debuggee_Pid     : Integer := 0;
      Has_Symbol_List  : Integer := -1;
      Has_Start_Cmd    : Integer := -1;
      Has_Wtx_Add_Symbol_File : Integer := -1;
      Initializing     : Boolean := False;

      Mode             : Remote_GDB_Mode := Native;
      Target_Connected : Boolean := False;
      --  Whether we have connected to a target.

      Cached_File  : GNATCOLL.VFS.Virtual_File;
      Cached_Lines : Line_Array_Access;
      --  Caches to speed up calls to Lines_With_Code
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
