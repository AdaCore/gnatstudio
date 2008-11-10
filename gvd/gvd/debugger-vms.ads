-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                   Copyright (C) 2000-2008, AdaCore                --
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

--  This is the implementation of Debugger for VMS Debug
--  See debugger.ads for complete documentation on this package.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;
with Debugger;
with GNAT.Regpat;
with Gtk.Window;
with GVD.Types;
with GNATCOLL.VFS;

package Debugger.VMS is

   type VMS_Debugger is new Debugger.Debugger_Root with private;

   overriding procedure Spawn
     (Debugger        : access VMS_Debugger;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Executable      : GNATCOLL.VFS.Virtual_File;
      Debugger_Args   : GNAT.Strings.String_List;
      Executable_Args : String;
      Proxy           : Process_Proxies.Process_Proxy_Access;
      Window          : Gtk.Window.Gtk_Window;
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "");

   overriding procedure Initialize (Debugger : access VMS_Debugger);

   overriding function Highlighting_Pattern
     (Debugger : access VMS_Debugger)
      return GNAT.Regpat.Pattern_Matcher;

   overriding procedure Close (Debugger : access VMS_Debugger);

   overriding procedure Wait_Prompt
     (Debugger : access VMS_Debugger);

   overriding function Wait_Prompt
     (Debugger : access VMS_Debugger;
      Timeout  : Integer) return Boolean;

   overriding procedure Display_Prompt
     (Debugger : access VMS_Debugger);

   overriding function Type_Of
     (Debugger : access VMS_Debugger;
      Entity   : String) return String;

   overriding function Info_Locals
     (Debugger : access VMS_Debugger) return String;

   overriding function Info_Args
     (Debugger : access VMS_Debugger) return String;

   overriding function Info_Registers
     (Debugger : access VMS_Debugger) return String;

   overriding function Value_Of
     (Debugger : access VMS_Debugger;
      Entity   : String;
      Format   : Value_Format := Default_Format) return String;

   overriding function Print_Value_Cmd
     (Debugger : access VMS_Debugger;
      Entity   : String) return String;

   ------------------------------
   -- Source/Path manipulation --
   ------------------------------

   overriding procedure Change_Directory
     (Debugger    : access VMS_Debugger;
      Dir         : String;
      Mode        : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Found_File_Name
     (Debugger    : access VMS_Debugger;
      Str         : String;
      Name_First  : out Natural;
      Name_Last   : out Positive;
      First, Last : out Natural;
      Line        : out Natural;
      Addr_First  : out Natural;
      Addr_Last   : out Natural);

   overriding procedure Found_Frame_Info
     (Debugger    : access VMS_Debugger;
      Str         : String;
      First, Last : out Natural;
      Message     : out Frame_Info_Type);

   overriding function Source_Files_List
     (Debugger : access VMS_Debugger) return GNAT.Strings.String_List;

   ------------------------
   -- Execution Commands --
   ------------------------

   overriding procedure Set_Executable
     (Debugger   : access VMS_Debugger;
      Executable : GNATCOLL.VFS.Virtual_File;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function Get_Executable
     (Debugger : access VMS_Debugger)
      return GNATCOLL.VFS.Virtual_File;

   overriding procedure Load_Core_File
     (Debugger : access VMS_Debugger;
      Core     : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Add_Symbols
     (Debugger : access VMS_Debugger;
      Module   : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Run
     (Debugger  : access VMS_Debugger;
      Arguments : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Start
     (Debugger : access VMS_Debugger;
      Arguments : String := "";
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Attach_Process
     (Debugger : access VMS_Debugger;
      Process  : String;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Detach_Process
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Kill_Process
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Into
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Over
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Into_Instruction
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Step_Over_Instruction
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Continue
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Interrupt (Debugger : access VMS_Debugger);

   overriding function Is_Execution_Command
     (Debugger : access VMS_Debugger;
      Command  : String) return Boolean;

   overriding function Is_Context_Command
     (Debugger : access VMS_Debugger;
      Command : String) return Boolean;

   overriding function Is_Load_Command
     (Debugger : access VMS_Debugger;
      Command  : String) return Boolean;

   overriding function Is_Break_Command
     (Debugger : access VMS_Debugger;
      Command : String) return Boolean;

   ----------------------
   -- Stack Management --
   ----------------------

   overriding procedure Stack_Down
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Stack_Up
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Stack_Frame
     (Debugger : access VMS_Debugger;
      Frame    : Positive;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Finish
     (Debugger : access VMS_Debugger;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Backtrace
     (Debugger : access VMS_Debugger;
      Value    : out Backtrace_Array;
      Len      : out Natural);

   -------------------------
   -- Breakpoint Handling --
   -------------------------

   overriding procedure Break_Subprogram
     (Debugger  : access VMS_Debugger;
      Name      : String;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Source
     (Debugger  : access VMS_Debugger;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Positive;
      Temporary : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Exception
     (Debugger  : access VMS_Debugger;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False;
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Address
     (Debugger   : access VMS_Debugger;
      Address    : GVD.Types.Address_Type;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Break_Regexp
     (Debugger   : access VMS_Debugger;
      Regexp     : String;
      Temporary  : Boolean := False;
      Mode       : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Enable_Breakpoint
     (Debugger : access VMS_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Enable   : Boolean := True;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding procedure Remove_Breakpoint
     (Debugger : access VMS_Debugger;
      Num      : GVD.Types.Breakpoint_Identifier;
      Mode     : GVD.Types.Command_Type := GVD.Types.Hidden);

   overriding function List_Breakpoints
     (Debugger  : access VMS_Debugger)
      return GVD.Types.Breakpoint_Array;

   overriding function Get_Last_Breakpoint_Id
     (Debugger  : access VMS_Debugger)
      return GVD.Types.Breakpoint_Identifier;

   -----------------
   -- Watchpoints --
   -----------------

   overriding procedure Watch
     (Debugger  : access VMS_Debugger;
      Name      : String;
      Trigger   : GVD.Types.Watchpoint_Trigger;
      Condition : String := "";
      Mode      : GVD.Types.Command_Type := GVD.Types.Hidden);

   -------------------
   -- Assembly code --
   -------------------

   overriding procedure Get_Machine_Code
     (Debugger        : access VMS_Debugger;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type;
      Code            : out GNAT.Strings.String_Access;
      Start_Address   : GVD.Types.Address_Type := GVD.Types.Invalid_Address;
      End_Address     : GVD.Types.Address_Type := GVD.Types.Invalid_Address);

   overriding procedure Get_Line_Address
     (Debugger        : access VMS_Debugger;
      Line            : Natural;
      Range_Start     : out GVD.Types.Address_Type;
      Range_End       : out GVD.Types.Address_Type);

   -----------------------
   -- Memory operations --
   -----------------------

   overriding function Get_Memory
     (Debugger : access VMS_Debugger;
      Size     : Integer;
      Address  : String) return String;

   overriding procedure Put_Memory_Byte
     (Debugger : access VMS_Debugger;
      Address  : String;
      Byte     : String);

   overriding function Get_Variable_Address
     (Debugger  : access VMS_Debugger;
      Variable  : String) return String;

   overriding function Get_Endian_Type
     (Debugger : access VMS_Debugger) return Endian_Type;

   -----------------------------
   -- Command Line operations --
   -----------------------------

   overriding function Complete
     (Debugger  : access VMS_Debugger;
      Beginning : String) return GNAT.Strings.String_List;

private

   overriding function Send
     (Debugger        : access VMS_Debugger;
      Cmd             : String;
      Mode            : GVD.Types.Invisible_Command := GVD.Types.Hidden)
      return String;

   type Breakpoint_Record is record
      Expression : Unbounded_String;
   end record;

   type BP_Array is array (Integer range <>) of Breakpoint_Record;

   Max_BP : constant := 128;
   type VMS_Debugger is new Debugger.Debugger_Root with record
      Executable  : GNATCOLL.VFS.Virtual_File;
      Breakpoints : BP_Array (1 .. Max_BP);
   end record;

end Debugger.VMS;
