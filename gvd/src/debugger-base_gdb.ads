------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

--  This package contains common routines for GDM.

private with GNAT.Regpat;
private with GNATCOLL.Tribooleans;
private with Language.Debugger;
private with GVD.Process;

package Debugger.Base_Gdb is

   type Base_Gdb_Debugger is abstract new Debugger.Debugger_Root with private;

   overriding procedure Set_Register
     (Debugger : access Base_Gdb_Debugger;
      Name     : String;
      Value    : String);

   ----------------------
   -- Version handling --
   ----------------------

   type Version_Number is record
      Major : Natural;
      Minor : Natural;
   end record;

   Unknown_Version : constant Version_Number := (Major => 0, Minor => 0);

   function Parse_GDB_Version (Output : String) return Version_Number;

private

   use GNAT.Regpat;
   use GVD.Process;

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

   type Base_Gdb_Debugger is abstract new Debugger.Debugger_Root with record
      Initializing            : Boolean := False;
      Executable              : GNATCOLL.VFS.Virtual_File;
      Executable_Args         : GNAT.Strings.String_Access;
      Stored_Language         : GNAT.Strings.String_Access;
      Endian                  : Endian_Type     := Unknown_Endian;
      Debuggee_Pid            : Integer         := 0;

      Target_Connected        : Boolean         := False;
      --  Whether we have connected to a target.

      Remote_Mode             : Remote_GDB_Mode := Native;
      Has_Wtx_Add_Symbol_File : GNATCOLL.Tribooleans.Triboolean :=
        GNATCOLL.Tribooleans.Indeterminate;
      Registers               : GVD.Types.Strings_Vectors.Vector;
   end record;

   procedure Prepare_Target_For_Send
     (Debugger : access Base_Gdb_Debugger;
      Cmd      : String);
   --  Prepare Tergate before Send

   procedure Detect_Debugger_Mode (Debugger : access Base_Gdb_Debugger);
   --  This detects the debugger mode depending on the remote protocol being
   --  used. Ideally, it should be called every time we change the remote
   --  protocol.

   procedure Test_If_Has_Command
     (Debugger : access Base_Gdb_Debugger;
      Flag     : in out GNATCOLL.Tribooleans.Triboolean;
      Command  : String);
   --  Test whether the command is supported by the current version of gdb.
   --  Flag should be set to Indeterminate initially, and will be set to either
   --  True or False depending on whether the command is supported or not.
   --  Further calls to this subprogram will do nothing.

   procedure Internal_Parse_Value
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Entity     : String;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive;
      Parent     : GVD.Variables.Types.GVD_Type_Holder);
   --  Internal function used to parse the value.
   --  The parameters are the same as for Parse_Value, plus Parent that is
   --  the item that contains Result.
   --  Parent should be null for the top-level item.

   Undefined_Command : constant String := "Undefined command";
   --  String used to detect undefined commands

   Failed_To_Connect_Pattern : constant String := "timed out";
   --  String used to detect failures when trying to connect remotely

   procedure Question_Filter1
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect questions from gdb

   procedure Question_Filter2
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect y/n questions from gdb

   procedure Language_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect a change in the current language

   procedure Remove_Breakpoint_Duplicates
     (Debugger : access Base_Gdb_Debugger'Class;
      Num      : GVD.Types.Breakpoint_Identifier);
   --  remove breakpoint with given number and all others in same location
   --  if the location has more than one breakpoint

   procedure Continuation_Line_Filter
     (Process : access Visual_Debugger_Record'Class;
      Str     : String;
      Matched : Match_Array);
   --  Filter used to detect commands handled on multiple lines

end Debugger.Base_Gdb;
