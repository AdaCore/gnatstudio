-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

pragma Warnings (Off);
with GNAT.Expect; use GNAT.Expect;
pragma Warnings (On);
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GVD.Preferences; use GVD.Preferences;
with GVD.Strings; use GVD.Strings;
with Ada.Unchecked_Deallocation;

package body GVD.Proc_Utils is

   package Builtin is

      type Builtin_Handle is private;

      function Is_Implemented return Boolean;
      --  Return True when this package is implemented.
      --  False is this package contains only stub, and the generic external
      --  process support should be used.

      procedure Open_Processes (Handle : out Builtin_Handle);
      --  Initialize a connexion to the current machine in order to retrieve
      --  process information.

      procedure Next_Process
        (Handle  : Builtin_Handle;
         Info    : out Process_Info;
         Success : out Boolean);
      --  Return information concerning the next process.
      --  Success is set to True if there is a remaining process.

      procedure Close_Processes (Handle : in out Builtin_Handle);
      --  Close the connexion established in handle.

   private
      type Builtin_Record;
      type Builtin_Handle is access all Builtin_Record;
   end Builtin;

   package body Builtin is separate;

   use Builtin;

   type Process_Record is record
      Builtin    : Builtin_Handle;
      Descriptor : Process_Descriptor;
      Remote     : Boolean;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Process_Record, Process_Handle);

   procedure Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   ---------------------
   -- Close_Processes --
   ---------------------

   procedure Close_Processes (Handle : in out Process_Handle) is
   begin
      Close (Handle.Descriptor);
      Close_Processes (Handle.Builtin);
      Free (Handle);
   end Close_Processes;

   ------------------
   -- Next_Process --
   ------------------

   procedure Next_Process
     (Handle  : Process_Handle;
      Info    : out Process_Info;
      Success : out Boolean)
   is
      Match : Expect_Match := 0;
   begin
      if Is_Implemented and then not Handle.Remote then
         Next_Process (Handle.Builtin, Info, Success);
         return;
      end if;

      Success := False;
      Expect (Handle.Descriptor, Match, "\n");

      if Match = 1 then
         declare
            S     : constant String :=
              Strip_CR (Expect_Out (Handle.Descriptor));
            Index : Integer := S'First;
         begin
            Skip_Blanks (S, Index);
            Skip_To_Char (S, Index, ' ');
            Info :=
              (Id_Len   => Index - S'First + 1,
               Info_Len => S'Last - Index - 1,
               Id       => S (S'First .. Index),
               Info     => S (Index + 1 .. S'Last - 1));
            Success := True;
         end;
      end if;

   exception
      when Process_Died => null;
   end Next_Process;

   --------------------
   -- Open_Processes --
   --------------------

   procedure Open_Processes (Handle : out Process_Handle) is
      Command_Index : Integer := Exec_Command'First;
      Args          : Argument_List_Access;
      Match         : Expect_Match := 0;

   begin
      Handle := new Process_Record;
      Handle.Remote := False;

      if Is_Implemented then
         Open_Processes (Handle.Builtin);
         return;
      end if;

      Skip_To_Char (Exec_Command, Command_Index, ' ');
      Args := Argument_String_To_List
        (Exec_Command (Command_Index + 1 .. Exec_Command'Last));

      declare
         New_Args : Argument_List (Args'First .. Args'Last + 1);
      begin
         New_Args (Args'First .. Args'Last) := Args.all;
         New_Args (New_Args'Last) := new String' (Get_Pref (List_Processes));
         Non_Blocking_Spawn
           (Handle.Descriptor,
            Exec_Command (Exec_Command'First .. Command_Index - 1),
            New_Args);
         Expect (Handle.Descriptor, Match, "\n");

         for J in New_Args'Range loop
            Free (New_Args (J));
         end loop;
      end;

      Free (Args);
   end Open_Processes;

   procedure Open_Processes (Handle : out Process_Handle; Host : String) is
      New_Args : Argument_List (1 .. 2);
      Match    : Expect_Match := 0;

   begin
      Handle := new Process_Record;
      Handle.Remote := True;
      New_Args (2) := new String' (Get_Pref (List_Processes));
      New_Args (1) := new String' (Host);
      Non_Blocking_Spawn
        (Handle.Descriptor, Get_Pref (Remote_Protocol), New_Args);
      Expect (Handle.Descriptor, Match, "\n");
      Free (New_Args (1));
      Free (New_Args (2));
   end Open_Processes;

end GVD.Proc_Utils;
