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

with System;
with Interfaces.C;

separate (GVD.Proc_Utils) 

package body Builtin is

   pragma Linker_Options ("-lpsapi");

   PROCESS_QUERY_INFORMATION : constant := 16#400#;
   PROCESS_VM_READ           : constant := 16#010#;

   type WIN_HANDLE is new System.Address;
   type HMODULE is new System.Address;
   type DWORD is new Interfaces.C.Unsigned_Long;
   type BOOL is new Integer;

   Null_Handle : constant WIN_HANDLE := WIN_HANDLE (System.Null_Address);

   type Handle_Set is array (Positive range <>) of aliased DWORD;

   Max_Process : constant := 1024;

   type Builtin_Record is record
      Processes : aliased Handle_Set (1 .. Max_Process);
      N_Process : Natural := 0;
      Index     : Positive := 1;
   end record;

   procedure Free is new
     Ada.Unchecked_Deallocation (Builtin_Record, Builtin_Handle);

   function EnumProcesses
     (lpidProcess : access DWORD;
      cb          : in DWORD;
      cbNeeded    : access DWORD) return BOOL;
   pragma Import (Stdcall, EnumProcesses, "EnumProcesses");

   function EnumProcessModules
     (hProcess   : WIN_HANDLE;
      lphModule  : access DWORD;
      cb         : DWORD;
      lpcbNeeded : access DWORD) return BOOL;
   pragma Import (Stdcall, EnumProcessModules, "EnumProcessModules");

   function OpenProcess
     (dwDesiredAccess : DWORD;
      bInheritHandle  : BOOL;
      dwProcessId     : DWORD) return WIN_HANDLE;
   pragma Import (Stdcall, OpenProcess, "OpenProcess");

   procedure CloseHandle (H : WIN_HANDLE);
   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   function GetModuleBaseName
     (hProcess   : WIN_HANDLE;
      haModule   : DWORD;
      lpBaseName : System.Address;
      nSize      : DWORD) return DWORD;
   pragma Import (Stdcall, GetModuleBaseName, "GetModuleBaseNameA");

   --------------------
   -- Is_Implemented --
   --------------------

   function Is_Implemented return Boolean is
   begin
      return True;
   end Is_Implemented;

   ---------------------
   -- Close_Processes --
   ---------------------

   procedure Close_Processes (Handle : in out Builtin_Handle) is
   begin
      Free (Handle);
   end Close_Processes;

   ------------------
   -- Next_Process --
   ------------------

   procedure Next_Process
     (Handle  : Builtin_Handle;
      Info    : out Process_Info;
      Success : out Boolean)
   is
      function Process_Info (PID : DWORD) return String;
      --  Return info about a given process ID.

      function Process_Info (PID : DWORD) return String is
         hProcess : WIN_HANDLE;
         hMod     : Handle_Set (1 .. Max_Process);
         cbNeeded : aliased DWORD;
         szProcessName : Interfaces.C.char_array (1 .. 1024);

      begin
         if PID = 0 then
            return "System Idle Process";
         elsif PID < 16 then
            return "System";
         end if;

         hProcess :=
           OpenProcess
             (PROCESS_QUERY_INFORMATION + PROCESS_VM_READ, 0, PID);

         if hProcess = Null_Handle then
            return "";
         end if;

         if EnumProcessModules
           (hProcess,
            hMod (1)'Unchecked_Access,
            hMod'Length,
            cbNeeded'Unchecked_Access) /= 0
         then
            if GetModuleBaseName
              (hProcess, hMod (1), szProcessName'Address, 1024) = 0
            then
               CloseHandle (hProcess);
               return "";
            end if;
         end if;

         CloseHandle (hProcess);
         return Interfaces.C.To_Ada (SzProcessName);
      end Process_Info;

   begin
      if Handle.Index >= Handle.N_Process then
         Success := False;
      else
         declare
            Pid   : DWORD renames Handle.Processes (Handle.Index);
            Id1   : constant String := "     " & DWORD'Image (Pid);
            Id2   : constant String := Id1 (Id1'Last - 5 .. Id1'Last);
            PInfo : constant String := Process_Info (Pid);

         begin
            Info :=
              (Id_Len   => Id2'Length,
               Info_Len => PInfo'Length,
               Id       => Id2,
               Info     => PInfo);
            Handle.Index := Handle.Index + 1;
            Success := True;
         end;
      end if;
   end Next_Process;

   --------------------
   -- Open_Processes --
   --------------------

   procedure Open_Processes (Handle : out Builtin_Handle) is
      N_Bytes : aliased DWORD;
   begin
      Handle := new Builtin_Record;

      if EnumProcesses
        (Handle.Processes (1)'Unchecked_Access,
         DWORD (Handle.Processes'Length),
         N_Bytes'Unchecked_Access) = 0
      then
         return;
      end if;

      Handle.N_Process := Natural (N_Bytes) * 8 / DWORD'Size;
   end Open_Processes;

end Builtin;
