------------------------------------------------------------------------------
--                         P S - For Windows NT/2000                        --
--                                                                          --
--                            Copyright (C) 2001                            --
--                                ACT-Europe                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------------

with GNAT.IO; use GNAT.IO;
with Interfaces.C;
with System;

procedure Winps is

   pragma Linker_Options ("-lpsapi");

   PROCESS_QUERY_INFORMATION : constant := 16#400#;
   PROCESS_VM_READ           : constant := 16#010#;

   type HANDLE is new System.Address;
   type HMODULE is new System.Address;
   type DWORD is new Interfaces.C.Unsigned_Long;
   type BOOL is new Integer;

   Null_Handle : constant HANDLE  := HANDLE (System.Null_Address);

   type Handle_Set is array (Positive range <>) of aliased DWORD;

   function EnumProcesses
     (lpidProcess : access DWORD;
      cb          : in DWORD;
      cbNeeded    : access DWORD) return BOOL;
   pragma Import (Stdcall, EnumProcesses, "EnumProcesses");

   function EnumProcessModules
     (hProcess  : HANDLE;
      lphModule : access DWORD;
      len       : DWORD;
      cb        : access DWORD) return BOOL;
   pragma Import (Stdcall, EnumProcessModules, "EnumProcessModules");

   function OpenProcess
     (dwDesiredAccess : DWORD;
      bInheritHandle  : BOOL;
      dwProcessId     : DWORD) return HANDLE;
   pragma Import (Stdcall, OpenProcess, "OpenProcess");

   procedure CloseHandle (H : HANDLE);
   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   function GetModuleBaseName
     (hProcess   : HANDLE;
      haModule   : DWORD;
      lpBaseName : System.Address;
      nSize      : DWORD) return DWORD;
   pragma Import (Stdcall, GetModuleBaseName, "GetModuleBaseNameA");

   Max_Process : constant := 1_024;

   -------------------
   -- Print_Process --
   -------------------

   procedure Print_Process (PID : DWORD) is
      hProcess      : HANDLE;
      hMod          : Handle_Set (1 .. Max_Process);
      cbNeeded      : aliased DWORD;
      szProcessName : Interfaces.C.char_array (1 .. 1_024);
      Failure       : Boolean := False;

      procedure Print_PID (PID : DWORD);
      --  print process ID using 6 digits.

      procedure Print_Process_Name (Name : String);
      --  print the process ID and output a newline.

      ---------------
      -- Print_PID --
      ---------------

      procedure Print_PID (PID : DWORD) is
         PID_Str : constant String := "     " & DWORD'Image (PID);
      begin
         Put (PID_Str (PID_Str'Last - 5 .. PID_Str'Last));
      end Print_PID;

      ------------------------
      -- Print_Process_Name --
      ------------------------

      procedure Print_Process_Name (Name : String) is
      begin
         Put_Line ("  " & Name);
      end Print_Process_Name;

   begin
      Print_PID (PID);

      if PID = 0 then
         Print_Process_Name ("System Idle Process");
         return;
      elsif PID < 16 then
         Print_Process_Name ("System");
         return;
      end if;

      hProcess :=
        OpenProcess
          (PROCESS_QUERY_INFORMATION + PROCESS_VM_READ, 0, PID);

      if hProcess = Null_Handle then
         Failure := True;
      else
         if EnumProcessModules
           (hProcess,
            hMod (hMod'First)'Unchecked_Access,
            hMod'Length,
            cbNeeded'Unchecked_Access) = 0
         then
            Failure := True;
         else
            --  Fill szProcessName with the program name.

            if GetModuleBaseName
              (hProcess,
               hMod (hMod'First),
               szProcessName'Address,
               1024) = 0
            then
               Failure := True;
            end if;
         end if;
      end if;

      if Failure then
         Print_Process_Name ("???");
         return;
      end if;

      declare
         Process_Name : constant String := Interfaces.C.To_Ada (SzProcessName);      begin
         if Process_Name = "" then
            Print_Process_Name ("???");
         else
            Print_Process_Name (Process_Name);
         end if;
      end;

      CloseHandle (hProcess);
   end Print_Process;

   All_Processes : aliased Handle_Set (1 .. Max_Process);
   N_Bytes       : aliased DWORD;
   N_Process     : Positive;

begin
   if EnumProcesses
     (All_Processes (All_Processes'First)'Unchecked_Access,
      DWORD (All_Processes'Length),
      N_Bytes'Unchecked_Access) = 0
   then
      Put_Line ("Internal error: PSAPI not working on this computer.");
   end if;

   --  Compute the number of process returned in the array. The EnumProcesses
   --  returns the number of bytes filled in the array.

   N_Process := Natural (N_Bytes) * 8 / DWORD'Size;

   Put_Line ("   PID  Command");

   for K in 1 .. N_Process loop
      Print_Process (All_Processes (K));
   end loop;
end Winps;
