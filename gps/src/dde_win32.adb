-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
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

--  Windows implementation

with System;
with Interfaces.C;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions;    use Ada.Exceptions;

with GPS.Kernel.Standard_Hooks;
with Traces;            use Traces;
with VFS;               use VFS;

package body DDE is

   Me : constant Debug_Handle := Create ("DDE");

   type DDE_Operation is (Unsupported, FileOpen);
   --  DDE operations supported by GPS

   --  Win32 constants, types and subprograms supporting DDEML

   XTYP_EXECUTE              : constant := 16#4050#;  --  ddeml.h:150
   XTYP_CONNECT              : constant := 16#1062#;  --  ddeml.h:151
   XTYP_ADVDATA              : constant := 16#4010#;  --  ddeml.h:146

   DDE_FACK                  : constant := 16#8000#;  --  ddeml.h:110

   CBF_FAIL_SELFCONNECTIONS  : constant := 16#1000#;  --  ddeml.h:220
   APPCMD_FILTERINITS        : constant := 16#20#;    --  ddeml.h:238
   CBF_FAIL_POKES            : constant := 16#10000#; --  ddeml.h:224
   CBF_SKIP_CONNECT_CONFIRMS : constant := 16#40000#; --  ddeml.h:228

   DNS_REGISTER              : constant := 16#1#;     --  ddeml.h:289

   subtype INT     is Interfaces.C.int;               --  windef.h
   subtype UINT    is Interfaces.C.unsigned;          --  windef.h
   subtype DWORD   is Interfaces.C.unsigned_long;     --  windef.h
   type    LPDWORD is access all DWORD;               --  windef.h

   type HCONV      is new DWORD;                      --  ddeml.h:23
   type HSZ        is new DWORD;                      --  ddeml.h:24
   type HDDEDATA   is new DWORD;                      --  ddeml.h:25

   type PFNCALLBACK is access function
     (wType      : UINT;
      wFmt       : UINT;
      hCnv       : HCONV;
      hsz1, hsz2 : HSZ;
      hData      : HDDEDATA;
      dwData1    : DWORD;
      dwData2    : DWORD) return HDDEDATA;
   pragma Convention (Stdcall, PFNCALLBACK);
   --  ddeml.h:204

   function DdeInitializeA
     (pidInst   : LPDWORD;
      pfnCallbk : PFNCALLBACK;
      afCmd     : DWORD;
      ulRes     : DWORD) return UINT;
   pragma Import (Stdcall, DdeInitializeA, "DdeInitializeA");
   --  ddeml.h:211

   function DdeUninitialize (idInst : DWORD) return INT;
   pragma Import (Stdcall, DdeUninitialize, "DdeUninitialize");
   --  ddeml.h:252

   function DdeNameService
     (idInst     : DWORD;
      hsz1, hsz2 : HSZ;
      afCmd      : UINT) return HDDEDATA;
   pragma Import (Stdcall, DdeNameService, "DdeNameService");
   --  ddeml.h:289

   function DdeAccessData
     (hData       : HDDEDATA;
      pcbDataSize : LPDWORD) return System.Address;
   pragma Import (Stdcall, DdeAccessData, "DdeAccessData");
   --  ddeml.h:312

   function DdeUnaccessData (hData : HDDEDATA) return INT;
   pragma Import (Stdcall, DdeUnaccessData, "DdeUnaccessData");
   --  ddeml.h:313

   function DdeCreateStringHandleA
     (idInst    : DWORD;
      psz       : System.Address;
      iCodePage : INT) return HSZ;
   pragma Import (Stdcall, DdeCreateStringHandleA, "DdeCreateStringHandleA");
   --  ddeml.h:346

   --  End of DDEML section

   Kernel_Local : GPS.Kernel.Kernel_Handle;
   --  Receives the kernel handle passed to Register_DDE_Server

   idInst : aliased DWORD := 0; --  DDE Instance

   function DDE_Callback
     (wType      : UINT;
      wFmt       : UINT;
      hCnv       : HCONV;
      hsz1, hsz2 : HSZ;
      hData      : HDDEDATA;
      dwData1    : DWORD;
      dwData2    : DWORD) return HDDEDATA;
   pragma Convention (Stdcall, DDE_Callback);

   ------------------
   -- DDE_Callback --
   ------------------

   function DDE_Callback
     (wType      : UINT;
      wFmt       : UINT;
      hCnv       : HCONV;
      hsz1, hsz2 : HSZ;
      hData      : HDDEDATA;
      dwData1    : DWORD;
      dwData2    : DWORD) return HDDEDATA
   is
      Res      : INT;
      pragma Unreferenced (wFmt, hCnv, hsz1, hsz2, dwData1, dwData2, Res);

   begin
      case wType is
         when XTYP_ADVDATA =>
            return DDE_FACK;

         when XTYP_CONNECT =>
            return 1;

         when XTYP_EXECUTE =>

            declare
               Data_Len : aliased DWORD;
               Data_Raw : constant System.Address :=
                 DdeAccessData (hData, Data_Len'Unchecked_Access);
               Data : String (1 .. Integer (Data_Len));
               for Data'Address use Data_Raw;

               Pos       : constant Natural := Index (Data, ":");
               Operation : DDE_Operation;
               Argument  : constant String :=
                 Data (Pos + 1 .. Index (Data, (1 => ASCII.NUL)) - 1);
               --  The data block is terminated by a nul character

            begin
               begin
                  Operation := DDE_Operation'Value (Data (1 .. Pos - 1));
               exception
                  when Constraint_Error =>
                     Operation := Unsupported;
                     --  Catch an unsupported DDE operation
               end;

               case Operation is
                  when Unsupported =>
                     null;
                  when FileOpen =>
                     begin
                        GPS.Kernel.Standard_Hooks.Open_File_Editor
                          (Kernel_Local, Create (Full_Filename => Argument));
                     exception
                        when Constraint_Error =>
                           --  ??? Currently Constraint_Error is raised if
                           --  file editor is unavailable. This needs redesign
                           --  if better feedback from invalid hooks is
                           --  available.
                           null;
                     end;
               end case;

            end;

            Res := DdeUnaccessData (hData); --  Release the resource

         when others =>
            return 0;
      end case;

      return 1;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return 1;
   end DDE_Callback;

   -------------------------
   -- Register_DDE_Server --
   -------------------------

   procedure Register_DDE_Server (Kernel : GPS.Kernel.Kernel_Handle) is
      hszAppName : HSZ;
      szAppName  : aliased String := "GPS" & ASCII.NUL;
      Res1       : UINT;
      Res2       : HDDEDATA;
      pragma Unreferenced (Res1, Res2);

      use type DWORD;

   begin
      Kernel_Local := Kernel;
      Res1 := DdeInitializeA
        (idInst'Access, --  receives instance identifier
         DDE_Callback'Access, --  callback function
         APPCMD_FILTERINITS or CBF_SKIP_CONNECT_CONFIRMS
           or CBF_FAIL_SELFCONNECTIONS or CBF_FAIL_POKES,
         0);

      hszAppName := DdeCreateStringHandleA (idInst, szAppName (1)'Address, 0);

      Res2 := DdeNameService
        (idInst,   --  instance identifier
         hszAppName, --  handle to service name string
         0, --  reserved
         DNS_REGISTER);
   end Register_DDE_Server;

   ---------------------------
   -- Unregister_DDE_Server --
   ---------------------------

   procedure Unregister_DDE_Server is
      Res1 : INT;
      pragma Unreferenced (Res1);
   begin
      Res1 := DdeUninitialize (idInst);
   end Unregister_DDE_Server;

end DDE;
