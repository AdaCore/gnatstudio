------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with GPS.Kernel;
with Ada.Strings.Unbounded;
with VSS.Strings;
with LSP.Raw_Clients;
with LSP.Types; use LSP.Types;

package LSP.DAP_Clients is

   type Client is new LSP.Raw_Clients.Raw_Client with private;

   procedure Initialize
     (Self   : in out Client;
      Kernel :        access GPS.Kernel.Kernel_Handle_Record'Class);

   function Started (Self : in out Client) return Boolean;

   function Configured (Self : in out Client) return Boolean;

   function Running (Self : in out Client) return Boolean;

   function Get_Request_ID (Self : in out Client) return LSP.Types.LSP_Number;

private

   type Client is new LSP.Raw_Clients.Raw_Client with record
      Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Request_Id    : LSP.Types.LSP_Number := 0;
      Error_Message : VSS.Strings.Virtual_String;
      Started       : Boolean              := False;
      Configured    : Boolean              := False;
      Running       : Boolean              := False;
   end record;

   overriding function Error_Message
     (Self : Client) return VSS.Strings.Virtual_String;

   overriding procedure On_Raw_Message
     (Self    : in out Client; Data : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean);

end LSP.DAP_Clients;
