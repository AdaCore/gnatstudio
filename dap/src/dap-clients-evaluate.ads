------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with DAP.Requests.Evaluate;

package DAP.Clients.Evaluate is

   type Evaluate_Request (<>) is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with private;
   type Evaluate_Request_Access is access all Evaluate_Request'Class;

   function Create_Command
     (Client            : DAP_Client;
      Command           : VSS.Strings.Virtual_String;
      Output            : Boolean := False;
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null)
      return Evaluate_Request_Access;
   --  Creates the request that executes the Command. Show the result in the
   --  debugger console if Output is true. On_* callbacks are used for the
   --  Python API.

   function Create_Value_Of
     (Client : DAP_Client;
      Label  : Gtk.Label.Gtk_Label;
      Cmd    : VSS.Strings.Virtual_String)
      return Evaluate_Request_Access;
   --  Creates the request to get the value of the Cmd

   function Create_Variable_Address
     (Client   : DAP_Client;
      Variable : String)
      return Evaluate_Request_Access;
   --  Creates the request to get the variable's address

   function Create_Set_TTY
     (Client : DAP_Client;
      TTY    : String)
      return Evaluate_Request_Access;
   --  Creates the request to set TTY

   function Create_Show_Endian
     (Client : DAP_Client)
      return Evaluate_Request_Access;
   --  Creates the request to get endian

private

   type Evaluate_Kind is
     (Hover,
      --  Used to display variable values in tooltips

      Variable_Address,
      --  Used to get the variable address to open the memory view

      Endian,
      --  Used to get target's Endian, needed for the memory view

      Command,
      --  Used for any other command (e.g: console)

      Set_TTY
      --  Used to set tty to have debuggee console
     );

   type Evaluate_Request is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with record
      Kind   : Evaluate_Kind := Hover;
      Label  : Gtk.Label.Gtk_Label;
      Output : Boolean := False;

      ---------------
      -- Callbacks --
      ---------------

      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null;
   end record;

   overriding procedure Finalize (Self : in out Evaluate_Request);
   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);
   overriding procedure On_Rejected
     (Self   : in out Evaluate_Request;
      Client : not null access DAP.Clients.DAP_Client'Class);
   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   function Create
     (Client            : DAP_Client;
      Kind              : Evaluate_Kind;
      Cmd               : VSS.Strings.Virtual_String;
      Output            : Boolean := False;
      On_Result_Message : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Error_Message  : GNATCOLL.Scripts.Subprogram_Type := null;
      On_Rejected       : GNATCOLL.Scripts.Subprogram_Type := null)
      return Evaluate_Request_Access;
   --  Creates the request

end DAP.Clients.Evaluate;
