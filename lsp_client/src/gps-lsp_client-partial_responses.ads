------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2021-2023, AdaCore                   --
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

with Ada.Unchecked_Deallocation;

with LSP.Client_Notification_Receivers;
with LSP.Messages;
with LSP.Types;

package GPS.LSP_Client.Partial_Responses is

   ------------------------------
   -- Partial_Response_Handler --
   ------------------------------

   type Partial_Response_Handler is abstract tagged null record;

   type Partial_Response_Handler_Access is
     access all Partial_Response_Handler'Class;

   function Get_Progress_Type
     (Self  : Partial_Response_Handler)
      return LSP.Client_Notification_Receivers.Progress_Value_Kind is
     abstract;

   procedure Process_Partial_Response
        (Self   : Partial_Response_Handler;
         Token  : LSP.Types.LSP_Number_Or_String;
         Vector : LSP.Messages.SymbolInformation_Vector) is null;

   procedure Free is new Ada.Unchecked_Deallocation
     (Partial_Response_Handler'Class, Partial_Response_Handler_Access);

end GPS.LSP_Client.Partial_Responses;
