------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

package body GPS.LSP_Client.Requests.Generic_References is

   -------------
   -- Execute --
   -------------

   function Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access) return Reference is
   begin
      return Result : Reference do
         Result.Initialize (GPS.LSP_Client.Requests.Request_Access (Request));
         Execute (Language, GPS.LSP_Client.Requests.Request_Access (Request));
      end return;
   end Execute;

   -----------------
   -- Has_Request --
   -----------------

   function Has_Request (Self : Reference) return Boolean is
   begin
      return Self.Request /= null;
   end Has_Request;

   -------------
   -- Request --
   -------------

   function Request (Self : Reference) return Request_Access is
   begin
      if Self.Request = null
        or else Self.Request.all not in Request_Type'Class
      then
         return null;

      else
         return Request_Access (Self.Request);
      end if;
   end Request;

   ------------------
   -- To_Reference --
   ------------------

   function To_Reference
     (Self : Reference) return GPS.LSP_Client.Requests.Reference is
   begin
      return Result : GPS.LSP_Client.Requests.Reference do
         Result.Initialize (Self.Request);
      end return;
   end To_Reference;

   ------------------
   -- To_Reference --
   ------------------

   function To_Reference
     (Self : GPS.LSP_Client.Requests.Reference) return Reference is
   begin
      return Result : Reference do
         if Self.Request /= null
           and then Self.Request.all in Request_Type'Class
         then
            --  Initialize reference when request has compatible type. In cases
            --  of incompatible type "null" reference will be returned.

            Result.Initialize (Self.Request);
         end if;
      end return;
   end To_Reference;

end GPS.LSP_Client.Requests.Generic_References;
