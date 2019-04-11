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

package body GPS.LSP_Client.Language_Servers is

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Self     : in out Abstract_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access) is
   begin
      Self.Manager.Associated (Document);
      Self.Text_Documents.Insert (Document.File, Document);
   end Associate;

   ----------------
   -- Dissociate --
   ----------------

   procedure Dissociate
     (Self     : in out Abstract_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access) is
   begin
      Self.Text_Documents.Delete (Document.File);
      Self.Manager.Dissociated (Document);
   end Dissociate;

   --------------------
   -- Dissociate_All --
   --------------------

   procedure Dissociate_All (Self : in out Abstract_Language_Server'Class) is
   begin
      while not Self.Text_Documents.Is_Empty loop
         Self.Dissociate
           (GPS.LSP_Client.Text_Documents.Text_Document_Handler_Maps.Element
              (Self.Text_Documents.First));
      end loop;
   end Dissociate_All;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Self    : in out Abstract_Language_Server;
      Request : in out GPS.LSP_Client.Requests.Request_Access)
   is
      pragma Unreferenced (Self);

      use type GPS.LSP_Client.Requests.Request_Access;

   begin
      if Request = null then
         raise Constraint_Error;
      end if;

      --  Stub for language server can't execute any requests, thus reject it.

      Request.On_Rejected;
      Request.Finalize;
      Free (Request);
   end Execute;

   -------------------
   -- Text_Document --
   -------------------

   function Text_Document
     (Self : Abstract_Language_Server'Class;
      File : GNATCOLL.VFS.Virtual_File)
      return GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access
   is
      Position : constant
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Maps.Cursor
        := Self.Text_Documents.Find (File);

   begin
      if GPS.LSP_Client.Text_Documents.Text_Document_Handler_Maps.Has_Element
        (Position)
      then
         return
           GPS.LSP_Client.Text_Documents.Text_Document_Handler_Maps.Element
             (Position);

      else
         return null;
      end if;
   end Text_Document;

end GPS.LSP_Client.Language_Servers;
