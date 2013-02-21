------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GNAT.OS_Lib;
with GNAT.Strings;                     use GNAT.Strings;

with Language_Handlers;                use Language_Handlers;

package body GPS.Core_Kernels is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Core_Kernel'Class) is
      Tmp : String_Access := GNAT.OS_Lib.Getenv ("VALGRIND");
   begin
      --  Most of the rest if for the sake of memory leaks checkin, and since
      --  it can take a while for big projects we do not do this in normal
      --  times.
      if Tmp.all /= "" then

         GNATCOLL.Scripts.Destroy (Self.Scripts);

         --  Destroy the entities database after we have finalized the
         --  scripting languages, in case some class instances were still
         --  owning references to entities

         Standard.Xref.Destroy (Self.Database);
      end if;

      Free (Tmp);

      Projects.Destroy (Self.Registry);

      --  Remove the language handlers (used for xref). This needs to be done
      --  after finalizing the xref database, source_files contain a pointer
      --  to their language handler.

      Destroy (Self.Lang_Handler);

      GNATCOLL.Symbols.Free (Self.Symbols);
   end Destroy;

   -----------------
   -- Get_Scripts --
   -----------------

   function Get_Scripts
     (Kernel : access Core_Kernel'Class)
      return GNATCOLL.Scripts.Scripts_Repository is
   begin
      return Kernel.Scripts;
   end Get_Scripts;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Core_Kernel'Class) is
      Handler : Language_Handler;
   begin
      Self.Symbols := GNATCOLL.Symbols.Allocate;
      Create_Handler (Handler, Self.Symbols);
      Self.Lang_Handler := Handler;
      Self.Create_Registry;
      Set_Registry (Self.Lang_Handler, Self.Registry);
      Self.Create_Database;
      Self.Create_Scripts_Repository;
   end Initialize;

end GPS.Core_Kernels;
