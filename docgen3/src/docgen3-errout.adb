------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with GPS.Kernel.Console;  use GPS.Kernel.Console;

package body Docgen3.Errout is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Output_Message
     (Context : access constant Docgen_Context;
      Loc     : General_Location;
      Msg     : String;
      Mode    : Message_Type);

   -----------
   -- Error --
   -----------

   procedure Error
     (Context : access constant Docgen_Context;
      Loc     : General_Location;
      Msg     : String)
   is
      Prefix : constant String := "error: ";
   begin
      if Context.Options.Report_Errors /= None then
         Output_Message
           (Context, Loc, Prefix & Msg, Mode => Error);
      end if;
   end Error;

   procedure Error
     (Context : access constant Docgen_Context;
      Entity  : General_Entity;
      Msg     : String)
   is
      Decl : constant General_Entity_Declaration :=
               Get_Declaration (Context.Database, Entity);
   begin
      Error (Context, Decl.Loc, Msg);
   end Error;

   --------------------
   -- Output_Message --
   --------------------

   procedure Output_Message
     (Context : access constant Docgen_Context;
      Loc     : General_Location;
      Msg     : String;
      Mode    : Message_Type)
   is
      Line : constant String := Natural'Image (Loc.Line);
      Col  : constant String := Natural'Image (Integer (Loc.Column));
      Err  : constant String :=
               Loc.File.Display_Base_Name & ":" &
               Line (Line'First + 1 .. Line'Last) & ":" &
               Col (Col'First + 1 .. Col'Last) & ": " & Msg;
   begin
      Insert (Context.Kernel, Err, Mode => Mode);
   end Output_Message;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Context : access constant Docgen_Context;
      Loc     : General_Location;
      Msg     : String)
   is
      Prefix : constant String := "warning: ";
   begin
      if Context.Options.Report_Errors = Errors_And_Warnings then
         Output_Message (Context, Loc, Prefix & Msg, Mode => Info);
      end if;
   end Warning;

   procedure Warning
     (Context : access constant Docgen_Context;
      Entity  : General_Entity;
      Msg     : String)
   is
      Decl : constant General_Entity_Declaration :=
               Get_Declaration (Context.Database, Entity);
   begin
      Warning (Context, Decl.Loc, Msg);
   end Warning;

end Docgen3.Errout;
