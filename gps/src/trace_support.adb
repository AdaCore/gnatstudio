------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2015, AdaCore                          --
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

with GNATCOLL.Memory;   use GNATCOLL.Memory;
with GNATCOLL.Traces;   use GNATCOLL.Traces;

package body Trace_Support is

   type Factory_Decorator is new Trace_Handle_Record with null record;
   overriding procedure Post_Decorator
      (Handle   : in out Factory_Decorator;
       Stream   : in out Trace_Stream_Record'Class;
       Location : String;
       Entity   : String;
       Message  : String);
   --  A decorator that displays the total amount of memory allocated so far
   --  in the application.

   function Memory_Factory return Trace_Handle is (new Factory_Decorator);
   --  Build the DEBUG.MEMORY decorator

   --------------------------
   -- Add_Trace_Decorators --
   --------------------------

   procedure Add_Trace_Decorators is
   begin
      Add_Global_Decorator
         (Create ("DEBUG.MEMORY", Off, Factory => Memory_Factory'Access));
   end Add_Trace_Decorators;

   -------------------
   -- Pre_Decorator --
   -------------------

   overriding procedure Post_Decorator
      (Handle   : in out Factory_Decorator;
       Stream   : in out Trace_Stream_Record'Class;
       Location : String;
       Entity   : String;
       Message  : String)
   is
      pragma Unreferenced (Handle, Location, Entity, Message);
      Watermark : constant Watermark_Info := Get_Ada_Allocations;
   begin
      if Watermark.High /= 0 then
         Put (Stream, " [Watermark:" & Watermark.Current'Img & '/'
            & Watermark.High'Img & "]");
      end if;
   end Post_Decorator;

end Trace_Support;
