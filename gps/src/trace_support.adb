------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

   type Factory_Decorator is new Trace_Handle_Record with record
      Previous : Byte_Count := 0;
   end record;
   overriding procedure Post_Decorator
      (Handle   : in out Factory_Decorator;
       Stream   : in out Trace_Stream_Record'Class;
       Location : String;
       Entity   : String;
       Message  : String);
   --  A decorator that displays the total amount of memory allocated so far
   --  in the application.

   type Ada_Factory_Decorator is new Trace_Handle_Record with record
      Previous : Byte_Count := 0;
   end record;
   overriding procedure Post_Decorator
      (Handle   : in out Ada_Factory_Decorator;
       Stream   : in out Trace_Stream_Record'Class;
       Location : String;
       Entity   : String;
       Message  : String);
   --  A decorator that displays the total amount of memory allocated from Ada
   --  so far in the application.

   function Memory_Factory return Trace_Handle is (new Factory_Decorator);
   function Memory_Ada_Factory return Trace_Handle
      is (new Ada_Factory_Decorator);
   --  Build the decorators

   --------------------------
   -- Add_Trace_Decorators --
   --------------------------

   procedure Add_Trace_Decorators is
   begin
      Add_Global_Decorator
         (Create ("DEBUG.ADA_MEMORY", Off,
                  Factory => Memory_Ada_Factory'Access));
      Add_Global_Decorator
         (Create ("DEBUG.MEMORY", Off, Factory => Memory_Factory'Access));
   end Add_Trace_Decorators;

   -------------------
   -- Pre_Decorator --
   -------------------

   overriding procedure Post_Decorator
      (Handle   : in out Ada_Factory_Decorator;
       Stream   : in out Trace_Stream_Record'Class;
       Location : String;
       Entity   : String;
       Message  : String)
   is
      pragma Unreferenced (Location, Entity, Message);
      Watermark : constant Watermark_Info := Get_Ada_Allocations;
   begin
      if Watermark.High /= 0 then
         Put (Stream, " [AdaWatermark:"
            & (if Watermark.Current > Handle.Previous then '>' else '<')
            & Watermark.Current'Img & '/'
            & Watermark.High'Img & "]");
      end if;
      Handle.Previous := Watermark.Current;
   end Post_Decorator;

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
      pragma Unreferenced (Location, Entity, Message);
      Watermark : constant Watermark_Info := Get_Allocations;
   begin
      Put (Stream, " [Watermark:"
         & (if Watermark.Current > Handle.Previous then '>' else '<')
         & Watermark.Current'Img & '/'
         & Watermark.High'Img & "]");
      Handle.Previous := Watermark.Current;
   end Post_Decorator;

end Trace_Support;
