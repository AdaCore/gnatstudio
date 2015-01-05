------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                         Copyright (C) 2014-2015, AdaCore                 --
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

package body GNATdoc.Customization.Markup_Generators is

   use GNATdoc.Markup_Streams;

   --------------
   -- End_List --
   --------------

   procedure End_List (Self : not null access Markup_Generator) is
   begin
      Self.Stream.Append ((End_Tag, To_Unbounded_String ("ul")));
   end End_List;

   -------------------
   -- End_List_Item --
   -------------------

   procedure End_List_Item (Self : not null access Markup_Generator) is
   begin
      Self.Stream.Append ((End_Tag, To_Unbounded_String ("li")));
   end End_List_Item;

   -------------------
   -- End_Paragraph --
   -------------------

   procedure End_Paragraph (Self : not null access Markup_Generator) is
   begin
      Self.Stream.Append ((End_Tag, To_Unbounded_String ("p")));
   end End_Paragraph;

   ----------
   -- HTML --
   ----------

   procedure HTML (Self : not null access Markup_Generator; HTML : String) is
   begin
      Self.Stream.Append
        ((Start_Tag, To_Unbounded_String ("html"), Null_Unbounded_String));
      Self.Stream.Append ((Markup_Streams.Text, To_Unbounded_String (HTML)));
      Self.Stream.Append ((End_Tag, To_Unbounded_String ("html")));
   end HTML;

   ----------------
   -- Start_List --
   ----------------

   procedure Start_List (Self : not null access Markup_Generator) is
   begin
      Self.Stream.Append
        ((Start_Tag, To_Unbounded_String ("ul"), Null_Unbounded_String));
   end Start_List;

   ---------------------
   -- Start_List_Item --
   ---------------------

   procedure Start_List_Item (Self : not null access Markup_Generator) is
   begin
      Self.Stream.Append
        ((Start_Tag, To_Unbounded_String ("li"), Null_Unbounded_String));
   end Start_List_Item;

   ---------------------
   -- Start_Paragraph --
   ---------------------

   procedure Start_Paragraph (Self : not null access Markup_Generator) is
   begin
      Self.Stream.Append
        ((Start_Tag, To_Unbounded_String ("p"), Null_Unbounded_String));
   end Start_Paragraph;

   ----------
   -- Text --
   ----------

   procedure Text (Self : not null access Markup_Generator; Text : String) is
   begin
      Self.Stream.Append ((Markup_Streams.Text, To_Unbounded_String (Text)));
   end Text;

end GNATdoc.Customization.Markup_Generators;
