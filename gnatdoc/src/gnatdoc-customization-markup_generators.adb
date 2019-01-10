------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                         Copyright (C) 2014-2019, AdaCore                 --
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
      Self.Streams (Self.Current).Append
        ((End_Tag, To_Unbounded_String ("ul")));
   end End_List;

   -------------------
   -- End_List_Item --
   -------------------

   procedure End_List_Item (Self : not null access Markup_Generator) is
   begin
      Self.Streams (Self.Current).Append
        ((End_Tag, To_Unbounded_String ("li")));
   end End_List_Item;

   -------------------
   -- End_Paragraph --
   -------------------

   procedure End_Paragraph (Self : not null access Markup_Generator) is
   begin
      Self.Streams (Self.Current).Append
        ((End_Tag, To_Unbounded_String ("p")));
   end End_Paragraph;

   ----------------------
   -- Get_After_Stream --
   ----------------------

   function Get_After_Stream
     (Self : Markup_Generator)
      return GNATdoc.Markup_Streams.Event_Vectors.Vector is
   begin
      return Self.Streams (After_Stream);
   end Get_After_Stream;

   -----------------------
   -- Get_Inline_Stream --
   -----------------------

   function Get_Inline_Stream
     (Self : Markup_Generator)
      return GNATdoc.Markup_Streams.Event_Vectors.Vector is
   begin
      return Self.Streams (Inline_Stream);
   end Get_Inline_Stream;

   ----------
   -- HTML --
   ----------

   procedure HTML (Self : not null access Markup_Generator; HTML : String) is
   begin
      Self.Streams (Self.Current).Append
        ((Kind       => Start_Tag,
          Name       => To_Unbounded_String ("html"),
          Attributes => <>));
      Self.Streams (Self.Current).Append
        ((Markup_Streams.Text, To_Unbounded_String (HTML)));
      Self.Streams (Self.Current).Append
        ((End_Tag, To_Unbounded_String ("html")));
   end HTML;

   -----------
   -- Image --
   -----------

   procedure Image
     (Self : not null access Markup_Generator;
      File : String)
   is
      Attributes : Name_Value_Maps.Map;

   begin
      Attributes.Insert ("file", File);

      Self.Streams (Self.Current).Append
        ((Kind       => Start_Tag,
          Name       => To_Unbounded_String ("image"),
          Attributes => Attributes));
      Self.Streams (Self.Current).Append
        ((End_Tag, To_Unbounded_String ("image")));
   end Image;

   ----------------
   -- Start_List --
   ----------------

   procedure Start_List
     (Self       : not null access Markup_Generator;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map) is
   begin
      Self.Streams (Self.Current).Append
        ((Kind       => Start_Tag,
          Name       => To_Unbounded_String ("ul"),
          Attributes => Attributes));
   end Start_List;

   ---------------------
   -- Start_List_Item --
   ---------------------

   procedure Start_List_Item
     (Self       : not null access Markup_Generator;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map) is
   begin
      Self.Streams (Self.Current).Append
        ((Kind       => Start_Tag,
          Name       => To_Unbounded_String ("li"),
          Attributes => Attributes));
   end Start_List_Item;

   ---------------------
   -- Start_Paragraph --
   ---------------------

   procedure Start_Paragraph
     (Self       : not null access Markup_Generator;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map) is
   begin
      Self.Streams (Self.Current).Append
        ((Kind       => Start_Tag,
          Name       => To_Unbounded_String ("p"),
          Attributes => Attributes));
   end Start_Paragraph;

   ----------------------------
   -- Switch_To_After_Stream --
   ----------------------------

   procedure Switch_To_After_Stream
     (Self : not null access Markup_Generator) is
   begin
      Self.Current := After_Stream;
   end Switch_To_After_Stream;

   -----------------------------
   -- Switch_To_Inline_Stream --
   -----------------------------

   procedure Switch_To_Inline_Stream
     (Self : not null access Markup_Generator) is
   begin
      Self.Current := Inline_Stream;
   end Switch_To_Inline_Stream;

   ----------
   -- Text --
   ----------

   procedure Text
     (Self       : not null access Markup_Generator;
      Text       : String;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map) is
   begin
      if not Attributes.Is_Empty then
         Self.Streams (Self.Current).Append
           ((Kind       => Start_Tag,
             Name       => To_Unbounded_String ("span"),
             Attributes => Attributes));
      end if;

      Self.Streams (Self.Current).Append
        ((Markup_Streams.Text, To_Unbounded_String (Text)));

      if not Attributes.Is_Empty then
         Self.Streams (Self.Current).Append
           ((Kind => End_Tag,
             Name => To_Unbounded_String ("span")));
      end if;
   end Text;

end GNATdoc.Customization.Markup_Generators;
