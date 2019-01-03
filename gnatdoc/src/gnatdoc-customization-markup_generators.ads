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
--  Auxiliary type to help to generate markup event stream by Python plugins.

with GNATdoc.Markup_Streams;

package GNATdoc.Customization.Markup_Generators is

   type Markup_Generator is tagged limited private;

   procedure Start_Paragraph
     (Self       : not null access Markup_Generator;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map);
   --  Generate start event for 'p' tag.

   procedure End_Paragraph (Self : not null access Markup_Generator);
   --  Generate end event for 'p' tag.

   procedure Start_List
     (Self       : not null access Markup_Generator;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map);
   --  Generate start event for 'ul' tag.

   procedure End_List (Self : not null access Markup_Generator);
   --  Generate end event for 'ul' tag.

   procedure Start_List_Item
     (Self       : not null access Markup_Generator;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map);
   --  Generate start event for 'li' tag.

   procedure End_List_Item (Self : not null access Markup_Generator);
   --  Generate end event for 'li' tag.

   procedure Image
     (Self : not null access Markup_Generator;
      File : String);
   --  Generate set of events to insert image into the document.

   procedure Switch_To_After_Stream (Self : not null access Markup_Generator);
   --  Switch generator to process events of stream that will be inserted after
   --  current element.

   procedure Switch_To_Inline_Stream (Self : not null access Markup_Generator);
   --  Switch generator to process events of stream that will be inserted
   --  inside current element.

   procedure Text
     (Self       : not null access Markup_Generator;
      Text       : String;
      Attributes : GNATdoc.Markup_Streams.Name_Value_Maps.Map);
   --  Generate text event.

   procedure HTML (Self : not null access Markup_Generator; HTML : String);
   --  Generate start event for 'html' tag, text event and end event for
   --  'html' text.

   function Get_Inline_Stream
     (Self : Markup_Generator)
      return GNATdoc.Markup_Streams.Event_Vectors.Vector;
   --  Returns stream on elements to be inserted into current element

   function Get_After_Stream
     (Self : Markup_Generator)
      return GNATdoc.Markup_Streams.Event_Vectors.Vector;
   --  Returns stream of elements to be inserted after current element

private

   type Stream_Kinds is (Inline_Stream, After_Stream);

   type Stream_Array is
     array (Stream_Kinds) of GNATdoc.Markup_Streams.Event_Vectors.Vector;

   type Markup_Generator is tagged limited record
      Streams : Stream_Array;
      Current : Stream_Kinds := Inline_Stream;
   end record;

end GNATdoc.Customization.Markup_Generators;
