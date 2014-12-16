------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                         Copyright (C) 2014, AdaCore                      --
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

   procedure Start_Paragraph (Self : not null access Markup_Generator);
   --  Generate start event for 'p' tag.

   procedure End_Paragraph (Self : not null access Markup_Generator);
   --  Generate end event for 'p' tag.

   procedure Start_List (Self : not null access Markup_Generator);
   --  Generate start event for 'ul' tag.

   procedure End_List (Self : not null access Markup_Generator);
   --  Generate end event for 'ul' tag.

   procedure Start_List_Item (Self : not null access Markup_Generator);
   --  Generate start event for 'li' tag.

   procedure End_List_Item (Self : not null access Markup_Generator);
   --  Generate end event for 'li' tag.

   procedure Text (Self : not null access Markup_Generator; Text : String);
   --  Generate text event.

   procedure HTML (Self : not null access Markup_Generator; HTML : String);
   --  Generate start event for 'html' tag, text event and end event for
   --  'html' text.

private

   type Markup_Generator is tagged limited record
      Stream : GNATdoc.Markup_Streams.Event_Vectors.Vector;
   end record;

end GNATdoc.Customization.Markup_Generators;
