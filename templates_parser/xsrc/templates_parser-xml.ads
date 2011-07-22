------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  This API provides a way to save a Translate_Set as an XML document.
--  There is special rules to know about composite tags.
--
--  Composite tags :
--
--     If a tag named TAG exists, then the name TAG_DESCRIPTION is used as a
--     description for this specific tag.
--
--  Composite tags (more than one nested level)
--
--     If a tag named TAG exists, then the names TAG_DIM[n]_LABELS is used as
--     a set of labels for the tag's nth axis. In this case TAG_DIM[n]_LABELS
--     must be a vector tag, each entry corresponds to a label on this
--     axis. Also TAG_DIM[n]_DESCRIPTION is used as a description for this
--     axis.
--
--  Here is the DTD :
--
--  <?xml version="1.0" encoding="UTF-8"?>
--  <!--Description of a tag or dimension (ex: year)-->
--  <!ELEMENT Description (#PCDATA)>
--  <!--a dimension-->
--  <!ELEMENT Dim (Description, Labels)>
--  <!ATTLIST Dim
--      n CDATA #REQUIRED
--  >
--  <!--entry of a CompositeTag-->
--  <!ELEMENT Entry (ind+, V)>
--  <!--label of an indice of a dimension (ex: 2000)-->
--  <!ELEMENT Label (#PCDATA)>
--  <!ATTLIST Label
--      ind CDATA #REQUIRED
--  >
--  <!--list of labels of one dimension (ex: 1999, 2000, 2001)-->
--  <!ELEMENT Labels (Label+)>
--  <!--alias and information-->
--  <!ELEMENT Tag (Name, Description)>
--  <!--tagged data to be published in templates-->
--  <!ELEMENT Tagged (SimpleTag*, CompositeTag*)>
--  <!--simple variable value-->
--  <!ELEMENT V (#PCDATA)>
--  <!ELEMENT ind (#PCDATA)>
--  <!ATTLIST ind
--      n CDATA #REQUIRED
--  >
--  <!--identification name for this tag-->
--  <!ELEMENT Name (#PCDATA)>
--  <!--Tag with no dimension (simple variable)-->
--  <!ELEMENT SimpleTag (Tag, V)>
--  <!--Tag with one or more dimensions-->
--  <!ELEMENT CompositeTag (Tag, Dim+, Entry)>

package Templates_Parser.XML is

   function Image (Translations : Translate_Set) return Unbounded_String;
   --  Returns a string representation encoded in XML for this
   --  translate table.

   function Value (Translations : String) return Translate_Set;
   --  Returns a translate set for this string representation

   function Value (Translations : Unbounded_String) return Translate_Set;
   --  Save as above but based on an Ubounded_String

   function Load (Filename : String) return Translate_Set;
   --  Read XML document Filename and create the corresponding Translate_set

   procedure Save (Filename : String; Translations : Translate_Set);
   --  Write the translate table into filename

end Templates_Parser.XML;
