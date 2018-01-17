------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2017-2018, AdaCore                  --
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

with Input_Sources;
private with Sax.Attributes;
with Sax.Readers;
private with Unicode.CES;

package CodePeer.Bridge.Annotations_Readers is

   type Reader is new Sax.Readers.Reader with private;

   procedure Parse
     (Self                  : in out Reader;
      Input                 : in out Input_Sources.Input_Source'Class;
      Annotation_Categories : Annotation_Category_Maps.Map;
      File                  : in out Code_Analysis.File'Class);

private

   type Reader is new Sax.Readers.Reader with record
      Categories : Annotation_Category_Maps.Map;
      File       : access Code_Analysis.File'Class;
      Subprogram : access CodePeer.Subprogram_Data'Class;
   end record;

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence);

   overriding procedure Characters
     (Self : in out Reader;
      Text : Unicode.CES.Byte_Sequence);

end CodePeer.Bridge.Annotations_Readers;
