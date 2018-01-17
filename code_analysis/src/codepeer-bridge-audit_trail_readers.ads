------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

package CodePeer.Bridge.Audit_Trail_Readers is

   type Reader is new Sax.Readers.Reader with private;

   procedure Parse
     (Self     : in out Reader;
      Input    : in out Input_Sources.Input_Source'Class;
      Messages : CodePeer.Message_Maps.Map);
   --  Parses data file and fill audit trail for mentioned messages.

private

   type Reader is new Sax.Readers.Reader with record
      Version         : Supported_Format_Version;
      Messages        : access constant CodePeer.Message_Maps.Map;
      Message         : CodePeer.Message_Access;
      Audit_Record_V3 : CodePeer.Audit_Record_V3_Access;
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

end CodePeer.Bridge.Audit_Trail_Readers;
