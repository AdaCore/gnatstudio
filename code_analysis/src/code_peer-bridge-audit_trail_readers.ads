-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Input_Sources;
private with Sax.Attributes;
with Sax.Readers;
private with Unicode.CES;

package Code_Peer.Bridge.Audit_Trail_Readers is

   type Reader is new Sax.Readers.Reader with private;

   procedure Parse
     (Self   : in out Reader;
      Input  : in out Input_Sources.Input_Source'Class;
      Audit  : out Code_Peer.Audit_Vectors.Vector);

private

   type Reader is new Sax.Readers.Reader with record
      Audit                 : Code_Peer.Audit_Vectors.Vector;
      Audit_Record          : Code_Peer.Audit_Record_Access;
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

end Code_Peer.Bridge.Audit_Trail_Readers;
