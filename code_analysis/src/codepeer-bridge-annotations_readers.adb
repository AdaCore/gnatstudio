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

with Ada.Unchecked_Deallocation;

with CodePeer.Bridge.Annotations_Readers.V4_5;
with CodePeer.Bridge.Annotations_Readers.V6;

package body CodePeer.Bridge.Annotations_Readers is

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

   begin
      Self.Reader.End_Element (Qname);
   end End_Element;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self                  : in out Reader;
      Format                : CodePeer.Supported_Format_Version;
      Input                 : in out Input_Sources.Input_Source'Class;
      Annotation_Categories : Annotation_Category_Maps.Map;
      File                  : in out Code_Analysis.File'Class)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Abstract_Annotations_Reader'Class, Annotations_Reader_Access);

   begin
      case Format is
         when 4 .. 5 =>
            Self.Reader :=
              CodePeer.Bridge.Annotations_Readers.V4_5.Create_Reader
                (Annotation_Categories, File'Unchecked_Access);

         when 6 =>
            Self.Reader :=
              CodePeer.Bridge.Annotations_Readers.V6.Create_Reader
                (Annotation_Categories, File'Unchecked_Access);
      end case;

      Self.Parse (Input);
      Free (Self.Reader);
   end Parse;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

   begin
      Self.Reader.Start_Element (Qname, Attrs);
   end Start_Element;

end CodePeer.Bridge.Annotations_Readers;
