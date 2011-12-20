------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;

package body Code_Peer.Bridge.Audit_Trail_Readers is

   Audit_Trail_Tag         : constant String := "audit_trail";
   Audit_Tag               : constant String := "audit";

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Reader;
      Text : Unicode.CES.Byte_Sequence)
   is
      Aux : GNAT.Strings.String_Access;

   begin
      if Self.Audit_Record /= null then
         if Self.Audit_Record.Comment /= null then
            Aux := Self.Audit_Record.Comment;
            Self.Audit_Record.Comment := new String'(Aux.all & Text);
            GNAT.Strings.Free (Aux);

         else
            Self.Audit_Record.Comment := new String'(Text);
         end if;
      end if;
   end Characters;

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
      if Qname = Audit_Tag then
         --  If there are no commect, then create empty string. This simplify
         --  things all around the code.

         if Self.Audit_Record.Comment = null then
            Self.Audit_Record.Comment := new String'("");
         end if;

         Self.Audit.Append (Self.Audit_Record);
         Self.Audit_Record := null;
      end if;
   end End_Element;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self   : in out Reader;
      Input  : in out Input_Sources.Input_Source'Class;
      Audit  : out Code_Peer.Audit_Vectors.Vector)
   is
   begin
      Self.Audit.Clear;
      Self.Audit_Record := null;

      Self.Parse (Input);

      Audit := Self.Audit;
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
      if Qname = Audit_Trail_Tag then
         null;

      elsif Qname = Audit_Tag then
         if Attrs.Get_Index ("probability") /= -1 then
            Self.Audit_Record :=
              new Code_Peer.Audit_Record'
                (Probability_Changed => True,
                 Timestamp           =>
                   new String'(Attrs.Get_Value ("timestamp")),
                 Ranking         =>
                   Code_Peer.Message_Ranking_Level'Value
                     (Attrs.Get_Value ("probability")),
                 Comment             => null);

         else
            Self.Audit_Record :=
              new Code_Peer.Audit_Record'
                (Probability_Changed => False,
                 Timestamp           =>
                   new String'(Attrs.Get_Value ("timestamp")),
                 Comment             => null);
         end if;

      else
         raise Program_Error with "Unexpected tag '" & Qname & "'";
      end if;
   end Start_Element;

end Code_Peer.Bridge.Audit_Trail_Readers;
