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
      use type GNAT.Strings.String_Access;

   begin
      if Self.Audit_Record /= null then
         if Self.Audit_Record.Comment /= null then
            raise Program_Error;

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
                 Probability         =>
                   Code_Peer.Message_Probability_Level'Value
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
