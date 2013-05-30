------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CodePeer.Bridge.Audit_Trail_Readers is

   Audit_Trail_Tag : constant String := "audit_trail";
   Audit_Tag       : constant String := "audit";

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Reader;
      Text : Unicode.CES.Byte_Sequence) is
   begin
      case Self.Version is
         when 2 =>
            if Self.Audit_Record_V2 /= null then
               Append (Self.Audit_Record_V2.Comment, Text);
            end if;

         when 3 =>
            if Self.Audit_Record_V3 /= null then
               Append (Self.Audit_Record_V3.Comment, Text);
            end if;
      end case;
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
         case Self.Version is
            when 2 =>
               Self.Audit_V2.Append (Self.Audit_Record_V2);
               Self.Audit_Record_V2 := null;

            when 3 =>
               Self.Audit_V3.Append (Self.Audit_Record_V3);
               Self.Audit_Record_V3 := null;
         end case;
      end if;
   end End_Element;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self     : in out Reader;
      Input    : in out Input_Sources.Input_Source'Class;
      Audit_V2 : out CodePeer.Audit_V2_Vectors.Vector;
      Audit_V3 : out CodePeer.Audit_V3_Vectors.Vector)
   is
   begin
      Self.Audit_V2.Clear;
      Self.Audit_Record_V2 := null;
      Self.Audit_V3.Clear;
      Self.Audit_Record_V3 := null;
      Self.Version := Supported_Format_Version'First;

      Self.Parse (Input);

      Audit_V2 := Self.Audit_V2;
      Audit_V3 := Self.Audit_V3;
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
         Self.Version := Format_Version'Value (Attrs.Get_Value ("format"));

      elsif Qname = Audit_Tag then
         case Self.Version is
            when 2 =>
               if Attrs.Get_Index ("probability") /= -1 then
                  Self.Audit_Record_V2 :=
                    new CodePeer.Audit_Record_V2'
                      (Ranking_Changed => True,
                       Timestamp       =>
                         To_Unbounded_String (Attrs.Get_Value ("timestamp")),
                       Ranking         =>
                         CodePeer.Message_Ranking_Level'Value
                           (Attrs.Get_Value ("probability")),
                       Comment         => Null_Unbounded_String);

               else
                  Self.Audit_Record_V2 :=
                    new CodePeer.Audit_Record_V2'
                      (Ranking_Changed => False,
                       Timestamp       =>
                         To_Unbounded_String (Attrs.Get_Value ("timestamp")),
                       Comment         => Null_Unbounded_String);
               end if;

            when 3 =>
               Self.Audit_Record_V3 :=
                 new CodePeer.Audit_Record_V3'
                   (Status      =>
                      Audit_Status_Kinds'Value (Attrs.Get_Value ("status")),
                    Approved_By =>
                      To_Unbounded_String (Attrs.Get_Value ("approved")),
                    Timestamp   =>
                      To_Unbounded_String (Attrs.Get_Value ("timestamp")),
                    Comment     => Null_Unbounded_String);
         end case;

      else
         raise Program_Error with "Unexpected tag '" & Qname & "'";
      end if;
   end Start_Element;

end CodePeer.Bridge.Audit_Trail_Readers;
