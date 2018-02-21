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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CodePeer.Bridge.Audit_Trail_Readers is

   Audit_Trail_Tag : constant String := "audit_trail";
   Audit_Tag       : constant String := "audit";
   Message_Tag     : constant String := "message";

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Self : in out Reader;
      Text : Unicode.CES.Byte_Sequence) is
   begin
      if Self.Audit_Record /= null then
         Append (Self.Audit_Record.Comment, Text);
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
         Self.Message.Audit.Append (Self.Audit_Record);
         Self.Audit_Record := null;
      end if;
   end End_Element;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self     : in out Reader;
      Input    : in out Input_Sources.Input_Source'Class;
      Messages : CodePeer.Message_Maps.Map) is
   begin
      Self.Messages     := Messages'Unchecked_Access;
      Self.Audit_Record := null;
      Self.Version      := Supported_Format_Version'First;

      Self.Parse (Input);
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

         if Self.Version = 3 then
            --  Version 3 of interchange format supports requesting data for
            --  one message only. Obtain identifier of this message.

            Self.Message :=
              Self.Messages.all (Integer'Value (Attrs.Get_Value ("message")));
            Self.Message.Audit_Loaded := True;
         end if;

      elsif Qname = Message_Tag then
         Self.Message :=
           Self.Messages.all (Integer'Value (Attrs.Get_Value ("identifier")));
         Self.Message.Audit_Loaded := True;

      elsif Qname = Audit_Tag then
         Self.Audit_Record :=
           new CodePeer.Audit_Record'
             (Status      =>
                Audit_Status_Kinds'Value (Attrs.Get_Value ("status")),
              Approved_By =>
                To_Unbounded_String (Attrs.Get_Value ("approved")),
              Timestamp   =>
                To_Unbounded_String (Attrs.Get_Value ("timestamp")),
              Comment     => Null_Unbounded_String);

      else
         raise Program_Error with "Unexpected tag '" & Qname & "'";
      end if;
   end Start_Element;

end CodePeer.Bridge.Audit_Trail_Readers;
