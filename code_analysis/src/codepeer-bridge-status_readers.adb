------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

with CodePeer.Module; use CodePeer.Module;

package body CodePeer.Bridge.Status_Readers is

   Status_Tag            : constant String := "status";
   Message_Tag           : constant String := "message";

   Editable_Attribute    : constant String := "editable";
   Identifier_Attribute  : constant String := "identifier";
   Status_Attribute      : constant String := "status";

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name, Qname);

   begin
      if Self.Ignore_Depth /= 0 then
         --  Decrase depth of ignored XML element.

         Self.Ignore_Depth := Self.Ignore_Depth - 1;
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
      Self.Messages := Messages'Unchecked_Access;
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

      function Get_Optional_Editable return Boolean;
      --  Returns value of "editable" attribute if it is specified,
      --  otherwise returns True.

      ---------------------------
      -- Get_Optional_Editable --
      ---------------------------

      function Get_Optional_Editable return Boolean is
         Index : constant Integer := Attrs.Get_Index (Editable_Attribute);

      begin
         if Index /= -1 then
            return Boolean'Value (Attrs.Get_Value (Index));

         else
            return True;
         end if;
      end Get_Optional_Editable;

      Message : Message_Access;

   begin
      if Self.Ignore_Depth /= 0 then
         Self.Ignore_Depth := Self.Ignore_Depth + 1;

      elsif Qname = Status_Tag then
         null;

      elsif Qname = Message_Tag then
         begin
            Message := Self.Messages.all
              (Natural'Value (Attrs.Get_Value (Identifier_Attribute)));
            Message.Status_Editable := Get_Optional_Editable;
            Message.Status :=
              CodePeer.Audit_Status_Kinds'Value
                (Attrs.Get_Value (Status_Attribute));

         exception
            when Constraint_Error =>
               --  In case we have an unexpected value (e.g. new status kind),
               --  revert to Uncategorized instead of crashing

               Message.Status := Uncategorized;
         end;

         Set_Review_Action (Message);

      else
         --  Activate ignore of nested XML elements to be able to load data
         --  files of newer version then supported by GPS.

         Self.Ignore_Depth := 1;
      end if;
   end Start_Element;

end CodePeer.Bridge.Status_Readers;
