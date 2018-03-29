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

package body CodePeer.Bridge.Annotations_Readers is

   Annotation_Element : constant String := "annotation";
   File_Element       : constant String := "file";
   Subprogram_Element : constant String := "subprogram";

   Category_Attribute : constant String := "category";
   Text_Attribute     : constant String := "text";
   Lifeage_Attribute  : constant String := "lifeage";

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence) is
   begin
      null;
   end End_Element;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self                  : in out Reader;
      Input                 : in out Input_Sources.Input_Source'Class;
      Annotation_Categories : Annotation_Category_Maps.Map;
      File                  : in out Code_Analysis.File'Class) is
   begin
      Self.Categories := Annotation_Categories;
      Self.File := File'Unchecked_Access;
      Self.Parse (Input);
      Self.File := null;
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

      Annotation_Category : CodePeer.Annotation_Category_Access;

      function Lifeage return Lifeage_Kinds;
      --  Returns value of "lifeage" attribute or Unchanged when not specified.

      -------------
      -- Lifeage --
      -------------

      function Lifeage return Lifeage_Kinds is
         Index : constant Integer := Attrs.Get_Index (Lifeage_Attribute);

      begin
         if Index = -1 then
            return Unchanged;

         else
            return Lifeage_Kinds'Value (Attrs.Get_Value (Index));
         end if;
      end Lifeage;

   begin
      if Qname = File_Element then
         null;

      elsif Qname = Subprogram_Element then
         declare
            Subprogram_Name : constant String :=
              Attrs.Get_Value ("name");

         begin
            if Self.File.Subprograms.Contains (Subprogram_Name) then
               Self.Subprogram :=
                 CodePeer.Subprogram_Data_Access
                   (Code_Analysis.Get_Or_Create
                      (Self.File,
                       Subprogram_Name).Analysis_Data.CodePeer_Data);
            end if;
         end;

      elsif Qname = Annotation_Element then
         Annotation_Category :=
           Self.Categories
             (Natural'Value (Attrs.Get_Value (Category_Attribute)));

         if not Self.Subprogram.Annotations.Contains
                  (Annotation_Category)
         then
            Self.Subprogram.Annotations.Insert
              (Annotation_Category,
               new CodePeer.Annotation_Vectors.Vector);
         end if;

         Self.Subprogram.Annotations.Element (Annotation_Category).Append
           (new CodePeer.Annotation'
              (Lifeage,
               Ada.Strings.Unbounded.To_Unbounded_String
                 (Attrs.Get_Value (Text_Attribute))));
      end if;
   end Start_Element;

end CodePeer.Bridge.Annotations_Readers;
