------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018-2019, AdaCore                   --
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

with CodePeer.Bridge.Reader_Utilities;

package body CodePeer.Bridge.Annotations_Readers.Base is

   Annotation_Element : constant String := "annotation";
   File_Element       : constant String := "file";
   Subprogram_Element : constant String := "subprogram";

   Category_Attribute : constant String := "category";
   Text_Attribute     : constant String := "text";

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self  : in out Annotations_Reader_Base;
      Name  : String) is
   begin
      null;
   end End_Element;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Self : Annotations_Reader_Base'Class) return Code_Analysis.File_Access is
   begin
      return Self.File;
   end Get_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Annotations_Reader_Base'Class;
      Categories : Annotation_Category_Maps.Map;
      File       : not null Code_Analysis.File_Access) is
   begin
      Self.Categories := Categories;
      Self.File       := File;
   end Initialize;

   ----------------------
   -- Start_Annotation --
   ----------------------

   procedure Start_Annotation
     (Self  : in out Annotations_Reader_Base;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Annotation_Category : constant CodePeer.Annotation_Category_Access :=
        Self.Categories (Natural'Value (Attrs.Get_Value (Category_Attribute)));
      Subprogram          : constant CodePeer.Subprogram_Data_Access :=
        Annotations_Reader_Base'Class (Self).Get_Subprogram;

   begin
      if not Subprogram.Annotations.Contains (Annotation_Category) then
         Subprogram.Annotations.Insert
           (Annotation_Category,
            new CodePeer.Annotation_Vectors.Vector);
      end if;

      Subprogram.Annotations.Element (Annotation_Category).Append
        (new CodePeer.Annotation'
           (Reader_Utilities.Get_Lifeage (Attrs),
            Reader_Utilities.Get_Value (Attrs, Text_Attribute)));
   end Start_Annotation;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self  : in out Annotations_Reader_Base;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class)
   is
   begin
      if Name = File_Element then
         null;

      elsif Name = Subprogram_Element then
         Annotations_Reader_Base'Class (Self).Start_Subprogram (Attrs);

      elsif Name = Annotation_Element then
         Annotations_Reader_Base'Class (Self).Start_Annotation (Attrs);
      end if;
   end Start_Element;

end CodePeer.Bridge.Annotations_Readers.Base;
