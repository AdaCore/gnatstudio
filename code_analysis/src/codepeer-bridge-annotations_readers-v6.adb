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

package body CodePeer.Bridge.Annotations_Readers.V6 is

   -------------------
   -- Create_Reader --
   -------------------

   function Create_Reader
     (Categories : Annotation_Category_Maps.Map;
      File       : not null Code_Analysis.File_Access)
      return not null Annotations_Reader_Access is
   begin
      return Result : constant not null Annotations_Reader_Access :=
        new Annotations_Reader_V6
      do
         declare
            Reader : Annotations_Reader_V6'Class
              renames Annotations_Reader_V6'Class (Result.all);

         begin
            Reader.Initialize (Categories, File);
         end;
      end return;
   end Create_Reader;

   --------------------
   -- Get_Subprogram --
   --------------------

   overriding function Get_Subprogram
     (Self : Annotations_Reader_V6)
      return CodePeer.Subprogram_Data_Access is
   begin
      return
        CodePeer.Subprogram_Data_Access
          (Self.Subprogram_Node.Analysis_Data.CodePeer_Data);
   end Get_Subprogram;

   ----------------------
   -- Start_Annotation --
   ----------------------

   overriding procedure Start_Annotation
     (Self  : in out Annotations_Reader_V6;
      Attrs : Sax.Attributes.Attributes'Class) is
   begin
      Self.Subprogram_Node :=
        Self.Subprogram_Map.Element
          (Positive'Value (Attrs.Get_Value ("subp_id")));

      Base.Annotations_Reader_Base (Self).Start_Annotation (Attrs);
   end Start_Annotation;

   ----------------------
   -- Start_Subprogram --
   ----------------------

   overriding procedure Start_Subprogram
     (Self  : in out Annotations_Reader_V6;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Subprogram_Name : constant String := Attrs.Get_Value ("name");
      Subprogram_Node : Code_Analysis.Subprogram_Access;

   begin
      if Self.Get_File.Subprograms.Contains (Subprogram_Name) then
         Subprogram_Node :=
           Code_Analysis.Get_Or_Create (Self.Get_File, Subprogram_Name);
         Self.Subprogram_Map.Insert
           (Positive'Value (Attrs.Get_Value ("id")), Subprogram_Node);
      end if;
   end Start_Subprogram;

end CodePeer.Bridge.Annotations_Readers.V6;
