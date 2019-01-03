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

private package CodePeer.Bridge.Annotations_Readers.Base is

   type Annotations_Reader_Base is
     abstract limited new Abstract_Annotations_Reader with private;

   procedure Initialize
     (Self       : in out Annotations_Reader_Base'Class;
      Categories : Annotation_Category_Maps.Map;
      File       : not null Code_Analysis.File_Access);
   --  Initialize object

   function Get_File
     (Self : Annotations_Reader_Base'Class) return Code_Analysis.File_Access;
   --  Returns file node for currently processed file

   function Get_Subprogram
     (Self : Annotations_Reader_Base)
      return CodePeer.Subprogram_Data_Access is abstract;
   --  Returns subprogram node for the currently processed subprogram

   overriding procedure Start_Element
     (Self  : in out Annotations_Reader_Base;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Self  : in out Annotations_Reader_Base;
      Name  : String);

   procedure Start_Subprogram
     (Self : in out Annotations_Reader_Base;
      Attrs : Sax.Attributes.Attributes'Class) is abstract;
   --  Process start of "subprogram" element

   procedure Start_Annotation
     (Self  : in out Annotations_Reader_Base;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process start of "annotation" element

private

   type Annotations_Reader_Base is
     abstract limited new Abstract_Annotations_Reader with record
      Categories : Annotation_Category_Maps.Map;
      File       : access Code_Analysis.File'Class;
   end record;

end CodePeer.Bridge.Annotations_Readers.Base;
