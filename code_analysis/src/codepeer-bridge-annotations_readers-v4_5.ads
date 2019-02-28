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

with CodePeer.Bridge.Annotations_Readers.Base;

private package CodePeer.Bridge.Annotations_Readers.V4_5 is

   type Annotations_Reader_V4_5 is
     new CodePeer.Bridge.Annotations_Readers.Base.Annotations_Reader_Base
       with private;

   function Create_Reader
     (Categories : Annotation_Category_Maps.Map;
      File       : not null Code_Analysis.File_Access)
      return not null Annotations_Reader_Access;

private

   type Annotations_Reader_V4_5 is
     new CodePeer.Bridge.Annotations_Readers.Base.Annotations_Reader_Base with
   record
      Subprogram : CodePeer.Subprogram_Data_Access;
   end record;

   overriding function Get_Subprogram
     (Self : Annotations_Reader_V4_5)
      return CodePeer.Subprogram_Data_Access;

   overriding procedure Start_Subprogram
     (Self : in out Annotations_Reader_V4_5;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process start of "subprogram" element

end CodePeer.Bridge.Annotations_Readers.V4_5;
