------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

--  Base implementation of inspection data reader.

private package CodePeer.Bridge.Inspection_Readers.Base is

   type Base_Inspection_Reader
     (Kernel : not null GPS.Kernel.Kernel_Handle) is
         abstract limited new Abstract_Inspection_Reader with private;

   procedure Initialize
     (Self            : in out Base_Inspection_Reader'Class;
      Base_Directory  : GNATCOLL.VFS.Virtual_File;
      Root_Inspection : Code_Analysis.CodePeer_Data_Access;
      Messages        : access CodePeer.Message_Maps.Map);
   --  Subprogram to initialize object

   overriding procedure Start_Element
     (Self  : in out Base_Inspection_Reader;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process start of XML element.

   overriding procedure End_Element
     (Self  : in out Base_Inspection_Reader;
      Name  : String);
   --  Process end of XML element

   overriding function Get_Code_Analysis_Tree
     (Self : Base_Inspection_Reader) return Code_Analysis.Code_Analysis_Tree;
   --  Returns build code analisys tree

   overriding function Get_Race_Category
     (Self : Base_Inspection_Reader) return CodePeer.Message_Category_Access;
   --  Returns messages category for race conditions

   overriding function Get_Annotation_Categories
     (Self : Base_Inspection_Reader) return Annotation_Category_Maps.Map;
   --  Returns set of annotation categories

   function File_Node
     (Self : Base_Inspection_Reader'Class) return Code_Analysis.File_Access;
   --  Returns currently processed file node

   function Subprogram_Node
     (Self : Base_Inspection_Reader)
      return Code_Analysis.Subprogram_Access is abstract;
   --  Returns currently processed subprogram node

   function Subprogram_Data
     (Self : Base_Inspection_Reader'Class)
      return CodePeer.Subprogram_Data_Access;
   --  Returns analysis information for currently processed subprogram node

   function Message
     (Self : Base_Inspection_Reader'Class) return CodePeer.Message_Access;
   --  Returns currently processed message. Null is return when called outside
   --  of between of Start_Message/End_Message calls.

   procedure Start_Message
     (Self  : in out Base_Inspection_Reader;
      Attrs : Sax.Attributes.Attributes'Class);
   --  Process start of 'message' element

   procedure End_Message
     (Self : in out Base_Inspection_Reader);
   --  Process end of 'message' element

   procedure Start_Subprogram
     (Self  : in out Base_Inspection_Reader;
      Attrs : Sax.Attributes.Attributes'Class) is abstract;
   --  Process start of 'subprogram' element

   function Annotation_Category
     (Self : Base_Inspection_Reader'Class;
      Id   : Natural) return Annotation_Category_Access;
   --  Return annotation category for given identifier.

private

   type Base_Inspection_Reader
     (Kernel : not null GPS.Kernel.Kernel_Handle) is
         abstract limited new Abstract_Inspection_Reader with
   record
      Ignore_Depth          : Natural := 0;
      --  Depth of ignore of nested XML elements to be able to load data files
      --  of newer version when GPS module supports.

      Base_Directory        : GNATCOLL.VFS.Virtual_File;
      --  base directory to reconstruct full paths to referenced data files
      --  (values, backtraces, annotations). Added in version 5.

      Root_Inspection       : Code_Analysis.CodePeer_Data_Access;
      Projects              : Code_Analysis.Code_Analysis_Tree;

      Annotation_Categories : Annotation_Category_Maps.Map;
      CWE_Categories        : CWE_Category_Maps.Map;
      Message_Categories    : Message_Category_Maps.Map;
      Messages              : access CodePeer.Message_Maps.Map;

      File_Node             : Code_Analysis.File_Access;
      Current_Message       : CodePeer.Message_Access;

      Entry_Point_Map       : Entry_Point_Maps.Map;
      Object_Race           : CodePeer.Object_Race_Information;
      Object_Accesses       : CodePeer.Entry_Point_Object_Access_Information;
      Race_Category         : CodePeer.Message_Category_Access;
   end record;

end CodePeer.Bridge.Inspection_Readers.Base;
