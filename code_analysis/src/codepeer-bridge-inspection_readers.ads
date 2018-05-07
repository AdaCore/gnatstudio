------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

private with Ada.Containers.Hashed_Maps;

with Input_Sources;
private with Sax.Attributes;
with Sax.Readers;
private with Unicode.CES;

with GPS.Kernel;
with Code_Analysis;

package CodePeer.Bridge.Inspection_Readers is

   type Reader is new Sax.Readers.Reader with private;

   procedure Parse
     (Self                  : in out Reader;
      Input                 : in out Input_Sources.Input_Source'Class;
      Kernel                : GPS.Kernel.Kernel_Handle;
      Tree                  : out Code_Analysis.Code_Analysis_Tree;
      Annotation_Categories : out Annotation_Category_Maps.Map;
      Messages              : out CodePeer.Message_Maps.Map;
      Version               : out Supported_Format_Version;
      Race_Category         : out CodePeer.Message_Category_Access);

private

   function Hash (Item : CWE_Identifier) return Ada.Containers.Hash_Type;

   package Message_Category_Maps is new Ada.Containers.Hashed_Maps
     (Natural, Message_Category_Access, Hash, "=", "=");

   package CWE_Category_Maps is new Ada.Containers.Hashed_Maps
     (CWE_Identifier, CWE_Category_Access, Hash, "=", "=");

   package Entry_Point_Maps is new Ada.Containers.Hashed_Maps
     (Natural, Entry_Point_Information_Access, Hash, "=");

   --------------------------------
   -- Abstract_Inspection_Reader --
   --------------------------------

   type Abstract_Inspection_Reader is limited interface;
   --  Base type for inspection reader for particular exchange format.

   type Inspection_Reader_Access is
     access all Abstract_Inspection_Reader'Class;

   not overriding procedure Start_Element
     (Self  : in out Abstract_Inspection_Reader;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class) is abstract;

   not overriding procedure End_Element
     (Self  : in out Abstract_Inspection_Reader;
      Name  : String) is abstract;

   not overriding function Get_Code_Analysis_Tree
     (Self : Abstract_Inspection_Reader)
      return Code_Analysis.Code_Analysis_Tree is abstract;

   not overriding function Get_Race_Category
     (Self : Abstract_Inspection_Reader)
      return CodePeer.Message_Category_Access is abstract;

   not overriding function Get_Annotation_Categories
     (Self : Abstract_Inspection_Reader)
      return Annotation_Category_Maps.Map is abstract;

   not overriding procedure End_Document
     (Self : in out Abstract_Inspection_Reader) is null;
   --  Called after processing of the XML data file.

   ------------
   -- Reader --
   ------------

   type Reader is new Sax.Readers.Reader with record
      Kernel                : GPS.Kernel.Kernel_Handle;

      Version               : Supported_Format_Version;
      --  Version number of interchange format.
      --
      --   1 - default value
      --   2 - is_warning attribute is reported by CodePeer
      --   3 - new content of audit records

      Reader                : Inspection_Reader_Access;
      Reader_Depth          : Natural := 0;
      --  Reader of the given version of exchange format and depth of nested
      --  XML elements.

      Ignore_Depth          : Natural := 0;
      --  Depth of ignore of nested XML elements to be able to load data files
      --  of newer version when GPS module supports.

      Base_Directory        : GNATCOLL.VFS.Virtual_File;
      --  base directory to reconstruct full paths to referenced data files
      --  (values, backtraces, annotations). Added in version 5.

      Root_Inspection       : Code_Analysis.CodePeer_Data_Access;
      Messages              : access CodePeer.Message_Maps.Map;
   end record;

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class);

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence);

end CodePeer.Bridge.Inspection_Readers;
