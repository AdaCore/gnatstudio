------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
     (Self   : in out Reader;
      Input  : in out Input_Sources.Input_Source'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : out Code_Analysis.Code_Analysis_Tree);

private

   function Hash (Item : Natural) return Ada.Containers.Hash_Type;

   package Message_Category_Maps is new Ada.Containers.Hashed_Maps
     (Natural, Message_Category_Access, Hash, "=", "=");

   package Annotation_Category_Maps is new Ada.Containers.Hashed_Maps
     (Natural, Annotation_Category_Access, Hash, "=");

   package Entry_Point_Maps is new Ada.Containers.Hashed_Maps
     (Natural, Entry_Point_Information_Access, Hash, "=");

   type Reader is new Sax.Readers.Reader with record
      Kernel                : GPS.Kernel.Kernel_Handle;

      Version               : Positive;
      --  Version number of interchange format.
      --
      --   1 - default value
      --   2 - is_warning attribute is reported by CodePeer

      Ignore_Depth          : Natural := 0;
      --  Depth of ignore of nested XML elements to be able to load data files
      --  of newer version when GPS module supports.

      Projects              : Code_Analysis.Code_Analysis_Tree;
      Root_Inspection       : Code_Analysis.CodePeer_Data_Access;
      Message_Categories    : Message_Category_Maps.Map;
      Annotation_Categories : Annotation_Category_Maps.Map;
      Entry_Point_Map       : Entry_Point_Maps.Map;
      File_Node             : Code_Analysis.File_Access;
      Subprogram_Node       : Code_Analysis.Subprogram_Access;
      Subprogram_Data       : CodePeer.Subprogram_Data_Access;
      Object_Race           : CodePeer.Object_Race_Information;
      Object_Accesses       : CodePeer.Entry_Point_Object_Access_Information;
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
