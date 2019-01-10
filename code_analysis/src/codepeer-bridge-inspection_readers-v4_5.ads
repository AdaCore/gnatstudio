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

--  Implementation of Inspection reader for format version 4 and 5.

with CodePeer.Bridge.Inspection_Readers.Base;

private package CodePeer.Bridge.Inspection_Readers.V4_5 is

   type Inspection_Reader_V4_5 is
     limited new Base.Base_Inspection_Reader with private;

   function Create_Inspection_Reader_V4_5
     (Kernel          : not null GPS.Kernel.Kernel_Handle;
      Base_Directory  : GNATCOLL.VFS.Virtual_File;
      Root_Inspection : Code_Analysis.CodePeer_Data_Access;
      Messages        : access CodePeer.Message_Maps.Map)
      return not null Inspection_Reader_Access;
   --  Creates inspection data reader for format version 4 and 5 and initialize
   --  it to process data.

private

   type Inspection_Reader_V4_5 is
     limited new Base.Base_Inspection_Reader with record
      Subprogram_Node : Code_Analysis.Subprogram_Access;
   end record;

   overriding procedure Start_Subprogram
     (Self  : in out Inspection_Reader_V4_5;
      Attrs : Sax.Attributes.Attributes'Class);

   overriding function Subprogram_Node
     (Self : Inspection_Reader_V4_5)
      return Code_Analysis.Subprogram_Access;

   overriding procedure Start_Element
     (Self  : in out Inspection_Reader_V4_5;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class);

   overriding procedure Start_Message
     (Self  : in out Inspection_Reader_V4_5;
      Attrs : Sax.Attributes.Attributes'Class);

end CodePeer.Bridge.Inspection_Readers.V4_5;
