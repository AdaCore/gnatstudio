------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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
--  This package provides code highlighter based on libadalang.
--
--  There are two procedure to highlight code. First one does only lexical
--  based highlighting. It doesn't require AST and work even on one line
--  piece of code. So it's called Highlight_Fast.
--
--  Second one uses AST to highlight type names, "block" names and aspects
--  using syntax information from AST.
--

with GPS.Editors;
limited with LAL.Core_Module;
private with Ada.Containers.Hashed_Sets;
private with GNATCOLL.VFS;

package LAL.Highlighters is

   type Highlighter is tagged limited private;

   procedure Initialize
     (Self    : in out Highlighter'Class;
      Module  : LAL.Core_Module.LAL_Module_Id);

   not overriding procedure Highlight_Using_Tree
     (Self   : in out Highlighter;
      Buffer : GPS.Editors.Editor_Buffer'Class;
      From   : Integer;
      To     : Integer);

private

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => GNATCOLL.VFS."=",
      "="                 => GNATCOLL.VFS."=");

   type Highlighter is tagged limited record
      Module : access LAL.Core_Module.LAL_Module_Id_Record'Class;
      Broken : File_Sets.Set;
   end record;

end LAL.Highlighters;
