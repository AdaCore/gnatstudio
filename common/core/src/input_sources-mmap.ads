------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GNATCOLL.Mmap;
with GNATCOLL.VFS;  use GNATCOLL.VFS;
with Unicode;
with Unicode.CES;

package Input_Sources.Mmap is

   type Mmap_Input is new Input_Source with private;
   type Mmap_Input_Access is access all Mmap_Input'Class;
   --  A special implementation of a reader, that reads from a file.

   procedure Open (Filename : Filesystem_String; Input : out Mmap_Input);
   overriding procedure Close (Input : in out Mmap_Input);
   overriding procedure Next_Char
     (From : in out Mmap_Input; C : out Unicode.Unicode_Char);
   overriding function Eof (From : Mmap_Input) return Boolean;

   overriding procedure Set_System_Id
     (Input : in out Mmap_Input; Id : Unicode.CES.Byte_Sequence);
   --  Override Input_Sources.Set_System_Id, and ensure we use an absolute
   --  file name. This is needed in lots of cases, for instance to resolve
   --  relative URIs, to ensure we do not parse a grammar twice,...

private
   type Mmap_Input is new Input_Source with
      record
         File   : GNATCOLL.Mmap.Mapped_File;
         Buffer : GNATCOLL.Mmap.Str_Access;
         Index  : Natural;
      end record;
end Input_Sources.Mmap;
