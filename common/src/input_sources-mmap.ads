-----------------------------------------------------------------------
--                XML/Ada - An XML suite for Ada95                   --
--                                                                   --
--                       Copyright (C) 2001-2007, AdaCore            --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with GNATCOLL.Mmap;
with Unicode;
with Unicode.CES;

package Input_Sources.Mmap is

   type Mmap_Input is new Input_Source with private;
   type Mmap_Input_Access is access all Mmap_Input'Class;
   --  A special implementation of a reader, that reads from a file.

   procedure Open (Filename : String; Input : out Mmap_Input);
   overriding procedure Close (Input : in out Mmap_Input);
   overriding procedure Next_Char
     (From : in out Mmap_Input; C : out Unicode.Unicode_Char);
   overriding function Eof (From : Mmap_Input) return Boolean;

   procedure Set_System_Id
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
