------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--  Text stream to write otput text into the file using UTF-8 encoding.

with GNATCOLL.VFS;

private with VSS.Characters;
with VSS.Text_Streams;

package GS_Text_Streams is

   type File_UTF8_Output_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with private;

   procedure Open
     (Self : in out File_UTF8_Output_Stream'Class;
      File : GNATCOLL.VFS.Virtual_File);
   --  Open given file to write data

   procedure Close (Self : in out File_UTF8_Output_Stream);
   --  Close used file

private

   type File_UTF8_Output_Stream is
     limited new VSS.Text_Streams.Output_Text_Stream with record
      Writable : GNATCOLL.VFS.Writable_File;
      Buffer   : String (1 .. 4_096);
      Last     : Natural := 0;
   end record;

   overriding procedure Put
     (Self    : in out File_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

end GS_Text_Streams;
