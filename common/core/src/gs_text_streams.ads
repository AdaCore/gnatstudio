------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2023, AdaCore                   --
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
--  Text stream to write output text into the file using UTF-8 encoding.
--
--  Note, this stream build on top of GNATCOLL.VFS.Writable_File, thus
--  it provides "atomic update" of the written file; which is important,
--  for example, when updating of configuration files. Otherwise
--  VSS.Text_Streams.File_Output is recommended for use.

with GNATCOLL.VFS;

private with VSS.Characters;
private with VSS.Strings;
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

   overriding function Has_Error
     (Self : File_UTF8_Output_Stream) return Boolean is (False);

   overriding function Error_Message
     (Self : File_UTF8_Output_Stream) return VSS.Strings.Virtual_String
   is (VSS.Strings.Empty_Virtual_String);

end GS_Text_Streams;
