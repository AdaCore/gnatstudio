-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2006                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Strings;   use GNAT.Strings;
with Ada.Exceptions; use Ada.Exceptions;
with Traces;         use Traces;

package body Language_Utils is

   ---------------------------
   -- Parse_File_Constructs --
   ---------------------------

   procedure Parse_File_Constructs
     (Lang      : access Language_Root'Class;
      File_Name : VFS.Virtual_File;
      Result    : out Construct_List)
   is
      Buffer : GNAT.Strings.String_Access;
   begin
      Buffer := VFS.Read_File (File_Name);

      if Buffer /= null then
         Parse_Constructs (Lang, Buffer.all, Result);
         Free (Buffer);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Free (Buffer);
   end Parse_File_Constructs;

end Language_Utils;
