-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007, AdaCore                  --
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

--  This package provides a set of high-level subprograms for handling UTF8
--  encoding

with Glib;
with Basic_Types; use Basic_Types;

package UTF8_Utils is

   function Unknown_To_UTF8
     (Input   : String;
      Success : access Boolean) return Glib.UTF8_String;
   --  Transform a string of unknown encoding to UTF-8.
   --  The heuristics used is the following:
   --    - if S already contains valid UTF-8, assume it is already encoded
   --      in UTF8 (the statistical chances for this are very high)
   --    - if S does not contain valid UTF-8, assume it is encoded using the
   --      locale, and attempt to convert it from the locale to UTF-8.
   --  Success is set to False if the conversion failed.

   procedure Unknown_To_UTF8
     (Input   : String;
      Output  : out Unchecked_String_Access;
      Len     : out Natural;
      Success : out Boolean);
   --  Same as above, but return Output as Unchecked_String_Access for
   --  efficiency. Output is still in UTF8 format, and the caller is
   --  responsible for freeing it.
   --  In addition, if Input is already a valid UTF8 string, then Output
   --  will be set to null: you should use Input in this case.
   --  If Success is set to False, Output will also be set to null.
   --  Warning: Never reference Output (Output'Range) or Output'Last,
   --  use Output (1 .. Len) and Len instead.

end UTF8_Utils;
