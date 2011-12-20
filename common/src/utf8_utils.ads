------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

   function Unknown_To_UTF8
     (Input   : String) return Glib.UTF8_String;
   --  Same as above, but return "<could not convert to UTF8>" if the
   --  conversion could not be done.

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

   function UTF8_To_Locale (Input : Glib.UTF8_String) return String;
   --  Convert Input to the GPS locale (ie, the contents of the environment
   --  variable CHARSET, defaulting to ISO-8859-1).
   --  If Input could not be converted, Input is returned as-is.

end UTF8_Utils;
