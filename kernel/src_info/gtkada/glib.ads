-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

--  <description>
--
--  This package provides definitions for the basic types used in Glib,
--  Gdk and Gtk.
--
--  </description>

with Interfaces.C;

package Glib is
   pragma Preelaborate;

   package C renames Interfaces.C;
   use type C.int;
   use type C.unsigned;

   -------------------------------------
   -- The basic types defined by glib --
   -------------------------------------

   subtype UTF8_String is String;
   type Gshort is new C.short;
   type Glong  is new C.long;
   type Gint   is new C.int;
   type Gchar  is new C.char;
   type Gboolean is new Gint;

   type Gushort is new C.unsigned_short;
   type Gulong  is new C.unsigned_long;
   type Guint   is new C.unsigned;
   type Guchar  is new C.unsigned_char;

   type Gfloat  is new C.C_float;
   type Gdouble is new C.double;

   type Gint8  is range -(2 ** 7) .. (2 ** 7 - 1);
   type Gint16 is range -(2 ** 15) .. (2 ** 15 - 1);
   type Gint32 is range -(2 ** 31) .. (2 ** 31 - 1);
   type Gint64 is range -(2 ** 63) .. (2 ** 63 - 1);

   type Guint8  is mod 2 ** 8;
   type Guint16 is mod 2 ** 16;
   type Guint32 is mod 2 ** 32;
   type Guint64 is mod 2 ** 64;

   type Gunichar is new Guint32;
end Glib;
