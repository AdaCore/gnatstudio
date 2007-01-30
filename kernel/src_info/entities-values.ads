-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2007                         --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Values;

--  This package contains utilities to store and load Entities to/from GValues.

package Entities.Values is

   --  The following subprograms can be used to store an entity in a GValue,
   --  for instance to store it in a tree view

   function To_GValue (Entity : Entity_Information) return Glib.Values.GValue;
   function From_GValue (Value : Glib.Values.GValue) return Entity_Information;
   --  Store an entity in a GValue, or get its value back. This properly
   --  handles reference counting, so that while the GValue is in use, the
   --  entity remains valid. The returned entity has a borrow reference, and
   --  thus needs to be Ref'ed if you want to keep it. Removing the row in the
   --  tree for instance makes the entity invalid.

   function Get_Entity_Information_Type return Glib.GType;
   --  Return the type associated with an entity. This is the type that should
   --  be used when creating the tree model.

end Entities.Values;
