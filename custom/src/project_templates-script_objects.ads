------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

--  This package provides functions to interract with the python scripts used
--  by the template projects. The python script must provide an object with 2
--  functions 'get_pages' and 'on_apply'.
--  get_pages : called to construct the successive pages of the Gtk.Assistant.
--  on_apply : called after the template has been instantiated.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Python;       use GNATCOLL.Python;
with GNATCOLL.Scripts;      use GNATCOLL.Scripts;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with Gtk.Assistant;         use Gtk.Assistant;
with Glib;                  use Glib;

with GPS.Core_Kernels;      use GPS.Core_Kernels;

package Project_Templates.Script_Objects is

   function TUS (Str : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Script_Object is tagged record
      Object     : PyObject := Py_None;
      Language   : Scripting_Language := null;
      Get_Pages  : Unbounded_String;
      On_Apply   : Unbounded_String;
      Get_Object : Unbounded_String;
   end record;

   procedure Build_Python_Object
      (Self          : in out Script_Object;
       Python_Script : Virtual_File;
       Kernel        : not null Core_Kernel);
   --  Builds the PyObject used to access 'get_pages' and 'on_apply' methods.
   --  Sets the scripting language used internally.

   function Add_Pages (Self      : in out Script_Object;
                       Assistant : in out Gtk_Assistant) return Gint
      with Pre => Self.Object /= Py_None and then Self.Language /= null;
   --  Returns zero if no page were added or if the object doesn't have a
   --  'get_pages' method.

   procedure Apply (Self : in out Script_Object)
      with Pre => Self.Object /= Py_None and then Self.Language /= null;
   --  Calls the 'on_apply' method of the internal PyObject.

   function Has_Method
      (Self        : in out Script_Object;
       Method_Name : String) return Boolean
      with Pre => Self.Object /= Py_None and then Self.Language /= null;
   --  Checks that the underlying PyObject has a method with a certain name.

   Null_Script_Object : constant Script_Object :=
   (Py_None,
    null,
    TUS ("get_pages"),
    TUS ("on_apply"),
    TUS ("get_object"));

end Project_Templates.Script_Objects;
