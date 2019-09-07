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

with Gtk.Widget;  use Gtk.Widget;
with Glib.Object; use Glib.Object;

with System;

package body Project_Templates.Script_Objects is

   -------------------------
   -- Build_Python_Object --
   -------------------------

   procedure Build_Python_Object
      (Self          : in out Script_Object;
       Python_Script : Virtual_File;
       Kernel        : not null Core_Kernel)
   is
      Base_Name : constant String :=
         String (Python_Script.Base_Name);

      Script_Name : constant String :=
        Base_Name (Base_Name'First .. Base_Name'Last - 3);

      Python : constant Scripting_Language :=
         Kernel.Scripts.Lookup_Scripting_Language ("python");

      Method_Name : constant String := To_String (Self.Get_Object);

      Module : PyObject;
   begin
      Self.Language := Python;

      Load_Directory (Self.Language, Python_Script.Get_Parent);

      Module := PyImport_ImportModule (Script_Name);

      if PyDict_Contains (PyModule_GetDict (Module),
                          PyString_FromString (Method_Name))
      then
         Self.Object := PyObject_CallMethod (Module, Method_Name);
      end if;

   end Build_Python_Object;

   ---------------
   -- Add_Pages --
   ---------------

   function Add_Pages
      (Self      : in out Script_Object;
       Assistant : in out Gtk_Assistant) return Gint
   is
      function Widget_From_PyObject (Object : PyObject)
         return System.Address;
      pragma Import (C, Widget_From_PyObject, "ada_widget_from_pyobject");

      Get_Pages : constant String := To_String (Self.Get_Pages);

      Nb_Added_Pages  : Gint := 0;

      Widget_List : PyObject;

      Stub : GObject_Record;

      Page_Number : Gint;
      --  Capture the return value of the C function call, unused.
      pragma Unreferenced (Page_Number);
   begin
      if Self.Has_Method (Get_Pages)
      then
         Widget_List
            := PyObject_CallMethod (Self.Object, Get_Pages);
         for I in reverse 0 .. PyObject_Size (Widget_List) - 1 loop
            declare
               Widget : constant Gtk_Widget := Gtk_Widget
            (Get_User_Data
               (Obj  => Widget_From_PyObject
                        (PyObject_GetItem (Widget_List, I)),
                Stub => Stub));

            begin
               Page_Number := Append_Page (Assistant, Widget);

               Assistant.Set_Page_Complete (Widget, True);
               Assistant.Set_Page_Type (Widget, Gtk_Assistant_Page_Content);
               Nb_Added_Pages := Nb_Added_Pages + 1;
            end;
         end loop;
      end if;
      return Nb_Added_Pages;
   end Add_Pages;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out Script_Object) is
      On_Apply : constant String := To_String (Self.On_Apply);

      Return_Object : PyObject;
      --  Captures the return value of the Python function, unused.
      pragma Unreferenced (Return_Object);
   begin
      if Self.Has_Method (On_Apply) then
         Return_Object := PyObject_CallMethod (Self.Object, On_Apply);
      end if;
   end Apply;

   ----------------
   -- Has_Method --
   ----------------

   function Has_Method
      (Self        : in out Script_Object;
       Method_Name : String) return Boolean
   is
   begin
      return PyObject_HasAttrString (Self.Object, Method_Name);
   end Has_Method;

end Project_Templates.Script_Objects;
