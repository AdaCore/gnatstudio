-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
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

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.MDI;               use Gtkada.MDI;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Python.GUI;               use Python, Python.GUI;
with Python.Ada;               use Python.Ada;
with Glide_Intl;               use Glide_Intl;
with Gtk.Enums;                use Gtk.Enums;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

package body Python_Module is

   type Python_Module_Record is new Module_ID_Record with record
      Interpreter : Python_Interpreter;
   end record;
   type Python_Module_Access is access all Python_Module_Record'Class;

   Python_Module_Id : Python_Module_Access;

   type Interpreter_View_Record is new Gtk_Scrolled_Window_Record
     with null record;
   type Interpreter_View is access all Interpreter_View_Record'Class;

   procedure Create_Python_Console
     (Item   : access Gtk_Widget_Record'Class;
      Kernel : Kernel_Handle);
   --  Create the python console if it doesn't exist yet.

   ---------------------------
   -- Create_Python_Console --
   ---------------------------

   procedure Create_Python_Console
     (Item   : access Gtk_Widget_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Item);
      Child   : MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Interpreter_View_Record'Tag);
      Console : Interpreter_View;
      Buffer : Gtk_Text_Buffer;
      View   : Gtk_Text_View;
   begin
      if Child = null then
         Console := new Interpreter_View_Record;
         Gtk.Scrolled_Window.Initialize (Console);
         Set_Policy (Console, Policy_Automatic, Policy_Automatic);

         Gtk_New (Buffer);
         Gtk_New (View, Buffer);
         Add (Console, View);
         Modify_Font (View, Get_Pref (Kernel, Source_Editor_Font));
         Set_Wrap_Mode (View, Wrap_Char);

         Set_Console (Python_Module_Id.Interpreter, View);

         Child := Put
           (Get_MDI (Kernel), Console, Focus_Widget => Gtk_Widget (View));
         Set_Focus_Child (Child);
         Set_Title (Child, -"Python");
         Set_Dock_Side (Child, Bottom);
         Dock_Child (Child);
      end if;
   end Create_Python_Console;

   ----------
   -- Test --
   ----------

   function Test (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Test);

   function Test (Self : PyObject; Args : PyObject) return PyObject is
      pragma Unreferenced (Self, Args);
   begin
      --  if Self = GPS_Module then
      --     return PyString_FromString ("test: first arg is gps_module");
      --  elsif Self = Kernel_Class then
      --     return PyString_FromString ("Test: first arg is Kernel");
      --  elsif Self = null then
      --     return PyString_FromString ("Test: first arg is null");
      --  else
      return PyString_FromString ("Calling test");
      --  end if;
   end Test;

   ---------
   -- Foo --
   ---------

   function Foo (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Foo);

   function Foo (Self : PyObject; Args : PyObject) return PyObject is
      A : aliased Integer;
   begin
      if not PyArg_ParseTuple (Args, "i", A'Address) then
         return null;
      end if;

      if Self = null then
         return PyString_FromString ("<null> Calling foo" & A'Img);
      else
         return PyString_FromString
           (PyString_AsString (PyObject_Str (Self))
            & "Calling foo" & A'Img);
      end if;
   end Foo;

   ---------------
   -- Foo_Class --
   ---------------

   function Foo_Class (Self : PyObject; Args : PyObject) return PyObject;
   pragma Convention (C, Foo_Class);

   function Foo_Class (Self : PyObject; Args : PyObject) return PyObject is
      A : aliased Integer;
      O : aliased PyObject;
      pragma Unreferenced (Self);
   begin
      --  Self is always null
      --  O is always set to the class that contains the method (ie Kernel)
      if not PyArg_ParseTuple (Args, "Oi", O'Address, A'Address) then
         return null;
      end if;

      if O = null then
         return PyString_FromString ("<null> Calling foo_class" & A'Img);
      else
         return PyString_FromString
           (PyString_AsString (PyObject_Str (O))
            & "Calling foo_class" & A'Img);
      end if;
   end Foo_Class;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Methods : constant PyMethodDef_Array :=
        (0 => Create_Method_Def ("test", Test'Access, "test function"));

      GPS_Module : PyObject;
      Kernel_Class : PyObject;
      Dict   : PyDictObject;
      Kernel_Obj : PyObject;
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      Python_Module_Id := new Python_Module_Record;
      Register_Module
        (Module      => Module_ID (Python_Module_Id),
         Kernel      => Kernel,
         Module_Name => "Python");

      Python_Module_Id.Interpreter := new Python_Interpreter_Record;
      Initialize (Python_Module_Id.Interpreter, Get_History (Kernel));

      Create_Python_Console (Get_Main_Window (Kernel), Kernel_Handle (Kernel));

      --  Create the new GPS module

      GPS_Module := Py_InitModule
        ("GPS", Methods, "Interface with the GPS environment");
      Run_Command
        (Python_Module_Id.Interpreter, "import GPS", Hide_Output => True);

      --  Add new functions

      Add_Function
        (GPS_Module, Create_Method_Def ("foo", Foo'Access, "foo function"));

      --  Create a new type

      Dict := PyDict_New;
      Ignored :=
        PyDict_SetItemString (Dict, New_String ("editors"), PyDict_New);
      Ignored := PyDict_SetItemString
        (Dict, New_String ("__doc__"), PyString_FromString ("doc for Kernel"));
      Ignored := PyDict_SetItemString
        (Dict, New_String ("__module__"), PyString_FromString ("GPS"));

      Kernel_Class := PyClass_New
        (Bases => null,
         Dict  => Dict,
         Name  => PyString_FromString ("Kernel"));
      Ignored := PyModule_AddObject
        (GPS_Module, New_String ("Kernel"), Kernel_Class);

      Add_Method
        (Kernel_Class,
         Create_Method_Def ("foo", Foo'Access, "foo method"));
      Add_Class_Method
        (Kernel_Class,
         Create_Method_Def ("foo_class", Foo_Class'Access, "foo method"));
      Add_Static_Method
        (Kernel_Class,
         Create_Method_Def ("foo_static", Foo'Access, "foo method"));

      Kernel_Obj := PyCObject_FromVoidPtr (Kernel.all'Address);


      Py_DECREF (Kernel_Obj);
   end Register_Module;

end Python_Module;

