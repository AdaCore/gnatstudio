------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Glib.Object;    use Glib.Object;
with Gtk.Widget;     use Gtk.Widget;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;         use System;

package body GNATCOLL.Scripts.Python.Gtkada is

   procedure Init_PyGtk;
   pragma Import (C, Init_PyGtk, "ada_init_pygtk");

   function Build_With_PyGtk return Integer;
   pragma Import (C, Build_With_PyGtk, "ada_build_with_pygtk");

   function PyObject_From_Widget (W : System.Address) return PyObject;
   pragma Import (C, PyObject_From_Widget, "ada_pyobject_from_widget");

   function Widget_From_PyObject (Object : PyObject) return System.Address;
   pragma Import (C, Widget_From_PyObject, "ada_widget_from_pyobject");

   procedure On_PyWidget
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles the new methods declared in this package

   PyGtk_Initialized : Boolean := False;
   --  Whether PyGtk was successfully initialized

   -----------------
   -- On_PyWidget --
   -----------------

   procedure On_PyWidget
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Object : GObject;
      Instance : Class_Instance;
   begin
      Instance := Nth_Arg (Data, 1, Any_Class);
      Object   := Get_Data (Instance);

      if Object = null then
         Python_Callback_Data (Data).Return_Value := Py_None;
         Py_INCREF (Python_Callback_Data (Data).Return_Value);
      else
         Python_Callback_Data (Data).Return_Value := PyObject_From_Widget
           (Get_Object (Object));
      end if;
   end On_PyWidget;

   ----------------
   -- From_PyGtk --
   ----------------

   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Glib.Object.GObject
   is
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      --  ??? Should check wether we have a widget or an object.
      --  Since the main goal here is to get a widget to be inserted in a GUI,
      --  in particular the MDI, let's assume we do have a widget.
      return Get_User_Data
        (Widget_From_PyObject (Get_Param (Python_Callback_Data (Data), N)),
         Stub);
   end From_PyGtk;

   ----------------
   -- From_PyGtk --
   ----------------

   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Gdk.Gdk_Window
   is
      function Internal (Object : PyObject) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "ada_window_from_pyobject");
      P : PyObject;
      Success : Boolean;
   begin
      Get_Param (Python_Callback_Data (Data), N, P, Success);
      if Success then
         return Internal (P);
      else
         return null;
      end if;
   end From_PyGtk;

   ------------------------
   -- Init_PyGtk_Support --
   ------------------------

   procedure Init_PyGtk_Support
     (Script : access Scripting_Language_Record'Class)
   is
      function Load_Pygtk return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Load_Pygtk, "ada_load_pygtk");
      --  Do not free returned value

      Errors  : aliased Boolean;
      Cmd : chars_ptr;
   begin
      if Build_With_PyGtk = 1 then
         Cmd := Load_Pygtk;
         Execute_Command
           (Script          => Script,
            CL              => Create (Value (Cmd)),
            Hide_Output     => True,
            Errors          => Errors);

         if not Errors then
            Init_PyGtk;
            PyGtk_Initialized := True;
         end if;
      end if;
   end Init_PyGtk_Support;

   -------------------------
   -- Add_PyWidget_Method --
   -------------------------

   procedure Add_PyWidget_Method
     (Repo   : access Scripts_Repository_Record'Class;
      Class  : Class_Type)
   is
   begin
      if PyGtk_Initialized then
         Register_Command
           (Repo,
            Command      => "pywidget",
            Handler      => On_PyWidget'Access,
            Class        => Class,
            Language     => Python_Name);
      end if;
   end Add_PyWidget_Method;

end GNATCOLL.Scripts.Python.Gtkada;
