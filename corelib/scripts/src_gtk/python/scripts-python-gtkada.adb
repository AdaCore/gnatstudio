
with Glib.Object;    use Glib.Object;
with Gtkada.MDI;     use Gtkada.MDI;
with Gtk.Container;  use Gtk.Container;
with Gtk.Widget;     use Gtk.Widget;
with Scripts.Gtkada; use Scripts.Gtkada;
with Scripts.Impl;   use Scripts.Impl;
with System;         use System;

package body Scripts.Python.Gtkada is

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
      Stub : Glib.Object.GObject_Record;
   begin
      return Get_User_Data
        (Widget_From_PyObject (Get_Param (Python_Callback_Data (Data), 2)),
         Stub);
   end From_PyGtk;

   ------------------------
   -- Init_PyGtk_Support --
   ------------------------

   procedure Init_PyGtk_Support
     (Script : access Scripting_Language_Record'Class)
   is
      Errors  : aliased Boolean;
   begin
      if Build_With_PyGtk = 1 then
         Execute_Command
           (Script          => Script,
            Command         => "import pygtk",
            Hide_Output     => True,
            Errors          => Errors);
      else
         --  Since we were not build with pygtk, don't even try to activate the
         --  special support for it
         Errors := True;
      end if;

      if not Errors then
         Insert_Log (Script, null, "Loading support for pygtk");
         Execute_Command
           (Script      => Script,
            Command     => "pygtk.require('2.0'); import gtk",
            Hide_Output => True,
            Errors      => Errors);
         if Errors then
            Insert_Log (Script, null, "Couldn't initialize gtk");
         else
            Init_PyGtk;
            PyGtk_Initialized := True;
         end if;
      end if;
   end Init_PyGtk_Support;

   -------------------------
   -- Add_PyWidget_Method --
   -------------------------

   procedure Add_PyWidget_Method
     (Script : access Scripting_Language_Record'Class;
      Class  : Class_Type)
   is
   begin
      if PyGtk_Initialized then
         Register_Command
           (Script,
            Command      => "pywidget",
            Handler      => On_PyWidget'Access,
            Class        => Class);
      end if;
   end Add_PyWidget_Method;

end Scripts.Python.Gtkada;
