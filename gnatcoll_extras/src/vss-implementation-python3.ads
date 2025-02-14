--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  C binding of Python3 Stable API

with Interfaces.C;

with GNATCOLL.Python;

with VSS.Implementation.Interfaces_C;

package VSS.Implementation.Python3 is

   subtype Py_ssize_t is Interfaces.C.size_t;

   function PyBytes_Size (Object : GNATCOLL.Python.PyObject) return Py_ssize_t
     with Import, Convention => C, External_Name => "PyBytes_Size";

   function PyBytes_AsString
     (Object : GNATCOLL.Python.PyObject)
      return VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access
        with Import,
             Convention    => C,
             External_Name => "PyBytes_AsString";

   function PyBytes_AsStringAndSize
     (Obj    : GNATCOLL.Python.PyObject;
      Buffer : out
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size   : out Py_ssize_t) return Integer
        with Import,
             Convention    => C,
             External_Name => "PyBytes_AsStringAndSize";

   function PyUnicode_AsUTF8String
     (Object : GNATCOLL.Python.PyObject) return GNATCOLL.Python.PyObject
        with Import,
             Convention    => C,
             External_Name => "PyUnicode_AsUTF8String";

   function PyUnicode_FromStringAndSize
     (Str  : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size : VSS.Implementation.Python3.Py_ssize_t)
      return GNATCOLL.Python.PyObject
        with Import,
             Convention    => C,
             External_Name => "PyUnicode_FromStringAndSize";

   function PyUnicode_GetLength
     (Object : GNATCOLL.Python.PyObject) return Py_ssize_t
        with Import, Convention => C, External_Name => "PyUnicode_GetLength";

end VSS.Implementation.Python3;
