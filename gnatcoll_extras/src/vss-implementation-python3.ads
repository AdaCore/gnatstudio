--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  C binding of Python3 Stable API, see
--
--  https://docs.python.org/3/c-api/
--
--  for detailed documentation of each entity declared here.

with Interfaces.C;

with GNATCOLL.Python;

with VSS.Implementation.Interfaces_C;

package VSS.Implementation.Python3 is

   subtype Py_ssize_t is Interfaces.C.size_t;

   function PyBytes_Size (Object : GNATCOLL.Python.PyObject) return Py_ssize_t
     with Import, Convention => C, External_Name => "PyBytes_Size";
   --  Return the length of the bytes in bytes object o.

   function PyBytes_AsString
     (Object : GNATCOLL.Python.PyObject)
      return VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access
        with Import,
             Convention    => C,
             External_Name => "PyBytes_AsString";
   --  Return a pointer to the contents of o. The pointer refers to the
   --  internal buffer of o, which consists of len(o) + 1 bytes. The last
   --  byte in the buffer is always null, regardless of whether there are any
   --  other null bytes. The data must not be modified in any way, unless the
   --  object was just created using PyBytes_FromStringAndSize(NULL, size).
   --  It must not be deallocated. If o is not a bytes object at all,
   --  PyBytes_AsString() returns NULL and raises TypeError.

   function PyBytes_AsStringAndSize
     (Obj    : GNATCOLL.Python.PyObject;
      Buffer : out
        VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size   : out Py_ssize_t) return Integer
        with Import,
             Convention    => C,
             External_Name => "PyBytes_AsStringAndSize";
   --  Return the null-terminated contents of the object obj through the output
   --  variables buffer and length. Returns 0 on success.
   --
   --  If length is NULL, the bytes object may not contain embedded null bytes;
   --  if it does, the function returns -1 and a ValueError is raised.
   --
   --  The buffer refers to an internal buffer of obj, which includes an
   --  additional null byte at the end (not counted in length). The data must
   --  not be modified in any way, unless the object was just created using
   --  PyBytes_FromStringAndSize(NULL, size). It must not be deallocated. If
   --  obj is not a bytes object at all, PyBytes_AsStringAndSize() returns -1
   --  and raises TypeError.

   function PyUnicode_AsUTF8String
     (Object : GNATCOLL.Python.PyObject) return GNATCOLL.Python.PyObject
        with Import,
             Convention    => C,
             External_Name => "PyUnicode_AsUTF8String";
   --  Encode a Unicode object using UTF-8 and return the result as Python
   --  bytes object. Error handling is “strict”. Return NULL if an
   --  exception was raised by the codec.
   --
   --  The function fails if the string contains surrogate code points (U+D800
   --  - U+DFFF).

   function PyUnicode_FromStringAndSize
     (Str  : VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
      Size : VSS.Implementation.Python3.Py_ssize_t)
      return GNATCOLL.Python.PyObject
        with Import,
             Convention    => C,
             External_Name => "PyUnicode_FromStringAndSize";
   --  Create a Unicode object from the char buffer str. The bytes will be
   --  interpreted as being UTF-8 encoded. The buffer is copied into the new
   --  object. The return value might be a shared object, i.e. modification
   --  of the data is not allowed.
   --
   --  This function raises SystemError when:
   --    size < 0,
   --    str is NULL and size > 0

   function PyUnicode_GetLength
     (Object : GNATCOLL.Python.PyObject) return Py_ssize_t
        with Import, Convention => C, External_Name => "PyUnicode_GetLength";
   --  Return the length of the Unicode object, in code points.
   --
   --  On error, set an exception and return -1.

end VSS.Implementation.Python3;
