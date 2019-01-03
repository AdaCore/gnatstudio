------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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
--  Base package for GPS scripting (GUI independent)

with GNATCOLL.Scripts;   use GNATCOLL.Scripts;
with GPS.Core_Kernels;   use GPS.Core_Kernels;

package GPS.Scripts is

   type Kernel_Scripts_Repository is
     new GNATCOLL.Scripts.Scripts_Repository_Record with private;
   --  Script repository with kernel pointer inside

   function Create
     (Kernel : GPS.Core_Kernels.Core_Kernel) return Kernel_Scripts_Repository;

   function Get_Kernel (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return GPS.Core_Kernels.Core_Kernel;
   --  Return the kernel associated with Data

   function Get_Kernel
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class)
      return GPS.Core_Kernels.Core_Kernel;
   --  Return the kernel associated with Script

   ------------------
   -- Script_Proxy --
   ------------------

   type Script_Proxy is abstract tagged record
      Instances : Instance_List;
   end record;
   --  A type that should be used in types that are exported to python.
   --  They provide the following support:
   --  * Creating a new python class instance to wrap an Ada object of this
   --    type.
   --  * Ensure that one given Ada object is always represented by the
   --    same python instance. This allows python users to store their own
   --    custom data, and be able to get it every time the same Ada instance
   --    is manipulated.
   --  * Ensure that the python instances exist at least as long as the Ada
   --    object exists.
   --  * Ensures that once the Ada object is destroyed, any remaining python
   --    instance no longer references it anymore. Instead, its internal
   --    data is reset to a default value. From then on, acting on the
   --    python object will no longer make sense, but will not crash the
   --    application either.
   --  * Support multiple scripting languages (one instance of class per
   --    scripting language and per Ada object).

   function Class_Name (Self : Script_Proxy) return String is abstract;
   pragma Inline (Class_Name);
   --  Class_Name is the name in the scripting language. This is also used for
   --  the name of the internal field in the python class that stores
   --  the Ada object. This way, it is possible to have a python class
   --  store reference to multiple types of Ada objects, if they use
   --  different names.
   --
   --  Implementation:
   --  We could have set this as a discriminant of the type, but then this
   --  is not accessible from the generic package below for some reason. This
   --  class name must be known by Finalize to properly free the memory.

   procedure Free (Self : in out Script_Proxy);
   --  Severe the links between class instances stored in Self, and the
   --  Ada object.
   --  This will possibly destroy the instances, or if they are currently
   --  in use in the scripting language, it will simply ensure that the
   --  instances reference a null element.
   --
   --  Implementation:
   --  We could make Script_Proxy a controlled type so that the memory is
   --  freed automatically. The difficulty however is that this type will
   --  be part of a record that it passed to the generic package below, so
   --  Finalize is called every time From_Instance is called.

   type Instances_Status is (Has_Instances, Has_No_Instances);

   No_Data_Set_For_Instance : exception;

   generic
      type Element_Type is private;
      --  Data stored in the python class instance. This type is the one
      --  that includes a Script_Proxy field.
      --  Element_Type must not be a reference counted type, since otherwise
      --  there is a refcount cycle: Element_Type owns a Script_Proxy, that
      --  contains python class instances that own the Element_Type. In this
      --  case, use a Weak reference as the Element_Type.

      type Proxy is new Script_Proxy with private;
      --  Used to get access to the class name.

      with procedure Free (E : in out Element_Type) is null;
      --  After using Transfer_Ownership:
      --  this subprogram is called when the last instance associated with
      --  the element is destroyed.
      --  This procedure is never called if you did not use Transfer_Ownership
      --  since otherwise the script instances are owned by the element, and
      --  thus the instances can only be destroyed when the element itself has
      --  already been freed.

   package Script_Proxies is

      function Get_Or_Create_Instance
         (Self   : in out Proxy'Class;
          Obj    : Element_Type;
          Script : not null access Scripting_Language_Record'Class;
          Class_To_Create : String := "")
         return Class_Instance;
      --  If Obj was already associated with a class instance in the given
      --  scripting language, returns that same instance.
      --  Otherwise, create a new instance to wrap Obj.
      --  Self should be stored in Obj, but we need to pass it explicitly
      --  since we do not have access to it from this generic package.
      --  Class_To_Create can be used to override the name of the class that
      --  we are creating, when there are various children classes possible

      procedure Store_In_Instance
         (Self   : in out Proxy'Class;
          Inst   : Class_Instance;
          Obj    : Element_Type);
      --  Associate Inst with Obj.
      --  This procedure is only needed when the instance was created
      --  independently, for instance when implementing a Constructor_Method
      --  in Ada. Otherwise, better to use Get_Or_Create_Instance.

      function Has_Element (Inst : Class_Instance) return Boolean;
      --  Whether there is an element associated with Inst.

      function From_Instance (Inst : Class_Instance) return Element_Type;
      --  Return the element stored in the instance.
      --  This will raise No_Data_Set_For_Instance if no element was associated
      --  with this instance or if Inst is No_Class_Instance.

      generic
         with function Detach (E : Element_Type) return Element_Type;
         --  Return a new version of E, which does not own any script
         --  instance. E will be freed (with Free above).
      function Transfer_Ownership
         (Self : in out Proxy'Class) return Instances_Status;
      --  This procedure is used to change the ownership of data. After
      --  calling this procedure, the following is true:
      --  * Self is no longer referencing any instance.
      --  * script instances still exist if they are used by a script. If
      --    their last reference was in Self, they are destroyed.
      --  * they now reference some new data (created by calling Detach).
      --    This data should not own a reference to the script instances
      --    (so their Proxy field should be left unset).
      --    Detach is called exacty once if there is at least one script
      --    instance, and never otherwise.
      --  * when the script instances are destroyed eventually, the new
      --    data will be freed automatically.
      --
      --  This procedure can be used when some Data is no longer required on
      --  the Ada side, but existing script instances still need to access it
      --  to extract some information. For instance, we use this for Commands,
      --  so that scripts can access the output of the command even after the
      --  command has finished executing.

   end Script_Proxies;

private

   type Kernel_Scripts_Repository is
     new GNATCOLL.Scripts.Scripts_Repository_Record
   with record
      Kernel : GPS.Core_Kernels.Core_Kernel;
   end record;

end GPS.Scripts;
