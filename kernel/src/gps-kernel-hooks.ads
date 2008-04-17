-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2008, AdaCore                  --
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

--  This package implements a general support for hooks.
--  See the GPS documentation on how to use hooks from the scripting languages.

with GNATCOLL.Scripts;
with GNAT.Strings;
with Glib.Object;

package GPS.Kernel.Hooks is

   procedure Register_Standard_Hooks
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register all predefined hooks

   function Get_Hook_Name
     (Data : GNATCOLL.Scripts.Callback_Data'Class; Nth : Natural)
      return String;
   --  Return the name of the hook instance stored in Data

   ----------------
   -- Hooks data --
   ----------------
   --  This type represents data that can be passed to a hook.
   --  We have not used generic packages here because it creates elaboration
   --  circularities in the kernel, and it is easier to access non-generic
   --  packages from the various scripting languages

   type Hooks_Data is abstract tagged private;

   procedure Destroy (Data : in out Hooks_Data);
   --  Free the memory used by Data. By default, this does nothing

   function Create_Callback_Data
     (Script    : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Hooks_Data)
      return GNATCOLL.Scripts.Callback_Data_Access is abstract;
   --  Create the callback_data to be passed to a shell command. The data
   --  itself will be freed automatically later on. However, when you add a
   --  class instance to the data, you must free it before returning from the
   --  function.
   --  Code typically looks like
   --      D : constant Callback_Data'Class := Create (Script, 2);
   --      F : constant Class_Instance := ...;
   --      Set_Nth_Arg (D, 1, Hook_Name);
   --      Set_Nth_Arg (D, 2, F);
   --      Free (F);
   --      return D;
   --  This function is called when a hook is called and the callback is
   --  written in shell.
   --
   --  ??? Why set the hooks_name here, that could be done automatically

   type From_Callback_Data_Function is access function
     (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return Hooks_Data'Class;
   --  Create a hooks data from the arguments pass from the shell. This
   --  function is used when run_hook is called from the shell.
   --  Code typically looks like
   --     D : constant .._Hook_Data;
   --     Inst : Class_Instance := Nth_Arg (Data, 2, Klass);
   --     D.Field1 := Inst;
   --     Free (Inst);
   --     return D;
   --  The contents of Hooks_Data matches was is created by
   --  Create_Callback_Data, ie the first argument is the hook name.

   procedure Register_Hook_Data_Type
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data_Type_Name : String;
      Args_Creator   : From_Callback_Data_Function);
   --  Register a new possible parameters profile for hooks. Calling this
   --  procedure is mandatory to make this type of hooks visible from the
   --  shell scripts, so that scripts can create their own hook types.

   ----------------
   -- Hook types --
   ----------------

   procedure Register_Hook_No_Return
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Data_Type_Name : String);
   --  Create a new hook. Associated callbacks will take the parameters
   --  described by Parameters_Profile (which must be the same name given to
   --  Register_Hook_Data_Type). Such callbacks are not expected to return any
   --  value.

   procedure Register_Hook_Return_Boolean
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Data_Type_Name : String);
   --  Same as above, except the callbacks are expected to return a boolean

   procedure Register_Hook_Return_String
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Data_Type_Name : String);
   --  Same as above, except the callbacks are expected to return a string

   procedure Register_Hook_No_Args
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String);
   --  Same as above, except the callbacks take no arguments and return nothing

   ---------------------------------
   -- Manipulating hook functions --
   ---------------------------------

   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook   : String;
      Func   : access GPS.Kernel.Hook_Function_Record'Class;
      Name   : String;
      Watch  : Glib.Object.GObject := null);
   --  Add a new function callback to the hook. The callback is automatically
   --  cancelled when Watch is destroyed.
   --  Name is used to describe the function when the user lists all functions
   --  attached to a hook from a scripting language

   procedure Remove_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook   : String;
      Func   : access GPS.Kernel.Hook_Function_Record'Class);
   --  Remove Func from the list of functions calle when the hook is run

   function Get_Hook_Func_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook   : String) return GNAT.Strings.String_List;
   --  Return the description of the functions attached to the specified hook

   --------------------------------------
   -- Hook functions with no arguments --
   --------------------------------------

   type Function_No_Args
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Function_No_Args_Access is access all Function_No_Args'Class;
   procedure Execute
     (Func   : Function_No_Args;
      Kernel : access Kernel_Handle_Record'Class) is abstract;
   --  Execute the function

   type Function_No_Args_Callback is access procedure
     (Kernel : access Kernel_Handle_Record'Class);
   function Wrapper
     (Callback : Function_No_Args_Callback)
      return Function_No_Args_Access;
   --  Provides a tagged object wrapper around the callback.
   --  We expose this function, instead of having another version of Add_Hook,
   --  so that the returned value can be kept and the function removed from the
   --  hook when it is no longer needed.

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Set_Busy : Boolean := True);
   --  Call all functions that were added to the hook.
   --  The functions are executed in the reverse order in which they were
   --  registered.
   --  Set_Busy indicates whether the busy cursor should be displayed while
   --  processing the hook

   -----------------------------------
   -- Hook functions with arguments --
   -----------------------------------

   type Function_With_Args
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Function_With_Args_Access is access all Function_With_Args'Class;
   procedure Execute
     (Func   : Function_With_Args;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) is abstract;
   --  Execute the action associated with the Hook Function

   type Function_With_Args_Callback is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   function Wrapper
     (Callback : Function_With_Args_Callback)
      return Function_With_Args_Access;
   --  See doc above for Wrapper

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True);
   --  See doc above for Run_Hook
   --  It is your responsability to Destroy Data afterward.

   ------------------------------------------------------
   -- Hook functions with arguments, returning boolean --
   ------------------------------------------------------

   type Function_With_Args_Return_Boolean
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Function_With_Args_Return_Boolean_Access is
     access all Function_With_Args_Return_Boolean'Class;
   function Execute
     (Func   : Function_With_Args_Return_Boolean;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean is abstract;
   --  Execute the action associated with the Hook Function

   type Function_With_Args_Return_Boolean_Callback is access function
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   function Wrapper
     (Callback : Function_With_Args_Return_Boolean_Callback)
      return Function_With_Args_Return_Boolean_Access;
   --  See doc above for wrapper

   function Run_Hook_Until_Success
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean;
   --  Same as Run_Doc above, but stops executing the functions as soon
   --  as one of the functions returns True.
   --  It is your responsability to Destroy Data afterward
   --  Return the value returned by the last function executed

   function Run_Hook_Until_Failure
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean;
   --  Same as above except stops as soon as a function returns False.
   --  It is your responsability to Destroy Data afterward

   -----------------------------------------------------
   -- Hook functions with arguments, returning string --
   -----------------------------------------------------

   type Function_With_Args_Return_String
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Function_With_Args_Return_String_Access is
     access all Function_With_Args_Return_String'Class;
   function Execute
     (Func   : Function_With_Args_Return_String;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return String is abstract;
   --  Execute the action associated with the Hook Function

   type Function_With_Args_Return_String_Callback is access function
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return String;
   function Wrapper
     (Callback : Function_With_Args_Return_String_Callback)
      return Function_With_Args_Return_String_Access;
   --  See doc above for wrapper

   function Run_Hook_Until_Not_Empty
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return String;
   --  Same as Run_Doc above, but stops executing the functions as soon
   --  as one of the functions returns a non-empty string.
   --  Return the value returned by the last function executed
   --  It is your responsability to Destroy Data afterward

private
   type Hooks_Data is abstract tagged record
      Data : GNATCOLL.Scripts.Callback_Data_List;
   end record;
end GPS.Kernel.Hooks;
