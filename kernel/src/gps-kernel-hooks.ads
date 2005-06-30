-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
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

--  This package implements a general support for hooks.
--  See the GPS documentation on how to use hooks from the scripting languages.
--  The following paragraphs describe how interaction between the shell and
--  Ada is done.
--
--  Adding callbacks to existing hooks
--  ----------------------------------
--
--  If a user adds a new callback to an existing hook through a call to
--     GPS.add_hook ("hook_name", "python function")
--  then we create internally a tagged object that wraps "python function"
--  through the interface expected for GPS.Kernel.Hooks.Add_Hook.
--  This wrapper will basically execute this function in the same scripting
--  language that the call to GPS.add_hook was done from, and will specify the
--  arguments from the arguments given to GPS.Kernel.Hooks.Run_Hook by the
--  user.
--
--  Creating new hook types and new hooks
--  --------------------------------------
--
--  Hooks have types, which describe what parameters they expect. This types
--  are named with any string that the user provides through Create_Hook_Type.
--  Any time the latter is called, a new shell function is created:
--     __run_hook__<type_name>
--  This function's implementation has to be provided by the caller of
--  Create_Hook_Type. Its role is to convert the arguments as sent by the
--  scripting language (a Callback_Data) into the actual Hooks_Data type that
--  Ada uses internally), and then to call the appropriate form of Run_Hook.
--
--  New hooks can be created by the user through calls to Register_Hook. This
--  immediately creates a new shell function:
--     __run_hook__<hook_name>
--  Internally, this is bound to the exact same code as __run_hook__<type_name>
--
--  As a result, any time the user executes the call
--     GPS.run_hook ("hook_name", arg1, arg2);
--  the following actions take place:
--    - Calls __run_hook__<hook_name> ("hook_name", arg1, arg2)
--    - Which is the same as __run_hook__<hook_type> ("hook_name", arg1, arg2)
--    - Which calls at the Ada level:
--      GPS.Kernel.Hooks.Run_Hook ("hook_name", Hooks_Data'(Arg1, Arg2));
--
--  The reason we need the __run_hook__<hook_type> step is so that the user
--  can also create new hooks for an existing type directly from the scripting
--  language. This is done by calling (in python for instance):
--      register_hook ("hook_name", "description", "hook_type")
--  which creates __run_hook__<hook_name> with the same implementation as
--  __run_hook__<hook_type>, except that the latter is done in Ada and can
--  therefore create Ada structures at will.
--
--  The user can also create new hook types directly from the scripting
--  language. These hooks can take any number of parameter. This is done by
--  calling
--     GPS.register_hook ("hook_name", "description", "general")
--  This special hook type "general" indicates at the Ada level that the hooks
--  will directly receive a Callback_Data argument instead of a
--  Hooks_Data'Class argument. This is implemented through the
--     __run_hook__general
--  function, exported by Ada.


with GPS.Kernel.Scripts;
with Glib.Object;

package GPS.Kernel.Hooks is

   procedure Register_Standard_Hooks
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register all predefined hooks.

   function Get_Hook_Name
     (Data : GPS.Kernel.Scripts.Callback_Data'Class; Nth : Natural)
      return String;
   --  Return the name of the hook instance stored in Data

   -----------------------------
   -- Hooks with no arguments --
   -----------------------------

   type Hook_No_Args_Record
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Hook_No_Args is access all Hook_No_Args_Record'Class;

   procedure Execute
     (Hook   : Hook_No_Args_Record;
      Kernel : access Kernel_Handle_Record'Class) is abstract;
   --  Execute the hook

   type No_Args_Execute is access procedure
     (Kernel : access Kernel_Handle_Record'Class);


   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_No_Args_Record'Class;
      Watch  : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name      : String;
      Hook      : No_Args_Execute;
      Watch     : Glib.Object.GObject := null;
      Func_Name : String := "");
   --  Add a callback to be called when the hook is executed. The hook must
   --  have been registered first.
   --  The second version is provided for ease of use, so that users do not
   --  have to systematically create a new tagged type. However, such a hook
   --  can never be removed from the list.
   --  When Watch is destroyed, Hook is automatically cancelled, and
   --  destroyed if it isn't associated with another hook list.
   --  Func_Name is used when listing the functions associated with the hook

   procedure Remove_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_No_Args_Record'Class);
   --  Remove Hook from the list of functions to be called when the hook Name
   --  is executed.

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Set_Busy : Boolean := True);
   --  Call all functions that were added to the hook.
   --  The functions are executed in the reverse order in which they were
   --  registered.
   --  Set_Busy indicates whether the busy cursor should be displayed while
   --  processing the hook

   ----------------
   -- Hooks data --
   ----------------
   --  This type represents data that can be passed to a hook.
   --  We have not used generic packages here because it creates elaboration
   --  circularities in the kernel, and it is easier to access non-generic
   --  packages from the various scripting languages

   type Hooks_Data is abstract tagged record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   --  If you need to destroy memory stored in Hooks_Data, you should do that
   --  after calling Run_Hook

   function Get_Name (Data : Hooks_Data) return String is abstract;
   --  Return the name of that type. This should be unique in the application,
   --  and will be used to identify this type of hooks from the shell

   function Execute_Shell
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : GPS.Kernel.Scripts.Subprogram_Type;
      Hook_Name : String;
      Data      : access Hooks_Data) return Boolean is abstract;
   --  Execute the shell command Command, passing it the arguments contained
   --  in Data. The idea is to create a Callback_Data, and then call
   --  directly Execute_Command.
   --  Should return True in case of success.

   ----------------
   -- Hook types --
   ----------------

   type Hook_Type is (Unknown, Hook_Without_Args, Hook_With_Args,
                      Hook_With_Shell_Args, Hook_With_Args_And_Return);

   procedure Create_Hook_Type
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Type_Name        : String;
      Profile          : Hook_Type;
      Run_Hook_Handler : GPS.Kernel.Scripts.Module_Command_Function);
   --  Create a new type of hooks with a specific kind of parameters. This
   --  type of parameters is described by Type_Name, which should be the same
   --  value as returned by Get_Name (Hooks_Data).
   --  Command_Handler will get two kinds of commands in argument:
   --      "__run_hook__" & Get_Name (Data)
   --      "__run_hook__" & Hook_Name
   --  In all thse cases, the behavior must be the same:
   --  Create a Hooks_Data from its Callback_Data parameter, and then call
   --  Run_Hook by passing the created Hooks_Data. The name of the hook to be
   --  run is the first argument of the Callback_Data.

   procedure Register_Hook
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name        : String;
      Type_Name   : String := "");
   --  Register a new hook in the kernel.
   --  Type_Name described the type of parameters for that hook. This should be
   --  the same value as given to Create_Hook_Type. The default value indicates
   --  that the hook doesn't have any argument.

   --------------------------------
   -- Hooks with shell arguments --
   --------------------------------
   --  This type of hooks are used when a hook is created from a scripting
   --  language directly

   type Hook_Shell_Args_Record
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Hook_Shell_Args is access all Hook_Shell_Args_Record;

   procedure Execute
     (Hook   : Hook_Shell_Args_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Scripts.Callback_Data'Class) is abstract;
   --  First argument in Data is the name of the hook

   type Shell_Args_Execute is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Scripts.Callback_Data'Class);
   --  First argument in Data is the name of the hook

   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Shell_Args_Record'Class;
      Watch  : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name      : String;
      Hook      : Shell_Args_Execute;
      Watch     : Glib.Object.GObject := null;
      Func_Name : String := "");
   --  See doc for Add_Hook above

   procedure Remove_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Shell_Args_Record'Class);
   --  See doc for Remove_Hook above

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : access GPS.Kernel.Scripts.Callback_Data'Class;
      Set_Busy : Boolean := True);
   --  See doc for Run_Hook above.
   --  It is your responsability to destroy data on exit

   --------------------------
   -- Hooks with arguments --
   --------------------------

   type Hook_Args_Record
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Hook_Args is access all Hook_Args_Record;

   procedure Execute
     (Hook   : Hook_Args_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) is abstract;

   type Args_Execute is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);

   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Record'Class;
      Watch  : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name      : String;
      Hook      : Args_Execute;
      Watch     : Glib.Object.GObject := null;
      Func_Name : String := "");
   --  See doc for Add_Hook above

   procedure Remove_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Record'Class);
   --  See doc for Remove_Hook above

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True);
   --  See doc for Run_Hook above

   -------------------------------------------
   -- Hooks with arguments and return value --
   -------------------------------------------

   type Hook_Args_Return_Record
      is abstract new GPS.Kernel.Hook_Function_Record with null record;
   type Hook_Args_Return is access all Hook_Args_Return_Record;

   function Execute
     (Hook   : Hook_Args_Return_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean is abstract;

   type Args_Return_Execute is access function
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
      return Boolean;

   procedure Add_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : access Hook_Args_Return_Record'Class;
      Watch    : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name      : String;
      Hook      : Args_Return_Execute;
      Watch     : Glib.Object.GObject := null;
      Func_Name : String := "");
   --  See doc for Add_Hook above

   procedure Remove_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Return_Record'Class);
   --  See doc for Remove_Hook above

   function Run_Hook_Until_Success
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean;
   --  Same as Run_Doc above, but stops executing the functions as soon
   --  as one of the functions returns True.
   --  Return the value returned by the last function executed

   function Run_Hook_Until_Failure
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean;
   --  Same as above except stops as soon as a function returns False.

end GPS.Kernel.Hooks;
