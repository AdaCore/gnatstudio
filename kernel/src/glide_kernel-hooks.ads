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

--  This package implements a general support for hooks.

with Glide_Kernel.Scripts;
with Glib.Object;

package Glide_Kernel.Hooks is

   procedure Register_Standard_Hooks
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register all predefined hooks.

   -----------------------------
   -- Hooks with no arguments --
   -----------------------------

   type Hook_No_Args_Record
      is abstract new Glide_Kernel.Hook_Function_Record with null record;
   type Hook_No_Args is access all Hook_No_Args_Record'Class;

   procedure Execute
     (Hook   : Hook_No_Args_Record;
      Kernel : access Kernel_Handle_Record'Class) is abstract;
   --  Execute the hook

   type No_Args_Execute is access procedure
     (Kernel : access Kernel_Handle_Record'Class);


   procedure Add_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_No_Args_Record'Class;
      Watch  : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : No_Args_Execute;
      Watch  : Glib.Object.GObject := null);
   --  Add a callback to be called when the hook is executed. The hook must
   --  have been registered first.
   --  The second version is provided for ease of use, so that users do not
   --  have to systematically create a new tagged type. However, such a hook
   --  can never be removed from the list.
   --  When Watch is destroyed, Hook is automatically cancelled, and
   --  destroyed if it isn't associated with another hook list.

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_No_Args_Record'Class);
   --  Remove Hook from the list of functions to be called when the hook Name
   --  is executed.

   procedure Run_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
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

   type Hooks_Data is abstract tagged null record;

   function Get_Name (Data : Hooks_Data) return String is abstract;
   --  Return the name of that type. This should be unique in the application,
   --  and will be used to identify this type of hooks from the shell

   function Execute_Shell
     (Script    : access Glide_Kernel.Scripts.Scripting_Language_Record'Class;
      Command   : String;
      Hook_Name : String;
      Data      : Hooks_Data) return Boolean is abstract;
   --  Execute the shell command Command, passing it the arguments contained
   --  in Data. The idea is to create a Callback_Data, and then call
   --  directly Execute_Command.
   --  Should return True in case of success.

   ----------------
   -- Hook types --
   ----------------

   type Hook_Type is (Unknown, Hook_Without_Args, Hook_With_Args,
                      Hook_With_Args_And_Return);

   procedure Create_Hook_Type
     (Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Name        : String;
      Description      : String;
      Profile          : Hook_Type;
      Run_Hook_Handler : Glide_Kernel.Scripts.Module_Command_Function);
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
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name        : String;
      Description : String;
      Type_Name   : String := "");
   --  Register a new hook in the kernel.
   --  Type_Name described the type of parameters for that hook. This should be
   --  the same value as given to Create_Hook_Type. The default value indicates
   --  that the hook doesn't have any argument

   --------------------------
   -- Hooks with arguments --
   --------------------------

   type Hook_Args_Record
      is abstract new Glide_Kernel.Hook_Function_Record with null record;
   type Hook_Args is access all Hook_Args_Record;

   procedure Execute
     (Hook   : Hook_Args_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Hooks_Data'Class) is abstract;

   type Args_Execute is access procedure
     (Kernel : access Kernel_Handle_Record'Class; Data : Hooks_Data'Class);

   procedure Add_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Record'Class;
      Watch  : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : Args_Execute;
      Watch    : Glib.Object.GObject := null);
   --  See doc for Add_Hook above

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Record'Class);
   --  See doc for Remove_Hook above

   procedure Run_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : Hooks_Data'Class;
      Set_Busy : Boolean := True);
   --  See doc for Run_Hook above


   -------------------------------------------
   -- Hooks with arguments and return value --
   -------------------------------------------

   type Hook_Args_Return_Record
      is abstract new Glide_Kernel.Hook_Function_Record with null record;
   type Hook_Args_Return is access all Hook_Args_Return_Record;

   function Execute
     (Hook   : Hook_Args_Return_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Hooks_Data'Class) return Boolean is abstract;

   type Args_Return_Execute is access function
     (Kernel : access Kernel_Handle_Record'Class; Data : Hooks_Data'Class)
      return Boolean;

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : access Hook_Args_Return_Record'Class;
      Watch    : Glib.Object.GObject := null);
   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : Args_Return_Execute;
      Watch    : Glib.Object.GObject := null);
   --  See doc for Add_Hook above

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Return_Record'Class);
   --  See doc for Remove_Hook above

   function Run_Hook_Until_Success
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean;
   --  Same as Run_Doc above, but stops executing the functions as soon
   --  as one of the functions returns True.
   --  Return the value returned by the last function executed

   function Run_Hook_Until_Failure
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean;
   --  Same as above except stops as soon as a function returns False.

end Glide_Kernel.Hooks;
