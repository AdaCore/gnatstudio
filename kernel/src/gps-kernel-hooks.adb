-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2007, AdaCore              --
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

with Traces;                    use Traces;
with Glib.Object;               use Glib.Object;
with System;                    use System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Generic_List;
with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Traces;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

package body GPS.Kernel.Hooks is

   Me : constant Debug_Handle := Create ("Hooks", Default => GNAT.Traces.Off);

   use GNAT.Strings;
   use GPS.Kernel.Hooks_Hash.String_Hash_Table;

   No_Args_Data_Type_Name : constant String := "<no_args>";
   --  Data type for hooks with no arguments

   Hook_Class_Name : constant String := "Hook";

   Name_Cst     : aliased constant String := "name";
   Descr_Cst    : aliased constant String := "description";
   Type_Cst     : aliased constant String := "type";
   Function_Cst : aliased constant String := "function_name";
   Last_Cst     : aliased constant String := "last";
   Constructor_Parameters : constant Cst_Argument_List :=
     (1 => Name_Cst'Access);
   Add_Hook_Args : constant Cst_Argument_List :=
     (1 => Function_Cst'Access, 2 => Last_Cst'Access);
   Register_Hook_Args : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Descr_Cst'Access, 3 => Type_Cst'Access);

   Type_Prefix : constant String := "__type__";
   --  Prefix prepend to type names when stored in the hash table of hooks.
   --  This is used to avoid name conflicts between hooks and hook types.

   type Hook_Function_Description is record
      Func : Hook_Function;
      Name : GNAT.Strings.String_Access;
   end record;
   procedure Free (F : in out Hook_Function_Description);
   package Hooks_List is new Generic_List (Hook_Function_Description, Free);
   use Hooks_List;

   type Hook_Description;
   type Hook_Description_Access is access all Hook_Description'Class;

   type Hook_Property is new Instance_Property_Record with record
      Hook : Hook_Description_Access;
   end record;
   type Hook_Property_Access is access all Hook_Property'Class;

   type Subprogram_Wrapper_Creator is access function
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function;
   --  Create a wrapper around Subprogram. Whether we want to return nothing,
   --  a string, a boolean,... depends on the hook type.
   --  Destroying the returned type should also free the subprogram

   type Hook_Description (Is_Hook_Type : Boolean) is new Hook_Description_Base
   with record
      case Is_Hook_Type is
         when True =>
            From_Data                 : From_Callback_Data_Function;
         when False =>
            Name                      : GNAT.Strings.String_Access;
            Funcs                     : Hooks_List.List;
            Create_Subprogram_Wrapper : Subprogram_Wrapper_Creator;
            Command_Handler           : Module_Command_Function;
            Parameters_Type           : Hook_Description_Access;
      end case;
   end record;
   --  Describes a hook or a hook type.

   procedure Free (Hook : in out Hook_Description);
   --  See inherited doc

   function Get_Or_Create_Hook
     (Kernel                    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name                      : String;
      Data_Type_Name            : String := No_Args_Data_Type_Name;
      Create_Subprogram_Wrapper : Subprogram_Wrapper_Creator := null;
      Command_Handler           : Module_Command_Function := null)
      return Hook_Description_Access;
   --  Get or create a new hook.

   procedure Command_Handler_No_Return
     (Data : in out Callback_Data'Class; Command : String);
   procedure Command_Handler_Return_Boolean
     (Data : in out Callback_Data'Class; Command : String);
   procedure Command_Handler_Return_String
     (Data : in out Callback_Data'Class; Command : String);
   procedure Command_Handler_No_Args
     (Data : in out Callback_Data'Class; Command : String);
   --  Run a hook from the shell. Each of these procedures handles a different
   --  profile for the hook functions.

   type Hook_User_Data is record
      Kernel : Kernel_Handle;
      Name   : String_Access;
      Func   : Hook_Function;
   end record;
   type Hook_User_Data_Access is access Hook_User_Data;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Hook_User_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Hook_User_Data_Access, System.Address);

   procedure Remove_Hook_Cb
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address);
   pragma Convention (C, Remove_Hook_Cb);
   --  Called when Object is destroyed, to remove the hook in Data

   type Wrapper_No_Args is new Function_No_Args with record
      Func : Function_No_Args_Callback;
   end record;
   procedure Execute
     (Func : Wrapper_No_Args; Kernel : access Kernel_Handle_Record'Class);
   --  Wrapper for a simple Ada function

   type Wrapper_Args is new Function_With_Args with record
      Func : Function_With_Args_Callback;
   end record;
   procedure Execute
     (Func   : Wrapper_Args;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Wrapper for a simple Ada function

   type Wrapper_Return_Boolean is new Function_With_Args_Return_Boolean with
   record
      Func : Function_With_Args_Return_Boolean_Callback;
   end record;
   function Execute
     (Func   : Wrapper_Return_Boolean;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  Wrapper for a simple Ada function

   type Wrapper_Return_String is new Function_With_Args_Return_String with
   record
      Func : Function_With_Args_Return_String_Callback;
   end record;
   function Execute
     (Func   : Wrapper_Return_String;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return String;
   --  Wrapper for a simple Ada function

   type Subprogram_Wrapper_No_Args is new Function_No_Args with record
      Hook       : Hook_Description_Access;
      Subprogram : Subprogram_Type;
   end record;
   procedure Destroy (Func : in out Subprogram_Wrapper_No_Args);
   procedure Execute
     (Func   : Subprogram_Wrapper_No_Args;
      Kernel : access Kernel_Handle_Record'Class);
   function Create_Subprogram_Wrapper_No_Args
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function;
   --  Wrapper for a shell subprogram

   type Subprogram_Wrapper_No_Return is new Function_With_Args with record
      Hook       : Hook_Description_Access;
      Subprogram : Subprogram_Type;
   end record;
   procedure Destroy (Func : in out Subprogram_Wrapper_No_Return);
   procedure Execute
     (Func   : Subprogram_Wrapper_No_Return;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   function Create_Subprogram_Wrapper_No_Return
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function;
   --  Wrapper for a shell subprogram

   type Subprogram_Wrapper_Return_Boolean is
     new Function_With_Args_Return_Boolean with
   record
      Hook       : Hook_Description_Access;
      Subprogram : Subprogram_Type;
   end record;
   procedure Destroy (Func : in out Subprogram_Wrapper_Return_Boolean);
   function Execute
     (Func   : Subprogram_Wrapper_Return_Boolean;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   function Create_Subprogram_Wrapper_Return_Boolean
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function;
   --  Wrapper for a shell subprogram

   type Subprogram_Wrapper_Return_String is
     new Function_With_Args_Return_String with
   record
      Hook       : Hook_Description_Access;
      Subprogram : Subprogram_Type;
   end record;
   procedure Destroy (Func : in out Subprogram_Wrapper_Return_String);
   function Execute
     (Func   : Subprogram_Wrapper_Return_String;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return String;
   function Create_Subprogram_Wrapper_Return_String
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function;
   --  Wrapper for a shell subprogram

   type Shell_Hooks_Data is new Hooks_Data with null record;
   function Create_Callback_Data
     (Script    : access GNAT.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Shell_Hooks_Data) return Callback_Data_Access;
   function General_From_Callback_Data
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  A wrapper around a Callback_Data. This is used for hook types created in
   --  a shell language, and for which we do not know how to unmarshall the
   --  arguments contained in the callback_data

   function Wraps
     (Hook : access Hook_Function_Record'Class;
      Func : Subprogram_Type) return Boolean;
   --  True if Hook wraps Subprogram

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands related to hooks

   function Get_Data
     (Data : Callback_Data'Class; Nth : Natural)
      return Hook_Description_Access;
   --  Get the hook information contained in the nth-arg of Data

   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Info   : Hook_Description_Access;
      Func   : access GPS.Kernel.Hook_Function_Record'Class;
      Name   : String;
      Watch  : Glib.Object.GObject := null;
      Last   : Boolean := False);
   --  Same as Add_Hook, but directly with the hook description. This saves a
   --  look up in a hash table

   ------------------------
   -- Get_Or_Create_Hook --
   ------------------------

   function Get_Or_Create_Hook
     (Kernel                    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name                      : String;
      Data_Type_Name            : String := No_Args_Data_Type_Name;
      Create_Subprogram_Wrapper : Subprogram_Wrapper_Creator := null;
      Command_Handler           : Module_Command_Function := null)
     return Hook_Description_Access
  is
      Info : Hook_Description_Access := Hook_Description_Access
        (Get (Kernel.Hooks, Type_Prefix & Data_Type_Name));
      Hook : Hook_Description_Access := Hook_Description_Access
        (Get (Kernel.Hooks, Name));
   begin
      Assert (Me, Info = null or else Info.Is_Hook_Type,
              Data_Type_Name & " is not a hook type");

      if Info = null then
         --  Create a dummy hook type for now. It will be overriden when
         --  the user calls Register_Hook_Data_Type.
         Info := new Hook_Description'
           (Is_Hook_Type => True,
            From_Data    => null);
         Set (Kernel.Hooks, Type_Prefix & Data_Type_Name,
              Hook_Description_Base_Access (Info));
      end if;

      if Hook = null then
         Trace (Me, "Creating hook description for " & Name);
         Hook := new Hook_Description'
           (Is_Hook_Type              => False,
            Name                      => new String'(Name),
            Funcs                     => Hooks_List.Null_List,
            Create_Subprogram_Wrapper => Create_Subprogram_Wrapper,
            Command_Handler           => Command_Handler,
            Parameters_Type           => Info);
         Set (Kernel.Hooks, Name, Hook_Description_Base_Access (Hook));
      elsif Hook.Command_Handler = null then
         Trace (Me, "Updating hook description for " & Name);
         Hook.Command_Handler           := Command_Handler;
         Hook.Create_Subprogram_Wrapper := Create_Subprogram_Wrapper;
         Hook.Parameters_Type           := Info;
      elsif Command_Handler /= null then
         Insert (Kernel, -"Hook registered twice: " & Name);
      end if;
      return Hook;
   end Get_Or_Create_Hook;

   ----------
   -- Free --
   ----------

   procedure Free (Hook : in out Hook_Description) is
   begin
      if not Hook.Is_Hook_Type then
         Free (Hook.Name);
         Free (Hook.Funcs);
      end if;
   end Free;

   -----------------------------
   -- Register_Hook_Data_Type --
   -----------------------------

   procedure Register_Hook_Data_Type
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data_Type_Name : String;
      Args_Creator   : From_Callback_Data_Function)
   is
      Info : Hook_Description_Access := Hook_Description_Access
        (Get (Kernel.Hooks, Type_Prefix & Data_Type_Name));
   begin
      if Info = null then
         Info := new Hook_Description'
           (Is_Hook_Type => True,
            From_Data    => Args_Creator);

         if Data_Type_Name'Length >= Type_Prefix'Length
           and then Data_Type_Name
             (Data_Type_Name'First
              .. Data_Type_Name'First + Type_Prefix'Length - 1) =
             Type_Prefix
         then
            Insert (Kernel, -"Hook names cannot start with " & Type_Prefix);
         end if;

         Set (Kernel.Hooks, Type_Prefix & Data_Type_Name,
              Hook_Description_Base_Access (Info));
      elsif Info.From_Data = null then
         --  In case the hook was created automatically through a call to
         --  Add_Hook, but not registered before
         Info.From_Data := Args_Creator;
      else
         Insert (Kernel, -"Hook type registered twice: " & Data_Type_Name);
      end if;
   end Register_Hook_Data_Type;

   -------------------------------
   -- Command_Handler_No_Return --
   -------------------------------

   procedure Command_Handler_No_Return
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String                  := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle           := Get_Kernel (Data);
      Info   : constant Hook_Description_Access :=
                 Get_Or_Create_Hook (Kernel, Name);
   begin
      Assert (Me, Command = "run", "Invalid command: " & Command);
      if Info.Parameters_Type = null
        or else Info.Parameters_Type.From_Data = null
      then
         Set_Error_Msg (Data, -"Unknown hook: " & Name);
      else
         declare
            Args : aliased Hooks_Data'Class :=
                     Info.Parameters_Type.From_Data (Data);
         begin
            Run_Hook (Kernel, Name, Args'Unchecked_Access, Set_Busy => True);
            Destroy (Args);
         end;
      end if;
   end Command_Handler_No_Return;

   -----------------------------
   -- Register_Hook_No_Return --
   -----------------------------

   procedure Register_Hook_No_Return
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Data_Type_Name : String)
   is
      Info : Hook_Description_Access;
      pragma Unreferenced (Info);
   begin
      Info := Get_Or_Create_Hook
        (Kernel, Name,
         Data_Type_Name            => Data_Type_Name,
         Create_Subprogram_Wrapper =>
           Create_Subprogram_Wrapper_No_Return'Access,
         Command_Handler           => Command_Handler_No_Return'Access);
   end Register_Hook_No_Return;

   ------------------------------------
   -- Command_Handler_Return_Boolean --
   ------------------------------------

   procedure Command_Handler_Return_Boolean
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String                  := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle           := Get_Kernel (Data);
      Info   : constant Hook_Description_Access :=
        Get_Or_Create_Hook (Kernel, Name);
   begin
      Assert (Me, Command = "run"
              or else Command = "run_until_failure"
              or else Command = "run_until_success",
              "Invalid command: " & Command);
      if Info.Parameters_Type = null
        or else Info.Parameters_Type.From_Data = null
      then
         Set_Error_Msg (Data, -"Unknown hook: " & Name);
      else
         declare
            Args : aliased Hooks_Data'Class :=
              Info.Parameters_Type.From_Data (Data);
         begin
            if Command = "run" or else Command = "run_until_success" then
               Set_Return_Value
                 (Data, Run_Hook_Until_Success
                    (Kernel, Name, Args'Unchecked_Access, Set_Busy => True));
            else
               Set_Return_Value
                 (Data, Run_Hook_Until_Failure
                    (Kernel, Name, Args'Unchecked_Access, Set_Busy => True));
            end if;
            Destroy (Args);
         end;
      end if;
   end Command_Handler_Return_Boolean;

   ----------------------------------
   -- Register_Hook_Return_Boolean --
   ----------------------------------

   procedure Register_Hook_Return_Boolean
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Data_Type_Name : String)
   is
      Info : Hook_Description_Access;
      pragma Unreferenced (Info);
   begin
      Info := Get_Or_Create_Hook
        (Kernel, Name,
         Data_Type_Name => Data_Type_Name,
         Create_Subprogram_Wrapper =>
           Create_Subprogram_Wrapper_Return_Boolean'Access,
         Command_Handler => Command_Handler_Return_Boolean'Access);
   end Register_Hook_Return_Boolean;

   -----------------------------------
   -- Command_Handler_Return_String --
   -----------------------------------

   procedure Command_Handler_Return_String
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String                  := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle           := Get_Kernel (Data);
      Info   : constant Hook_Description_Access :=
                 Get_Or_Create_Hook (Kernel, Name);
   begin
      Assert (Me, Command = "run", "Invalid command: " & Command);
      if Info.Parameters_Type = null
        or else Info.Parameters_Type.From_Data = null
      then
         Set_Error_Msg (Data, -"Unknown hook: " & Name);
      else
         declare
            Args : aliased Hooks_Data'Class :=
              Info.Parameters_Type.From_Data (Data);
         begin
            Set_Return_Value
              (Data, Run_Hook_Until_Not_Empty
                 (Kernel, Name, Args'Unchecked_Access, Set_Busy => True));
            Destroy (Args);
         end;
      end if;
   end Command_Handler_Return_String;

   ---------------------------------
   -- Register_Hook_Return_String --
   ---------------------------------

   procedure Register_Hook_Return_String
     (Kernel         : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name           : String;
      Data_Type_Name : String)
   is
      Info : Hook_Description_Access;
      pragma Unreferenced (Info);
   begin
      Info := Get_Or_Create_Hook
        (Kernel, Name, Data_Type_Name => Data_Type_Name,
         Create_Subprogram_Wrapper =>
           Create_Subprogram_Wrapper_Return_String'Access,
         Command_Handler => Command_Handler_Return_String'Access);
   end Register_Hook_Return_String;

   -----------------------------
   -- Command_Handler_No_Args --
   -----------------------------

   procedure Command_Handler_No_Args
     (Data : in out Callback_Data'Class; Command : String)
   is
      Name   : constant String        := Get_Hook_Name (Data, 1);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      Assert (Me, Command = "run", "Invalid command: " & Command);
      Run_Hook (Kernel, Name, Set_Busy => True);
   end Command_Handler_No_Args;

   ---------------------------
   -- Register_Hook_No_Args --
   ---------------------------

   procedure Register_Hook_No_Args
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name   : String)
   is
      Info : Hook_Description_Access;
      pragma Unreferenced (Info);
   begin
      Info := Get_Or_Create_Hook
        (Kernel, Name,
         Data_Type_Name            => No_Args_Data_Type_Name,
         Create_Subprogram_Wrapper =>
           Create_Subprogram_Wrapper_No_Args'Access,
         Command_Handler           => Command_Handler_No_Args'Access);
   end Register_Hook_No_Args;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Info   : Hook_Description_Access;
      Func   : access GPS.Kernel.Hook_Function_Record'Class;
      Name   : String;
      Watch  : Glib.Object.GObject := null;
      Last   : Boolean := False)
   is
   begin
      if Active (Me) then
         Trace
           (Me, "Adding function to hook " & Info.Name.all & ": " & Name);
      end if;

      if Last then
         Append (Info.Funcs,
                 (Func => Hook_Function (Func), Name => new String'(Name)));
      else
         Prepend (Info.Funcs,
                  (Func => Hook_Function (Func), Name => new String'(Name)));
      end if;
      Func.Ref_Count := Func.Ref_Count + 1;

      if Watch /= null then
         Func.Ref_Count := Func.Ref_Count + 1;
         Weak_Ref
           (Watch, Remove_Hook_Cb'Access,
            Convert (new Hook_User_Data'(Kernel_Handle (Kernel),
                     new String'(Info.Name.all),
                     Hook_Function (Func))));
      end if;
   end Add_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook   : String;
      Func   : access GPS.Kernel.Hook_Function_Record'Class;
      Name   : String;
      Watch  : Glib.Object.GObject := null)
   is
      Info : constant Hook_Description_Access :=
        Get_Or_Create_Hook (Kernel, Hook);
      --  If the information about that hook (parameters or command handler)
      --  is not know yet, it will be set later when calling Register_Hook_*
   begin
      if Info /= null then
         Add_Hook (Kernel, Info, Func, Name, Watch);
      end if;
   end Add_Hook;

   -----------------
   -- Remove_Hook --
   -----------------

   procedure Remove_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook   : String;
      Func   : access GPS.Kernel.Hook_Function_Record'Class)
   is
      Info    : constant Hook_Description_Access :=
                  Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N, Prev : Hooks_List.List_Node := Hooks_List.Null_Node;
   begin
      if Info /= null then
         N := Hooks_List.First (Info.Funcs);
         while N /= Null_Node loop
            if Hooks_List.Data (N).Func = Hook_Function (Func) then
               Trace (Me, "Remove_Hook " & Hook
                      & " function: " & Hooks_List.Data (N).Name.all);
               Remove_Nodes (Info.Funcs, Prev, N);
               return;
            end if;

            Prev := N;
            N := Next (N);
         end loop;
         Trace (Me, "Remove_Hook " & Hook & " couldn't find function");
      else
         Trace (Me, "Remove_Hook, no description available for hook "
                & Hook);
      end if;
   end Remove_Hook;

   ------------------------
   -- Get_Hook_Func_List --
   ------------------------

   function Get_Hook_Func_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook   : String) return String_List
   is
      Info    : constant Hook_Description_Access :=
                  Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N       : Hooks_List.List_Node := Hooks_List.Null_Node;
      N_Funcs : Natural;
      F_Index : Natural;
   begin
      if Info /= null then
         N := Hooks_List.First (Info.Funcs);
         N_Funcs := 0;
         while N /= Null_Node loop
            N_Funcs := N_Funcs + 1;
            N := Next (N);
         end loop;

         declare
            Ret : String_List (1 .. N_Funcs);
         begin
            F_Index := 1;
            N := Hooks_List.First (Info.Funcs);

            while N /= Null_Node loop
               Ret (F_Index) := Hooks_List.Data (N).Name;
               F_Index := F_Index + 1;
               N := Next (N);
            end loop;

            return Ret;
         end;
      else
         return (1 .. 0 => null);
      end if;
   end Get_Hook_Func_List;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Func : Wrapper_No_Args; Kernel : access Kernel_Handle_Record'Class) is
   begin
      Func.Func (Kernel);
   end Execute;

   -------------
   -- Wrapper --
   -------------

   function Wrapper
     (Callback : Function_No_Args_Callback)
      return Function_No_Args_Access is
   begin
      return new Wrapper_No_Args'(Function_No_Args with Func => Callback);
   end Wrapper;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Func   : Wrapper_Args;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) is
   begin
      Func.Func (Kernel, Data);
   end Execute;

   -------------
   -- Wrapper --
   -------------

   function Wrapper
     (Callback : Function_With_Args_Callback)
      return Function_With_Args_Access is
   begin
      return new Wrapper_Args'(Function_With_Args with Func => Callback);
   end Wrapper;

   -------------
   -- Execute --
   -------------

   function Execute
     (Func   : Wrapper_Return_Boolean;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean is
   begin
      return Func.Func (Kernel, Data);
   end Execute;

   -------------
   -- Wrapper --
   -------------

   function Wrapper
     (Callback : Function_With_Args_Return_Boolean_Callback)
      return Function_With_Args_Return_Boolean_Access is
   begin
      return new Wrapper_Return_Boolean'
        (Function_With_Args_Return_Boolean with Func => Callback);
   end Wrapper;

   -------------
   -- Execute --
   -------------

   function Execute
     (Func   : Wrapper_Return_String;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return String is
   begin
      return Func.Func (Kernel, Data);
   end Execute;

   -------------
   -- Wrapper --
   -------------

   function Wrapper
     (Callback : Function_With_Args_Return_String_Callback)
      return Function_With_Args_Return_String_Access is
   begin
      return new Wrapper_Return_String'
        (Function_With_Args_Return_String with Func => Callback);
   end Wrapper;

   ---------------------------------------
   -- Create_Subprogram_Wrapper_No_Args --
   ---------------------------------------

   function Create_Subprogram_Wrapper_No_Args
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function is
   begin
      return new Subprogram_Wrapper_No_Args'
        (Function_No_Args with Subprogram => Subprogram, Hook => Hook);
   end Create_Subprogram_Wrapper_No_Args;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Func : in out Subprogram_Wrapper_No_Args) is
   begin
      Free (Func.Subprogram);
   end Destroy;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Func   : Subprogram_Wrapper_No_Args;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Tmp  : Boolean;
      pragma Unreferenced (Tmp, Kernel);
      Data : Callback_Data'Class :=
               Create (Get_Script (Func.Subprogram.all), 1);
   begin
      Set_Nth_Arg (Data, 1, Func.Hook.Name.all);
      Tmp := Execute (Func.Subprogram, Data);
      Free (Data);
   end Execute;

   -----------------------------------------
   -- Create_Subprogram_Wrapper_No_Return --
   -----------------------------------------

   function Create_Subprogram_Wrapper_No_Return
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function is
   begin
      return new Subprogram_Wrapper_No_Return'
        (Function_With_Args with Subprogram => Subprogram, Hook => Hook);
   end Create_Subprogram_Wrapper_No_Return;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Func : in out Subprogram_Wrapper_No_Return) is
   begin
      Free (Func.Subprogram);
   end Destroy;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Func   : Subprogram_Wrapper_No_Return;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
      D   : Callback_Data_Access :=
              Get (Kernel, Data.Data, Get_Script (Func.Subprogram.all));
   begin
      if D = null then
         D := Create_Callback_Data
           (Script    => Get_Script (Func.Subprogram.all),
            Hook_Name => Func.Hook.Name.all,
            Data      => Data);
         Set (Kernel, Data.Data, Get_Script (Func.Subprogram.all), D);
      end if;
      Tmp := Execute (Func.Subprogram, D.all);
   end Execute;

   ----------------------------------------------
   -- Create_Subprogram_Wrapper_Return_Boolean --
   ----------------------------------------------

   function Create_Subprogram_Wrapper_Return_Boolean
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function is
   begin
      return new Subprogram_Wrapper_Return_Boolean'
        (Function_With_Args_Return_Boolean
         with Subprogram => Subprogram, Hook => Hook);
   end Create_Subprogram_Wrapper_Return_Boolean;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Func : in out Subprogram_Wrapper_Return_Boolean) is
   begin
      Free (Func.Subprogram);
   end Destroy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Func   : Subprogram_Wrapper_Return_Boolean;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D : Callback_Data_Access :=
            Get (Kernel, Data.Data, Get_Script (Func.Subprogram.all));
   begin
      if D = null then
         D := Create_Callback_Data
           (Script    => Get_Script (Func.Subprogram.all),
            Hook_Name => Func.Hook.Name.all,
            Data      => Data);
         Set (Kernel, Data.Data, Get_Script (Func.Subprogram.all), D);
      end if;
      return Execute (Func.Subprogram, D.all);
   end Execute;

   ---------------------------------------------
   -- Create_Subprogram_Wrapper_Return_String --
   ---------------------------------------------

   function Create_Subprogram_Wrapper_Return_String
     (Subprogram : Subprogram_Type;
      Hook       : Hook_Description_Access) return Hook_Function is
   begin
      return new Subprogram_Wrapper_Return_String'
        (Function_With_Args_Return_String
         with Subprogram => Subprogram, Hook => Hook);
   end Create_Subprogram_Wrapper_Return_String;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Func : in out Subprogram_Wrapper_Return_String) is
   begin
      Free (Func.Subprogram);
   end Destroy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Func   : Subprogram_Wrapper_Return_String;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return String
   is
      D : Callback_Data_Access :=
            Get (Kernel, Data.Data, Get_Script (Func.Subprogram.all));
   begin
      if D = null then
         D := Create_Callback_Data
           (Script    => Get_Script (Func.Subprogram.all),
            Hook_Name => Func.Hook.Name.all,
            Data      => Data);
         Set (Kernel, Data.Data, Get_Script (Func.Subprogram.all), D);
      end if;
      return Execute (Func.Subprogram, D.all);
   end Execute;

   --------------
   -- Run_Hook --
   --------------

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Set_Busy : Boolean := True)
   is
      Info : constant Hook_Description_Access :=
               Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N    : List_Node := Null_Node;
      F    : Hook_Function_Description;
   begin
      if Info = null then
         Insert (Kernel, -"No such hook: " & Hook);
      else
         if Set_Busy then
            Push_State (Kernel_Handle (Kernel), Busy);
         end if;

         Trace (Me, "Run_Hook: " & Hook);
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Data (N);
            Assert (Me, F.Func.all in Function_No_Args'Class,
                    "Hook expects no argument: " & Hook
                    & " for function: " & F.Name.all);
            Execute (Function_No_Args_Access (F.Func).all, Kernel);
            N := Next (N);
         end loop;

         if Set_Busy then
            Pop_State (Kernel_Handle (Kernel));
         end if;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Run_Hook;

   --------------
   -- Run_Hook --
   --------------

   procedure Run_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True)
   is
      Info : constant Hook_Description_Access :=
               Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N    : List_Node := Null_Node;
      F    : Hook_Function_Description;
   begin
      if Info = null then
         Insert (Kernel, -"No such hook: " & Hook);
      else
         if Set_Busy then
            Push_State (Kernel_Handle (Kernel), Busy);
         end if;

         Trace (Me, "Run_Hook: " & Hook);
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.Func.all in Function_With_Args'Class,
                    "Hook expects arguments: " & Hook
                    & " for function: " & F.Name.all);
            Execute (Function_With_Args_Access (F.Func).all, Kernel, Data);
            N := Next (N);
         end loop;

         Free (Data.Data);

         if Set_Busy then
            Pop_State (Kernel_Handle (Kernel));
         end if;
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Run_Hook;

   ----------------------------
   -- Run_Hook_Until_Success --
   ----------------------------

   function Run_Hook_Until_Success
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean
   is
      Info : constant Hook_Description_Access :=
               Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N    : List_Node := Null_Node;
      F    : Hook_Function_Description;
      Tmp  : Boolean := False;
   begin
      if Info = null then
         Insert (Kernel, -"No such hook: " & Hook);
      else
         if Set_Busy then
            Push_State (Kernel_Handle (Kernel), Busy);
         end if;

         Trace (Me, "Run_Hook: " & Hook);
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.Func.all in Function_With_Args_Return_Boolean'Class,
                    "Hook expects arguments and return boolean: " & Hook
                    & " for function: " & F.Name.all);
            Tmp := Execute
              (Function_With_Args_Return_Boolean_Access (F.Func).all,
               Kernel, Data);
            exit when Tmp;
            N := Next (N);
         end loop;

         Free (Data.Data);

         if Set_Busy then
            Pop_State (Kernel_Handle (Kernel));
         end if;
      end if;
      return Tmp;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Run_Hook_Until_Success;

   ----------------------------
   -- Run_Hook_Until_Failure --
   ----------------------------

   function Run_Hook_Until_Failure
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return Boolean
   is
      Info : constant Hook_Description_Access :=
               Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N    : List_Node := Null_Node;
      F    : Hook_Function_Description;
      Tmp  : Boolean := True;
   begin
      if Info = null then
         Insert (Kernel, -"No such hook: " & Hook);
      else
         if Set_Busy then
            Push_State (Kernel_Handle (Kernel), Busy);
         end if;

         Trace (Me, "Run_Hook: " & Hook);
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.Func.all in Function_With_Args_Return_Boolean'Class,
                    "Hook expects arguments and return boolean: " & Hook
                    & " for function: " & F.Name.all);
            Tmp := Execute
              (Function_With_Args_Return_Boolean_Access (F.Func).all,
               Kernel, Data);
            exit when not Tmp;
            N := Next (N);
         end loop;

         Free (Data.Data);

         if Set_Busy then
            Pop_State (Kernel_Handle (Kernel));
         end if;
      end if;

      return Tmp;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Run_Hook_Until_Failure;

   ------------------------------
   -- Run_Hook_Until_Not_Empty --
   ------------------------------

   function Run_Hook_Until_Not_Empty
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Hook     : String;
      Data     : access Hooks_Data'Class;
      Set_Busy : Boolean := True) return String
   is
      Info : constant Hook_Description_Access :=
               Hook_Description_Access (Get (Kernel.Hooks, Hook));
      N    : List_Node := Null_Node;
      F    : Hook_Function_Description;
   begin
      if Info = null then
         Insert (Kernel, -"No such hook: " & Hook);
      else
         if Set_Busy then
            Push_State (Kernel_Handle (Kernel), Busy);
         end if;

         Trace (Me, "Run_Hook: " & Hook);
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.Func.all in Function_With_Args_Return_String'Class,
                    "Hook expects arguments and return string: " & Hook
                    & " for function: " & F.Name.all);
            declare
               Tmp : constant String := Execute
                 (Function_With_Args_Return_String_Access (F.Func).all,
                  Kernel, Data);
            begin
               if Tmp /= "" then
                  if Set_Busy then
                     Pop_State (Kernel_Handle (Kernel));
                     return Tmp;
                  end if;
               end if;
            end;

            N := Next (N);
         end loop;

         Free (Data.Data);

         if Set_Busy then
            Pop_State (Kernel_Handle (Kernel));
         end if;
      end if;
      return "";

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return "";
   end Run_Hook_Until_Not_Empty;

   --------------------
   -- Remove_Hook_Cb --
   --------------------

   procedure Remove_Hook_Cb
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address)
   is
      pragma Unreferenced (Where_The_Object_Was);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hook_User_Data, Hook_User_Data_Access);
      D : Hook_User_Data_Access := Convert (Data);
   begin
      Remove_Hook (D.Kernel, D.Name.all, D.Func);
      D.Func.Ref_Count := D.Func.Ref_Count - 1;
      Free (D.Name);
      Unchecked_Free (D);
   end Remove_Hook_Cb;

   ----------
   -- Free --
   ----------

   procedure Free (F : in out Hook_Function_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hook_Function_Record'Class, Hook_Function);
   begin
      F.Func.Ref_Count := F.Func.Ref_Count - 1;

      if F.Func.Ref_Count = 0 then
         Free (F.Name);
         Destroy (F.Func.all);
         Unchecked_Free (F.Func);
      end if;
   end Free;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   function Create_Callback_Data
     (Script    : access Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Shell_Hooks_Data) return Callback_Data_Access
   is
      D : Callback_Data_Access := Get (Get_Kernel (Script), Data.Data, Script);
   begin
      if D = null then
         --  "general" type hooks cannot be passed across languages with their
         --  full arguments. We just create a dummy data that only includes the
         --  hook name
         --  Do not store the newly created callback data into Data yet, this
         --  will be done by the caller of Create_Callback_Data
         D := new Callback_Data'Class'(Create (Script, 1));
         Set_Nth_Arg (D.all, 1, Hook_Name);
      end if;
      return D;
   end Create_Callback_Data;

   --------------------------------
   -- General_From_Callback_Data --
   --------------------------------

   function General_From_Callback_Data
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      D    : constant Callback_Data_Access :=
               new Callback_Data'Class'(Clone (Data));
      Args : Shell_Hooks_Data;
   begin
      --  The first parameter should not be a hook instance, but the hook name
      Set_Nth_Arg (D.all, 1, Get_Hook_Name (Data, 1));
      Set (Get_Kernel (Data), Args.Data, Get_Script (Data), D);
      return Args;
   end General_From_Callback_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; Nth : Natural) return Hook_Description_Access
   is
      Instance : constant Class_Instance :=
        Nth_Arg (Data, Nth, Get_Hook_Class (Get_Kernel (Data)));
   begin
      return Hook_Property_Access
        (Instance_Property'(Get_Data (Instance, Hook_Class_Name))).Hook;
   end Get_Data;

   -----------
   -- Wraps --
   -----------

   function Wraps
     (Hook : access Hook_Function_Record'Class;
      Func : Subprogram_Type) return Boolean
   is
   begin
      --  ??? We should have a primitive operationof Hook_Function_Record, but
      --  that would mean that gps-kernel.ads knows about Subprogram_Type...

      --  In the tests below, we compare the names, instead of directly Func,
      --  since the latter really depends on the language. Comparing names is
      --  less precise, but should work in all situations.

      if Hook.all in Subprogram_Wrapper_No_Args'Class then
         return Get_Name (Func) =
           Get_Name (Subprogram_Wrapper_No_Args (Hook.all).Subprogram);

      elsif Hook.all in Subprogram_Wrapper_No_Return'Class then
         return Get_Name (Func) =
           Get_Name (Subprogram_Wrapper_No_Return (Hook.all).Subprogram);

      elsif Hook.all in Subprogram_Wrapper_Return_Boolean'Class then
         return Get_Name (Func) =
           Get_Name (Subprogram_Wrapper_Return_Boolean (Hook.all).Subprogram);

      elsif Hook.all in Subprogram_Wrapper_Return_String'Class then
         return Get_Name (Func) =
           Get_Name (Subprogram_Wrapper_Return_String (Hook.all).Subprogram);

      else
         return False;
      end if;
   end Wraps;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Info : Hook_Description_Access;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Constructor_Parameters);
         declare
            Name     : constant String := Nth_Arg (Data, 2);
            Class    : constant Class_Type :=
                         Get_Hook_Class (Get_Kernel (Data));
            Instance : Class_Instance;
         begin
            Info :=
              Hook_Description_Access (Get (Get_Kernel (Data).Hooks, Name));
            if Info = null then
               Set_Error_Msg (Data, -"No such hook: " & Name);
            else
               Instance := Nth_Arg (Data, 1, Class);
               Set_Data
                 (Instance, Hook_Class_Name, Hook_Property'(Hook => Info));
            end if;
         end;

      elsif Command = "run"
        or else Command = "run_until_success"
        or else Command = "run_until_failure"
      then
         begin
            Info := Get_Data (Data, 1);
            Info.Command_Handler (Data, Command);
         exception
            when No_Such_Parameter =>
               Trace (Me, "Invalid number of parameters for hook");
               Set_Error_Msg
                 (Data, "Invalid number of parameters in call to Hook.run");
         end;

      elsif Command = "add" then
         Name_Parameters (Data, Add_Hook_Args);
         Info := Get_Data (Data, 1);
         declare
            Func : constant Subprogram_Type := Nth_Arg (Data, 2);
            Last : constant Boolean := Nth_Arg (Data, 3, False);
         begin
            if Info = null then
               Set_Error_Msg (Data, "Unknown hook");
            elsif Info.Create_Subprogram_Wrapper = null then
               Set_Error_Msg
                 (Data, "Cannot create internal wrapper for this hook");
            else
               Add_Hook (Kernel => Get_Kernel (Data),
                         Info   => Info,
                         Func   => Info.Create_Subprogram_Wrapper (Func, Info),
                         Name   => Get_Name (Func),
                         Last   => Last);
            end if;
         end;

      elsif Command = "remove" then
         Name_Parameters (Data, Add_Hook_Args);
         Info := Get_Data (Data, 1);
         declare
            Func  : constant Subprogram_Type := Nth_Arg (Data, 2);
            Iter  : Hooks_List.List_Node;
            Descr : Hook_Function_Description;
         begin
            if Info = null then
               Set_Error_Msg (Data, "Unknown hook");
            else
               Iter := Hooks_List.First (Info.Funcs);
               while Iter /= Hooks_List.Null_Node loop
                  Descr := Hooks_List.Data (Iter);
                  if Descr.Func /= null
                    and then Wraps (Descr.Func, Func)
                  then
                     Remove_Hook
                       (Get_Kernel (Data), Info.Name.all, Descr.Func);
                     exit;
                  end if;

                  Iter := Hooks_List.Next (Iter);
               end loop;
            end if;
         end;

      elsif Command = "register" then
         Name_Parameters (Data, Register_Hook_Args);
         declare
            Name : constant String := Nth_Arg (Data, 1);
            Typ  : constant String := Nth_Arg (Data, 2, "");
         begin
            if Typ = "" then
               Register_Hook_No_Args (Get_Kernel (Data), Name);
            else
               Register_Hook_No_Return
                 (Get_Kernel (Data), Name, Data_Type_Name => Typ);
            end if;
         end;

      elsif Command = "list" then
         declare
            Iter : Iterator;
            Info : Hook_Description_Access;
         begin
            Set_Return_Value_As_List (Data);
            Get_First (Get_Kernel (Data).Hooks, Iter);
            loop
               Info := Hook_Description_Access (Get_Element (Iter));
               exit when Info = null;

               declare
                  Name : constant String := Get_Key (Iter);
               begin
                  if Name'Length < Type_Prefix'Length
                    or else Type_Prefix /=
                      Name (Name'First .. Name'First + Type_Prefix'Length - 1)
                  then
                     Set_Return_Value (Data, Name);
                  end if;
               end;

               Get_Next (Get_Kernel (Data).Hooks, Iter);
            end loop;
         end;

      elsif Command = "describe_functions" then
         Info := Get_Data (Data, 1);
         declare
            Iter : Hooks_List.List_Node;
         begin
            Set_Return_Value_As_List (Data);

            --  Rest is the list of functions connected to that hook
            Iter := Hooks_List.First (Info.Funcs);

            while Iter /= Hooks_List.Null_Node loop
               Set_Return_Value (Data, Hooks_List.Data (Iter).Name.all);
               Iter := Hooks_List.Next (Iter);
            end loop;
         end;

      elsif Command = "list_types" then
         declare
            Iter : Iterator;
            Info : Hook_Description_Access;
         begin
            Get_First (Get_Kernel (Data).Hooks, Iter);
            Set_Return_Value_As_List (Data);
            loop
               Info := Hook_Description_Access (Get_Element (Iter));
               exit when Info = null;

               declare
                  Name : constant String := Get_Key (Iter);
               begin
                  if Name'Length > Type_Prefix'Length
                    and then Type_Prefix =
                      Name (Name'First .. Name'First + Type_Prefix'Length - 1)
                  then
                     Set_Return_Value
                       (Data,
                        Name (Name'First + Type_Prefix'Length .. Name'Last));
                  end if;
               end;

               Get_Next (Get_Kernel (Data).Hooks, Iter);
            end loop;
         end;

      else
         --  Marshaller for hooks with no arguments
         --  This includes Command="__run_hook__" and
         --  Command="__run_hook__<name>" for all hooks with no arguments

         Info := Get_Data (Data, 1);
         Run_Hook (Get_Kernel (Data), Info.Name.all);
      end if;
   end Default_Command_Handler;

   -------------------
   -- Get_Hook_Name --
   -------------------

   function Get_Hook_Name
     (Data : Callback_Data'Class; Nth : Natural) return String
   is
      Info : constant Hook_Description_Access := Get_Data (Data, Nth);
   begin
      return Info.Name.all;
   end Get_Hook_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Hooks_Data) is
      pragma Unreferenced (Data);
   begin
      null;
   end Destroy;

   -----------------------------
   -- Register_Standard_Hooks --
   -----------------------------

   procedure Register_Standard_Hooks
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Hook_Class : constant Class_Type := Get_Hook_Class (Kernel);
   begin
      Register_Hook_Data_Type
        (Kernel, "general",
         General_From_Callback_Data'Access);

      Register_Hook_No_Args (Kernel, Preferences_Changed_Hook);
      Register_Hook_No_Args (Kernel, Contextual_Menu_Open_Hook);
      Register_Hook_No_Args (Kernel, Contextual_Menu_Close_Hook);
      Register_Hook_No_Args (Kernel, Search_Reset_Hook);
      Register_Hook_No_Args (Kernel, Search_Functions_Changed_Hook);
      Register_Hook_No_Args (Kernel, Search_Regexps_Changed_Hook);
      Register_Hook_No_Args (Kernel, Variable_Changed_Hook);
      Register_Hook_No_Args (Kernel, Project_View_Changed_Hook);
      Register_Hook_No_Args (Kernel, Project_Changed_Hook);
      Register_Hook_No_Return
        (Kernel, Compilation_Finished_Hook, String_Hook_Type);
      Register_Hook_No_Return (Kernel, Project_Saved_Hook, Project_Hook_Type);
      Register_Hook_No_Return
        (Kernel, Context_Changed_Hook, Context_Hook_Type);
      Register_Hook_No_Return (Kernel, Project_Changing_Hook, File_Hook_Type);
      Register_Hook_No_Return (Kernel, File_Edited_Hook, File_Hook_Type);
      Register_Hook_No_Return (Kernel, File_Closed_Hook, File_Hook_Type);
      Register_Hook_No_Return (Kernel, Before_File_Saved_Hook, File_Hook_Type);
      Register_Hook_No_Return (Kernel, File_Saved_Hook, File_Hook_Type);
      Register_Hook_No_Return (Kernel, File_Deleted_Hook, File_Hook_Type);
      Register_Hook_No_Return (Kernel, File_Renamed_Hook, Files_2_Hook_Type);
      Register_Hook_No_Return
        (Kernel, File_Changed_On_Disk_Hook, File_Hook_Type);
      Register_Hook_No_Return
        (Kernel, Source_Lines_Revealed_Hook, Context_Hook_Type);
      Register_Hook_Return_Boolean
        (Kernel, File_Changed_Detected_Hook, File_Hook_Type);

      Register_Hook_Return_Boolean
        (Kernel, Compilation_Starting_Hook, String_Boolean_Hook_Type);

      Register_Command
        (Kernel, Constructor_Method,
         Class        => Hook_Class,
         Minimum_Args => Constructor_Parameters'Length,
         Maximum_Args => Constructor_Parameters'Length,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "run",
         Class        => Hook_Class,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "run_until_success",
         Class        => Hook_Class,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "run_until_failure",
         Class        => Hook_Class,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Hook_Class,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "remove",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class        => Hook_Class,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "register",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class         => Hook_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list",
         Class         => Hook_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);

      Register_Command
        (Kernel, "describe_functions",
         Class        => Hook_Class,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_types",
         Class         => Hook_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
   end Register_Standard_Hooks;

end GPS.Kernel.Hooks;
