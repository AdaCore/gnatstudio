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

with Traces;       use Traces;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Glib.Object;  use Glib.Object;
with System;       use System;
with Ada.Tags;     use Ada.Tags;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Glide_Intl; use Glide_Intl;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;

package body Glide_Kernel.Hooks is

   Me : constant Debug_Handle := Create ("Hooks");

   Name_Cst     : aliased constant String := "name";
   Descr_Cst    : aliased constant String := "description";
   Type_Cst     : aliased constant String := "type";
   Arg_Cst      : aliased constant String := "arg";
   Function_Cst : aliased constant String := "function_name";
   Run_Hook_Args : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Arg_Cst'Access);
   Add_Hook_Args : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Function_Cst'Access);
   Register_Hook_Args : constant Cst_Argument_List :=
     (1 => Name_Cst'Access, 2 => Descr_Cst'Access, 3 => Type_Cst'Access);

   Type_Prefix : constant String := "__type__";
   --  Prefix prepend to type names when stored in the hash table of hooks.
   --  This is used to avoid name conflicts between hooks and hook types.

   procedure Free (F : in out Hook_Function);
   package Hooks_List is new Generic_List (Hook_Function, Free);
   use Hooks_List;

   type Hook_Description is new Hook_Description_Base with record
      Funcs           : Hooks_List.List;
      Description     : GNAT.OS_Lib.String_Access;
      Command_Handler : Module_Command_Function;
      Profile         : Hook_Type;
   end record;
   type Hook_Description_Access is access all Hook_Description'Class;

   procedure Free (Hook : in out Hook_Description);
   --  See inherited doc


   type Shell_Wrapper_Record is new Hook_Args_Record with record
      Func   : GNAT.OS_Lib.String_Access;
      Hook   : GNAT.OS_Lib.String_Access;
      Script : Scripting_Language;
   end record;
   type Shell_Wrapper is access all Shell_Wrapper_Record'Class;
   procedure Destroy (Wrapper : in out Shell_Wrapper_Record);
   procedure Execute
     (Wrapper : Shell_Wrapper_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class);
   --  See inherited doc
   --  This wrapper is used to give access to hooks from shell languages.

   type Shell_Wrapper_Return_Record is new Hook_Args_Return_Record with record
      Func   : GNAT.OS_Lib.String_Access;
      Hook   : GNAT.OS_Lib.String_Access;
      Script : Scripting_Language;
   end record;
   type Shell_Wrapper_Return is access all Shell_Wrapper_Return_Record'Class;
   procedure Destroy (Wrapper : in out Shell_Wrapper_Return_Record);
   function Execute
     (Wrapper : Shell_Wrapper_Return_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class) return Boolean;
   --  See inherited doc
   --  This wrapper is used to give access to hooks from shell languages.

   type Shell_Wrapper_Shell_Record is new Hook_Shell_Args_Record with record
      Func   : GNAT.OS_Lib.String_Access;
      Script : Scripting_Language;
   end record;
   type Shell_Wrapper_Shell is access all Shell_Wrapper_Shell_Record'Class;
   procedure Destroy (Wrapper : in out Shell_Wrapper_Shell_Record);
   procedure Execute
     (Wrapper : Shell_Wrapper_Shell_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Data    : Callback_Data'Class);
   --  See inherited doc
   --  This wrapper is used to give access to hooks defined in shell
   --  languages


   type Shell_Wrapper_No_Args_Record is new Hook_No_Args_Record with record
      Func   : GNAT.OS_Lib.String_Access;
      Hook   : GNAT.OS_Lib.String_Access;
      Script : Scripting_Language;
   end record;
   type Shell_Wrapper_No_Args is access all Shell_Wrapper_No_Args_Record'Class;
   procedure Destroy (Wrapper : in out Shell_Wrapper_No_Args_Record);
   procedure Execute
     (Wrapper : Shell_Wrapper_No_Args_Record;
      Kernel  : access Kernel_Handle_Record'Class);
   --  See inherited doc
   --  This wrapper is used to give access to hooks from shell languages.

   use Glide_Kernel.Hooks_Hash.String_Hash_Table;

   type Hook_User_Data is record
      Kernel : Kernel_Handle;
      Hook   : String_Access;
      Func   : Hook_Function;
   end record;
   type Hook_User_Data_Access is access Hook_User_Data;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Hook_User_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Hook_User_Data_Access, System.Address);

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands related to hooks

   procedure General_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles all shell commands related to hooks defined from a scripting
   --  language.

   procedure Remove_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : Hook_Function);
   --  Internal version of Remove_Hook

   procedure Remove_Hook_Cb
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address);
   pragma Convention (C, Remove_Hook_Cb);
   --  Called when Object is destroyed, to remove the hook in User_Data

   procedure Add_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : Hook_Function;
      Profile : Hook_Type;
      Watch  : Glib.Object.GObject := null);
   --  Internal version of Add_Hook

   function Get_Or_Create_Hook
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name            : String;
      Description     : String := "";
      Command_Handler : Module_Command_Function := null;
      Profile         : Hook_Type := Unknown;
      Is_Hook_Type    : Boolean := False)
      return Hook_Description_Access;
   --  Get or create the description of a hook.
   --  Is_Hook_Type indicates whether this is a hook type description, or
   --  a hook itself

   function Get_Command_Handler
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Name : String) return Module_Command_Function;
   --  Return the default command handler for specific hook types

   function Get_Type_Profile
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Name : String) return Hook_Type;
   --  Return the profile to use for hooks of this type

   type Simple_No_Args_Wrapper_Record is new Hook_No_Args_Record with record
      Func : No_Args_Execute;
   end record;
   type Simple_No_Args_Wrapper
     is access all Simple_No_Args_Wrapper_Record'Class;
   --  A simple wrapper to encapsulate a function as a hook

   procedure Execute
     (Hook   : Simple_No_Args_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  See inherited doc

   type Simple_Args_Wrapper_Record is new Hook_Args_Record with record
      Func : Args_Execute;
   end record;
   type Simple_Args_Wrapper is access all Simple_Args_Wrapper_Record'Class;
   procedure Execute
     (Hook   : Simple_Args_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Hooks_Data'Class);
   --  See inherited doc

   type Simple_Shell_Args_Wrapper_Record
     is new Hook_Shell_Args_Record with record
      Func : Shell_Args_Execute;
     end record;
   type Simple_Shell_Args_Wrapper
     is access all Simple_Shell_Args_Wrapper_Record'Class;
   procedure Execute
     (Hook   : Simple_Shell_Args_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Callback_Data'Class);
   --  See inherited doc

   type Simple_Return_Wrapper_Record is new Hook_Args_Return_Record with record
      Func : Args_Return_Execute;
   end record;
   type Simple_Return_Wrapper is access all Simple_Return_Wrapper_Record'Class;
   function Execute
     (Hook   : Simple_Return_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Hooks_Data'Class) return Boolean;
   --  See inherited doc

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Simple_No_Args_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      Hook.Func (Kernel);
   end Execute;

   ------------------------
   -- Get_Or_Create_Hook --
   ------------------------

   function Get_Or_Create_Hook
     (Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name            : String;
      Description     : String := "";
      Command_Handler : Module_Command_Function := null;
      Profile         : Hook_Type := Unknown;
      Is_Hook_Type    : Boolean := False)
      return Hook_Description_Access
   is
      Info : Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      Descr : String_Access;
   begin
      if Info = null then
         if Description /= "" then
            Descr := new String'(Description);
         end if;

         Info := new Hook_Description'
           (Funcs           => Null_List,
            Description     => Descr,
            Profile         => Profile,
            Command_Handler => Command_Handler);

         if not Is_Hook_Type
           and then Name'Length >= Type_Prefix'Length
           and then Name (Name'First .. Name'First + Type_Prefix'Length - 1) =
             Type_Prefix
         then
            Insert (Kernel, -"Hook names cannot start with " & Type_Prefix);
         end if;

         if Is_Hook_Type then
            Set (Kernel.Hooks, Type_Prefix & Name,
                 Hook_Description_Base_Access (Info));
         else
            Set (Kernel.Hooks, Name, Hook_Description_Base_Access (Info));
         end if;
      elsif Info.Profile = Unknown then
         Info.Profile := Profile;
      end if;

      return Info;
   end Get_Or_Create_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : Hook_Function;
      Profile  : Hook_Type;
      Watch    : Glib.Object.GObject := null)
   is
      Info : Hook_Description_Access :=
        Get_Or_Create_Hook (Kernel, Name, Profile => Profile);
   begin
      Prepend (Info.Funcs, Hook);
      Hook.Ref_Count := Hook.Ref_Count + 1;

      if Watch /= null then
         Weak_Ref
           (Watch, Remove_Hook_Cb'Access,
            Convert (new Hook_User_Data'(Kernel_Handle (Kernel),
                                         new String'(Name),
                                         Hook)));
      end if;
   end Add_Hook;

   ----------------------
   -- Create_Hook_Type --
   ----------------------

   procedure Create_Hook_Type
     (Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Name        : String;
      Description      : String;
      Profile          : Hook_Type;
      Run_Hook_Handler : Glide_Kernel.Scripts.Module_Command_Function)
   is
      Info : Hook_Description_Access;
      pragma Unreferenced (Info);
   begin
      Info := Get_Or_Create_Hook
        (Kernel,
         Name            => Type_Name,
         Description     => Description,
         Command_Handler => Run_Hook_Handler,
         Profile         => Profile,
         Is_Hook_Type    => True);

      Register_Command
        (Kernel,
         Command      => "__run_hook__" & Type_Name,
         Description  => "internal function for run_hook",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Run_Hook_Handler);
   end Create_Hook_Type;

   -------------------------
   -- Get_Command_Handler --
   -------------------------

   function Get_Command_Handler
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Name : String) return Module_Command_Function
   is
      Info : constant Hook_Description_Access := Hook_Description_Access
        (Get (Kernel.Hooks, Type_Prefix & Type_Name));
   begin
      if Info = null then
         return null;
      else
         return Info.Command_Handler;
      end if;
   end Get_Command_Handler;

   ----------------------
   -- Get_Type_Profile --
   ----------------------

   function Get_Type_Profile
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Type_Name : String) return Hook_Type
   is
      Info : constant Hook_Description_Access := Hook_Description_Access
        (Get (Kernel.Hooks, Type_Prefix & Type_Name));
   begin
      if Info = null then
         return Unknown;
      else
         return Info.Profile;
      end if;
   end Get_Type_Profile;

   -------------------
   -- Register_Hook --
   -------------------

   procedure Register_Hook
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name        : String;
      Description : String;
      Type_Name   : String := "")
   is
      Info : constant Hook_Description_Access := Get_Or_Create_Hook
        (Kernel, Name, Description, Get_Command_Handler (Kernel, Type_Name),
         Profile => Get_Type_Profile (Kernel, Type_Name));
   begin
      Register_Command
        (Kernel,
         Command      => "__run_hook__" & Name,
         Description  => "internal function for run_hook",
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Info.Command_Handler);
   end Register_Hook;

   -----------------
   -- Remove_Hook --
   -----------------

   procedure Remove_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : Hook_Function)
   is
      Info    : constant Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      N, Prev : Hooks_List.List_Node := Hooks_List.Null_Node;
   begin
      if Info /= null then
         N := Hooks_List.First (Info.Funcs);
         while N /= Null_Node loop
            if Hooks_List.Data (N) = Hook then
               Remove_Nodes (Info.Funcs, Prev, N);
               return;
            end if;

            Prev := N;
            N := Next (N);
         end loop;
      end if;
   end Remove_Hook;

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
      Remove_Hook (D.Kernel, D.Hook.all, D.Func);
      Free (D.Hook);
      Unchecked_Free (D);
   end Remove_Hook_Cb;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : access Hook_No_Args_Record'Class;
      Watch    : Glib.Object.GObject := null) is
   begin
      Add_Hook (Kernel, Name, Hook_Function (Hook), Hook_Without_Args, Watch);
   end Add_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : No_Args_Execute;
      Watch    : Glib.Object.GObject := null)
   is
      Wrap : Simple_No_Args_Wrapper := new Simple_No_Args_Wrapper_Record;
   begin
      Wrap.Func := Hook;
      Add_Hook (Kernel, Name, Wrap, Watch);
   end Add_Hook;

   -----------------
   -- Remove_Hook --
   -----------------

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_No_Args_Record'Class) is
   begin
      Remove_Hook (Kernel, Name, Hook_Function (Hook));
   end Remove_Hook;

   --------------
   -- Run_Hook --
   --------------

   procedure Run_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Set_Busy : Boolean := True)
   is
      Info : constant Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      N    : List_Node := Null_Node;
      F    : Hook_Function;
   begin
      if Set_Busy then
         Push_State (Kernel_Handle (Kernel), Busy);
      end if;

      Trace (Me, "Run_No_Args_Hook: " & Name);
      if Info /= null then
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Data (N);
            Assert (Me, F.all in Hook_No_Args_Record'Class,
                    "Hook expects no argument: " & Name);

            Execute (Hook_No_Args (F).all, Kernel);

            N := Next (N);
         end loop;
      end if;

      if Set_Busy then
         Pop_State (Kernel_Handle (Kernel));
      end if;
   end Run_Hook;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Simple_Args_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Hooks_Data'Class) is
   begin
      Hook.Func (Kernel, Data);
   end Execute;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : access Hook_Args_Record'Class;
      Watch    : Glib.Object.GObject := null) is
   begin
      Add_Hook (Kernel, Name, Hook_Function (Hook), Hook_With_Args, Watch);
   end Add_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : Args_Execute;
      Watch    : Glib.Object.GObject := null)
   is
      Wrap : Simple_Args_Wrapper := new Simple_Args_Wrapper_Record;
   begin
      Wrap.Func := Hook;
      Add_Hook (Kernel, Name, Wrap, Watch);
   end Add_Hook;

   -----------------
   -- Remove_Hook --
   -----------------

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Record'Class) is
   begin
      Remove_Hook (Kernel, Name, Hook_Function (Hook));
   end Remove_Hook;

   --------------
   -- Run_Hook --
   --------------

   procedure Run_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : Hooks_Data'Class;
      Set_Busy : Boolean := True)
   is
      Info : constant Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      N    : List_Node := Null_Node;
      F    : Hook_Function;
   begin
      if Set_Busy then
         Push_State (Kernel_Handle (Kernel), Busy);
      end if;

      Trace (Me, "Run_Args_Hook: " & Name);
      if Info /= null then
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.all in Hook_Args_Record'Class,
                    "Hook expects arguments: " & Name
                    & " " & External_Tag (F.all'Tag));
            Execute (Hook_Args_Record'Class (F.all), Kernel, Data);
            N := Next (N);
         end loop;
      end if;

      if Set_Busy then
         Pop_State (Kernel_Handle (Kernel));
      end if;
   end Run_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Shell_Args_Record'Class;
      Watch  : Glib.Object.GObject := null) is
   begin
      Add_Hook
        (Kernel, Name, Hook_Function (Hook), Hook_With_Shell_Args, Watch);
   end Add_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : Shell_Args_Execute;
      Watch    : Glib.Object.GObject := null)
   is
      Wrap : Simple_Shell_Args_Wrapper := new Simple_Shell_Args_Wrapper_Record;
   begin
      Wrap.Func := Hook;
      Add_Hook (Kernel, Name, Wrap, Watch);
   end Add_Hook;

   -----------------
   -- Remove_Hook --
   -----------------

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Shell_Args_Record'Class) is
   begin
      Remove_Hook (Kernel, Name, Hook_Function (Hook));
   end Remove_Hook;

   --------------
   -- Run_Hook --
   --------------

   procedure Run_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : Glide_Kernel.Scripts.Callback_Data'Class;
      Set_Busy : Boolean := True)
   is
      Info : constant Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      N    : List_Node := Null_Node;
      F    : Hook_Function;
   begin
      if Set_Busy then
         Push_State (Kernel_Handle (Kernel), Busy);
      end if;

      Trace (Me, "Run_Shell_Hook: " & Name);
      if Info /= null then
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.all in Hook_Shell_Args_Record'Class,
                    "Hook expects arguments: " & Name
                    & " " & External_Tag (F.all'Tag));
            Execute (Hook_Shell_Args_Record'Class (F.all), Kernel, Data);
            N := Next (N);
         end loop;
      else
         Trace (Me, "No function in this hook");
      end if;

      if Set_Busy then
         Pop_State (Kernel_Handle (Kernel));
      end if;
   end Run_Hook;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Simple_Shell_Args_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Callback_Data'Class) is
   begin
      Hook.Func (Kernel, Data);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Hook   : Simple_Return_Wrapper_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : Hooks_Data'Class) return Boolean is
   begin
      return Hook.Func (Kernel, Data);
   end Execute;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : access Hook_Args_Return_Record'Class;
      Watch    : Glib.Object.GObject := null) is
   begin
      Add_Hook (Kernel, Name, Hook_Function (Hook),
                Hook_With_Args_And_Return, Watch);
   end Add_Hook;

   --------------
   -- Add_Hook --
   --------------

   procedure Add_Hook
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Hook     : Args_Return_Execute;
      Watch    : Glib.Object.GObject := null)
   is
      Wrap : Simple_Return_Wrapper := new Simple_Return_Wrapper_Record;
   begin
      Wrap.Func := Hook;
      Add_Hook (Kernel, Name, Wrap, Watch);
   end Add_Hook;

   -----------------
   -- Remove_Hook --
   -----------------

   procedure Remove_Hook
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Hook   : access Hook_Args_Return_Record'Class) is
   begin
      Remove_Hook (Kernel, Name, Hook_Function (Hook));
   end Remove_Hook;

   ----------------------------
   -- Run_Hook_Until_Success --
   ----------------------------

   function Run_Hook_Until_Success
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name     : String;
      Data     : Hooks_Data'Class;
      Set_Busy : Boolean := True)
         return Boolean
   is
      Info : constant Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      N    : List_Node := Null_Node;
      F    : Hook_Function;
   begin
      if Set_Busy then
         Push_State (Kernel_Handle (Kernel), Busy);
      end if;

      Trace (Me, "Run_Hook_Until_Success: " & Name);
      if Info /= null then
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.all in Hook_Args_Return_Record'Class,
                    "Hook returns value: " & Name);
            if Execute
              (Hook_Args_Return_Record'Class (F.all), Kernel, Data)
            then
               if Set_Busy then
                  Pop_State (Kernel_Handle (Kernel));
               end if;
               return True;
            end if;
            N := Next (N);
         end loop;
      end if;

      if Set_Busy then
         Pop_State (Kernel_Handle (Kernel));
      end if;

      return False;
   end Run_Hook_Until_Success;

   ----------------------------
   -- Run_Hook_Until_Failure --
   ----------------------------

   function Run_Hook_Until_Failure
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Name   : String;
      Data   : Hooks_Data'Class;
      Set_Busy : Boolean := True)
         return Boolean
   is
      Info : constant Hook_Description_Access :=
        Hook_Description_Access (Get (Kernel.Hooks, Name));
      N    : List_Node := Null_Node;
      F    : Hook_Function;
   begin
      if Set_Busy then
         Push_State (Kernel_Handle (Kernel), Busy);
      end if;

      Trace (Me, "Run_Hook_Until_Failure: " & Name);
      if Info /= null then
         N := First (Info.Funcs);
         while N /= Null_Node loop
            F := Hooks_List.Data (N);
            Assert (Me, F.all in Hook_Args_Return_Record'Class,
                    "Hook returns value: " & Name);
            if not Execute
              (Hook_Args_Return_Record'Class (F.all), Kernel, Data)
            then
               if Set_Busy then
                  Pop_State (Kernel_Handle (Kernel));
               end if;
               return False;
            end if;
            N := Next (N);
         end loop;
      end if;

      if Set_Busy then
         Pop_State (Kernel_Handle (Kernel));
      end if;

      return True;
   end Run_Hook_Until_Failure;

   -----------------------------
   -- Register_Standard_Hooks --
   -----------------------------

   procedure Register_Standard_Hooks
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Create_Hook_Type
        (Kernel, "",
         "Common type for all hooks with no parameters",
         Hook_Without_Args,
         Default_Command_Handler'Access);
      Create_Hook_Type
        (Kernel, "general",
         "Common type for all hooks created from a scripting language",
         Hook_With_Shell_Args,
         General_Command_Handler'Access);
      Register_Hook
        (Kernel, Preferences_Changed_Hook,
         -("Hook called when the value of some of the preferences"
           & " changes. Modules should refresh themselves dynamically."));
      Register_Hook
        (Kernel, Search_Reset_Hook,
         -("Hook called when the current search pattern is reset or changed"
           & " by the user, or when the current search is no longer possible"
           & " because the setup of GPS has changed"));
      Register_Hook
        (Kernel, Search_Functions_Changed_Hook,
         -"Hook called when the list of registered search functions changes");
      Register_Hook
        (Kernel, Search_Regexps_Changed_Hook,
         -("Hook called when a new regexp has been added to the list of"
           & " predefined search patterns"));
      Register_Hook
        (Kernel, Variable_Changed_Hook,
         -("Hook called when one of the scenario variables has been renamed,"
           & " removed or when one of its possible values has changed"));
      Register_Hook
        (Kernel, Project_View_Changed_Hook,
         -("Hook called when the project view has been changed, for instance"
           & " because one of the environment variables has changed. This"
           & " means that the list of directories, files or switches might"
           & " now be different"));
      Register_Hook
        (Kernel, Project_Changed_Hook,
         -("Hook called when the project has changed. A new project has been"
           & " loaded, and all previous settings and caches are now"
           & " obsolete."));

      Register_Command
        (Kernel,
         Command      => "run_hook",
         Params       => Parameter_Names_To_Usage (Run_Hook_Args),
         Description  =>
           -("Run any hook defined in GPS. This will call all the functions"
             & " that attached to that hook."),
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "add_hook",
         Params       => Parameter_Names_To_Usage (Add_Hook_Args),
         Description  =>
           -("Connect a new function to a specific hook. Any time this hook"
             & " is run through run_hook, this function will be called with"
             & " the same parameters passed to run_hook"),
         Minimum_Args => 2,
         Maximum_Args => 2,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "register_hook",
         Params       => Parameter_Names_To_Usage (Register_Hook_Args, 1),
         Description  =>
         -("Defines a new hook. This hook can take any number of parameters,"
           & " the default is none. The type and number of parameters is"
           & " called the type of the hook, and this is described by the"
           & " optional third parameter. The value of this parameter should be"
           & " either the empty string for a hook that doesn't take any"
           & " parameter. Or it could be one of the predefined types exported"
           & " by GPS itself (see list_hook_types). Finally, it could be the"
           & " word ""generic"" if this is a new type of hook purely defined"
           & " for this scripting language."),
         Minimum_Args => 2,
         Maximum_Args => 3,
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "list_hooks",
         Description  =>
         -("List all defined hooks. See also run_hook, register_hook and"
           & " add_hook. The return value is a hash-table containing the"
           & " description for each of these hooks."),
         Handler      => Default_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command     => "list_hook_types",
         Description =>
         -("List all defined type hooks. See also register_hook. The return"
           & " value is a hash-table containing the description for each of"
           & " these hooks."),
         Handler     => Default_Command_Handler'Access);
   end Register_Standard_Hooks;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Wrapper : in out Shell_Wrapper_Record) is
   begin
      Free (Wrapper.Func);
      Free (Wrapper.Hook);
   end Destroy;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Wrapper : Shell_Wrapper_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class)
   is
      Tmp : Boolean;
      pragma Unreferenced (Kernel, Tmp);
   begin
      Tmp := Execute_Shell
        (Wrapper.Script, Wrapper.Func.all, Wrapper.Hook.all, Data);
   end Execute;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Wrapper : in out Shell_Wrapper_No_Args_Record) is
   begin
      Free (Wrapper.Func);
      Free (Wrapper.Hook);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Wrapper : in out Shell_Wrapper_Return_Record) is
   begin
      Free (Wrapper.Func);
      Free (Wrapper.Hook);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Wrapper : in out Shell_Wrapper_Shell_Record) is
   begin
      Free (Wrapper.Func);
   end Destroy;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Wrapper : Shell_Wrapper_Shell_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Data    : Callback_Data'Class)
   is
      Tmp : Boolean;
      pragma Unreferenced (Kernel, Tmp);
   begin
      Tmp := Execute_Command (Wrapper.Script, Wrapper.Func.all, Data);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Wrapper : Shell_Wrapper_Return_Record;
      Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class) return Boolean
   is
      pragma Unreferenced (Kernel);
   begin
      return Execute_Shell
        (Wrapper.Script, Wrapper.Func.all, Wrapper.Hook.all, Data);
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Wrapper : Shell_Wrapper_No_Args_Record;
      Kernel  : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Error : Boolean;
   begin
      --  ??? Python specific
      Execute_Command
        (Wrapper.Script,
         Wrapper.Func.all & "(""" & Wrapper.Hook.all & """)",
         Show_Command => False,
         Errors => Error);
   end Execute;

   -----------------------------
   -- General_Command_Handler --
   -----------------------------

   procedure General_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Name : constant String := Nth_Arg (Data, 1);
   begin
      Run_Hook (Get_Kernel (Data), Name, Data);
   end General_Command_Handler;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
   begin
      if Command = "run_hook" then
         Name_Parameters (Data, Run_Hook_Args);
         declare
            Name   : constant String := Nth_Arg (Data, 1);
            Info : constant Hook_Description_Access :=
              Hook_Description_Access (Get (Get_Kernel (Data).Hooks, Name));
         begin
            if Info /= null then
               Info.Command_Handler (Data, Command => "__run_hook__" & Name);
            else
               Trace (Me, "Hook " & Name & " undefined");
            end if;

         exception
            when Glide_Kernel.Scripts.No_Such_Parameter =>
               Trace (Me, "Invalid number of parameters for hook " & Name);
               Set_Error_Msg
                 (Data,
                  "Invalid number of parameters for hook " & Name
                  & " in call to run_hook");
         end;

      elsif Command = "register_hook" then
         Name_Parameters (Data, Register_Hook_Args);
         declare
            Name  : constant String := Nth_Arg (Data, 1);
            Descr : constant String := Nth_Arg (Data, 2);
            Typ   : constant String := Nth_Arg (Data, 3, "");
         begin
            Register_Hook (Get_Kernel (Data), Name, Descr, Typ);
         end;

      elsif Command = "list_hooks" then
         declare
            Iter : Iterator;
            Info : Hook_Description_Access;
         begin
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
                     if Info.Description /= null then
                        Set_Return_Value (Data, Info.Description.all);
                     else
                        Set_Return_Value (Data, "");
                     end if;
                     Set_Return_Value_Key (Data, Name);
                  end if;
               end;

               Get_Next (Get_Kernel (Data).Hooks, Iter);
            end loop;
         end;

      elsif Command = "list_hook_types" then
         declare
            Iter : Iterator;
            Info : Hook_Description_Access;
         begin
            Get_First (Get_Kernel (Data).Hooks, Iter);
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
                     if Info.Description /= null then
                        Set_Return_Value (Data, Info.Description.all);
                     else
                        Set_Return_Value (Data, "");
                     end if;
                     Set_Return_Value_Key
                       (Data,
                        Name (Name'First + Type_Prefix'Length .. Name'Last));
                  end if;
               end;

               Get_Next (Get_Kernel (Data).Hooks, Iter);
            end loop;
         end;

      elsif Command = "add_hook" then
         Name_Parameters (Data, Add_Hook_Args);
         declare
            Name : constant String := Nth_Arg (Data, 1);
            Func : constant String := Nth_Arg (Data, 2);
            Info : constant Hook_Description_Access :=
              Hook_Description_Access (Get (Get_Kernel (Data).Hooks, Name));
            Wrapper  : Shell_Wrapper;
            Wrapper2 : Shell_Wrapper_No_Args;
            Wrapper3 : Shell_Wrapper_Return;
            Wrapper4 : Shell_Wrapper_Shell;
         begin
            --  If we expect a hook with no arguments:
            if Info = null
              or else Info.Command_Handler = Default_Command_Handler'Access
            then
               Wrapper2        := new Shell_Wrapper_No_Args_Record;
               Wrapper2.Func   := new String'(Func);
               Wrapper2.Hook   := new String'(Name);
               Wrapper2.Script := Get_Script (Data);
               Add_Hook (Get_Kernel (Data), Name, Wrapper2);

            elsif Info.Profile = Hook_With_Args then
               Wrapper        := new Shell_Wrapper_Record;
               Wrapper.Func   := new String'(Func);
               Wrapper.Hook   := new String'(Name);
               Wrapper.Script := Get_Script (Data);
               Add_Hook (Get_Kernel (Data), Name, Wrapper);

            elsif Info.Profile = Hook_With_Args_And_Return then
               Wrapper3        := new Shell_Wrapper_Return_Record;
               Wrapper3.Func   := new String'(Func);
               Wrapper3.Hook   := new String'(Name);
               Wrapper3.Script := Get_Script (Data);
               Add_Hook (Get_Kernel (Data), Name, Wrapper3);

            elsif Info.Profile = Hook_With_Shell_Args then
               Wrapper4        := new Shell_Wrapper_Shell_Record;
               Wrapper4.Func   := new String'(Func);
               Wrapper4.Script := Get_Script (Data);
               Add_Hook (Get_Kernel (Data), Name, Wrapper4);

            else
               Trace (Me, "Cannot connect to hook " & Name
                      & " since profile=" & Info.Profile'Img);
            end if;
         end;

      else
         --  Marshaller for hooks with no arguments
         --  This includes Command="__run_hook__" and
         --  Command="__run_hook__<name>" for all hooks with no arguments

         declare
            Name : constant String := Nth_Arg (Data, 1);
         begin
            Run_Hook (Get_Kernel (Data), Name);
         end;
      end if;
   end Default_Command_Handler;

   ----------
   -- Free --
   ----------

   procedure Free (F : in out Hook_Function) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hook_Function_Record'Class, Hook_Function);
   begin
      F.Ref_Count := F.Ref_Count - 1;

      if F.Ref_Count = 0 then
         Destroy (F.all);
         Unchecked_Free (F);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Hook : in out Hook_Description) is
   begin
      Free (Hook.Funcs);
      Free (Hook.Description);
   end Free;

end Glide_Kernel.Hooks;
