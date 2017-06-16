------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Glib.Object;               use Glib.Object;

package body GPS.Kernel.Scripts.Hooks is

   type Hook_Property is new Instance_Property_Record with record
      Hook : not null access Hook_Types'Class;
   end record;
   type Hook_Property_Access is access all Hook_Property'Class;
   --  Some special data that is stored in a Class_Instance to associate
   --  it with a specific hook. This is for instances of the "Hook"
   --  class.

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handle for the "GPS.Hook" class

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Info : access Hook_Types'Class;
   begin
      if Command = Constructor_Method then
         declare
            Inst : constant Class_Instance := Data.Nth_Arg (1);
            Name : constant String := Data.Nth_Arg (2);
         begin
            Info := Get_Hook (Get_Kernel (Data), Name);
            if Info = null then
               Set_Error_Msg (Data, -"No such hook: " & Name);
            else
               Set_Data
                 (Inst, Hook_Class_Name, Hook_Property'(Hook => Info));
            end if;
         end;

      elsif Command = "run"
        or else Command = "run_until_success"
        or else Command = "run_until_failure"
      then
         Info := Get_Hook (Data, 1);
         Info.Run_From_Python (Data);

      elsif Command = "add" then
         declare
            Func : constant Subprogram_Type := Data.Nth_Arg (2);
         begin
            Info := Get_Hook (Data, 1);
            Info.Add_Hook_Func
               (Func  => new Python_Hook_Function'
                   (Hook_Function with Func => Func),
                Last  => Data.Nth_Arg (3, True));
         end;

      elsif Command = "add_debounce" then
         declare
            Func : constant Subprogram_Type := Data.Nth_Arg (2);
         begin
            Info := Get_Hook (Data, 1);
            Debounce_Hook_Types (Info.all).Add_Debounce_Hook_Func
              (Func  => new Python_Hook_Function'
                 (Hook_Function with Func => Func),
               Last  => Data.Nth_Arg (3, True));
         end;

      elsif Command = "remove" then
         Info := Get_Hook (Data, 1);
         declare
            Func : constant Subprogram_Type := Data.Nth_Arg (2);

            function If_Matches
              (F : not null access Hook_Function'Class) return Boolean;
            --  Return True if the F matches with the python hook function to
            --  remove, False otherwise.

            ----------------
            -- If_Matches --
            ----------------

            function If_Matches
              (F : not null access Hook_Function'Class) return Boolean
            is
            begin
               if F.all in Python_Hook_Function'Class then
                  declare
                     Python_F : constant Python_Hook_Function :=
                                  Python_Hook_Function (F.all);
                  begin
                     if Python_F.Func /= null and then Func /= null then
                        return Python_F.Func.all = Func.all;
                     end if;
                  end;
               end if;

               return False;
            end If_Matches;

         begin
            Info.Remove (If_Matches'Access);
         end;

      elsif Command = "register" then
         declare
            Name : constant String := Data.Nth_Arg (1);
            Typ  : constant String := Data.Nth_Arg (2, "simple_hooks");
            T    : constant access Hook_Types'Class :=
               Get_Hook (Get_Kernel (Data), Hook_Type_Prefix & Typ);
            H    : access Hook_Types'Class;
         begin
            if T /= null then
               H := new Hook_Types'Class'(T.all);
               H.Name := new String'(Name);  --  do not free old value
               H.Funcs.Clear;  --  We had a template, ignore the actual funcs
               if H.all in Debounce_Hook_Types'Class then
                  Debounce_Hook_Access (H).Asynch_Funcs.Clear;
                  Debounce_Hook_Access (H).Asynch_Data.Clear;
               end if;
               H.Register (Get_Kernel (Data));
            else
               Data.Set_Error_Msg ("Hook type not found: " & Typ);
            end if;
         end;

      elsif Command = "list" then
         declare
            use Hooks_Maps;
            C    : Hooks_Maps.Cursor := Get_Kernel (Data).Hooks.First;
         begin
            Data.Set_Return_Value_As_List;
            while Has_Element (C) loop
               if not Starts_With (Key (C), Hook_Type_Prefix) then
                  Data.Set_Return_Value (Key (C));
               end if;
               Next (C);
            end loop;
         end;

      elsif Command = "describe_functions" then
         Data.Set_Return_Value_As_List;

         declare
            R : GNAT.Strings.String_List := Get_Hook (Data, 1).List_Functions;
         begin
            for R2 of R loop
               Data.Set_Return_Value (R2.all);
            end loop;
            Free (R);
         end;

      elsif Command = "list_types" then
         declare
            use Hooks_Maps;
            C    : Hooks_Maps.Cursor := Get_Kernel (Data).Hooks.First;
         begin
            Data.Set_Return_Value_As_List;
            while Has_Element (C) loop
               declare
                  N : constant String := Key (C);
               begin
                  if Starts_With (N, Hook_Type_Prefix) then
                     Data.Set_Return_Value
                        (N (N'First + Hook_Type_Prefix'Length .. N'Last));
                  end if;
               end;
               Next (C);
            end loop;
         end;
      end if;
   end Default_Command_Handler;

   --------------
   -- Get_Hook --
   --------------

   function Get_Hook
      (Kernel : not null access Kernel_Handle_Record'Class;
       Name   : String) return access Hook_Types'Class
   is
      use Hooks_Maps;
      C : constant Hooks_Maps.Cursor := Kernel.Hooks.Find (Name);
   begin
      if Has_Element (C) then
         return Element (C);
      else
         return null;
      end if;
   end Get_Hook;

   --------------
   -- Get_Hook --
   --------------

   function Get_Hook
      (Data : Callback_Data'Class; Param : Positive)
      return access Hook_Types'Class
   is
      Inst : constant Class_Instance :=
         Data.Nth_Arg (Param, Get_Hook_Class (Get_Kernel (Data)));
   begin
      return Hook_Property_Access
        (Instance_Property'(Get_Data (Inst, Hook_Class_Name))).Hook;
   end Get_Hook;

   --------------------
   -- Get_Hook_Class --
   --------------------

   function Get_Hook_Class
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return Class_Type is
   begin
      return Kernel.Scripts.New_Class (Hook_Class_Name);
   end Get_Hook_Class;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Hook_Class : constant Class_Type := Get_Hook_Class (Kernel);
   begin
      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class        => Hook_Class,
         Params       => (1 => Param ("name")),
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("run",
         Class        => Hook_Class,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("run_until_success",
         Class        => Hook_Class,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("run_until_failure",
         Class        => Hook_Class,
         Maximum_Args => Natural'Last,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("add",
         Class        => Hook_Class,
         Params       => (1 => Param ("function_name"),
                          2 => Param ("last", Optional => True)),
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("add_debounce",
         Class        => Hook_Class,
         Params       => (1 => Param ("function_name"),
                          2 => Param ("last", Optional => True)),
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("remove",
         Class        => Hook_Class,
         Params       => (1 => Param ("function_name")),
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("register",
         Class         => Hook_Class,
         Static_Method => True,
         Params        => (1 => Param ("name"),
                           2 => Param ("type", Optional => True)),
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("list",
         Class         => Hook_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("describe_functions",
         Class        => Hook_Class,
         Handler      => Default_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("list_types",
         Class         => Hook_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
   end Register_Module;

end GPS.Kernel.Scripts.Hooks;
