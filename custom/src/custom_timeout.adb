-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Glib;               use Glib;

with Gtk.Main;           use Gtk.Main;

with Custom_Module;      use Custom_Module;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GPS.Intl;           use GPS.Intl;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with Traces;             use Traces;

package body Custom_Timeout is

   Timeout_Cst         : aliased constant String := "timeout";
   Action_Cst          : aliased constant String := "action";
   Constructor_Args    : constant Cst_Argument_List :=
     (Timeout_Cst'Access, Action_Cst'Access);

   Timeout_Class_Name : constant String := "Timeout";

   type Custom_Timeout is record
      Handler  : Timeout_Handler_Id;
      Instance : Class_Instance;
      Action   : Subprogram_Type;
   end record;

   type Custom_Timeout_Access is access Custom_Timeout;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Custom_Timeout, Custom_Timeout_Access);

   package Action_Timeout is new Timeout (Custom_Timeout_Access);

   type Timeout_Property is new Instance_Property_Record with record
      Timeout : Custom_Timeout_Access;
   end record;
   type Timeout_Property_Access is access all Timeout_Property;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Custom_Timeout_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handle the custom timeout commands.

   function Callback (D : in Custom_Timeout_Access) return Boolean;
   --  Generic timeout callback.

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Timeout_Access;
   --  Get or store some data in an instance of GPS.Process

   procedure Free (X : in out Custom_Timeout_Access);
   --  Free memory associated to X.

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Custom_Timeout_Access) is
   begin
      Free (X.Action);
      Unchecked_Free (X);
   end Free;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class; N : Positive) return Custom_Timeout_Access
   is
      Timeout_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), Timeout_Class_Name);
      Inst : constant Class_Instance := Nth_Arg (Data, N, Timeout_Class);
   begin
      return Timeout_Property_Access
        (Instance_Property'(Get_Data (Inst, Timeout_Class_Name))).Timeout;
   end Get_Data;

   --------------
   -- Callback --
   --------------

   function Callback (D : in Custom_Timeout_Access) return Boolean is
      C : Callback_Data'Class := Create
        (Get_Script (D.Action.all), Arguments_Count => 1);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Set_Nth_Arg (C, 1, D.Instance);
      Tmp := Execute (D.Action, C);
      Free (C);
      return True;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return True;
   end Callback;

   ----------------------------
   -- Custom_Timeout_Handler --
   ----------------------------

   procedure Custom_Timeout_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Custom_Module_ID.all);
      Timeout_Class : constant Class_Type :=
        New_Class (Kernel, Timeout_Class_Name);
      D             : Custom_Timeout_Access;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Constructor_Args);

         declare
            Inst    : constant Class_Instance :=
              Nth_Arg (Data, 1, Timeout_Class);
            Timeout : constant Integer := Nth_Arg (Data, 2);
            Act     : constant Subprogram_Type := Nth_Arg (Data, 3);
         begin
            if Act = null then
               Set_Error_Msg
                 (Data,
                  -"Could not find action """ & Nth_Arg (Data, 3) & """");
               return;
            end if;

            if Timeout <= 0 then
               Set_Error_Msg (Data, -"Cannot register a timeout for 0 ms.");
               return;
            end if;

            D := new Custom_Timeout;
            D.Instance := Inst;
            D.Action := Act;
            D.Handler := Action_Timeout.Add
              (Guint32 (Timeout), Callback'Access, D);

            Set_Data
              (Inst, Timeout_Class_Name, Timeout_Property'(Timeout => D));
         end;

      elsif Command = "remove" then
         D := Get_Data (Data, 1);
         Timeout_Remove (D.Handler);
         Free (D);
      end if;
   end Custom_Timeout_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Timeout_Class : constant Class_Type := New_Class (Kernel, "Timeout");
   begin
      Register_Command
        (Kernel, Constructor_Method,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Timeout_Class,
         Handler       => Custom_Timeout_Handler'Access);
      Register_Command
        (Kernel, "remove",
         Class         => Timeout_Class,
         Handler       => Custom_Timeout_Handler'Access);
   end Register_Commands;

end Custom_Timeout;
