------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib.Module;               use Glib.Module;
with Glib.Object;               use Glib.Object;

package body GPS.Kernel.Modules is

   Me : constant Trace_Handle :=
          Create ("GPS.KERNEL.MODULES", GNATCOLL.Traces.Off);

   procedure Free (Module : in out Module_ID);
   --  Free memory associated to a Module_ID

   -------------------
   -- Save_Function --
   -------------------

   function Save_Function
     (Module       : access Module_ID_Record;
      Child        : Glib.Object.GObject;
      Mode         : Save_Function_Mode;
      Single_Child : Boolean;
      Force        : Boolean) return Boolean
   is
      pragma Unreferenced (Module, Child, Mode, Single_Child, Force);
   begin
      return False;
   end Save_Function;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   function Tooltip_Handler
     (Module  : access Module_ID_Record;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Module, Context);
   begin
      return null;
   end Tooltip_Handler;

   ----------
   -- Free --
   ----------

   procedure Free (Module : in out Module_ID) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_ID_Record'Class, Module_ID);
   begin
      if Module /= null then
         Destroy (Module.all);
         GNAT.Strings.Free (Module.Name);
         Unchecked_Free (Module);
      end if;
   end Free;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (ID : Module_ID_Record'Class) return Kernel_Handle is
   begin
      return ID.Kernel;
   end Get_Kernel;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Module : Module_ID) return String is
   begin
      --  The module might be null, for instance the Welcome page has
      --  no associated module.

      if Module = null
        or else Module.Name = null
      then
         return "";
      end if;

      return Module.Name.all;
   end Get_Name;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Module      : access Module_ID_Record;
      Kernel      : access Kernel_Handle_Record'Class;
      Module_Name : String;
      Priority    : Module_Priority := Default_Priority) is
   begin
      Module.Name     := new String'(Module_Name);
      Module.Priority := Priority;
      Module.Kernel   := Kernel_Handle (Kernel);

      Kernel.Register_Module (Module);
   end Register_Module;

   -----------------------------
   -- Dynamic_Register_Module --
   -----------------------------

   procedure Dynamic_Register_Module
     (Kernel      : access Kernel_Handle_Record'Class;
      Shared_Lib  : String;
      Module_Name : String;
      Success     : out Boolean)
   is
      type Register_Module_Access is access procedure
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

      type Init_Proc is access procedure;

      Dyn_Module   : G_Module;
      Dyn_Register : Register_Module_Access;
      Init         : Init_Proc;

      procedure Get_Symbol is new
        Generic_Module_Symbol (Register_Module_Access);

      procedure Get_Symbol is new
        Generic_Module_Symbol (Init_Proc);

   begin
      Dyn_Module := Module_Open (Module_Build_Path ("", Shared_Lib));

      if Dyn_Module = null then
         Dyn_Module := Module_Open (Shared_Lib);
      end if;

      if Dyn_Module = null then
         Trace (Me, "Couldn't open shared lib: " & Shared_Lib);
         Success := False;
      else
         Get_Symbol (Dyn_Module, Module_Name & "_init", Init, Success);

         if Success then
            Init.all;

            Get_Symbol
              (Dyn_Module, Module_Name & "__register_module",
               Dyn_Register, Success);

            if Success then
               Trace (Me, "Registering module: " & Module_Name);
               Dyn_Register (Kernel);
            else
               Trace (Me, "Couldn't find register_module symbol");
            end if;

         else
            Trace (Me, "Couldn't find _init symbol");
         end if;
      end if;
   end Dynamic_Register_Module;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
     (ID : access Module_ID_Record'Class) return Module_Priority is
   begin
      return ID.Priority;
   end Get_Priority;

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name (ID : access Module_ID_Record'Class) return String is
   begin
      return ID.Name.all;
   end Module_Name;

   ------------------
   -- Free_Modules --
   ------------------

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class) is
      use Abstract_Module_List;
      List : constant Abstract_Module_List.List :=
        Kernel.Module_List (Module_ID_Record'Tag);
      Current : Cursor := Abstract_Module_List.Last (List);
   begin
      --  Destroy the modules in the reverse order,
      --  otherwise, the scripts module is no longer available for the other
      --  modules, and some modules (e.g. editor) is freed too early.

      while Has_Element (Current) loop
         declare
            Module : Abstract_Module := Element (Current);
         begin
            Free (Module_ID (Module));
         end;

         Current := Abstract_Module_List.Previous (Current);
      end loop;
   end Free_Modules;

end GPS.Kernel.Modules;
