------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with GNAT.Expect;              use GNAT.Expect;

with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with GPS.Kernel.Hooks;
with Gtk.Main;

with GPS.Customizable_Modules; use GPS.Customizable_Modules;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Remote;

with Remote;                   use Remote;
with Toolchains;               use Toolchains;
with Toolchains.Known;
with XML_Utils;

package body Toolchains_Editor is

   type Toolchains_Module_Record is new Module_ID_Record with null record;
   type Toolchains_Module is access all Toolchains_Module_Record'Class;

   Toolchains_Module_ID   : Toolchains_Module;
   Toolchains_Module_Name : constant String := "Toolchains_Editor";

   overriding procedure Customize
     (Module : access Toolchains_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   --  See doc for inherited subprogram

   type GPS_Toolchain_Manager_Record is
     new Toolchains.Toolchain_Manager_Record with record
      Kernel : Kernel_Handle;
   end record;

   overriding function Execute
     (This              : GPS_Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False) return String;
   --  Executes the command and returns the result

   type On_Server_Changed is new GPS.Kernel.Hooks.Server_Hooks_Function
   with null record;
   overriding procedure Execute
     (Self     : On_Server_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Server   : Distant_Server_Type;
      Nickname : String);
   --  Called when a file has been modified

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (This              : GPS_Toolchain_Manager_Record;
      Command           : String;
      Timeout_MS        : Integer;
      Handle_GUI_Events : Boolean := False) return String
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (GNAT.Expect.Process_Descriptor'Class,
         GNAT.Expect.Process_Descriptor_Access);

      Status  : Boolean;
      Pd      : GNAT.Expect.Process_Descriptor_Access;
      Match   : Expect_Match := 0;
      Ret     : Unbounded_String;
      Args    : constant Arg_List :=
                  GNATCOLL.Arg_Lists.Parse_String (Command, Separate_Args);
      Start   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Timeout : constant Duration := Duration (Timeout_MS) / 1000.0;
      Ignore  : Boolean;
      pragma Unreferenced (Ignore);

   begin
      --  If no such command exist, no need to try to spawn it
      if Locate_On_Path (+Get_Command (Args), Get_Nickname (Build_Server)) =
        No_File
      then
         raise Process_Died;
      end if;

      GPS.Kernel.Remote.Spawn
        (This.Kernel, GNATCOLL.Arg_Lists.Parse_String (Command, Separate_Args),
         Remote.Build_Server, Pd, Status);

      if not Status then
         raise Process_Died;
      else
         begin
            loop
               if Handle_GUI_Events then
                  while Gtk.Main.Events_Pending loop
                     Ignore := Gtk.Main.Main_Iteration;
                  end loop;
               end if;

               Expect (Pd.all, Match, "\n", 100);

               if Match = Expect_Timeout then
                  if Clock - Start > Timeout then
                     Status := False;
                     Close (Pd.all);
                     raise Process_Died;
                  end if;
               else
                  Ada.Strings.Unbounded.Append (Ret, Expect_Out (Pd.all));
               end if;

            end loop;
         exception
            when Process_Died =>
               Free (Pd);
         end;

         return To_String (Ret);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Server_Changed;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Server   : Distant_Server_Type;
      Nickname : String)
   is
      pragma Unreferenced (Self, Nickname);
      Kernel_Mgr : Toolchain_Manager := Kernel.Get_Toolchains_Manager;
   begin
      if Server = Build_Server and then Kernel_Mgr /= null then
         Kernel_Mgr.Clear_Toolchains;
         Free (Kernel_Mgr);
         Kernel_Mgr := new GPS_Toolchain_Manager_Record;
         Kernel.Set_Toolchains_Manager (Kernel_Mgr);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Toolchains_Module_ID := new Toolchains_Module_Record;
      Register_Module
        (Module      => Toolchains_Module_ID,
         Kernel      => Kernel,
         Module_Name => Toolchains_Module_Name,
         Priority    => Default_Priority);

      Kernel.Set_Toolchains_Manager (new GPS_Toolchain_Manager_Record);

      GPS.Kernel.Hooks.Server_Config_Hook.Add (new On_Server_Changed);
   end Register_Module;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Toolchains_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Module, File, Level);
   begin
      Toolchains.Known.Read_From_XML (Node);
   end Customize;

end Toolchains_Editor;
