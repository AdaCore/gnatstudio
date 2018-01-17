------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Shell;    use GNATCOLL.Scripts.Shell;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel;                use GPS.Kernel;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with String_Utils;              use String_Utils;

package body Shell_Script is

   -------------------------
   -- Shell_GPS_Scripting --
   -------------------------

   type Shell_GPS_Scripting_Record
     is new Shell_Scripting_Record with null record;
   overriding function Create
     (Script          : access Shell_GPS_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   --  Create our own callback_data

   --  The memory for script is never reclaimed: doing so might
   --  interfer with some controlled script objects that are only freed when
   --  the application finalizes, and these objects might store pointers to
   --  the scripting language. To avoid having a memory leak reported by
   --  valgrind (where ignoring all leaks from Register_Module might miss some
   --  real leak in the future), we therefore use a global variable on the
   --  stack. This is never accessed directly though

   Global_Shell_Script : aliased Shell_GPS_Scripting_Record;

   ----------------------
   -- Shell_Subprogram --
   ----------------------

   type Shell_GPS_Subprogram_Record is new Shell_Subprogram_Record
      with null record;
   type Shell_GPS_Subprogram is access all Shell_GPS_Subprogram_Record'Class;
   --  A subprogram that executes its command as a GPS action

   overriding function Execute
     (Subprogram : access Shell_GPS_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return String;
   overriding function Get_Name
     (Subprogram : access Shell_GPS_Subprogram_Record) return String;
   --  See doc from inherited subprograms

   -------------------
   -- Callback_Data --
   -------------------

   type Shell_GPS_Callback_Data is new Shell_Callback_Data with null record;
   overriding function Nth_Arg
     (Data : Shell_GPS_Callback_Data; N : Positive) return Subprogram_Type;
   --  See doc from inherited subprogram

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Script : constant Shell_Scripting := Global_Shell_Script'Access;
   begin
      Register_Shell_Scripting (Kernel.Scripts, Script);
   end Register_Module;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Script          : access Shell_GPS_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class
   is
      Data : Shell_GPS_Callback_Data;
      pragma Unreferenced (Arguments_Count);
   begin
      --  ??? we could get rid of Arguments_Count
      Initialize (Data, Script);
      return Data;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Subprogram : access Shell_GPS_Subprogram_Record;
      Args       : Callback_Data'Class;
      Error      : not null access Boolean) return String
   is
      D    : constant Shell_Callback_Data := Shell_Callback_Data (Args);
      Custom : Command_Access;
      A    : constant Action_Record_Access := Lookup_Action
        (Get_Kernel (Args), Get_Command (Subprogram));
   begin
      Custom := Create_Proxy
        (Get_Command (A),
         Context => (Event       => null,
                     Context     => No_Context,
                     Synchronous => True,
                     Dir         => No_File,
                     Args        => new Argument_List'(Clone (Get_Args (D))),
                     Label       => null,
                     Via_Menu    => False,
                     Repeat_Count => 1,
                     Remaining_Repeat => 0));

      Launch_Background_Command
        (Kernel          => Get_Kernel (Args),
         Command         => Custom,
         Active          => True,
         Show_Bar        => True,
         Queue_Id        => "");

      --  ??? Should evaluate output properly, but we are in asynchronous mode
      --  ??? In fact, this is no longer true if we use Active set to False
      --  above, since we now know how to execute external actions
      --  synchronously
      Error.all := False;
      return "";
   end Execute;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Subprogram : access Shell_GPS_Subprogram_Record) return String is
   begin
      return "action: " & Get_Command (Subprogram);
   end Get_Name;

   -------------
   -- Nth_Arg --
   -------------

   overriding function Nth_Arg
     (Data : Shell_GPS_Callback_Data; N : Positive) return Subprogram_Type
   is
      A    : Action_Record_Access;
      Name : constant String := Nth_Arg (Data, N);
      Subp : Shell_GPS_Subprogram;
   begin
      A := Lookup_Action (Get_Kernel (Data), Name);
      if A = null then
         raise Invalid_Parameter;
      else
         Subp := new Shell_GPS_Subprogram_Record;
         Initialize (Subp.all, Get_Script (Data), Command => Name);
         return Subprogram_Type (Subp);
      end if;
   end Nth_Arg;

end Shell_Script;
