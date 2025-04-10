------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Glib;
with Glib.Object;            use Glib.Object;
with Glib.Option;
with GNAT.Strings;           use GNAT.Strings;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GPS.Main_Window;        use GPS.Main_Window;
with Gtk;                    use Gtk;
with Gtk.Label;              use Gtk.Label;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Window;             use Gtk.Window;
with Spawn.Environments;
with VSS.Strings;

package GPS.Globals is
   Main_Trace : constant Trace_Handle := Create ("GPS.MAIN.GPS");

   subtype String_Access is GNAT.Strings.String_Access;

   type File_To_Open is record
      File         : Unbounded_String;
      Line         : Natural := 1;
      From_Project : Boolean := False;
   end record;

   package File_To_Open_Vectors is new Ada.Containers.Vectors
     (Positive, File_To_Open);

   type Config_File_Setup is record
      Autoconf    : Boolean := False;
      Config_File : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      DB_Dirs     : GNATCOLL.VFS.File_Array_Access;
   end record;

   type GPS_Option_Context is record
      Context : Glib.Option.Goption_Context;

      Do_Exit : Boolean := False;
      --  Set to True if GNAT Studio should exit after parsing command line
      --  switches

      Line  : Positive := 1;
      --  Line to use when opening files from the command line.
   end record;

   type GPS_Splash_Screen_Record is new Gtk_Window_Record with record
      Progress_Label : Gtk_Label;
      --  Label displaying the loading progress of GNAT Studio.
   end record;
   type GPS_Splash_Screen is access all GPS_Splash_Screen_Record'Class;
   --  The GNAT Studio initial splash screen

   package Cmd_Line_Scenario_Vars_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (String, String);

   GPS_Command_Line           : GPS_Option_Context;
   --  Handling of command line

   Config_Files               : Config_File_Setup;

   Build_Tree_Dir             : Virtual_File := No_File;
   GPS_Log_Dir                : Virtual_File;
   GNATStudio_Home_Dir        : Virtual_File;
   Home_Dir                   : Virtual_File;
   Prefix_Dir                 : Virtual_File;
   Project_Name               : Virtual_File := No_File;
   Root_Dir                   : Virtual_File := No_File;
   Files_To_Open              : File_To_Open_Vectors.Vector;

   Ignore_Saved_Values        : Boolean := False;
   Hide_GPS                   : Boolean := False;
   Memory_Monitor             : Boolean := False;
   Server_Mode                : Boolean := False;
   Show_Preferences_Assistant : Boolean := False;
   Unexpected_Exception       : Boolean := False;

   Batch_File                 : String_Access;
   Batch_Script               : String_Access;
   DAP_GDB_Adapter            : VSS.Strings.Virtual_String;
   Passed_Project_Name        : String_Access;
   Program_Args               : String_Access;
   Protocol                   : String_Access;
   Startup_Dir                : String_Access;
   Target                     : String_Access;
   Tools_Host                 : String_Access;

   Memory_Stack_Depth         : constant := 3;
   --  Stack depth for GNATCOLL.Memory
   Port_Number                : Natural := 0;

   GPS_Main                   : GPS_Window;
   Splash                     : GPS_Splash_Screen;

   Env                        : Spawn.Environments.Process_Environment :=
     Spawn.Environments.System_Environment;

   Cmd_Line_Scenario_Vars     : Cmd_Line_Scenario_Vars_Maps.Map;
   --  Stores -XVAR=VALUE command line switches as Key=VAR, Element=VALUE

end GPS.Globals;
