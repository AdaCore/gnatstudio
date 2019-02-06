------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Default_Preferences;       use Default_Preferences;
with Debugger.Base_Gdb.Gdb_CLI; use Debugger.Base_Gdb.Gdb_CLI;
with Debugger.Base_Gdb.Gdb_MI;  use Debugger.Base_Gdb.Gdb_MI;
with Debugger.LLDB;             use Debugger.LLDB;
with Debugger.Base_Gdb.Cpp;     use Debugger.Base_Gdb.Cpp;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.IO;                   use GNAT.IO;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Ada.Calendar;              use Ada.Calendar;
with Process_Proxies;           use Process_Proxies;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Debugger;                  use Debugger;
with GVD_Module;                use GVD_Module;
with GVD.Types;                 use GVD.Types;
with GVD.Variables.Types;       use GVD.Variables.Types;
with Gtk.Main;                  use Gtk.Main;
with GVD.Preferences;           use GVD.Preferences;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Parse_Support;             use Parse_Support;

procedure Test_Parse_Cpp is

   Gdb      : Debugger_Access;
   Language : access Gdb_Cpp_Language := new Gdb_Cpp_Language;

   ---------------
   -- Print_Var --
   ---------------

   procedure Print_Var (Var : String);
   procedure Print_Var (Var : String) is
      V : GVD_Type_Holder;
      Found : Boolean;
   begin
      Put_Line ("------------------------------");
      V := Parse_Type (Gdb, Var);
      if V /= Empty_GVD_Type_Holder then
         Parse_Value (Gdb, Var, V, Default_Format, Found);
         Print (V, Language, Var);
      else
         Put_Line (Var & ": Unknown variable");
      end if;
   end Print_Var;

   GVD_Prefs : Preferences_Manager;
   List : Argument_List (1 .. 0);
   Num  : Breakpoint_Identifier;

begin
   Parse_Config_File (Create_From_Base (".gnatdebug"));
   Init;
   Create_GVD_Module (Kernel => null);
   GVD_Prefs := new Preferences_Manager_Record;
   Register_Default_Preferences (GVD_Prefs);
   Load_Preferences (GVD_Prefs, Create_From_Base ("preferences"));

   case GVD.Types.Debugger_Type'(Debugger_Kind.Get_Pref) is
      when GVD.Types.Gdb =>
         Gdb := new Gdb_Debugger;
      when GVD.Types.Gdb_MI =>
         Gdb := new Gdb_MI_Debugger;
      when GVD.Types.LLDB =>
         Gdb := new LLDB_Debugger;
   end case;

   Set_Language (Gdb, Language.all'Unchecked_Access);
   Set_Debugger (Language, Gdb);

   Spawn (Gdb, null, No_File, List, "", new Process_Proxy, 1);

   Initialize (Gdb);
   Set_Executable (Gdb, Create (Full_Filename => "parse_cpp"));
   Num := Break_Subprogram (Gdb, "foo");

   Run (Gdb);
   Stack_Up (Gdb);

   Print_Var ("Parse::Non_Existant_Variable");
   --  Check there is no error in that case.

   Print_Var ("A");
   Print_Var ("B");
   Print_Var ("C");
   Print_Var ("Sh");
   Print_Var ("L");
   Print_Var ("Uns");
   Print_Var ("UL");
   Print_Var ("S");
   Print_Var ("S2");
   Print_Var ("S3");
   Print_Var ("Act");
   Print_Var ("My_Enum_Variable");
   Print_Var ("T");
   Print_Var ("Ea");
   Print_Var ("Aoa");
   Print_Var ("U");
   Print_Var ("A3d");
   Print_Var ("Iaa");
   Print_Var ("V");
   Print_Var ("Anonymous_Var");
   Print_Var ("V2");
   Print_Var ("cl1");
   Print_Var ("cl2");
   Print_Var ("cl3");
   Print_Var ("cl4");
   Print_Var ("Mror");
   Print_Var ("Mrora");
   Print_Var ("Mrorpa");
   Print_Var ("Uni");
   Print_Var ("Uni2");
   Print_Var ("Uni3");
   Print_Var ("Mrwu");
   Print_Var ("as");
   Print_Var ("asa");
   Print_Var ("Mrws");
   Print_Var ("Mrws2");
   Print_Var ("tv");
   Print_Var ("list");
   Print_Var ("FC");
   Print_Var ("SC");
   Print_Var ("SAC");
   Print_Var ("MI");
   Print_Var ("NF");
   Close (Gdb);

exception
   when others =>
      Close (Gdb);
      raise;

end Test_Parse_Cpp;
