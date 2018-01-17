------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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
with Debugger.Base_Gdb.C;       use Debugger.Base_Gdb.C;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.IO;                   use GNAT.IO;
with GNATCOLL.Traces;
with Ada.Calendar;              use Ada.Calendar;
with Process_Proxies;           use Process_Proxies;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Debugger;                  use Debugger;
with GVD_Module;                use GVD_Module;
with GVD.Types;                 use GVD.Types;
with Gtk.Main;                  use Gtk.Main;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Variables.Types;       use GVD.Variables.Types;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Parse_Support;             use Parse_Support;

procedure Test_Parse_C is

   Gdb      : Debugger_Access;
   Language : access Gdb_C_Language := new Gdb_C_Language;

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
   GNATCOLL.Traces.Parse_Config_File (Create_From_Base (".gnatdebug"));
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
   end case;

   Set_Language (Gdb, Language.all'Unchecked_Access);
   Set_Debugger (Language, Gdb);

   Spawn (Gdb, null, No_File, List, "", new Process_Proxy, 1);

   Initialize (Gdb);
   Set_Executable (Gdb, Create (Full_Filename => "parse_c"));
   Num := Break_Subprogram (Gdb, "foo");

   Run (Gdb);

   Print_Var ("Parse::Non_Existant_Variable");
   --  Check there is no error in that case.

   Print_Var ("main::A");
   Print_Var ("main::B");
   Print_Var ("main::C");
   Print_Var ("main::Sh");
   Print_Var ("main::L");
   Print_Var ("main::Uns");
   Print_Var ("main::CC");
   Print_Var ("main::UL");
   Print_Var ("main::S");
   Print_Var ("main::S2");
   Print_Var ("main::S3");
   Print_Var ("main::Act");
   Print_Var ("main::My_Enum_Variable");
   Print_Var ("main::T");
   Print_Var ("main::Ea");
   Print_Var ("main::Aoa");
   Print_Var ("main::U");
   Print_Var ("main::A3d");
   Print_Var ("main::Iaa");
   Print_Var ("main::V");
   Print_Var ("main::Anonymous_Var");
   Print_Var ("main::V2");
   Print_Var ("main::Mror");
   Print_Var ("main::Mror2");
   Print_Var ("main::mrou");
   Print_Var ("main::mroe");
   Print_Var ("main::Mrora");
   Print_Var ("main::Mrorpa");
   Print_Var ("main::Uni");
   Print_Var ("main::Uni2");
   Print_Var ("main::Uni3");
   Print_Var ("main::Mrwu");
   Print_Var ("main::as");
   Print_Var ("main::asa");
   Print_Var ("main::Mrws");
   Print_Var ("main::Mrws2");
   Print_Var ("main::tv");
   Print_Var ("main::list");
   Print_Var ("main::test_volatile");

   --  The following test is part of debug.parsing.4
   Print_Var (Dereference_Name (Language, "main::Mrora"));
   Close (Gdb);

exception
   when others =>
      Close (Gdb);
      raise;

end Test_Parse_C;
