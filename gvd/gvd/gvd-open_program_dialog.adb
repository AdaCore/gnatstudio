-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Main;          use Gtk.Main;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Radio_Button;  use Gtk.Radio_Button;
with Basic_Types;       use Basic_Types;

package body GVD.Open_Program_Dialog is

   ----------
   -- Free --
   ----------

   procedure Free (Descriptor : in out Program_Descriptor) is
   begin
      Free (Descriptor.Program);
      Free (Descriptor.Remote_Host);
      Free (Descriptor.Remote_Target);
      Free (Descriptor.Protocol);
      Free (Descriptor.Debugger_Name);
   end Free;

   ------------------
   -- Open_Program --
   ------------------

   procedure Open_Program
     (Open       : in out Open_Program_Dialog_Access;
      Descriptor : out Program_Descriptor) is
   begin
      if Open = null then
         Open := new Open_Program_Dialog_Record;
         Open_Program_Pkg.Initialize (Open);
      end if;

      Show_All (Open);
      Gtk.Main.Main;

      if not Open.Valid then
         Descriptor.Launch := None;
         Hide (Open);
         return;
      end if;

      if Get_Active (Open.Gdb_Button) then
         Descriptor.Debugger := Gdb_Type;
      elsif Get_Active (Open.Dbx_Button) then
         Descriptor.Debugger := Dbx_Type;
      elsif Get_Active (Open.Xdb_Button) then
         Descriptor.Debugger := Xdb_Type;
      elsif Get_Active (Open.Jdb_Button) then
         Descriptor.Debugger := Jdb_Type;
      elsif Get_Active (Open.Pydb_Button) then
         Descriptor.Debugger := Pydb_Type;
      elsif Get_Active (Open.Perl_Button) then
         Descriptor.Debugger := Perl_Type;
      end if;

      Descriptor.Program := new String' (Get_Text (Open.Program_Entry));
      Descriptor.Remote_Host := new String' (Get_Text (Open.Host_Entry));
      Descriptor.Remote_Target := new String' (Get_Text (Open.Target_Entry));
      Descriptor.Protocol := new String' (Get_Text (Open.Protocol_Entry));
      Descriptor.Debugger_Name := new String' (Get_Text (Open.Debugger_Entry));

      if Get_Active (Open.Replace_Check) then
         Descriptor.Launch := Current_Debugger;
      else
         Descriptor.Launch := New_Debugger;
      end if;

      Hide (Open);
   end Open_Program;

end GVD.Open_Program_Dialog;
