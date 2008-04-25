-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2008, AdaCore             --
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

with Config;                             use Config;
with Machine_Descriptors;                use Machine_Descriptors;
with GNATCOLL.Filesystem.Unix.Remote;    use GNATCOLL.Filesystem.Unix.Remote;
with GNATCOLL.Filesystem.Windows;        use GNATCOLL.Filesystem.Windows;
with GNATCOLL.Filesystem.Windows.Remote;
use  GNATCOLL.Filesystem.Windows.Remote;
with GNATCOLL.Filesystem.Transport;      use GNATCOLL.Filesystem.Transport;
with GNAT.Strings;                       use GNAT.Strings;
with Remote;

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;             use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

package body Filesystems is

   type GPS_Transport_Record
     is new Filesystem_Transport_Record with null record;
   overriding procedure Execute_Remotely
     (Transport           : access GPS_Transport_Record;
      Host                : String;
      Args                : GNAT.Strings.String_List;
      Status              : out Boolean;
      Execution_Directory : String := "");
   overriding procedure Execute_Remotely
     (Transport           : access GPS_Transport_Record;
      Host                : String;
      Args                : GNAT.Strings.String_List;
      Result              : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : String := "");
   overriding procedure Spawn_Remotely
     (Transport           : access GPS_Transport_Record;
      Descriptor          : out GNAT.Expect.Process_Descriptor_Access;
      Host                : String;
      Args                : GNAT.Strings.String_List);

   Transport : aliased GPS_Transport_Record;
   --  A global variable, only used as a dispatcher below

   ----------------------
   -- Execute_Remotely --
   ----------------------

   procedure Execute_Remotely
     (Transport           : access GPS_Transport_Record;
      Host                : String;
      Args                : GNAT.Strings.String_List;
      Status              : out Boolean;
      Execution_Directory : String := "")
   is
      pragma Unreferenced (Transport);
   begin
      Sync_Execute (Host, Args, Status, Execution_Directory);
   end Execute_Remotely;

   ----------------------
   -- Execute_Remotely --
   ----------------------

   procedure Execute_Remotely
     (Transport           : access GPS_Transport_Record;
      Host                : String;
      Args                : GNAT.Strings.String_List;
      Result              : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : String := "")
   is
      pragma Unreferenced (Transport);
   begin
      Sync_Execute (Host, Args, Result, Status, Execution_Directory);
   end Execute_Remotely;

   --------------------
   -- Spawn_Remotely --
   --------------------

   procedure Spawn_Remotely
     (Transport           : access GPS_Transport_Record;
      Descriptor          : out GNAT.Expect.Process_Descriptor_Access;
      Host                : String;
      Args                : GNAT.Strings.String_List)
   is
      pragma Unreferenced (Transport);
   begin
      Remote_Spawn (Descriptor, Host, Args);
   end Spawn_Remotely;

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (Nickname : String) return Filesystem_Record'Class is
   begin
      if Nickname = "" then
         return Get_Local_Filesystem;
      else
         return Get_Filesystem (Get_Machine_Descriptor (Nickname)).all;
      end if;
   end Get_Filesystem;

   --------------------------
   -- Get_Local_Filesystem --
   --------------------------

   function Get_Local_Filesystem return Filesystem_Record'Class is
      Windows_FS : GNATCOLL.Filesystem.Windows.Windows_Filesystem_Record;
      Unix_FS    : GNATCOLL.Filesystem.Unix.Unix_Filesystem_Record;
   begin
      if Host = Config.Windows then
         return Windows_FS;
      else
         --  Unix and windows support only
         return Unix_FS;
      end if;
   end Get_Local_Filesystem;

   ------------------------
   -- Filesystem_Factory --
   ------------------------

   function Filesystem_Factory
     (Typ      : Filesystem_Type;
      Nickname : String) return GNATCOLL.Filesystem.Filesystem_Access
   is
      FS : Filesystem_Access;
   begin
      if Nickname = Remote.Local_Nickname
        or else Nickname = ""
      then
         --  Local filesystem
         case Typ is
            when Unix =>
               FS := new GNATCOLL.Filesystem.Unix.Unix_Filesystem_Record;

            when Windows =>
               FS := new Windows_Filesystem_Record;
         end case;

      else
         case Typ is
            when Unix =>
               FS := new Remote_Unix_Filesystem_Record;
               Setup (Remote_Unix_Filesystem_Record (FS.all),
                      Host      => Nickname,
                      Transport => Transport'Access);
            when Windows =>
               FS := new Remote_Windows_Filesystem_Record;
               Setup (Remote_Windows_Filesystem_Record (FS.all),
                      Host      => Nickname,
                      Transport => Transport'Access);
         end case;
      end if;

      return FS;
   end Filesystem_Factory;

end Filesystems;
