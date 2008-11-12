-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
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

with Glib.Convert;                       use Glib.Convert;

pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;             use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.Strings;                       use GNAT.Strings;

with GNATCOLL.Filesystem.Unix;           use GNATCOLL.Filesystem.Unix;
with GNATCOLL.Filesystem.Unix.Remote;    use GNATCOLL.Filesystem.Unix.Remote;
with GNATCOLL.Filesystem.Windows;        use GNATCOLL.Filesystem.Windows;
with GNATCOLL.Filesystem.Windows.Remote;
use  GNATCOLL.Filesystem.Windows.Remote;
with GNATCOLL.Filesystem.Transport;      use GNATCOLL.Filesystem.Transport;

with Config;                             use Config;
with Machine_Descriptors;                use Machine_Descriptors;
with Remote;

package body Filesystems is

   package SLU renames String_List_Utils;

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

   function Encode_To_UTF8 (Name : String) return String;
   --  Convert Name into a proper utf8 string if needed

   --------------------
   -- Encode_To_UTF8 --
   --------------------

   function Encode_To_UTF8 (Name : String) return String is
      Result : constant String := Locale_To_UTF8 (Name);
   begin
      --  The name might already be UTF-8 (for instance on windows), but we
      --  try anyway and handle errors appropriately

      if Result = "" then
         return Name;
      else
         return Result;
      end if;
   end Encode_To_UTF8;

   ----------------------
   -- Execute_Remotely --
   ----------------------

   overriding procedure Execute_Remotely
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

   overriding procedure Execute_Remotely
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

   overriding procedure Spawn_Remotely
     (Transport  : access GPS_Transport_Record;
      Descriptor : out GNAT.Expect.Process_Descriptor_Access;
      Host       : String;
      Args       : GNAT.Strings.String_List)
   is
      pragma Unreferenced (Transport);
   begin
      Remote_Spawn (Descriptor, Host, Args);
   end Spawn_Remotely;

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (Nickname : String) return Filesystem_Access is
   begin
      if Nickname = "" or else Nickname = Remote.Local_Nickname then
         return Get_Local_Filesystem;
      else
         return Get_Filesystem (Get_Machine_Descriptor (Nickname));
      end if;
   end Get_Filesystem;

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
               FS := new Unix_Filesystem_Record;
               Set_Locale_To_Display_Encoder (FS.all, Encode_To_UTF8'Access);

            when Windows =>
               --  On Windows we do not want to convert file name to UTF-8 as
               --  they are already in UTF-8.
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

         Set_Locale_To_Display_Encoder (FS.all, Encode_To_UTF8'Access);
      end if;

      return FS;
   end Filesystem_Factory;

   -------------------------
   -- Is_Local_Filesystem --
   -------------------------

   function Is_Local_Filesystem
     (FS : access Filesystem_Record'Class) return Boolean is
   begin
      return FS.all not in Remote_Unix_Filesystem_Record'Class
        and then FS.all not in Remote_Windows_Filesystem_Record'Class;
   end Is_Local_Filesystem;

   --------------
   -- Get_Host --
   --------------

   function Get_Host
     (FS : access Filesystem_Record'Class) return String is
   begin
      --  ??? should use dispatching directly
      if FS.all in Remote_Unix_Filesystem_Record'Class then
         return Get_Host (Remote_Unix_Filesystem_Record (FS.all));
      elsif FS.all in Remote_Windows_Filesystem_Record'Class then
         return Get_Host (Remote_Windows_Filesystem_Record (FS.all));
      else
         return "";
      end if;
   end Get_Host;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (File : Virtual_File) return Boolean is
   begin
      return File = No_File
        or else Is_Local_Filesystem (File.Get_Filesystem);
   end Is_Local;

   ------------
   -- Create --
   ------------

   function Create (Files : SLU.String_List.List) return File_Array is
      FA   : File_Array (1 .. SLU.String_List.Length (Files));
      Iter : SLU.String_List.List_Node;
   begin
      Iter := SLU.String_List.First (Files);

      for K in FA'Range loop
         FA (K) := Create (SLU.String_List.Data (Iter));
         Iter := SLU.String_List.Next (Iter);
      end loop;

      return FA;
   end Create;

   --------------
   -- Get_Host --
   --------------

   function Get_Host (File : Virtual_File) return String is
   begin
      return Get_Host (File.Get_Filesystem);
   end Get_Host;

   ----------------------
   -- Filename_To_UTF8 --
   ----------------------

   function Filename_To_UTF8 (Name : String) return String is
   begin
      if Config.Host = Windows then
         return Name;
      else
         return Locale_To_UTF8 (Name);
      end if;
   end Filename_To_UTF8;

   ------------------------
   -- Filename_From_UTF8 --
   ------------------------

   function Filename_From_UTF8 (Name : String) return String is
   begin
      if Config.Host = Windows then
         return Name;
      else
         return Locale_From_UTF8 (Name);
      end if;
   end Filename_From_UTF8;

begin
   --  On Windows we do not want to convert the filename in UTF-8 as all
   --  filenames are returned from the OS as UTF-8 encoded.

   if Config.Host /= Windows then
      Set_Locale_To_Display_Encoder
        (Get_Local_Filesystem.all, Encode_To_UTF8'Access);
   end if;
end Filesystems;
