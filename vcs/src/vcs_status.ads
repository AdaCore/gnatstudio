------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Ada.Calendar;      use Ada.Calendar;

with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPS.Kernel;        use GPS.Kernel;
with HTables;
with String_List_Utils; use String_List_Utils;
with VCS;               use VCS;

package VCS_Status is

   type Status_Cache is private;

   type Line_Record is record
      Status : File_Status_Record;
      --  The file status

      Log    : Boolean;
      --  Whether the file is associated with a changelog
   end record;

   No_Data : constant Line_Record;

   function Get_Cache
     (Cache : Status_Cache;
      File  : GNATCOLL.VFS.Virtual_File) return Line_Record;
   --  Return the cached status for the given file. The result must not be
   --  freed.

   procedure Set_Cache
     (Cache  : Status_Cache;
      File   : GNATCOLL.VFS.Virtual_File;
      Status : in out Line_Record);
   --  Record the Status for the given file

   procedure Clear_Cache
     (Cache : in out Status_Cache; Free_Memory : Boolean := False);
   --  Clear all recorded file status

   function Has_Status
     (Cache  : Status_Cache;
      File   : GNATCOLL.VFS.Virtual_File;
      Ref    : VCS_Access;
      Status : Status_Id) return Boolean;
   --  Returns True if the File status correspond to Status

   procedure Save_Cache
     (Kernel : access Kernel_Handle_Record'Class; Cache : Status_Cache);
   --  Save cache information

   procedure Load_Cache
     (Kernel : access Kernel_Handle_Record'Class; Cache : out Status_Cache);
   --  Load cache information

private

   type Internal_Record is record
      LR        : Line_Record;
      Timestamp : Time;
   end record;

   No_Data : constant Line_Record :=
               ((GNATCOLL.VFS.No_File, Unknown, null, null,
                others => String_List.Empty_Vector),
                False);

   No_I_Data : constant Internal_Record := (No_Data, Time_Of (1970, 1, 1));

   type Header_Num is range 1 .. 5_000;

   procedure Free (X : in out Internal_Record);

   function Hash (F : Virtual_File) return Header_Num;

   package Status_Hash is new HTables.Simple_HTable
     (Header_Num   => Header_Num,
      Element      => Internal_Record,
      Free_Element => Free,
      No_Element   => No_I_Data,
      Key          => Virtual_File,
      Hash         => Hash,
      Equal        => "=");
   --  Store for each file the current status. This is a cache to avoid sending
   --  requests to the VCS.

   type HTable_Access is access Status_Hash.Instance;

   type Table is record
      T : HTable_Access := new Status_Hash.Instance;
   end record;

   type Status_Cache is new Table;

end VCS_Status;
