-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2006-2008, AdaCore                --
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
--  This package defines the remote path element needed for path translation
--  between remote and local host.

with GNAT.Strings;
package Remote.Path is

   type Synchronisation_Type is (Never,
                                 Once_To_Local,
                                 Once_To_Remote,
                                 Always);
   --  Synchronisation mechanism used for a given remote path.

   type Mirror_Path is tagged private;

   Null_Path : constant Mirror_Path;

   procedure Init (Mirror          : in out Mirror_Path'Class;
                   Local_Path      : String;
                   Remote_Path     : String;
                   Synchronisation : Synchronisation_Type);
   function Get_Local_Path (Mirror    : Mirror_Path;
                            Tentative : Boolean := False) return String;
   function Get_Remote_Path (Mirror    : Mirror_Path;
                             Tentative : Boolean := False) return String;
   function Get_Synchronisation
     (Mirror : Mirror_Path;
      Tentative : Boolean := False) return Synchronisation_Type;

   procedure Set_Tentative_Local_Path
     (Mirror : in out Mirror_Path;
      Path   : String);
   --  Tentatively set the local path. Call Apply to effectively set this path.

   procedure Set_Tentative_Remote_Path
     (Mirror : in out Mirror_Path;
      Path   : String);
   --  Same as above for the remote path

   procedure Set_Tentative_Synchronisation
     (Mirror : in out Mirror_Path;
      Synchronisation : Synchronisation_Type);
   --  Same as above for sync

   procedure Set_Deleted_State (Mirror : in out Mirror_Path);
   function Get_Deleted_State (Mirror : Mirror_Path) return Boolean;
   --  Set/Get the deleted state

   function Is_Modified (Mirror : Mirror_Path) return Boolean;
   --  Tell if Mirror as a tentative value set.

   procedure Apply (Mirror : in out Mirror_Path);

   procedure Cancel (Mirror : in out Mirror_Path);

   overriding function "=" (M1, M2 : Mirror_Path) return Boolean;

private

   type Mirror_Path_Record is record
      Local_Path  : GNAT.Strings.String_Access := null;
      Remote_Path : GNAT.Strings.String_Access := null;
      Sync        : Synchronisation_Type       := Never;
   end record;

   --  ??? should be a controlled type
   type Mirror_Path is tagged record
      Actual             : Mirror_Path_Record;
      Tentative          : Mirror_Path_Record;
      Tentative_Sync_Set : Boolean := False;
      Tentative_Delete   : Boolean := False;
   end record;

   Null_Path : constant Mirror_Path :=
                 (Actual             => (Local_Path  => null,
                                         Remote_Path => null,
                                         Sync        => Never),
                  Tentative          => (Local_Path  => null,
                                         Remote_Path => null,
                                         Sync        => Never),
                  Tentative_Sync_Set => False,
                  Tentative_Delete   => False);

end Remote.Path;
