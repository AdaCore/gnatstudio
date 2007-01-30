-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
--                              AdaCore                              --
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

with Traces; use Traces;
with GNAT.Strings; use GNAT.Strings;
package body Remote.Path is

   Me : constant Debug_Handle := Create ("Remote.Path");

   ----------
   -- Init --
   ----------

   procedure Init
     (Mirror          : in out Mirror_Path'Class;
      Local_Path      : String;
      Remote_Path     : String;
      Synchronisation : Synchronisation_Type)
   is
   begin
      Mirror.Actual :=
        (Local_Path  => new String'(Local_Path),
         Remote_Path => new String'(Remote_Path),
         Sync        => Synchronisation);
      Mirror.Tentative :=
        (Local_Path  => null,
         Remote_Path => null,
         Sync        => Never);
      Mirror.Tentative_Sync_Set := False;
      Mirror.Tentative_Delete   := False;
   end Init;

   --------------------
   -- Get_Local_Path --
   --------------------

   function Get_Local_Path
     (Mirror    : Mirror_Path;
      Tentative : Boolean := False)
      return String is
   begin
      if not Tentative or else Mirror.Tentative.Local_Path = null then
         if Mirror.Actual.Local_Path = null then
            return "";
         else
            return Mirror.Actual.Local_Path.all;
         end if;

      else
         return Mirror.Tentative.Local_Path.all;
      end if;
   end Get_Local_Path;

   ---------------------
   -- Get_Remote_Path --
   ---------------------

   function Get_Remote_Path
     (Mirror    : Mirror_Path;
      Tentative : Boolean := False)
      return String is
   begin
      if not Tentative or else Mirror.Tentative.Remote_Path = null then
         if Mirror.Actual.Remote_Path = null then
            return "";
         else
            return Mirror.Actual.Remote_Path.all;
         end if;

      else
         return Mirror.Tentative.Remote_Path.all;
      end if;
   end Get_Remote_Path;

   -------------------------
   -- Get_Synchronisation --
   -------------------------

   function Get_Synchronisation
     (Mirror : Mirror_Path;
      Tentative : Boolean := False)
      return Synchronisation_Type is
   begin
      if not Tentative or else not Mirror.Tentative_Sync_Set then
         return Mirror.Actual.Sync;
      else
         return Mirror.Tentative.Sync;
      end if;
   end Get_Synchronisation;

   ------------------------------
   -- Set_Tentative_Local_Path --
   ------------------------------

   procedure Set_Tentative_Local_Path
     (Mirror : in out Mirror_Path;
      Path   : String) is
   begin
      if Mirror.Tentative.Local_Path /= null then
         Free (Mirror.Tentative.Local_Path);
      end if;

      if Mirror.Actual.Local_Path /= null
        and then Mirror.Actual.Local_Path.all = Path
      then
         Trace (Me, "tentative local path identical to actual: " & Path);
         return;
      end if;

      Mirror.Tentative.Local_Path := new String'(Path);
      Trace (Me, "tentative local path set to " & Path);
   end Set_Tentative_Local_Path;

   -------------------------------
   -- Set_Tentative_Remote_Path --
   -------------------------------

   procedure Set_Tentative_Remote_Path
     (Mirror : in out Mirror_Path;
      Path   : String) is
   begin
      if Mirror.Tentative.Remote_Path /= null then
         Free (Mirror.Tentative.Remote_Path);
      end if;

      if Mirror.Actual.Remote_Path /= null
        and then Mirror.Actual.Remote_Path.all = Path
      then
         Trace (Me, "tentative remote path identical to actual: " & Path);
         return;
      end if;

      Mirror.Tentative.Remote_Path := new String'(Path);
      Trace (Me, "tentative remote path set to " & Path);
   end Set_Tentative_Remote_Path;

   -----------------------------------
   -- Set_Tentative_Synchronisation --
   -----------------------------------

   procedure Set_Tentative_Synchronisation
     (Mirror          : in out Mirror_Path;
      Synchronisation : Synchronisation_Type)
   is
   begin
      if Mirror.Actual.Sync = Synchronisation then
         Mirror.Tentative_Sync_Set := False;
         return;
      end if;

      Mirror.Tentative_Sync_Set := True;
      Mirror.Tentative.Sync := Synchronisation;
      Trace (Me, "tentative sync set to " &
             Synchronisation_Type'Image (Synchronisation));
   end Set_Tentative_Synchronisation;

   -----------------------
   -- Set_Deleted_State --
   -----------------------

   procedure Set_Deleted_State (Mirror : in out Mirror_Path) is
   begin
      Trace (Me, "Deleted state set");
      Mirror.Tentative_Delete := True;
   end Set_Deleted_State;

   -----------------------
   -- Get_Deleted_State --
   -----------------------

   function Get_Deleted_State (Mirror : in Mirror_Path) return Boolean is
   begin
      Trace (Me, "Deleted state is " &
             Boolean'Image (Mirror.Tentative_Delete));
      return Mirror.Tentative_Delete;
   end Get_Deleted_State;

   -----------------
   -- Is_Modified --
   -----------------

   function Is_Modified (Mirror : in Mirror_Path) return Boolean is
   begin
      return Mirror.Tentative.Local_Path /= null
        or else Mirror.Tentative.Remote_Path /= null
        or else Mirror.Tentative_Sync_Set;
   end Is_Modified;

   -----------
   -- Apply --
   -----------

   procedure Apply (Mirror : in out Mirror_Path) is
   begin
      if Mirror.Tentative.Local_Path /= null then
         if Mirror.Actual.Local_Path /= null then
            Free (Mirror.Actual.Local_Path);
         end if;

         Mirror.Actual.Local_Path := Mirror.Tentative.Local_Path;
         Mirror.Tentative.Local_Path := null;
         Trace (Me, "Apply local path " & Mirror.Actual.Local_Path.all);
      end if;

      if Mirror.Tentative.Remote_Path /= null then
         if Mirror.Actual.Remote_Path /= null then
            Free (Mirror.Actual.Remote_Path);
         end if;

         Mirror.Actual.Remote_Path := Mirror.Tentative.Remote_Path;
         Mirror.Tentative.Remote_Path := null;
         Trace (Me, "Apply remote path " & Mirror.Actual.Remote_Path.all);
      end if;

      if Mirror.Tentative_Sync_Set then
         Mirror.Tentative_Sync_Set := False;
         Mirror.Actual.Sync := Mirror.Tentative.Sync;
         Trace (Me, "Apply sync " &
                Synchronisation_Type'Image (Mirror.Actual.Sync));
      end if;
   end Apply;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Mirror : in out Mirror_Path) is
   begin
      if Mirror.Tentative.Local_Path /= null then
         Free (Mirror.Tentative.Local_Path);
      end if;

      if Mirror.Tentative.Remote_Path /= null then
         Free (Mirror.Tentative.Remote_Path);
      end if;

      Mirror.Tentative_Delete := False;
      Mirror.Tentative_Sync_Set := False;
   end Cancel;

   ---------
   -- "=" --
   ---------

   function "=" (M1, M2 : Mirror_Path) return Boolean is
      function Same (P1, P2 : String_Access) return Boolean;

      function Same (P1, P2 : String_Access) return Boolean is
      begin
         if P1 = null then
            return P2 = null;
         end if;

         if P2 = null then
            return False;
         end if;

         return P1.all = P2.all;
      end Same;
   begin
      return Same (M1.Actual.Local_Path, M2.Actual.Local_Path)
        and then Same (M1.Actual.Remote_Path, M2.Actual.Remote_Path)
        and then M1.Actual.Sync = M2.Actual.Sync;
   end "=";

end Remote.Path;
