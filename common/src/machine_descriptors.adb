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

with Ada.Unchecked_Deallocation;
with Filesystems;                 use Filesystems;
with GNATCOLL.Filesystem;         use GNATCOLL.Filesystem;
with Shell_Descriptors;           use Shell_Descriptors;

package body Machine_Descriptors is

   Machine_Descriptor_List : Machine_Descriptor_Access := null;
   --  ??? Should get rid of this global variable

   ---------------------------------
   -- Register_Machine_Descriptor --
   ---------------------------------

   procedure Register_Machine_Descriptor
     (Machine    : Machine_Descriptor;
      Descriptor : Machine_Descriptor_Access)
   is
      Current    : Machine_Descriptor_Access;
      Prev       : Machine_Descriptor_Access;
   begin
      Machine.Ref := Machine.Ref + 1;

      --  Place the new machine in an alphabetically ordered list.
      Current := Machine_Descriptor_List;
      Prev := null;

      while Current /= null loop

         if Current.Desc.Nickname.all = Machine.Nickname.all then
            --  Same nickname : replace it
            Descriptor.Next := Current.Next;

            if Prev /= null then
               Prev.Next := Descriptor;
            else
               Machine_Descriptor_List := Descriptor;
            end if;

            Close (Current);
            Unref (Current.Desc);

            return;

         elsif Current.Desc.Nickname.all > Machine.Nickname.all then
            Descriptor.Next := Current;

            if Prev /= null then
               Prev.Next := Descriptor;
            else
               Machine_Descriptor_List := Descriptor;
            end if;

            return;
         end if;

         Prev := Current;
         Current := Current.Next;
      end loop;

      --  Place the new Descriptor at the end of the list
      if Prev /= null then
         Prev.Next := Descriptor;
      else
         Machine_Descriptor_List := Descriptor;
      end if;
   end Register_Machine_Descriptor;

   -------------------------------
   -- Remove_Machine_Descriptor --
   -------------------------------

   procedure Remove_Machine_Descriptor (Desc : in out Machine_Descriptor) is
      Current : Machine_Descriptor_Access;
      Prev    : Machine_Descriptor_Access;
   begin
      Current := Machine_Descriptor_List;
      Prev := null;

      while Current /= null loop
         if Current.Desc = Desc then
            if Prev /= null then
               Prev.Next := Current.Next;
            else
               Machine_Descriptor_List := Current.Next;
            end if;
            Close (Current);
            Unref (Current.Desc);
            exit;
         end if;

         Prev := Current;
         Current := Current.Next;
      end loop;

      Desc := null;
   end Remove_Machine_Descriptor;

   ------------------------------------
   -- Remove_All_Machine_Descriptors --
   ------------------------------------

   procedure Remove_All_Machine_Descriptors is
      Current : Machine_Descriptor_Access;
      Next    : Machine_Descriptor_Access;
   begin
      Current := Machine_Descriptor_List;

      while Current /= null loop
         Next := Current.Next;
         Close (Current);
         Unref (Current.Desc);
         Current := Next;
      end loop;

      Machine_Descriptor_List := null;
   end Remove_All_Machine_Descriptors;

   -------------------------------
   -- Get_Nb_Machine_Descriptor --
   -------------------------------

   function Get_Nb_Machine_Descriptor return Natural is
      Nb : Natural := 0;
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      while Desc /= null loop
         Nb := Nb + 1;
         Desc := Desc.Next;
      end loop;

      return Nb;
   end Get_Nb_Machine_Descriptor;

   ----------------------------
   -- Get_Machine_Descriptor --
   ----------------------------

   function Get_Machine_Descriptor (N : Natural) return Machine_Descriptor is
      Nb   : Natural := 1;
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      while Desc /= null loop
         if Nb = N then
            return Desc.Desc;
         end if;

         Nb := Nb + 1;
         Desc := Desc.Next;
      end loop;

      raise Invalid_Nickname;
   end Get_Machine_Descriptor;

   -----------------------------------
   -- Get_Machine_Descriptor_Access --
   -----------------------------------

   function Get_Machine_Descriptor_Access
     (Nickname : String) return Machine_Descriptor_Access
   is
      Desc : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      while Desc /= null loop
         if Desc.Desc.Nickname.all = Nickname then
            return Desc;
         end if;

         Desc := Desc.Next;
      end loop;

      raise Invalid_Nickname with Nickname;
   end Get_Machine_Descriptor_Access;

   ----------------------------
   -- Get_Machine_Descriptor --
   ----------------------------

   function Get_Machine_Descriptor
     (Nickname : String) return Machine_Descriptor
   is
      Desc : Machine_Descriptor_Access;
   begin
      Desc := Get_Machine_Descriptor_Access (Nickname);
      return Desc.Desc;
   end Get_Machine_Descriptor;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (N : Natural) return String is
   begin
      return Get_Machine_Descriptor (N).Nickname.all;
   end Get_Nickname;

   -----------
   -- Unref --
   -----------

   procedure Unref (Desc : in out Machine_Descriptor) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Machine_Descriptor_Record'Class, Machine_Descriptor);
   begin
      if Desc.Ref <= 1 then
         Free (Desc.Nickname);
         Free (Desc.Network_Name);
         Free (Desc.Access_Name);
         Free (Desc.Shell_Name);
         Free (Desc.Extra_Init_Commands);
         Free (Desc.User_Name);
         Free (Desc.FS);
         --  ??? Free Desc.Dbg if any.
         Unchecked_Free (Desc);
      else
         Desc.Ref := Desc.Ref - 1;
      end if;
   end Unref;

   ---------------
   -- Close_All --
   ---------------

   procedure Close_All is
      Machine : Machine_Descriptor_Access := Machine_Descriptor_List;
   begin
      --  Be careful to not trace anything in here. This is called after
      --  the kernel is destroyed, and then cannot trace anymore

      while Machine /= null loop
         Close (Machine);
         Machine := Machine.Next;
      end loop;
   end Close_All;

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem
     (Machine : access Machine_Descriptor_Record'Class)
      return Filesystem_Access
   is
      Shell : Shell_Descriptor_Access;
   begin
      if Machine.FS = null then
         Shell := Get_Descriptor_From_Name (Machine.Shell_Name.all);
         if Shell = null then
            --  Configuration problem, assume we have Unix. That should never
            --  happen anyway
            Machine.FS := Filesystem_Factory (Filesystems.Unix, "");

         else
            Machine.FS := Filesystem_Factory
              (Shell.Filesystem, Machine.Nickname.all);
         end if;

      end if;
      return Machine.FS;
   end Get_Filesystem;

end Machine_Descriptors;
