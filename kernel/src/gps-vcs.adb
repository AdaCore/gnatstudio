------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

package body GPS.VCS is

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Abstract_VCS_Engine) is
   begin
      Free (Self.Instances);
   end Free;

   -------------------------
   -- Create_VCS_Instance --
   -------------------------

   function Create_VCS_Instance
     (Script : access Scripting_Language_Record'Class;
      VCS    : not null access Abstract_VCS_Engine'Class)
      return Class_Instance
   is
   begin
      return Engine_Proxies.Get_Or_Create_Instance
        (VCS.Instances,
         Obj    => VCS,
         Script => Script);
   end Create_VCS_Instance;

   -------------
   -- Get_VCS --
   -------------

   function Get_VCS
     (Inst   : Class_Instance)
      return not null access Abstract_VCS_Engine'Class is
   begin
      return Engine_Proxies.From_Instance (Inst);
   end Get_VCS;

   -------------
   -- Has_VCS --
   -------------

   function Has_VCS (Inst   : Class_Instance) return Boolean is
   begin
      return Engine_Proxies.Has_Element (Inst);
   end Has_VCS;

   ----------------------
   -- Set_VCS_Instance --
   ----------------------

   procedure Set_VCS_Instance
     (VCS    : not null access Abstract_VCS_Engine'Class;
      Inst   : Class_Instance)
   is
   begin
      Engine_Proxies.Store_In_Instance
        (VCS.Instances,
         Inst => Inst,
         Obj  => Abstract_VCS_Engine_Access (VCS));
   end Set_VCS_Instance;

   ------------------------
   -- Make_File_Writable --
   ------------------------

   procedure Make_File_Writable
     (Self       : not null access Abstract_VCS_Engine;
      File       : GNATCOLL.VFS.Virtual_File;
      Writable   : Boolean)
   is
      pragma Unreferenced (Self);
   begin
      File.Set_Writable (Writable);
   end Make_File_Writable;

end GPS.VCS;
