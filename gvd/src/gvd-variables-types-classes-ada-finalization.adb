------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Debugger.Base_Gdb.Ada;

package body GVD.Variables.Types.Classes.Ada.Finalization is

   ------------------
   -- Add_Ancestor --
   ------------------

   overriding procedure Add_Ancestor
     (Self     : not null access GVD_Ada_Finalization_Type;
      Num      : Positive;
      Ancestor : GVD_Type_Holder) is
   begin
      raise Constraint_Error with
        "GVD_Ada_Finalization_Type cannot have an ancestor";
   end Add_Ancestor;

   -----------------------
   -- Create_Controlled --
   -----------------------

   function Create_Controlled return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Ada_Finalization_Type);
   begin
      GVD_Ada_Finalization_Type_Access (Data.Instance).Name :=
        To_Unbounded_String ("Ada.Finalization.Limited_Controlled");

      return GVD_Type_Holder'(Standard.Ada.Finalization.Controlled with Data);
   end Create_Controlled;

   -------------------------------
   -- Create_Limited_Controlled --
   -------------------------------

   function Create_Limited_Controlled return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Ada_Finalization_Type);
   begin
      GVD_Ada_Finalization_Type_Access (Data.Instance).Name :=
        To_Unbounded_String ("Ada.Finalization.Limited_Controlled");

      return GVD_Type_Holder'(Standard.Ada.Finalization.Controlled with Data);
   end Create_Limited_Controlled;

   ---------------
   -- Set_Child --
   ---------------

   overriding procedure Set_Child
     (Self  : not null access GVD_Ada_Finalization_Type;
      Child : GVD_Type_Holder) is
   begin
      raise Constraint_Error with
        "GVD_Ada_Finalization_Type cannot have a chaild";
   end Set_Child;

begin
   Debugger.Base_Gdb.Ada.Predefined_Type_Reestr.Insert
     ("ada.finalization.controlled", Create_Controlled'Access);
   Debugger.Base_Gdb.Ada.Predefined_Type_Reestr.Insert
     ("ada.finalization.limited_controlled", Create_Limited_Controlled'Access);

end GVD.Variables.Types.Classes.Ada.Finalization;
