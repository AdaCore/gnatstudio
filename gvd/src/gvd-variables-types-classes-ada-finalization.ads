------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  Package for Ada.Finalization classes

private with Ada.Strings.Unbounded;

package GVD.Variables.Types.Classes.Ada.Finalization is

   -------------------------------
   -- GVD_Ada_Finalization_Type --
   -------------------------------

   type GVD_Ada_Finalization_Type is new GVD_Class_Type (0) with private;
   type GVD_Ada_Finalization_Type_Access is
     access all GVD_Ada_Finalization_Type'Class;

   overriding procedure Add_Ancestor
     (Self     : not null access GVD_Ada_Finalization_Type;
      Num      : Positive;
      Ancestor : GVD_Type_Holder);

   overriding procedure Set_Child
     (Self  : not null access GVD_Ada_Finalization_Type;
      Child : GVD_Type_Holder);

   overriding function Get_Child
     (Self : not null access GVD_Ada_Finalization_Type)
      return GVD_Type_Holder is (Empty_GVD_Type_Holder);

   overriding function Get_Ancestor
     (Self       : not null access GVD_Ada_Finalization_Type;
      Unused_Num : Positive)
      return GVD_Type_Holder is (Empty_GVD_Type_Holder);

   overriding function Get_Num_Ancestors
     (Self : not null access GVD_Ada_Finalization_Type)
      return Natural is (0);

private

   type GVD_Ada_Finalization_Type is new GVD_Class_Type (0) with record
      Name : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Ada_Finalization_Type) return String is
     (To_String (Self.Name));

   overriding procedure Clear
     (Self : not null access GVD_Ada_Finalization_Type) is null;

   overriding procedure Set_Type_Name
     (Self : not null access GVD_Ada_Finalization_Type;
      Name : String) is null;

   overriding function Get_Type_Name
     (Self : not null access GVD_Ada_Finalization_Type)
      return String is (To_String (Self.Name));

   function Create_Controlled return GVD_Type_Holder;
   function Create_Limited_Controlled return GVD_Type_Holder;

end GVD.Variables.Types.Classes.Ada.Finalization;
