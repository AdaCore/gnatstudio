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

--  Package for Ada.Finalization classes

private with Ada.Strings.Unbounded;

package GVD.Variables.Types.Classes.Ada.Strings.Unbounded is

   -----------------------------------
   -- GVD_Ada_Unbounded_String_Type --
   -----------------------------------

   type GVD_Ada_Unbounded_String_Type is new GVD_Class_Type with private;
   type GVD_Ada_Unbounded_String_Type_Access is
     access all GVD_Ada_Unbounded_String_Type'Class;

   overriding function Get_Simple_Value
     (Self : not null access GVD_Ada_Unbounded_String_Type) return String;

   overriding function Get_Value_Command
     (Self   : not null access GVD_Ada_Unbounded_String_Type;
      Entity : String)
      return String;

   overriding procedure Set_Value
     (Self  : not null access GVD_Ada_Unbounded_String_Type;
      Value : String);

private

   type GVD_Ada_Unbounded_String_Type is new GVD_Class_Type with record
      Value : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Ada_Unbounded_String_Type) return String is
     ("Ada.Strings.Unbounded.Unbounded_String");

   overriding procedure Set_Type_Name
     (Self : not null access GVD_Ada_Unbounded_String_Type;
      Name : String) is null;

   overriding function Get_Type_Name
     (Self : not null access GVD_Ada_Unbounded_String_Type)
      return String is ("Ada.Strings.Unbounded.Unbounded_String");

   overriding procedure Clear
     (Self : not null access GVD_Ada_Unbounded_String_Type);

   overriding procedure Clone
     (Self : not null access GVD_Ada_Unbounded_String_Type;
      Item : not null GVD_Generic_Type_Access);

   function Create return GVD_Type_Holder;

end GVD.Variables.Types.Classes.Ada.Strings.Unbounded;
