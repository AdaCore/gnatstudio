------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

--  This package provides type to format profiles in documentation queries

with Ada.Strings.Unbounded;

package Language.Profile_Formaters is

   type Profile_Formater is abstract tagged null record;
   --  Helper type to format profile. See Get_Profile in Language.Tree.Database

   procedure Add_Parameter
     (Self    : access Profile_Formater;
      Name    : String;
      Mode    : String;
      Of_Type : String;
      Default : String) is abstract;
   --  Add parameter of subprogram to formater.
   --  Longest is length of longest name of all parameters' names.
   --  Default is parameter's default value if any.

   procedure Add_Result
     (Self    : access Profile_Formater;
      Mode    : String;
      Of_Type : String) is abstract;
   --  Add result type of a function to formater

   procedure Add_Variable
     (Self    : access Profile_Formater;
      Mode    : String;
      Of_Type : String) is abstract;
   --  Add type of a variable to formater

   procedure Add_Aspects
     (Self : access Profile_Formater;
      Text : String) is abstract;
   --  Add text of aspects to formater

   procedure Add_Comments
     (Self : access Profile_Formater;
      Text : String) is abstract;
   --  Add text of comments to formater

   function Get_Text
     (Self : access Profile_Formater) return String is abstract;
   --  Return resulting formated text of profile

   type Text_Profile_Formater is new Profile_Formater with private;
   --  Profile formater to generate plain text

   procedure Configure
     (Self             : in out Text_Profile_Formater;
      Show_Param_Names : Boolean := True);
   --  Configure the output of the formater

private

   type Text_Profile_Formater is new Profile_Formater with record
      Text             : Ada.Strings.Unbounded.Unbounded_String;
      Has_Parameter    : Boolean := False;
      Show_Param_Names : Boolean := True;
   end record;

   overriding procedure Add_Parameter
     (Self    : access Text_Profile_Formater;
      Name    : String;
      Mode    : String;
      Of_Type : String;
      Default : String);
   overriding procedure Add_Result
     (Self    : access Text_Profile_Formater;
      Mode    : String;
      Of_Type : String);
   overriding procedure Add_Variable
     (Self    : access Text_Profile_Formater;
      Mode    : String;
      Of_Type : String);
   overriding procedure Add_Aspects
     (Self : access Text_Profile_Formater;
      Text : String);
   overriding procedure Add_Comments
     (Self : access Text_Profile_Formater;
      Text : String);
   overriding function Get_Text
     (Self : access Text_Profile_Formater) return String;

end Language.Profile_Formaters;
