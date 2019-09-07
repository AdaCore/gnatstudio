------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;

package body Language.Icons is

   -------------------------
   -- Stock_From_Category --
   -------------------------

   function Stock_From_Category
     (Is_Declaration : Boolean;
      Visibility     : Construct_Visibility;
      Category       : Language_Category) return String
   is
      Theme : constant String :=
        (if Is_Declaration and then Gtk_Theme.Get_Pref.Dark
         then "-dark"
         else "");

      function Get_Name (Suffix : String) return String;

      --------------
      -- Get_Name --
      --------------

      function Get_Name (Suffix : String) return String is
        ( --  Do not use -symbolic icons, since we want to preserve the colors
         case Category is
            when Cat_Unknown | Cat_With
               | Cat_Use   | Cat_Include
               | Construct_Category | Cat_Exception_Handler
               | Cat_Pragma | Cat_Aspect =>
               "gps-emblem-entity-generic" & Suffix & Theme,
            when Cat_Package | Cat_Namespace | Cat_Custom =>
               "gps-emblem-entity-package" & Suffix & Theme,
            when Cat_Task | Cat_Procedure   | Cat_Function
               | Cat_Method    | Cat_Constructor | Cat_Destructor
               | Cat_Protected | Cat_Entry =>
               "gps-emblem-entity-subprogram" & Suffix & Theme,
            when Cat_Class | Cat_Structure | Cat_Union
               | Cat_Type  | Cat_Subtype | Cat_Case_Inside_Record =>
               "gps-emblem-entity-type" & Suffix & Theme,
            when Cat_Variable    | Cat_Local_Variable
               | Cat_Parameter | Cat_Discriminant | Cat_Field
               | Cat_Literal   | Cat_Representation_Clause =>
               "gps-emblem-entity-variable" & Suffix & Theme);

   begin
      if Is_Declaration then
         case Visibility is
            when Visibility_Public    => return Get_Name ("-spec");
            when Visibility_Protected => return Get_Name ("-protected-spec");
            when Visibility_Private   => return Get_Name ("-private-spec");
         end case;

      else
         case Visibility is
            when Visibility_Public    => return Get_Name ("");
            when Visibility_Protected => return Get_Name ("-protected");
            when Visibility_Private   => return Get_Name ("-private");
         end case;
      end if;
   end Stock_From_Category;
end Language.Icons;
