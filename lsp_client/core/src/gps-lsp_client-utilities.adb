------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2025, AdaCore                  --
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

--  Minimal utilities - only pure protocol conversions
--  (no GPS.Editors/GPS.Kernel)

with VSS.Strings.Conversions;
with URIs;
with LSP.Types;

package body GPS.LSP_Client.Utilities is

   ------------
   -- To_URI --
   ------------

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri is
   begin
      return LSP.Types.To_LSP_URI
        (VSS.Strings.Conversions.To_Virtual_String
           (URIs.Conversions.From_File (Item.Display_Full_Name)));
   end To_URI;

   ---------------------
   -- To_Virtual_File --
   ---------------------

   function To_Virtual_File
     (Item : LSP.Messages.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
      File : constant String := URIs.Conversions.To_File
        (LSP.Types.To_UTF_8_String (Item),
         Normalize => True);  -- Normalize for security (was Preferences)
   begin
      return GNATCOLL.VFS.Create_From_UTF8 (File);
   end To_Virtual_File;

   ---------------------------
   -- To_Language_Category --
   ---------------------------

   function To_Language_Category
     (K            : LSP.Messages.SymbolKind;
      Is_Procedure : Boolean := False)
      return Language.Language_Category
   is
      use LSP.Messages;
   begin
      case K is
         when Module                        => return Language.Cat_Package;
         when Namespace                     => return Language.Cat_With;
         when A_Package                     => return Language.Cat_Package;
         when Class | Enum | An_Interface   => return Language.Cat_Type;
         when Struct                        => return Language.Cat_Structure;
         when Method                        => return Language.Cat_Function;
         when A_Function                    =>
            return (if Is_Procedure then Language.Cat_Procedure
                    else Language.Cat_Function);
         when Property                      => return Language.Cat_Pragma;
         when Field                         => return Language.Cat_Field;
         when Constructor                   => return Language.Cat_Constructor;
         when A_Constant                    => return Language.Cat_Constant;
         when Variable | LSP.Messages.String .. Object
                                            => return Language.Cat_Variable;
         when others                        => return Language.Cat_Unknown;
      end case;
   end To_Language_Category;

   -------------------------------
   -- To_Construct_Visibility --
   -------------------------------

   function To_Construct_Visibility
     (V : LSP.Messages.Als_Visibility)
      return Language.Construct_Visibility
   is
      use LSP.Messages;
   begin
      case V is
         when Als_Public    => return Language.Visibility_Public;
         when Als_Protected => return Language.Visibility_Protected;
         when Als_Private   => return Language.Visibility_Private;
      end case;
   end To_Construct_Visibility;

end GPS.LSP_Client.Utilities;
