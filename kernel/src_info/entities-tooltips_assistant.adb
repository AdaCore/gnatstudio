------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with GNATCOLL.Symbols;    use GNATCOLL.Symbols;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Glib;                use Glib;
with Glib.Convert;        use Glib.Convert;

with GPS.Intl;            use GPS.Intl;
with Language.Tree;       use Language.Tree;
with String_Utils;        use String_Utils;

package body Entities.Tooltips_Assistant is

   ------------------------
   -- Get_Tooltip_Header --
   ------------------------

   function Get_Tooltip_Header (Entity : Entity_Information) return String is
   begin
      if Get_Kind (Entity).Kind = Include_File then
         return  "<b>" & Escape_Text (Get (Entity.Name).all)
           & "</b>" & ASCII.LF
           & (-Kind_To_String (Get_Kind (Entity))
           & ' ' & Entity.Live_Declaration.File.Name.Display_Full_Name);
      else
         return  "<b>"
           & Escape_Text (Get_Full_Name (Entity))
           & "</b>" & ASCII.LF
           & Attributes_To_String (Get_Attributes (Entity)) &
           ' ' & (-Kind_To_String (Get_Kind (Entity))) & ' ' &
           (-"declared at ") &
           Display_Base_Name (Get_Filename
                            (Get_File (Get_Declaration_Of (Entity)))) &
           ':' & Image (Get_Line (Get_Declaration_Of (Entity)));
      end if;
   end Get_Tooltip_Header;

   -----------------------------
   -- Get_Tooltip_Information --
   -----------------------------

   function Get_Tooltip_Information
     (Entity : Entity_Information) return Tooltip_Information
   is
      Kind         : constant E_Kind := Get_Kind (Entity);
      Attributes   : Entity_Attributes;
      Tooltip_Info : Tooltip_Information;

   begin
      Tooltip_Info.Visibility := Visibility_Public;
      Tooltip_Info.Category := Cat_Variable;
      Tooltip_Info.Is_Spec := False;

      if Kind.Kind = Package_Kind then
         Tooltip_Info.Category := Cat_Package;
      elsif Is_Subprogram (Entity) then
         Tooltip_Info.Category := Cat_Function;
      elsif Kind.Is_Type then
         Tooltip_Info.Category := Cat_Type;
      end if;

      Attributes := Get_Attributes (Entity);

      if Attributes (Private_Field) then
         Tooltip_Info.Visibility := Visibility_Private;
      elsif Attributes (Protected_Field) then
         Tooltip_Info.Visibility := Visibility_Protected;
      end if;

      return Tooltip_Info;
   end Get_Tooltip_Information;

   -------------------------------
   -- Get_Tooltip_Guess_Message --
   -------------------------------

   function Get_Tooltip_Guess_Message return String is
   begin
      return "<i>" &
       (-("(Cross-references info not up-to-date, this is a guess)")) & "</i>";
   end Get_Tooltip_Guess_Message;

   ----------------------
   -- Is_Tooltip_Guess --
   ----------------------

   function Is_Tooltip_Guess
     (Status        : Find_Decl_Or_Body_Query_Status;
      Accurate_Xref : Boolean) return Boolean
   is
   begin
      return Status = Overloaded_Entity_Found
        or else (Accurate_Xref and then Status = Fuzzy_Match);
   end Is_Tooltip_Guess;

end Entities.Tooltips_Assistant;
