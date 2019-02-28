------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Gtkada.Style;              use Gtkada.Style;

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Language.Tree;
with String_Utils;              use String_Utils;
with Tooltips;

package body Entities_Tooltips_Utility is

   function Get_Instance
     (Entity_Ref : Root_Entity_Reference'Class) return String;
   --  Return the text describing from what instance the entity is

   ------------------------
   -- Get_Tooltip_Header --
   ------------------------

   function Get_Tooltip_Header
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class) return String
   is
      pragma Unreferenced (Kernel);
      Decl : General_Entity_Declaration;
      Attrs : Unbounded_String;
   begin
      Decl := Get_Declaration (Entity);

         if Is_Global (Entity) then
            Append (Attrs, "global ");
         elsif Is_Static_Local (Entity) then
            Append (Attrs, "static ");
         end if;

         return  "<b>"
           & Escape_Text (Qualified_Name (Entity))
           & "</b>" & ASCII.LF
           & To_String (Attrs)
           & Get_Display_Kind (Entity)
           & (-" declared at ")
           & Decl.Loc.File.Display_Base_Name & ':'
           & Image (Decl.Loc.Line);
   end Get_Tooltip_Header;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Entity_Ref : Root_Entity_Reference'Class) return String
   is
      use Ada.Strings.Unbounded;
      Result  : Unbounded_String;
      Loc     : General_Location;

   begin
      if Entity_Ref /= No_Root_Entity_Reference then
         declare
            Insts : Xref.Entity_Array := Entity_Ref.From_Instances;
         begin
            for Inst in Insts'Range loop
               declare
                  Inst_E  : constant Root_Entity'Class := Insts (Inst).all;
                  Inst_Of : constant Root_Entity'Class := Instance_Of (Inst_E);
               begin
                  if Inst_Of = No_Root_Entity then
                     Append (Result,  -"from instance at ");
                  else
                     Loc := Get_Declaration (Inst_Of).Loc;
                     Append
                       (Result,
                        (-"from instance of ")
                        & Get_Name (Inst_Of) & ':'
                        & Loc.File.Display_Base_Name & ':'
                        & Image (Loc.Line) & ASCII.LF & "  at ");
                  end if;

                  Loc    := Get_Declaration (Inst_E).Loc;
                  Append
                    (Result,
                     Get_Name (Inst_E) & ':'
                     & Loc.File.Display_Base_Name & ':'
                     & Image (Loc.Line) & ASCII.LF & ASCII.LF);
               end;
            end loop;

            Free (Insts);
         end;
      end if;

      return To_String (Result);
   end Get_Instance;

   ------------------------
   -- Get_Tooltip_Header --
   ------------------------

   function Get_Tooltip_Header
     (Entity      : Entity_Access) return String
   is
   begin
      return "<b>" & Get (Get_Construct (Entity).Name).all & "</b>";
   end Get_Tooltip_Header;

   -------------------------------
   -- Get_Tooltip_Documentation --
   -------------------------------

   function Get_Tooltip_Documentation
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Root_Entity'Class;
      Ref           : Root_Entity_Reference'Class) return String
   is
   begin
      return Get_Instance (Ref)
        & Documentation
        (Self    => Kernel.Databases,
         Handler => Kernel.Get_Language_Handler,
         Color_For_Optional_Param =>
           To_Hex (Shade_Or_Lighten (Tooltips.Tooltips_Foreground_Color)),
         Entity  => Entity);
   end Get_Tooltip_Documentation;

   -------------------------------
   -- Get_Tooltip_Documentation --
   -------------------------------

   function Get_Tooltip_Documentation
     (Kernel      : access Kernel_Handle_Record'Class;
      Entity      : Entity_Access) return String
   is
   begin
      return Documentation
        (Self    => Kernel.Databases,
         Handler => Kernel.Get_Language_Handler,
         Color_For_Optional_Param =>
           To_Hex (Shade_Or_Lighten (Tooltips.Tooltips_Foreground_Color)),
         Entity  => From_Constructs (Kernel.Databases, Entity));
   end Get_Tooltip_Documentation;

   -----------------------------
   -- Get_Tooltip_Information --
   -----------------------------

   function Get_Tooltip_Information
     (Entity : Entity_Access) return Tooltip_Information
   is
      Tooltip_Info : Tooltip_Information;
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Entity);
   begin
      Tooltip_Info.Visibility := Construct.Visibility;
      Tooltip_Info.Category := Construct.Category;
      Tooltip_Info.Is_Spec := Construct.Is_Declaration;

      return Tooltip_Info;
   end Get_Tooltip_Information;

   -----------------------------
   -- Get_Tooltip_Information --
   -----------------------------

   function Get_Tooltip_Information
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class) return Tooltip_Information
   is
      pragma Unreferenced (Kernel);
      Tooltip_Info : Tooltip_Information;

   begin
      Tooltip_Info.Visibility := Visibility_Public;
      Tooltip_Info.Category := Cat_Variable;
      Tooltip_Info.Is_Spec := False;

      if Is_Subprogram (Entity) then
         Tooltip_Info.Category := Cat_Function;
      elsif Is_Type (Entity) then
         Tooltip_Info.Category := Cat_Type;
      elsif Is_Container (Entity) then
         Tooltip_Info.Category := Cat_Package;
      end if;

      --  When we were using SourceNavigator for the C++ xref, we used to know
      --  about the private/protected/public visibility of entities, but this
      --  is no longer the case with g++-based xref.
      return Tooltip_Info;
   end Get_Tooltip_Information;

   --------------
   -- Is_Guess --
   --------------

   function Is_Guess
     (Entity : Root_Entity'Class) return Boolean
   is
   begin
      return Is_Fuzzy (Entity);
   end Is_Guess;

   ---------------------------
   -- Tooltip_Guess_Message --
   ---------------------------

   function Tooltip_Guess_Message return String
   is
   begin
      return "<i>(Cross-references info not up-to-date, this is a guess)</i>";
   end Tooltip_Guess_Message;

end Entities_Tooltips_Utility;
