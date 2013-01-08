------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Gtkada.Style;              use Gtkada.Style;

with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Language.Tree;             use Language.Tree;
with String_Utils;              use String_Utils;
with Tooltips;                  use Tooltips;

package body Entities_Tooltips_Utility is

   function Get_Instance
     (Db         : access General_Xref_Database_Record'Class;
      Entity_Ref : General_Entity_Reference) return String;
   --  Return the text describing from what instance the entity is

   ------------------------
   -- Get_Tooltip_Header --
   ------------------------

   function Get_Tooltip_Header
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : General_Entity) return String
   is
      Decl : General_Entity_Declaration;
      Attrs : Unbounded_String;
   begin
      Decl := Kernel.Databases.Get_Declaration (Entity);

         if Kernel.Databases.Is_Global (Entity) then
            Append (Attrs, "global ");
         elsif Kernel.Databases.Is_Static_Local (Entity) then
            Append (Attrs, "static ");
         end if;

         return  "<b>"
           & Escape_Text (Kernel.Databases.Qualified_Name (Entity))
           & "</b>" & ASCII.LF
           & To_String (Attrs)
           & Kernel.Databases.Get_Display_Kind (Entity)
           & (-" declared at ")
           & Decl.Loc.File.Display_Base_Name & ':'
           & Image (Decl.Loc.Line);
   end Get_Tooltip_Header;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Db         : access General_Xref_Database_Record'Class;
      Entity_Ref : General_Entity_Reference) return String
   is
      use Ada.Strings.Unbounded;
      Result  : Unbounded_String;
      Inst_E  : General_Entity;
      Inst_Of : General_Entity;
      Loc     : General_Location;

   begin
      if Entity_Ref /= No_General_Entity_Reference then
         declare
            Insts : constant Xref.Entity_Array :=
              Db.From_Instances (Entity_Ref);
         begin
            for Inst in Insts'Range loop
               Inst_E := Insts (Inst);
               Inst_Of := Db.Instance_Of (Inst_E);

               if Inst_Of = No_General_Entity then
                  Append (Result,  -"from instance at ");
               else
                  Loc := Db.Get_Declaration (Inst_Of).Loc;
                  Append
                    (Result,
                     (-"from instance of ")
                     & Db.Get_Name (Inst_Of) & ':'
                     & Loc.File.Display_Base_Name & ':'
                     & Image (Loc.Line) & ASCII.LF & "  at ");
               end if;

               Loc := Db.Get_Declaration (Inst_E).Loc;
               Append
                 (Result,
                  Db.Get_Name (Inst_E) & ':'
                  & Loc.File.Display_Base_Name & ':'
                  & Image (Loc.Line) & ASCII.LF & ASCII.LF);
            end loop;
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
      Entity        : General_Entity;
      Ref           : General_Entity_Reference) return String
   is
   begin
      return  Get_Instance (Kernel.Databases, Ref)
        & Kernel.Databases.Documentation
        (Handler => Kernel.Get_Language_Handler,
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
      return Kernel.Databases.Documentation
        (Handler => Kernel.Get_Language_Handler,
         Color_For_Optional_Param =>
           To_Hex (Shade_Or_Lighten (Tooltips.Tooltips_Foreground_Color)),
         Entity  => From_Constructs (Entity));
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
      Entity : General_Entity) return Tooltip_Information
   is
      Tooltip_Info : Tooltip_Information;

   begin
      Tooltip_Info.Visibility := Visibility_Public;
      Tooltip_Info.Category := Cat_Variable;
      Tooltip_Info.Is_Spec := False;

      if Kernel.Databases.Is_Subprogram (Entity) then
         Tooltip_Info.Category := Cat_Function;
      elsif Kernel.Databases.Is_Type (Entity) then
         Tooltip_Info.Category := Cat_Type;
      elsif Kernel.Databases.Is_Container (Entity) then
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
     (Entity : General_Entity) return Boolean
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
