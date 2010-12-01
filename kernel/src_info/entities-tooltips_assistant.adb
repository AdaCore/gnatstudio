-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with GPS.Intl; use GPS.Intl;
with Language;                  use Language;
with Language.Tree;             use Language.Tree;
with Language_Handlers;         use Language_Handlers;
with String_Utils;              use String_Utils;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Entities.Queries;          use Entities.Queries;
with Traces; use Traces;

package body Entities.ToolTips_Assistant is

   Me : constant Debug_Handle := Create ("ToolTip_Assistant");

   ------------------------
   -- Get_ToolTip_Header --
   ------------------------

   function Get_ToolTip_Header (Entity : Entity_Information) return String is
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
   end Get_ToolTip_Header;

   -------------------------------
   -- Get_ToolTip_Documentation --
   -------------------------------

   function Get_ToolTip_Documentation
     (Handler    : Language_Handler;
      Database   : Construct_Database_Access;
      Entity : Entity_Information) return String
   is
      Loc        : constant File_Location := Get_Declaration_Of (Entity);
      Decl_File  : constant Virtual_File := Get_Filename (Loc.File);
      Tree_Lang  : constant Tree_Language_Access :=
                     Get_Tree_Language_From_File (Handler, Decl_File, False);

      Data_File  : Structured_File_Access;
      Node       : Construct_Tree_Iterator;
      Tree       : Construct_Tree;

   begin

      Data_File := Language.Tree.Database.Get_Or_Create
        (Db   => Database,
         File => Decl_File);

      if Data_File = null then
         Trace (Me, "Get_ToolTip_Documentation, Data_File = null");
         --  This probably means that this is not a Ada file. Caller will try
         --  to get the documentation from somewhere else than the construct
         --  database.
         return "";
      end if;

      Tree := Get_Tree (Data_File);

      Node := Get_Iterator_At
        (Tree        => Tree,
         Location    =>
           (Absolute_Offset => False,
            Line            => Loc.Line,
            Line_Offset     =>
              To_Line_String_Index (Data_File, Loc.Line, Loc.Column)),
         From_Type   => Start_Name);

      if Node = Null_Construct_Tree_Iterator then
         Trace (Me, "Get_ToolTip_Documentation, Null_Construct_Tree_Iterator");
         --  caller will try to get the documentation from somewhere else
         --  than the construct database.
         return "";
      end if;

      return Language.Tree.Database.Get_Documentation
        (Lang     => Tree_Lang,
         Entity   => To_Entity_Access (Data_File, Node));
   end Get_ToolTip_Documentation;

   -----------------------------
   -- Get_ToolTip_Information --
   -----------------------------

   function Get_ToolTip_Information
      (Entity : Entity_Information) return ToolTip_Information is
      Kind : constant E_Kind := Get_Kind (Entity);
      Attributes : Entity_Attributes;
      ToolTip_Info : ToolTip_Information;
   begin

      ToolTip_Info.Visibility := Visibility_Public;
      ToolTip_Info.Category := Cat_Variable;
      ToolTip_Info.Is_Spec := false;

      if Kind.Kind = Package_Kind then
         ToolTip_Info.Category := Cat_Package;
      elsif Is_Subprogram (Entity) then
         ToolTip_Info.Category := Cat_Function;
      elsif Kind.Is_Type then
         ToolTip_Info.Category := Cat_Type;
      end if;

      Attributes := Get_Attributes (Entity);

      if Attributes (Private_Field) then
         ToolTip_Info.Visibility := Visibility_Private;
      elsif Attributes (Protected_Field) then
         ToolTip_Info.Visibility := Visibility_Protected;
      end if;

      return ToolTip_Info;

   end Get_ToolTip_Information;

end Entities.ToolTips_Assistant;
