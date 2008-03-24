-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

package body Docgen2_Backend.HTML is

   ------------------
   -- Get_Template --
   ------------------

   function Get_Template (Backend    : access HTML_Backend_Record;
                          System_Dir : String;
                          Kind       : Template_Kind) return String
   is
      pragma Unreferenced (Backend);
   begin
      case Kind is
         when Tmpl_Spec =>
            return System_Dir & "share/gps/docgen2/html.tmpl";
         when Tmpl_Class_Tree =>
            return System_Dir & "share/gps/docgen2/tree.tmpl";
         when Tmpl_Class_Tree_Elem =>
            return System_Dir & "share/gps/docgen2/tree_elem.tmpl";
         when Tmpl_Index =>
            return System_Dir & "share/gps/docgen2/entities.tmpl";
         when Tmpl_TOC =>
            return System_Dir & "share/gps/docgen2/index.tmpl";
         when Tmpl_Src =>
            return System_Dir & "share/gps/docgen2/src.tmpl";
         when Tmpl_Src_Index =>
            return System_Dir & "share/gps/docgen2/src_index.tmpl";
      end case;
   end Get_Template;

   ---------------------
   -- Get_Support_Dir --
   ---------------------

   function Get_Support_Dir (Backend    : access HTML_Backend_Record;
                             System_Dir : String) return String
   is
      pragma Unreferenced (Backend);
   begin
      return System_Dir & "share/gps/docgen2/support/";
   end Get_Support_Dir;

   -------------------------
   -- To_Destination_Name --
   -------------------------

   function To_Destination_Name
     (Backend  : access HTML_Backend_Record;
      Basename : String)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      return Basename & ".html";
   end To_Destination_Name;

   -------------------------
   -- To_Destination_Name --
   -------------------------

   function To_Destination_Name
     (Backend  : access HTML_Backend_Record;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      if Pkg_Nb = 1 then
         return Src_File & ".html";
      else
         declare
            Str : constant String := Natural'Image (Pkg_Nb);
         begin
            return Src_File & "-" & Str (Str'First + 1 .. Str'Last) & ".html";
         end;
      end if;
   end To_Destination_Name;

   -------------
   -- To_Href --
   -------------

   function To_Href
     (Backend  : access HTML_Backend_Record;
      Location : String;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String
   is
   begin
      return Backend.To_Destination_Name (Src_File, Pkg_Nb) & "#" & Location;
   end To_Href;

   -------------
   -- Gen_Ref --
   -------------

   function Gen_Ref
     (Backend : access HTML_Backend_Record;
      Name    : String) return String
   is
      pragma Unreferenced (Backend);
   begin
      return "<a name=""" & Name & """></a>";
   end Gen_Ref;

   --------------
   -- Gen_Href --
   --------------

   function Gen_Href
     (Backend                : access HTML_Backend_Record;
      Name, Href, Title : String)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      return "<a href=""" & Href & """ title=""" & Title &
        """>" & Name & "</a>";
   end Gen_Href;

   -------------
   -- Gen_Tag --
   -------------

   function Gen_Tag
     (Backend  : access HTML_Backend_Record;
      Tag      : Language_Entity;
      Value    : String;
      Emphasis : Boolean := False)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      if Tag = Identifier_Text then
         if Emphasis then
            return "<span class=""name""><b>" & Value & "</b></span>";
         else
            return "<span class=""name"">" & Value & "</span>";
         end if;
      elsif Tag = Keyword_Text then
         return "<span class=""keyword"">" & Value & "</span>";
      elsif Tag = Comment_Text then
         return "<span class=""comment"">" & Value & "</span>";
      elsif Tag = Character_Text
        or else Tag = String_Text
      then
         return "<span class=""string"">" & Value & "</span>";
      else
         return Value;
      end if;
   end Gen_Tag;

end Docgen2_Backend.HTML;
