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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with String_Utils;          use String_Utils;

package body Docgen2_Backend.HTML is

   ------------------
   -- Get_Template --
   ------------------

   overriding function Get_Template
     (Backend    : access HTML_Backend_Record;
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

   overriding function Get_Support_Dir
     (Backend    : access HTML_Backend_Record;
      System_Dir : String) return String
   is
      pragma Unreferenced (Backend);
   begin
      return System_Dir & "share/gps/docgen2/support/";
   end Get_Support_Dir;

   -------------------------
   -- To_Destination_Name --
   -------------------------

   overriding function To_Destination_Name
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

   overriding function To_Destination_Name
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

   overriding function To_Href
     (Backend  : access HTML_Backend_Record;
      Location : String;
      Src_File : String;
      Pkg_Nb   : Natural)
      return String
   is
   begin
      return Backend.To_Destination_Name (Src_File, Pkg_Nb) & "#" & Location;
   end To_Href;

   -------------------
   -- Gen_Paragraph --
   -------------------

   overriding function Gen_Paragraph
     (Backend : access HTML_Backend_Record;
      Msg     : String) return String
   is
      pragma Unreferenced (Backend);
   begin
      return "<p>" & Msg & "</p>";
   end Gen_Paragraph;

   -------------
   -- Gen_Ref --
   -------------

   overriding function Gen_Ref
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

   overriding function Gen_Href
     (Backend                : access HTML_Backend_Record;
      Name, Href, Title : String)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      return "<a href=""" & Href & """ title=""" & Title &
        """>" & Name & "</a>";
   end Gen_Href;

   ------------
   -- Filter --
   ------------

   overriding function Filter
     (Backend  : access HTML_Backend_Record;
      S        : String) return String
   is
      pragma Unreferenced (Backend);

      function Replace
        (S      : String;
         C      : Character;
         Entity : String) return String;
      --  Replace all occurences of C by Entity

      -------------
      -- Replace --
      -------------

      function Replace
        (S      : String;
         C      : Character;
         Entity : String) return String
      is
         Idx  : Natural;
         Nxt  : Natural;
         Res  : Unbounded_String;

      begin
         Idx := S'First;

         loop
            Nxt := Idx;
            Skip_To_Char (S, Nxt, C);

            if Nxt > S'Last then
               Ada.Strings.Unbounded.Append (Res, S (Idx .. S'Last));
               exit;
            end if;

            Ada.Strings.Unbounded.Append (Res, S (Idx .. Nxt - 1));
            Ada.Strings.Unbounded.Append (Res, Entity);
            Idx := Nxt + 1;
         end loop;

         return To_String (Res);
      end Replace;

   begin
      return Replace
        (Replace
           (Replace
              (S, '&', "&amp;"),
            '<', "&lt;"),
         '>', "&gt;");
   end Filter;

   -------------
   -- Gen_Tag --
   -------------

   overriding function Gen_Tag
     (Backend  : access HTML_Backend_Record;
      Tag      : Language_Entity;
      Value    : String;
      Emphasis : Boolean := False)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      case Tag is
         when Identifier_Text =>
            if Emphasis then
               return "<span class=""name""><b>" & Value & "</b></span>";
            else
               return "<span class=""name"">" & Value & "</span>";
            end if;
         when Keyword_Text =>
            return "<span class=""keyword"">" & Value & "</span>";
         when Comment_Text | Annotated_Comment_Text =>
            return "<span class=""comment"">" & Value & "</span>";
         when Character_Text | String_Text =>
            return "<span class=""string"">" & Value & "</span>";
         when Normal_Text | Partial_Identifier_Text | Operator_Text =>
            return Value;
      end case;
   end Gen_Tag;

   ------------------
   -- Gen_User_Tag --
   ------------------

   overriding function Gen_User_Tag
     (Backend    : access HTML_Backend_Record;
      User_Tag   : String;
      Attributes : String;
      Value      : String) return String
   is
      pragma Unreferenced (Backend);
   begin
      return "<div class=""" & User_Tag & """ " & Attributes & ">" &
        Value & "</div>";
   end Gen_User_Tag;

end Docgen2_Backend.HTML;
