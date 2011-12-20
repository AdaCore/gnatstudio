------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with String_Utils;          use String_Utils;
with XML_Utils;             use XML_Utils;

package body Docgen2_Backend.HTML is

   ------------------
   -- Get_Template --
   ------------------

   overriding function Get_Template
     (Backend    : access HTML_Backend_Record;
      System_Dir : Virtual_File;
      Kind       : Template_Kind) return Virtual_File
   is
      pragma Unreferenced (Backend);
   begin
      case Kind is
         when Tmpl_Spec =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/html.tmpl");
         when Tmpl_Class_Tree =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/tree.tmpl");
         when Tmpl_Class_Tree_Elem =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/tree_elem.tmpl");
         when Tmpl_Index =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/entities.tmpl");
         when Tmpl_User_Defined_File =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/userdef.tmpl");
         when Tmpl_Src =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/src.tmpl");
         when Tmpl_Src_Index =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen2/src_index.tmpl");
      end case;
   end Get_Template;

   ---------------------
   -- Get_Support_Dir --
   ---------------------

   overriding function Get_Support_Dir
     (Backend    : access HTML_Backend_Record;
      System_Dir : Virtual_File) return Virtual_File
   is
      pragma Unreferenced (Backend);
   begin
      return Create_From_Dir (System_Dir, "share/gps/docgen2/support/");
   end Get_Support_Dir;

   -------------------------
   -- To_Destination_Name --
   -------------------------

   overriding function To_Destination_Name
     (Backend  : access HTML_Backend_Record;
      Basename : Filesystem_String)
      return Filesystem_String
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
      Src_File : Filesystem_String;
      Pkg_Nb   : Natural)
      return Filesystem_String
   is
      pragma Unreferenced (Backend);
   begin
      if Pkg_Nb = 1 then
         return Src_File & ".html";
      else
         declare
            Str : constant String := Natural'Image (Pkg_Nb);
         begin
            return Src_File & "-" &
            (+Str (Str'First + 1 .. Str'Last)) & ".html";
         end;
      end if;
   end To_Destination_Name;

   ----------------
   -- Line_Image --
   ----------------

   overriding function Line_Image
     (Backend  : access HTML_Backend_Record;
      Line     : Integer)
      return String
   is
      pragma Unreferenced (Backend);
   begin
      return "l" & String_Utils.Image (Line);
   end Line_Image;

   -------------
   -- To_Href --
   -------------

   overriding function To_Href
     (Backend  : access HTML_Backend_Record;
      Location : String;
      Src_File : Filesystem_String;
      Pkg_Nb   : Natural)
      return String
   is
   begin
      return +Backend.To_Destination_Name (Src_File, Pkg_Nb) & "#" & Location;
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
     (Backend           : access HTML_Backend_Record;
      Name, Href, Title : String)
      return String
   is
      pragma Unreferenced (Backend);

   begin
      return "<a href=""" & Href & """ title=""" & Title &
        """>" & Name & "</a>";
   end Gen_Href;

   ----------------------
   -- Multi_Href_Start --
   ----------------------

   overriding function Multi_Href_Start
     (Backend : access HTML_Backend_Record;
      Name    : String) return String
   is
      pragma Unreferenced (Backend);

   begin
      return "<span class=""droplink"">" & Name & "<ul>";
   end Multi_Href_Start;

   ---------------------
   -- Multi_Href_Item --
   ---------------------

   overriding function Multi_Href_Item
     (Backend : access HTML_Backend_Record;
      Name, Href : String) return String
   is
   begin
      return "<li>" & Backend.Gen_Href (Name, Href, Name) & "</li>";
   end Multi_Href_Item;

   --------------------
   -- Multi_Href_End --
   --------------------

   overriding function Multi_Href_End
     (Backend : access HTML_Backend_Record) return String
   is
      pragma Unreferenced (Backend);
   begin
      return "</ul></span>";
   end Multi_Href_End;

   ------------
   -- Filter --
   ------------

   overriding function Filter
     (Backend  : access HTML_Backend_Record;
      S        : String) return String
   is
      pragma Unreferenced (Backend);
   begin
      --  We want to keep LF characters for further filtering (add <p></p>)
      return XML_Utils.Protect (S, True);
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
         when Identifier_Text | Block_Text | Type_Text =>
            if Emphasis then
               return "<span class=""name_emphasis"">" & Value & "</span>";
            else
               return "<span class=""name"">" & Value & "</span>";
            end if;
         when Keyword_Text =>
            return "<span class=""keyword"">" & Value & "</span>";
         when Comment_Text | Annotated_Comment_Text
           | Annotated_Keyword_Text =>
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

   -----------------------
   -- Begin_Handle_Code --
   -----------------------

   overriding procedure Begin_Handle_Code
     (Backend : access HTML_Backend_Record;
      Buffer  : in out Unbounded_String;
      Current : out Unbounded_String)
   is
      pragma Unreferenced (Backend);
   begin
      Append (Buffer, "<ol>");
      Current := Null_Unbounded_String;
   end Begin_Handle_Code;

   ---------------------
   -- End_Handle_Code --
   ---------------------

   overriding procedure End_Handle_Code
     (Backend  : access HTML_Backend_Record;
      Buffer   : in out Unbounded_String;
      Current  : in out Unbounded_String;
      Line     : in out Natural)
   is
   begin
      if Current /= Null_Unbounded_String then
         Backend.Handle_Code
           ("" & ASCII.LF, Buffer, Current, Line, null);
      end if;

      Append (Buffer, "</ol>");
   end End_Handle_Code;

   -----------------
   -- Handle_Code --
   -----------------

   overriding procedure Handle_Code
     (Backend : access HTML_Backend_Record;
      Text    :        String;
      Buffer  : in out Unbounded_String;
      Current : in out Unbounded_String;
      Line    : in out Natural;
      Cb      : access function (S : String) return String)
   is
      Idx  : Natural;
      Prev : Natural := Text'First;

   begin
      loop
         Idx := Index (Text, Set => To_Set (ASCII.LF), From => Prev);

         if Idx >= Text'First then
            if Cb /= null then
               Append (Current, Cb (Text (Prev .. Idx - 1)));
            else
               Append (Current, Text (Prev .. Idx - 1));
            end if;
         else
            if Cb /= null then
               Append (Current, Cb (Text (Prev .. Text'Last)));
            else
               Append (Current, Text (Prev .. Text'Last));
            end if;

            exit;
         end if;

         Append
           (Buffer,
            "<li id=""" & Backend.Line_Image (Line) & """><pre>" &
            To_String (Current) & " </pre></li>" & ASCII.LF);

         Line := Line + 1;
         Current := Null_Unbounded_String;
         Prev := Idx + 1;

         exit when Prev > Text'Last;
      end loop;
   end Handle_Code;

end Docgen2_Backend.HTML;
