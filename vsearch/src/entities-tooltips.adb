-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2006                        --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Glib;                      use Glib;
with Gdk.Color;                 use Gdk, Gdk.Color;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.GC;                    use Gdk.GC;
with Gdk.Drawable;              use Gdk.Drawable;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;
with Pango.Tabs;                use Pango.Tabs;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with VFS;                       use VFS;
with String_Utils;              use String_Utils;

with Language;                  use Language;
with Language.Icons;            use Language.Icons;

with Doc_Utils;                 use Doc_Utils;

package body Entities.Tooltips is

   function Get_Instance (Entity_Ref : Entity_Reference) return String;
   --  Return the text describing from what instance the entity is

   function Get_Documentation
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String;
   --  Return the documentation for the entity (prefixed by a LF char if not
   --  null)

   function Get_Parameters
     (Entity : Entity_Information;
      Widget : access Gtk_Widget_Record'Class;
      Font   : Pango_Font_Description) return Pango_Layout;
   --  Return the list of parameters for the entity

   function Get_Header (Entity : Entity_Information) return String;
   --  Return a string in pango markup format to represent the header of a
   --  tooltip.

   function Get_Pixbuf (Entity : Entity_Information) return Gdk_Pixbuf;
   --  Return the image associated to an entity.

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf (Entity : Entity_Information) return Gdk_Pixbuf is
      Cat        : Language_Category := Cat_Variable;
      Is_Spec    : constant Boolean := False;
      Visibility : Construct_Visibility := Visibility_Public;
      Attributes : Entity_Attributes;

      Kind : constant E_Kind := Get_Kind (Entity);
   begin
      if Kind.Kind = Package_Kind then
         Cat := Cat_Package;
      elsif Is_Subprogram (Entity) then
         Cat := Cat_Function;
      elsif Kind.Is_Type then
         Cat := Cat_Type;
      end if;

      Attributes := Get_Attributes (Entity);

      if Attributes (Private_Field) then
         Visibility := Visibility_Protected;
      elsif Attributes (Protected_Field) then
         Visibility := Visibility_Protected;
      end if;

      return Entity_Icons (Is_Spec, Visibility) (Cat);
   end Get_Pixbuf;

   --------------------
   -- Get_Parameters --
   --------------------

   function Get_Parameters
     (Entity : Entity_Information;
      Widget : access Gtk_Widget_Record'Class;
      Font   : Pango_Font_Description) return Pango_Layout
   is
      use Ada.Strings.Unbounded;
      --  ??? Display should depend on the language
      Iter                          : Subprogram_Iterator;
      Param                         : Entity_Information;
      Param_Type                    : Entity_Information;
      Result                        : Unbounded_String;
      Longuest_Param, Longuest_Type : Gint := 0;
      Layout                        : Pango_Layout;
      Tabs                          : Pango_Tab_Array;
      Char_Width, Char_Height       : Gint;
   begin
      if Is_Subprogram (Entity) then
         Layout := Create_Pango_Layout (Widget, "");
         Set_Font_Description (Layout, Font);

         Iter := Get_Subprogram_Parameters (Subprogram => Entity);

         Get (Iter, Param);

         if Param = null then
            Result := To_Unbounded_String ("<u><i>(No parameter)</i></u>");

         else
            Result := To_Unbounded_String ("<u>Parameters:</u>");

            while Param /= null loop
               Set_Markup (Layout, "   <b>" & Get_Name (Param).all & "</b>");
               Get_Pixel_Size (Layout, Char_Width, Char_Height);
               Longuest_Param := Gint'Max (Longuest_Param, Char_Width);

               Set_Text (Layout, ": " & Image (Get_Type (Iter)));
               Get_Pixel_Size (Layout, Char_Width, Char_Height);
               Longuest_Type := Gint'Max (Longuest_Type, Char_Width);

               Next (Iter);
               Get (Iter, Param);
            end loop;
         end if;

         Pango_New (Tabs, Initial_Size => 2, Positions_In_Pixels => True);
         Set_Tab (Tabs, 0, Location => Longuest_Param + 2);
         Set_Tab (Tabs, 1, Location => Longuest_Param + 2 + Longuest_Type);
         Set_Tabs (Layout, Tabs);

         Iter := Get_Subprogram_Parameters (Subprogram => Entity);
         loop
            Get (Iter, Param);
            exit when Param = null;

            Result := Result & ASCII.LF
              & "   <b>" & Get_Name (Param).all & "</b>" & ASCII.HT
              & ": " & Image (Get_Type (Iter)) & ASCII.HT & " ";

            if Get_Type (Iter) = Access_Parameter then
               Param_Type := Pointed_Type (Param);
            else
               Param_Type := Get_Type_Of (Param);
            end if;

            if Param_Type /= null then
               Result := Result & Get_Name (Param_Type).all;
            end if;

            Next (Iter);
         end loop;

         Param := Returned_Type (Entity);
         if Param /= null then
            Result := Result & ASCII.LF & "   return <b>"
              & Get_Name (Param).all & "</b>";
         end if;

         Set_Markup (Layout, To_String (Result));

         Free (Tabs);
         return Layout;
      else
         return null;
      end if;
   end Get_Parameters;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String is
   begin
      return Get_Documentation (Get_Language_Handler (Kernel), Entity);
   end Get_Documentation;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance (Entity_Ref : Entity_Reference) return String is
      use Ada.Strings.Unbounded;
      Result  : Unbounded_String;
      Inst    : Entity_Instantiation;
      Inst_E  : Entity_Information;
      Inst_Of : Entity_Information;

   begin
      if Entity_Ref /= No_Entity_Reference then
         Inst := From_Instantiation_At (Entity_Ref);

         while Inst /= No_Instantiation loop
            Inst_E := Get_Entity (Inst);
            Inst_Of := Is_Instantiation_Of (Inst_E);

            if Inst_Of = null then
               Result := Result
                 & (-"from instance at ");
            else
               Result := Result
                 & (-"from instance of ")
                 & Get_Name (Inst_Of).all & ':'
                 & Base_Name (Get_Filename
                     (Get_File (Get_Declaration_Of (Inst_Of)))) & ':'
                 & Image (Get_Line (Get_Declaration_Of (Inst_Of)))
                 & ASCII.LF & "  at ";
            end if;

            Result := Result
              & Get_Name (Inst_E).all
              & ':'
              &  Base_Name (Get_Filename
                              (Get_File (Get_Declaration_Of (Inst_E)))) & ':'
              & Image (Get_Line (Get_Declaration_Of (Inst_E)))
              & ASCII.LF & ASCII.LF;
            Inst := Generic_Parent (Inst);
         end loop;
      end if;

      return To_String (Result);
   end Get_Instance;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header (Entity : Entity_Information) return String is
   begin
      return  "<b>" & Get_Full_Name (Entity, ".") & "</b>" & ASCII.LF
        & Attributes_To_String (Get_Attributes (Entity)) &
        ' ' & (-Kind_To_String (Get_Kind (Entity))) & ' ' &
      (-"declared at ") &
      Base_Name (Get_Filename
                 (Get_File (Get_Declaration_Of (Entity)))) &
        ':' & Image (Get_Line (Get_Declaration_Of (Entity)));
   end Get_Header;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      Ref    : Entity_Reference;
      Status : Find_Decl_Or_Body_Query_Status) return Gdk.Pixmap.Gdk_Pixmap
   is
      Widget : constant Gtk_Widget := Gtk_Widget (Get_Main_Window (Kernel));
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Header : constant String := Get_Header (Entity);
      Doc    : constant String := Get_Instance (Ref)
        & Get_Documentation (Kernel, Entity);

      Font   : constant Pango_Font_Description := Get_Pref (Default_Font);
      Fixed  : constant Pango_Font_Description := Get_Pref (View_Fixed_Font);

      Header_Layout, Doc_Layout, Params_Layout : Pango_Layout;

      Width, Height, W1, H1, W2, H2, W3, H3 : Gint := 0;
      GC     : Gdk.Gdk_GC;
      Pixbuf : Gdk_Pixbuf;

      H_Pad : constant := 3;
      V_Pad : constant := 3;

   begin
      Header_Layout := Create_Pango_Layout (Widget, "");

      Pixbuf := Get_Pixbuf (Entity);

      if Status = Overloaded_Entity_Found
        or else Status = Fuzzy_Match
      then
         Set_Markup (Header_Layout,
                     -("<span foreground =""#555555""><i>" &
             "(Cross-references info not up-to-date, this is a guess)"
             & "</i></span>")
           & ASCII.LF & Header);
      else
         Set_Markup (Header_Layout, Header);
      end if;

      Set_Font_Description (Header_Layout, Font);
      Get_Pixel_Size (Header_Layout, W1, H1);

      if Doc /= "" then
         Doc_Layout := Create_Pango_Layout (Widget, Doc);
         Set_Font_Description (Doc_Layout, Fixed);
         Get_Pixel_Size (Doc_Layout, W2, H2);
      end if;

      Params_Layout := Get_Parameters (Entity, Widget, Fixed);
      if Params_Layout /= null then
         Get_Pixel_Size (Params_Layout, W3, H3);
         H3 := H3 + H_Pad * 2 + 1;
      end if;

      Width  := Gint'Max (W1 + Get_Width (Pixbuf) + H_Pad * 2, W2 + H_Pad * 2);
      Height := V_Pad * 3 + H1 + H2;
      Width  := H_Pad * 2 + Gint'Max (Width, W3);
      Height := Height + H3;

      Gdk_New (GC, Get_Window (Widget));
      Set_Foreground (GC, Get_Pref (Tooltip_Color));

      Gdk.Pixmap.Gdk_New (Pixmap, Get_Window (Widget), Width, Height);
      Draw_Rectangle (Pixmap, GC, True, 0, 0, Width - 1, Height - 1);

      Set_Foreground (GC, Black (Get_Default_Colormap));
      Draw_Rectangle (Pixmap, GC, False, 0, 0, Width - 1, Height - 1);

      Render_To_Drawable (Pixbuf, Pixmap, GC, 0, 0, V_Pad, H_Pad,
                          Get_Width (Pixbuf), Get_Height (Pixbuf));

      Draw_Layout
        (Pixmap, GC, V_Pad + Get_Width (Pixbuf), H_Pad, Header_Layout);
      Unref (Header_Layout);

      if Doc_Layout /= null or else Params_Layout /= null then
         Draw_Line (Pixmap, GC, 0, H1 + V_Pad * 2, Width - 1, H1 + V_Pad * 2);
      end if;

      if Doc_Layout /= null then
         Draw_Layout (Pixmap, GC, H_Pad, H1 + 1 + V_Pad * 3, Doc_Layout);
         Unref (Doc_Layout);
      end if;

      if Params_Layout /= null then
         Draw_Layout
           (Pixmap, GC, H_Pad, H1 + H2 + 1 + V_Pad * 4, Params_Layout);
         Unref (Params_Layout);
      end if;

      Unref (GC);

      return Pixmap;
   end Draw_Tooltip;

end Entities.Tooltips;
