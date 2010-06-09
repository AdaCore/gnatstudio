-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2010, AdaCore                 --
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
with Glib.Convert;              use Glib.Convert;
with Gdk.Color;                 use Gdk, Gdk.Color;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.GC;                    use Gdk.GC;
with Gdk.Drawable;              use Gdk.Drawable;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Font;                use Pango.Font;
with Pango.Layout;              use Pango.Layout;

with Doc_Utils;                 use Doc_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Language;                  use Language;
with Language.Icons;            use Language.Icons;
with Language.Tree;             use Language.Tree;
with Language_Handlers;         use Language_Handlers;
with String_Utils;              use String_Utils;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Gdk.Display; use Gdk.Display;
with Gdk.Screen;  use Gdk.Screen;

package body Entities.Tooltips is

   function Get_Instance (Entity_Ref : Entity_Reference) return String;
   --  Return the text describing from what instance the entity is

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
         Visibility := Visibility_Private;
      elsif Attributes (Protected_Field) then
         Visibility := Visibility_Protected;
      end if;

      return Entity_Icons (Is_Spec, Visibility) (Cat);
   end Get_Pixbuf;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information) return String
   is
      Loc        : constant File_Location := Get_Declaration_Of (Entity);
      Decl_File  : constant Virtual_File := Get_Filename (Loc.File);
      Handler    : constant Language_Handler := Get_Language_Handler (Kernel);
      Database   : constant Construct_Database_Access :=
                     Get_Construct_Database (Kernel);
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
         --  This probably means that this is not a Ada file. Try to get the
         --  documentation from somewhere else than the construct database.
         return Get_Documentation (Handler, Entity);
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
         --  Try to get the documentation from somewhere else than the
         --  construct database.
         return Get_Documentation (Handler, Entity);
      end if;

      return Language.Tree.Database.Get_Documentation
        (Lang     => Tree_Lang,
         Entity   => To_Entity_Access (Data_File, Node));
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
                 & Get (Get_Name (Inst_Of)).all & ':'
                 & Display_Base_Name (Get_Filename
                     (Get_File (Get_Declaration_Of (Inst_Of)))) & ':'
                 & Image (Get_Line (Get_Declaration_Of (Inst_Of)))
                 & ASCII.LF & "  at ";
            end if;

            Result := Result
              & Get (Get_Name (Inst_E)).all
              & ':'
              &  Display_Base_Name (Get_Filename
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
      if Get_Kind (Entity).Kind = Include_File then
         return  "<b>" & Escape_Text (Get (Entity.Name).all)
           & "</b>" & ASCII.LF
           & (-Kind_To_String (Get_Kind (Entity))
           & ' ' & Entity.Declaration.File.Name.Display_Full_Name);
      else
         return  "<b>" & Escape_Text (Get_Full_Name (Entity, "."))
           & "</b>" & ASCII.LF
           & Attributes_To_String (Get_Attributes (Entity)) &
           ' ' & (-Kind_To_String (Get_Kind (Entity))) & ' ' &
           (-"declared at ") &
           Display_Base_Name (Get_Filename
                            (Get_File (Get_Declaration_Of (Entity)))) &
           ':' & Image (Get_Line (Get_Declaration_Of (Entity)));
      end if;
   end Get_Header;

   ------------------
   -- Draw_Tooltip --
   ------------------

   function Draw_Tooltip
     (Kernel : access Kernel_Handle_Record'Class;
      Header : String;
      Doc    : String;
      Pixbuf : Gdk_Pixbuf;
      Guess  : Boolean := False) return Gdk.Pixmap.Gdk_Pixmap;
   --  Helper function, factorizing the tooltip widget creation

   function Draw_Tooltip
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information;
      Ref           : Entity_Reference;
      Status        : Find_Decl_Or_Body_Query_Status;
      Accurate_Xref : Boolean) return Gdk.Gdk_Pixmap
   is
      Doc : constant String := Get_Instance (Ref)
        & Get_Documentation (Kernel, Entity);
   begin
      return Draw_Tooltip
        (Kernel => Kernel,
         Guess  => Status = Overloaded_Entity_Found
           or else (Accurate_Xref and then Status = Fuzzy_Match),
         Header => Get_Header (Entity),
         Pixbuf => Get_Pixbuf (Entity),
         Doc    => Doc);
   end Draw_Tooltip;

   function Draw_Tooltip
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Access;
      Guess  : Boolean := False) return Gdk.Pixmap.Gdk_Pixmap
   is
      pragma Unreferenced (Guess);

      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Entity);
   begin
      return Draw_Tooltip
        (Kernel => Kernel,
         Guess  => False,
         Header => "<b>" & Get (Get_Construct (Entity).Name).all & "</b>",
         Doc    => Get_Documentation
           (Get_Tree_Language (Get_File (Entity)),
            Entity),
         Pixbuf => Entity_Icons
           (Construct.Is_Declaration, Construct.Visibility)
           (Construct.Category));
   end Draw_Tooltip;

   function Draw_Tooltip
     (Kernel : access Kernel_Handle_Record'Class;
      Header : String;
      Doc    : String;
      Pixbuf : Gdk_Pixbuf;
      Guess  : Boolean := False) return Gdk.Pixmap.Gdk_Pixmap
   is
      Widget : constant Gtk_Widget := Gtk_Widget (Get_Main_Window (Kernel));
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;

      Font   : constant Pango_Font_Description := Default_Font.Get_Pref_Font;
      Fixed  : constant Pango_Font_Description := View_Fixed_Font.Get_Pref;

      Header_Layout, Doc_Layout : Pango_Layout;

      Width, Height, W1, H1, W2, H2 : Gint := 0;
      GC     : Gdk.Gdk_GC;
      Max_Height, Max_Width : Gint;

      H_Pad : constant := 4;
      V_Pad : constant := 3;
   begin
      Header_Layout := Create_Pango_Layout (Widget, "");

      if Guess then
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
      Height := Height + V_Pad * 2 + H1;

      if Doc /= "" then
         Doc_Layout := Create_Pango_Layout (Widget, "");
         Set_Markup (Doc_Layout, Doc);
         Set_Font_Description (Doc_Layout, Fixed);
         Get_Pixel_Size (Doc_Layout, W2, H2);
         Height := Height + V_Pad * 2 + 1 + H2;
      end if;

      Width  := Gint'Max (W1 + Get_Width (Pixbuf) + H_Pad * 2, W2 + H_Pad * 2);

      Gdk_New (GC, Get_Window (Widget));
      Set_Foreground (GC, Tooltip_Color.Get_Pref);

      Max_Height := Get_Height (Get_Default_Screen (Gdk.Display.Get_Default));
      Height := Gint'Min (Height, Max_Height);

      Max_Width := Get_Width (Get_Default_Screen (Gdk.Display.Get_Default));
      Width := Gint'Min (Width, Max_Width);

      Gdk.Pixmap.Gdk_New (Pixmap, Get_Window (Widget), Width, Height);
      Draw_Rectangle (Pixmap, GC, True, 0, 0, Width - 1, Height - 1);

      Set_Foreground (GC, Black (Get_Default_Colormap));
      Draw_Rectangle (Pixmap, GC, False, 0, 0, Width - 1, Height - 1);

      Render_To_Drawable (Pixbuf, Pixmap, GC, 0, 0, V_Pad, H_Pad,
                          Get_Width (Pixbuf), Get_Height (Pixbuf));

      Draw_Layout
        (Pixmap, GC, V_Pad + Get_Width (Pixbuf), H_Pad, Header_Layout);
      Unref (Header_Layout);

      if Doc_Layout /= null then
         Draw_Line (Pixmap, GC, 0, H1 + V_Pad * 2, Width - 1, H1 + V_Pad * 2);
         Draw_Layout (Pixmap, GC, H_Pad, H1 + 1 + V_Pad * 3, Doc_Layout);
         Unref (Doc_Layout);
      end if;

      Unref (GC);

      return Pixmap;
   end Draw_Tooltip;

end Entities.Tooltips;
