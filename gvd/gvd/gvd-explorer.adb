-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.Color;    use Gdk.Color;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gtk.Handlers; use Gtk.Handlers;
with Gtkada.Types; use Gtkada.Types;
with Odd.Pixmaps;  use Odd.Pixmaps;
with GNAT.Regpat;  use GNAT.Regpat;
with Language;     use Language;

package body Odd.Explorer is

   type Explorer_Data is record
      Handler : Explorer_Handler;
      Widget  : Gtk_Widget;
   end record;

   type Internal_Category is record
      Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
      Mask    : Gdk.Bitmap.Gdk_Bitmap;
      Node    : Gtk_Ctree_Node;
   end record;
   type Internal_Categories is array (Category_Index range <>)
     of Internal_Category;

   package Tree_Cb is new
     Gtk.Handlers.User_Callback (Gtk_Ctree_Record, Explorer_Data);

   package Row_Data_Explorer is new Gtk.Ctree.Row_Data (Position_Type);

   procedure First_Handler
     (Ctree : access Gtk_Ctree_Record'Class;
      Data  : Explorer_Data);
   --  Callback handler for Ctree signals.

   function Get_Pos (Buffer : String; Index : Positive) return Position_Type;
   --  Return the line and column corresponding to Index in Buffer.

   -------------
   -- Get_Pos --
   -------------

   function Get_Pos (Buffer : String; Index : Positive) return Position_Type is
      Result     : Position_Type;
      Line_Start : Positive := 1;
   begin
      Result.Line := 0;
      Result.Index := Index;

      for J in Buffer'First .. Index loop
         if Buffer (J) = ASCII.LF then
            Result.Line := Result.Line + 1;
            Line_Start := J;
         end if;
      end loop;

      Result.Column := Index - Line_Start;
      return Result;
   end Get_Pos;

   -------------------
   -- First_Handler --
   -------------------

   procedure First_Handler
     (Ctree : access Gtk_Ctree_Record'Class;
      Data  : Explorer_Data)
   is
      Node : Gtk_Ctree_Node := Node_List.Get_Data
        (Node_List.First (Get_Selection (Ctree)));

   begin
      if Data.Handler = null
        or else not Row_Get_Is_Leaf (Node_Get_Row (Node))
      then
         return;
      end if;

      Data.Handler
        (Data.Widget, Row_Data_Explorer.Node_Get_Row_Data (Ctree, Node));
   end First_Handler;

   -------------
   -- Explore --
   -------------

   procedure Explore
     (Tree      : access Gtk.Ctree.Gtk_Ctree_Record'Class;
      Root      : Gtk.Ctree.Gtk_Ctree_Node;
      Window    : access Gtk_Widget_Record'Class;
      Buffer    : String;
      Lang      : Language.Language_Access;
      File_Name : String;
      Handler   : Explorer_Handler := null)
   is
      Matches            : Match_Array (0 .. 10);

      Categories         : constant Explorer_Categories :=
        Explorer_Regexps (Lang);
      Internal_Cat       : Internal_Categories (Categories'Range);

      First              : Natural;
      Folder_Pixmap      : Gdk_Pixmap;
      Folder_Mask        : Gdk_Bitmap;
      Folder_Open_Pixmap : Gdk_Pixmap;
      Folder_Open_Mask   : Gdk_Bitmap;
      Node               : Gtk_Ctree_Node;

   begin
      Realize (Window);
      Freeze (Tree);

      Tree_Cb.Connect
        (Tree, "tree_select_row", Tree_Cb.To_Marshaller (First_Handler'Access),
         (Handler, Window.all'Access));

      --  Create all required icons

      for C in Categories'Range loop
         Create_From_Xpm_D
           (Internal_Cat (C).Pixmap,
            Get_Window (Window),
            Internal_Cat (C).Mask,
            Null_Color,
            Categories (C).Icon.all);
      end loop;

      Create_From_Xpm_D
        (Folder_Open_Pixmap, Get_Window (Window), Folder_Open_Mask, Null_Color,
         mini_ofolder_xpm);
      Create_From_Xpm_D
        (Folder_Pixmap, Get_Window (Window), Folder_Mask, Null_Color,
         mini_folder_xpm);

      --  For each category, parse the file

      for C in Categories'Range loop
         if Categories (C).Make_Entry /= null then
            Node := null;
            First := Buffer'First;
            loop
               Match (Categories (C).Regexp.all,
                      Buffer (First .. Buffer'Last),
                      Matches);

               exit when Matches (0) = No_Match;

               declare
                  Cat : aliased Category_Index := C;
                  S : String := Categories (C).Make_Entry
                    (Buffer, Matches, Cat'Access);
               begin
                  --  Create the parent node for the category, if needed.

                  if Internal_Cat (Cat).Node = null then
                     Internal_Cat (Cat).Node := Insert_Node
                       (Tree, Root, null,
                        Null_Array + Categories (Cat).Name.all,
                        5, Folder_Pixmap, Folder_Mask,
                        Folder_Open_Pixmap, Folder_Open_Mask,
                        False, False);
                  end if;

                  Node := Insert_Node
                    (Tree, Internal_Cat (Cat).Node, null,
                     Null_Array + S,
                     5,
                     Internal_Cat (Cat).Pixmap, Internal_Cat (Cat).Mask,
                     null, null,
                     True, False);
               end;

               Row_Data_Explorer.Node_Set_Row_Data
                 (Tree, Node, Get_Pos (Buffer,
                  Matches (Categories (C).Position_Index).First));
               First := Matches (0).Last;
            end loop;
         end if;
      end loop;

      --  Free all icons

      for C in Categories'Range loop
         if Internal_Cat (C).Node /= null then
            Sort_Recursive (Tree, Internal_Cat (C).Node);
         end if;

         Gdk.Pixmap.Unref (Internal_Cat (C).Pixmap);
         Gdk.Bitmap.Unref (Internal_Cat (C).Mask);
      end loop;

      Gdk.Pixmap.Unref (Folder_Open_Pixmap);
      Gdk.Bitmap.Unref (Folder_Open_Mask);
      Gdk.Pixmap.Unref (Folder_Pixmap);
      Gdk.Bitmap.Unref (Folder_Mask);

      --  This is a workaround for a horizontal scrollbar problem: When the
      --  ctree is put in a scrolled window, and if this is not called, the
      --  scrollbar does not allow us to scroll as far right as possible...
      Set_Column_Auto_Resize (Tree, 0, True);

      Thaw (Tree);
   end Explore;

end Odd.Explorer;
