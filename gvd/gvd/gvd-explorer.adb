with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.Color;    use Gdk.Color;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gtk.Handlers; use Gtk.Handlers;
with Gtkada.Types; use Gtkada.Types;
with Odd.Pixmaps;  use Odd.Pixmaps;
with GNAT.Regpat;  use GNAT.Regpat;
with GNAT.IO;      use GNAT.IO;

package body Odd.Explorer is

   type Explorer_Data is record
      Handler : Explorer_Handler;
      Widget  : Gtk_Widget;
   end record;

   package Tree_Cb is new
     Gtk.Handlers.User_Callback (Gtk_Ctree_Record, Explorer_Data);

   package Row_Data_Explorer is new Gtk.Ctree.Row_Data (Position_Type);

   Subprogram_RE : constant Pattern_Matcher :=
     Compile
       ("^[ \t]*(procedure|function)\s+" &
        "((\w|_)+)(\s+|\s*\([^\)]+\))\s*" &
        "(return\s+(\w|[_\.])+\s*)?is\s", Multiple_Lines);

   Package_RE    : constant Pattern_Matcher :=
     Compile
       ("^[ \t]*package[ \t]+((body[ \t]+)?((\w|[_\.])+))", Multiple_Lines);

   Type_Def_RE   : constant Pattern_Matcher :=
     Compile ("^[ \t]*(sub)?type[ \t]+((\w|_)+)", Multiple_Lines);

   Task_RE       : constant Pattern_Matcher :=
     Compile
       ("^[ \t]*task[ \t]+((body|type)[ \t]+)?((\w|_)+)", Multiple_Lines);

   Spec_RE       : constant Pattern_Matcher :=
     Compile
       ("^[ \t]*(procedure|function)\s+((\w|_)+)" &
        "((\s+|\s*\([^\)]+\))(\s*return\s+(\w|[_\.])+\s*)?)?;",
        Multiple_Lines);

   function Reduce (S : String) return String;
   --  Replace in string S all ASCII.LF and ASCII.HT characters with a space,
   --  and replace multiple spaces with a single one.
   --  Return the resulting string.

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
      Line_Start : Positive;
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

   ------------
   -- Reduce --
   ------------

   function Reduce (S : String) return String is
      Result : String (S'Range);
      Len    : Positive := Result'First;
      Blank  : Boolean  := False;

   begin
      for J in S'Range loop
         if S (J) = ASCII.LF or else S (J) = ASCII.HT or else S (J) = ' ' then
            if not Blank then
               Result (Len) := ' ';
               Len := Len + 1;
               Blank := True;
            end if;
         else
            Blank := False;
            Result (Len) := S (J);
            Len := Len + 1;
         end if;
      end loop;

      return Result (Result'First .. Len - 1);
   end Reduce;

   -------------
   -- Explore --
   -------------

   function Explore
     (Window  : access Gtk_Widget_Record'Class;
      Buffer  : String;
      Handler : Explorer_Handler := null) return Gtk_Ctree
   is
      Matches            : Match_Array (0 .. 10);
      First              : Natural;
      Tree               : Gtk_Ctree;
      Folder_Pixmap      : Gdk_Pixmap;
      Folder_Mask        : Gdk_Bitmap;
      Folder_Open_Pixmap : Gdk_Pixmap;
      Folder_Open_Mask   : Gdk_Bitmap;
      Package_Pixmap     : Gdk_Pixmap;
      Package_Mask       : Gdk_Bitmap;
      Subprogram_Pixmap  : Gdk_Pixmap;
      Subprogram_Mask    : Gdk_Bitmap;
      Var_Pixmap         : Gdk_Pixmap;
      Var_Mask           : Gdk_Bitmap;
      Parent_Node,
      Node               : Gtk_Ctree_Node;

   begin
      Gtk_New (Tree, 1);
      Realize (Window);

      Freeze (Tree);

      Tree_Cb.Connect
        (Tree, "tree_select_row", Tree_Cb.To_Marshaller (First_Handler'Access),
         (Handler, Window.all'Access));

      Create_From_Xpm_D
        (Folder_Pixmap, Get_Window (Window), Folder_Mask, Null_Color,
         mini_folder_xpm);
      Create_From_Xpm_D
        (Folder_Open_Pixmap, Get_Window (Window), Folder_Open_Mask, Null_Color,
         mini_ofolder_xpm);
      Create_From_Xpm_D
        (Package_Pixmap, Get_Window (Window), Package_Mask, Null_Color,
         package_xpm);
      Create_From_Xpm_D
        (Subprogram_Pixmap, Get_Window (Window), Subprogram_Mask, Null_Color,
         subprogram_xpm);
      Create_From_Xpm_D
        (Var_Pixmap, Get_Window (Window), Var_Mask, Null_Color, var_xpm);

      --  Subprograms

      Node := null;
      First := Buffer'First;
      loop
         Match (Subprogram_RE, Buffer (First .. Buffer'Last), Matches);

         exit when Matches (0) = No_Match;

         if Node = null then
            Parent_Node := Insert_Node
              (Tree, null, null, Null_Array + "Subprograms", 5,
               Folder_Pixmap, Folder_Mask,
               Folder_Open_Pixmap, Folder_Open_Mask,
               False, False);
         end if;

         if Matches (4) = No_Match then
            Node := Insert_Node
              (Tree, Parent_Node, null,
               Null_Array + Buffer (Matches (2).First .. Matches (2).Last), 5,
               Subprogram_Pixmap, Subprogram_Mask,
               null, null,
               True, False);
         else
            Node := Insert_Node
              (Tree, Parent_Node, null,
               Null_Array +
                 (Buffer (Matches (2).First .. Matches (2).Last) & ' ' &
                  Reduce
                    (Buffer (Matches (4).First .. Matches (4).Last))), 5,
               Subprogram_Pixmap, Subprogram_Mask,
               null, null,
               True, False);
         end if;

         Row_Data_Explorer.Node_Set_Row_Data
           (Tree, Node, Get_Pos (Buffer, Matches (2).First));
         First := Matches (0).Last;
      end loop;

      --  Packages

      Node := null;
      First := Buffer'First;
      loop
         Match (Package_RE, Buffer (First .. Buffer'Last), Matches);

         exit when Matches (0) = No_Match;

         if Node = null then
            Parent_Node := Insert_Node
              (Tree, null, null, Null_Array + "Packages", 5,
               Folder_Pixmap, Folder_Mask,
               Folder_Open_Pixmap, Folder_Open_Mask,
               False, False);
         end if;

         Node := Insert_Node
           (Tree, Parent_Node, null,
            Null_Array + Buffer (Matches (3).First .. Matches (3).Last), 5,
            Package_Pixmap, Package_Mask,
            null, null,
            True, False);
         Row_Data_Explorer.Node_Set_Row_Data
           (Tree, Node, Get_Pos (Buffer, Matches (3).First));
         First := Matches (0).Last;
      end loop;

      --  Types

      Node := null;
      First := Buffer'First;
      loop
         Match (Type_Def_RE, Buffer (First .. Buffer'Last), Matches);

         exit when Matches (0) = No_Match;

         if Node = null then
            Parent_Node := Insert_Node
              (Tree, null, null, Null_Array + "Types", 5,
               Folder_Pixmap, Folder_Mask,
               Folder_Open_Pixmap, Folder_Open_Mask,
               False, False);
         end if;

         Node := Insert_Node
           (Tree, Parent_Node, null,
            Null_Array + Buffer (Matches (2).First .. Matches (2).Last), 5,
            Var_Pixmap, Var_Mask,
            null, null,
            True, False);
         Row_Data_Explorer.Node_Set_Row_Data
           (Tree, Node, Get_Pos (Buffer, Matches (2).First));
         First := Matches (0).Last;
      end loop;

      --  Tasks

      Node := null;
      First := Buffer'First;
      loop
         Match (Task_RE, Buffer (First .. Buffer'Last), Matches);

         exit when Matches (0) = No_Match;

         if Node = null then
            Parent_Node := Insert_Node
              (Tree, null, null, Null_Array + "Tasks", 5,
               Folder_Pixmap, Folder_Mask,
               Folder_Open_Pixmap, Folder_Open_Mask,
               False, False);
         end if;

         if Matches (2) = No_Match then
            Node := Insert_Node
              (Tree, Parent_Node, null,
               Null_Array + Buffer (Matches (3).First .. Matches (3).Last), 5,
               Package_Pixmap, Package_Mask,
               null, null,
               True, False);
         else
            Node := Insert_Node
              (Tree, Parent_Node, null,
               Null_Array +
                 (Buffer (Matches (3).First .. Matches (3).Last) & " (" &
                  Reduce (Buffer (Matches (2).First .. Matches (2).Last)) &
                  ")"),
               5,
               Package_Pixmap, Package_Mask,
               null, null,
               True, False);
         end if;

         Row_Data_Explorer.Node_Set_Row_Data
           (Tree, Node, Get_Pos (Buffer, Matches (3).First));
         First := Matches (0).Last;
      end loop;

      --  Specs

      Node := null;
      First := Buffer'First;
      loop
         Match (Spec_RE, Buffer (First .. Buffer'Last), Matches);

         exit when Matches (0) = No_Match;

         if Node = null then
            Parent_Node := Insert_Node
              (Tree, null, null, Null_Array + "Specs", 5,
               Folder_Pixmap, Folder_Mask,
               Folder_Open_Pixmap, Folder_Open_Mask,
               False, False);
         end if;

         if Matches (5) = No_Match then
            Node := Insert_Node
              (Tree, Parent_Node, null,
               Null_Array + Buffer (Matches (2).First .. Matches (2).Last), 5,
               Var_Pixmap, Var_Mask,
               null, null,
               True, False);
         else
            Node := Insert_Node
              (Tree, Parent_Node, null,
               Null_Array +
                 (Buffer (Matches (2).First .. Matches (2).Last) &
                  Reduce
                    (Buffer (Matches (5).First .. Matches (5).Last))),
               5,
               Var_Pixmap, Var_Mask,
               null, null,
               True, False);
         end if;

         Row_Data_Explorer.Node_Set_Row_Data
           (Tree, Node, Get_Pos (Buffer, Matches (2).First));
         First := Matches (0).Last;
      end loop;

      Thaw (Tree);
      return Tree;
   end Explore;

end Odd.Explorer;
