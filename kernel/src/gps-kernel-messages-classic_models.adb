-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009-2010, AdaCore                  --
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
with Ada.Unchecked_Conversion;

with Gtk.Enums;
with Gtk.Tree_Model.Utils;

with GNATCOLL.VFS.GtkAda;
with GPS.Editors.GtkAda;
with String_Utils;

package body GPS.Kernel.Messages.Classic_Models is

   use Gdk.Color;
   use Gdk.Pixbuf;
   use Gtk.Tree_Model;
   use Gtk.Widget;
   use GPS.Editors;
   use GPS.Editors.GtkAda;
   use String_Utils;

   function Create_Iter
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Node : Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Node
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Iter : Gtk_Tree_Iter) return Node_Access;

   function Create_Path
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Node : not null Node_Access) return Gtk_Tree_Path;

   Non_Leaf_Color_Name : constant String := "blue";
   --  Name of the color to be used for category and file names

   Location_Padding : constant := 10;
   --  Size of field for line:column location information in view

   ----------------------
   -- Category_Removed --
   ----------------------

   overriding procedure Category_Removed
     (Self  : not null access Classic_Tree_Model_Record;
      Index : Positive)
   is
      Path : constant Gtk_Tree_Path := Gtk_New;

   begin
      Append_Index (Path, Gint (Index) - 1);
      Self.Row_Deleted (Path);
      Path_Free (Path);
   end Category_Removed;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Classic_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : Node_Access := Self.Get_Node (Parent);

   begin
      if Node = null then
         if Self.Container.Categories.Is_Empty then
            Node := null;

         else
            Node := Self.Container.Categories.First_Element;
         end if;

      else
         if Node.Children.Is_Empty then
            Node := null;

         else
            Node := Node.Children.First_Element;
         end if;
      end if;

      return Self.Create_Iter (Node);
   end Children;

   -----------------
   -- Create_Iter --
   -----------------

   function Create_Iter
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Node : Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      function To_Address is
        new Ada.Unchecked_Conversion (Node_Access, System.Address);

   begin
      if Node = null then
         return Null_Iter;

      else
         return
           Gtk.Tree_Model.Utils.Init_Tree_Iter (Self.Stamp, To_Address (Node));
      end if;
   end Create_Iter;

   -----------------
   -- Create_Path --
   -----------------

   function Create_Path
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Node : not null Node_Access) return Gtk_Tree_Path
   is
      Aux    : Node_Access := Node;
      Result : constant Gtk_Tree_Path := Gtk_New;
      Index  : Natural;

   begin
      while Aux /= null loop
         if Aux.Parent = null then
            Index := Self.Container.Categories.Find_Index (Aux);

         else
            Index := Aux.Parent.Children.Find_Index (Aux);
         end if;

         Prepend_Index (Result, Gint (Index) - 1);
         Aux := Aux.Parent;
      end loop;

      return Result;
   end Create_Path;

   ------------------
   -- File_Removed --
   ------------------

   overriding procedure File_Removed
     (Self     : not null access Classic_Tree_Model_Record;
      Category : not null Node_Access;
      Index    : Positive)
   is
      Path : constant Gtk_Tree_Path := Self.Create_Path (Category);

   begin
      Append_Index (Path, Gint (Index) - 1);
      Self.Row_Deleted (Path);
      Path_Free (Path);
   end File_Removed;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Classic_Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Category_Column =>
            return Glib.GType_String;

         when Subcategory_Column =>
            return Glib.GType_String;

         when File_Column =>
            return GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type;

         when Line_Column =>
            return Glib.GType_Int;

         when Column_Column =>
            return Glib.GType_Int;

         when Text_Column =>
            return Glib.GType_String;

         when Node_Icon_Column =>
            return Gdk.Pixbuf.Get_Type;

         when Node_Markup_Column =>
            return Glib.GType_String;

         when Node_Foreground_Column =>
            return Gdk.Color.Gdk_Color_Type;

         when Node_Tooltip_Column =>
            return Glib.GType_String;

         when Node_Mark_Column =>
            return Get_Editor_Mark_Type;

         when Action_Pixbuf_Column =>
            return Gdk.Pixbuf.Get_Type;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Classic_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Gint_Array := Get_Indices (Path);
      Node    : Node_Access;

   begin
      for J in Indices'Range loop
         if J = 0 then
            if Indices (J) < Gint (Self.Container.Categories.Length) then
               Node :=
                 Self.Container.Categories.Element (Natural (Indices (J)) + 1);

            else
               Node := null;

               exit;
            end if;

         else
            if Indices (J) < Gint (Node.Children.Length) then
               Node := Node.Children.Element (Natural (Indices (J)) + 1);

            else
               Node := null;

               exit;
            end if;
         end if;
      end loop;

      return Self.Create_Iter (Node);
   end Get_Iter;

   ------------------
   -- Get_N_Column --
   ------------------

   overriding function Get_N_Columns
     (Self : access Classic_Tree_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Total_Columns;
   end Get_N_Columns;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Iter : Gtk_Tree_Iter) return Node_Access
   is
      function To_Node is
        new Ada.Unchecked_Conversion (System.Address, Node_Access);

   begin
      if Iter = Null_Iter then
         return null;

      else
         if Gtk.Tree_Model.Utils.Get_Stamp (Iter) /= Self.Stamp then
            raise Program_Error;

         else
            return To_Node (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter));
         end if;
      end if;
   end Get_Node;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
   begin
      return Self.Create_Path (Self.Get_Node (Iter));
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Classic_Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      use Ada.Strings.Unbounded;
      use Glib.Values;

      Node : constant Node_Access := Self.Get_Node (Iter);

   begin
      case Column is
         when Category_Column =>
            Init (Value, GType_String);

            case Node.Kind is
               when Node_Category =>
                  Set_String (Value, To_String (Node.Name));

               when Node_File =>
                  Set_String (Value, To_String (Node.Parent.Name));

               when Node_Message =>
                  Set_String
                    (Value, To_String (Message_Access (Node).Get_Category));
            end case;

         when Subcategory_Column =>
            raise Program_Error;

         when File_Column =>
            Init (Value, GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type);

            case Node.Kind is
               when Node_Category =>
                  GNATCOLL.VFS.GtkAda.Set_File (Value, No_File);

               when Node_File =>
                  GNATCOLL.VFS.GtkAda.Set_File (Value, Node.File);

               when Node_Message =>
                  GNATCOLL.VFS.GtkAda.Set_File
                    (Value, Message_Access (Node).Get_File);
            end case;

         when Line_Column =>
            Init (Value, Glib.GType_Int);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Int (Value, -1);

               when Node_Message =>
                  Set_Int (Value, Gint (Message_Access (Node).Line));
            end case;

         when Column_Column =>
            Init (Value, Glib.GType_Int);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Int (Value, -1);

               when Node_Message =>
                  Set_Int (Value, Gint (Message_Access (Node).Line));
            end case;

         when Text_Column =>
            Init (Value, GType_String);

            case Node.Kind is
               when Node_Category =>
                  Set_String (Value, To_String (Node.Name));

               when Node_File =>
                  Set_String (Value, String (Node.File.Base_Name));

               when Node_Message =>
                  Set_String
                    (Value,
                To_String (Abstract_Message'Class (Node.all).Get_Text));
            end case;

         when Node_Icon_Column =>
            Init (Value, Gdk.Pixbuf.Get_Type);

            --  Create puxbufs for category and file nodes. It is down here
            --  because icon factory is not customized at message container
            --  initialization time.

            if Self.Category_Pixbuf = null then
               Self.Category_Pixbuf :=
                 Self.Container.Kernel.Get_Main_Window.Render_Icon
                   ("gps-box", Gtk.Enums.Icon_Size_Menu);
            end if;

            if Self.File_Pixbuf = null then
               Self.File_Pixbuf :=
                 Self.Container.Kernel.Get_Main_Window.Render_Icon
                   ("gps-file", Gtk.Enums.Icon_Size_Menu);
            end if;

            case Node.Kind is
               when Node_Category =>
                  Set_Object
                    (Value, Glib.Object.GObject (Self.Category_Pixbuf));

               when Node_File =>
                  Set_Object (Value, Glib.Object.GObject (Self.File_Pixbuf));

               when others =>
                  null;
            end case;

         when Node_Markup_Column =>
            Init (Value, GType_String);

            case Node.Kind is
               when Node_Category =>
                  Set_String (Value, To_String (Node.Name));

               when Node_File =>
                  Set_String (Value, String (Node.File.Base_Name));

               when Node_Message =>
                  case Message_Access (Node).Level is
                     when Primary =>
                        declare
                           Location : constant String :=
                             Image (Node.Line)
                             & ':' & Image (Natural (Node.Column));
                           Padding  : String (1 .. Location_Padding) :=
                             (others => ' ');

                        begin
                           if Location'Length <= Padding'Length then
                              Padding (1 .. Location'Length) := Location;
                              Set_String
                                (Value,
                                 "<b>" & Padding & "</b> "
                                 & To_String
                                   (Abstract_Message'Class
                                      (Node.all).Get_Markup));

                           else
                              Set_String
                                (Value,
                                 "<b>" & Location & "</b> "
                                 & To_String
                                   (Abstract_Message'Class
                                      (Node.all).Get_Markup));
                           end if;
                        end;

                     when Secondary =>
                        declare
                           Padding : constant String (1 .. Location_Padding) :=
                             (others => ' ');

                        begin
                           Set_String
                             (Value,
                              "<b>" & Padding & "</b> "
                              & To_String
                                (Abstract_Message'Class
                                   (Node.all).Get_Markup));
                        end;
                  end case;
            end case;

         when Node_Foreground_Column =>
            Init (Value, Gdk_Color_Type);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Value (Value, Self.Non_Leaf_Color);

               when Node_Message =>
                  null;
            end case;

         when Node_Tooltip_Column =>
            Init (Value, GType_String);

            case Node.Kind is
               when Node_Category =>
                  Set_String (Value, To_String (Node.Name));

               when Node_File =>
                  Set_String (Value, String (Node.File.Base_Name));

               when Node_Message =>
                  declare
                     Markup : Unbounded_String;
                     N      : Node_Access := Node;
                     M      : Message_Access;

                  begin
                     loop
                        M := Message_Access (N);

                        case M.Level is
                           when Primary =>
                              Markup := ASCII.LF & M.Get_Markup & Markup;

                              exit;

                           when Secondary =>
                              Markup :=
                                ASCII.LF & "  " & M.Get_Markup & Markup;
                        end case;

                        N := N.Parent;
                     end loop;

                     Markup :=
                       N.Parent.Parent.Name
                         & ASCII.LF & String (N.Parent.File.Base_Name)
                         & ":" & Image (Node.Line)
                         & ':' & Image (Natural (Node.Column))
                         & Markup;

                     Set_String (Value, To_String (Markup));
                  end;
            end case;

         when Node_Mark_Column =>
            Init (Value, Get_Editor_Mark_Type);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Mark (Value, Nil_Editor_Mark);

               when Node_Message =>
                  if Node.Mark = null then
                     Set_Mark (Value, Nil_Editor_Mark);

                  else
                     Set_Mark (Value, Node.Mark.all);
                  end if;
            end case;

         when Action_Pixbuf_Column =>
            Init (Value, Gdk.Pixbuf.Get_Type);

         when others =>
            null;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Object    : out Classic_Tree_Model;
      Container : not null access Messages_Container'Class) is
   begin
      Object := new Classic_Tree_Model_Record (Container);
      Initialize (Object);
   end Gtk_New;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Node : constant Node_Access := Self.Get_Node (Iter);

   begin
      if Node = null then
         return not Self.Container.Categories.Is_Empty;

      else
         return not Node.Children.Is_Empty;
      end if;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Classic_Tree_Model_Record'Class) is
      Success : Boolean;

   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);

      --  Allocate foreground color for category and file nodes

      Self.Non_Leaf_Color := Parse (Non_Leaf_Color_Name);
      Alloc_Color
        (Get_Default_Colormap, Self.Non_Leaf_Color, False, True, Success);

      --  Create puxbufs for category and file nodes
      --  XXX Don't work because customization is not done at the point of
      --  kernel initialization.

      Self.Category_Pixbuf :=
        Self.Container.Kernel.Get_Main_Window.Render_Icon
          ("gps-box", Gtk.Enums.Icon_Size_Menu);
      Self.File_Pixbuf :=
        Self.Container.Kernel.Get_Main_Window.Render_Icon
          ("gps-file", Gtk.Enums.Icon_Size_Menu);
   end Initialize;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Classic_Tree_Model_Record;
      Message : not null Message_Access) is
   begin
      Self.Node_Added (Node_Access (Message));
   end Message_Added;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self  : not null access Classic_Tree_Model_Record;
      File  : not null Node_Access;
      Index : Positive)
   is
      Path : constant Gtk_Tree_Path := Self.Create_Path (File);

   begin
      Append_Index (Path, Gint (Index) - 1);
      Self.Row_Deleted (Path);
      Path_Free (Path);
   end Message_Removed;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Classic_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Node : constant Node_Access := Self.Get_Node (Iter);

   begin
      if Node = null then
         return Gint (Self.Container.Categories.Length);

      else
         return Gint (Node.Children.Length);
      end if;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Classic_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use Node_Vectors;

      Position : Node_Vectors.Cursor;
      Node     : Node_Access := Self.Get_Node (Iter);

   begin
      if Node /= null then
         if Node.Parent = null then
            if Node.Kind /= Node_Category then
               raise Program_Error;
            end if;

            Position := Self.Container.Categories.Find (Node);

         else
            Position := Node.Parent.Children.Find (Node);
         end if;

         if Has_Element (Position) then
            Next (Position);
         end if;

         if Has_Element (Position) then
            Node := Element (Position);

         else
            Node := null;
         end if;
      end if;

      Iter := Self.Create_Iter (Node);
   end Next;

   ----------------
   -- Node_Added --
   ----------------

   procedure Node_Added
     (Self : not null access Classic_Tree_Model_Record;
      Node : not null Node_Access)
   is
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter;

   begin
      Self.Stamp := Self.Stamp + 1;
      Iter := Self.Create_Iter (Node);
      Path := Self.Create_Path (Node);
      Self.Row_Inserted (Path, Iter);
      Path_Free (Path);
   end Node_Added;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Classic_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : Node_Access := Self.Get_Node (Parent);

   begin
      if Node = null then
         if N < Gint (Self.Container.Categories.Length) then
            Node := Self.Container.Categories.Element (Natural (N) + 1);

         else
            Node := null;
         end if;

      else
         if N < Gint (Node.Children.Length) then
            Node := Node.Children.Element (Natural (N) + 1);

         else
            Node := null;
         end if;
      end if;

      return Self.Create_Iter (Node);
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Classic_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : Node_Access := Self.Get_Node (Child);

   begin
      if Node /= null then
         Node := Node.Parent;
      end if;

      return Self.Create_Iter (Node);
   end Parent;

end GPS.Kernel.Messages.Classic_Models;
