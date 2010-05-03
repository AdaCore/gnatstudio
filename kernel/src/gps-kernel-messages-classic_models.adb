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

with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

with Gtk.Enums;
with Gtk.Tree_Model.Utils;

with GNATCOLL.VFS.GtkAda;
with GPS.Editors.GtkAda;
with String_Utils;
with Traces;

package body GPS.Kernel.Messages.Classic_Models is

   use Ada.Strings.Fixed;
   use Gdk.Color;
   use Gdk.Pixbuf;
   use Gtk.Tree_Model;
   use Gtk.Widget;
   use GPS.Editors;
   use GPS.Editors.GtkAda;
   use Node_Vectors;
   use String_Utils;
   use Traces;

   function Create_Iter
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Node : Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Node
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Iter : Gtk_Tree_Iter) return Node_Access;

   function Create_Path
     (Self : not null access constant Classic_Tree_Model_Record'Class;
      Node : not null Node_Access) return Gtk_Tree_Path;

   procedure On_Destroy (Data : System.Address; Model : System.Address);
   pragma Convention (C, On_Destroy);
   --  Called when the model is being destroyed

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
      Path  : constant Gtk_Tree_Path := Gtk_New;
      Ind   : Natural := 0;
   begin
      for J in Self.Container.Categories.First_Index
        .. Self.Container.Categories.Last_Index
      loop
         if Match (Self.Flags,
                   Self.Container.Categories.Element (J).Flags)
         then
            Ind := Ind + 1;
         end if;

         if Ind = Index then
            Append_Index (Path, Gint (Ind) - 1);
            Self.Row_Deleted (Path);
         end if;
      end loop;

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
            for J in Self.Container.Categories.First_Index
              .. Self.Container.Categories.Last_Index
            loop
               if Match (Self.Flags,
                         Self.Container.Categories.Element (J).Flags)
               then

                  Node := Self.Container.Categories.Element (J);
                  exit;
               end if;
            end loop;
         end if;

      else
         if Node.Children.Is_Empty then
            Node := null;

         else
            Node := null;
            for J in Node.Children.First_Index
              .. Node.Children.Last_Index
            loop
               if Match (Self.Flags,
                         Node.Children.Element (J).Flags)
               then
                  Node := Node.Children.Element (J);
                  exit;
               end if;
            end loop;
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
      Index  : Natural := 1;

   begin
      while Aux /= null loop
         if Aux.Parent = null then
            Index := 1;
            for J in Self.Container.Categories.First_Index
              .. Self.Container.Categories.Last_Index
            loop
               exit when Self.Container.Categories.Element (J) = Aux;

               if Match (Self.Container.Categories.Element (J).Flags,
                         Self.Flags)
               then
                  Index := Index + 1;
               end if;
            end loop;
         else
            Index := 1;
            for J in Aux.Parent.Children.First_Index
              .. Aux.Parent.Children.Last_Index
            loop
               exit when Aux.Parent.Children.Element (J) = Aux;

               if Match (Aux.Parent.Children.Element (J).Flags,
                         Self.Flags)
               then
                  Index := Index + 1;
               end if;
            end loop;
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
      Path  : constant Gtk_Tree_Path := Self.Create_Path (Category);
      Dummy : Boolean;
      Found : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Append_Index (Path, Gint (Index) - 1);
      Self.Row_Deleted (Path);

      if Category.Children.Is_Empty then
         Dummy := Up (Path);
         Self.Row_Has_Child_Toggled (Path, Self.Create_Iter (Category));
      else
         Found := False;
         for J in Category.Children.First_Index
           .. Category.Children.Last_Index
         loop
            if Match (Self.Flags, Category.Children.Element (J).Flags) then
               Found := True;
               exit;
            end if;
         end loop;

         if Found then
            Dummy := Up (Path);
            Self.Row_Has_Child_Toggled (Path, Self.Create_Iter (Category));
         end if;
      end if;

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

         when Weight_Column =>
            return Glib.GType_Int;

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

         when Action_Command_Column =>
            return Glib.GType_Pointer;

         when Number_Of_Children_Column =>
            return Glib.GType_Int;

         when Sort_Order_Hint_Column =>
            return Glib.GType_Int;

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
      Index   : Natural;
      Count   : Natural;
   begin
      for J in Indices'Range loop
         if J = 0 then
            Index := Self.Container.Categories.First_Index;
            Count := 0;
            Node  := null;

            while Index <= Self.Container.Categories.Last_Index loop
               if Match (Self.Container.Categories.Element (Index).Flags,
                         Self.Flags)
               then
                  if Gint (Count) = Indices (0) then
                     Node := Self.Container.Categories.Element (Index);
                     exit;
                  end if;
                  Count := Count + 1;
               end if;

               Index := Index + 1;
            end loop;

         else
            if Node /= null
              and then Indices (J) < Gint (Node.Children.Length)
            then
               Index := Node.Children.First_Index;
               Count := 0;
               while Index <= Node.Children.Last_Index loop
                  if Match (Self.Flags,
                            Node.Children.Element (Index).Flags)
                  then
                     if Gint (Count) = Indices (J) then
                        Node := Node.Children.Element (Index);
                        exit;
                     end if;
                     Count := Count + 1;
                  end if;
                  Index := Index + 1;
                  if Index > Node.Children.Last_Index then
                     Node := null;
                     exit;
                  end if;
               end loop;

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

      function To_Address is
        new Ada.Unchecked_Conversion (Action_Item, System.Address);

      procedure Set_Node_Markup
        (Value : in out Glib.Values.GValue;
         Node  : not null Node_Access);
      --  Sets node's markup into the value

      ---------------------
      -- Set_Node_Markup --
      ---------------------

      procedure Set_Node_Markup
        (Value : in out Glib.Values.GValue;
         Node  : not null Node_Access) is
      begin
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
                        Length   : constant Natural :=
                          Integer'Max (0, Location_Padding - Location'Length);

                     begin
                        Set_String
                          (Value,
                           "<b>" & Location & "</b>" & (Length * ' ')
                           & To_String (Message_Access (Node).Get_Markup));
                     end;

                  when Secondary =>
                     Set_String
                       (Value,
                        (Location_Padding * ' ')
                        & To_String (Message_Access (Node).Get_Markup));
               end case;
         end case;
      end Set_Node_Markup;

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

         when Weight_Column =>
            Init (Value, GType_Int);

            case Node.Kind is
               when Node_Category =>
                  Set_Int (Value, 0);

               when Node_File =>
                  declare
                     Position : Node_Vectors.Cursor := Node.Children.First;
                     Maximum  : Natural := 0;

                  begin
                     while Has_Element (Position) loop
                        Maximum :=
                          Natural'Max
                            (Maximum,
                             Message_Access (Element (Position)).Weight);
                        Next (Position);
                     end loop;

                     Set_Int (Value, Gint (Maximum));
                  end;

               when Node_Message =>
                  case Message_Access (Node).Level is
                     when Primary =>
                        Set_Int (Value, Gint (Message_Access (Node).Weight));

                     when Secondary =>
                        Set_Int (Value, 0);
                  end case;
            end case;

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
                  Set_Int (Value, Gint (Message_Access (Node).Column));
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
            Set_Node_Markup (Value, Node);

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

            case Node.Kind is
               when Node_Category | Node_File =>
                  null;

               when Node_Message =>
                  if Node.Action /= null then
                     Set_Object
                       (Value, Glib.Object.GObject (Node.Action.Image));
                  end if;
            end case;

         when Action_Command_Column =>
            Init (Value, Glib.GType_Pointer);

            case Node.Kind is
               when Node_Category | Node_File =>
                  null;

               when Node_Message =>
                  Set_Address (Value, To_Address (Node.Action));
            end case;

         when Number_Of_Children_Column =>
            Init (Value, Glib.GType_Int);
            Set_Int (Value, Gint (Node.Message_Count));

         when Sort_Order_Hint_Column =>
            declare
               Category_Node : Node_Access := Node;

            begin
               while Category_Node.Kind /= Node_Category loop
                  Category_Node := Category_Node.Parent;
               end loop;

               Init (Value, Glib.GType_Int);
               Set_Int (Value, Sort_Order_Hint'Pos (Category_Node.Sort_Hint));
            end;

         when others =>
            null;
      end case;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Init (Value, Glib.GType_Invalid);
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
         if Self.Container.Categories.Is_Empty then
            return False;
         else
            for J in Self.Container.Categories.First_Index
              .. Self.Container.Categories.Last_Index
            loop
               if Match (Self.Flags,
                         Self.Container.Categories.Element (J).Flags)
               then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      else
         if Node.Children.Is_Empty then
            return False;
         else
            for J in Node.Children.First_Index ..
              Node.Children.Last_Index
            loop
               if Match (Self.Flags, Node.Children.Element (J).Flags) then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      end if;
   end Has_Child;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Data : System.Address;
      Model  : System.Address)
   is
      pragma Unreferenced (Data);
      Stub : Glib.Object.GObject_Record;
      M : constant Classic_Tree_Model :=
        Classic_Tree_Model (Glib.Object.Get_User_Data (Model, Stub));
   begin
      if M /= null and then M.Category_Pixbuf /= null then
         Unref (M.Category_Pixbuf);
         Unref (M.File_Pixbuf);
      end if;
   end On_Destroy;

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

      Glib.Object.Weak_Ref (Self, On_Destroy'Access);
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

   ------------------------------
   -- Message_Property_Changed --
   ------------------------------

   overriding procedure Message_Property_Changed
     (Self    : not null access Classic_Tree_Model_Record;
      Message : not null Message_Access)
   is
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter;

   begin
      Self.Stamp := Self.Stamp + 1;
      Iter := Self.Create_Iter (Node_Access (Message));
      Path := Self.Create_Path (Node_Access (Message));
      Self.Row_Changed (Path, Iter);
      Path_Free (Path);
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self  : not null access Classic_Tree_Model_Record;
      File  : not null Node_Access;
      Index : Positive)
   is
      Path  : constant Gtk_Tree_Path := Self.Create_Path (File);
      Dummy : Boolean;
      Found : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Append_Index (Path, Gint (Index) - 1);
      Self.Row_Deleted (Path);

      if File.Children.Is_Empty then
         Dummy := Up (Path);
         Self.Row_Has_Child_Toggled (Path, Self.Create_Iter (File));
      else
         Found := False;
         for J in File.Children.First_Index .. File.Children.Last_Index loop
            if Match (Self.Flags,
                      File.Children.Element (J).Flags)
            then
               Found := True;
               exit;
            end if;
         end loop;

         if Found then
            Dummy := Up (Path);
            Self.Row_Has_Child_Toggled (Path, Self.Create_Iter (File));
         end if;
      end if;

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
      Node  : constant Node_Access := Self.Get_Node (Iter);
      Count : Natural := 0;
   begin
      if Node = null then
         for J in Self.Container.Categories.First_Index
           .. Self.Container.Categories.Last_Index
         loop
            if Match (Self.Flags,
                      Self.Container.Categories.Element (J).Flags)
            then
               Count := Count + 1;
            end if;
         end loop;

         return Gint (Count);

      else
         for J in Node.Children.First_Index .. Node.Children.Last_Index loop
            if Match (Self.Flags,
                      Node.Children.Element (J).Flags)
            then
               Count := Count + 1;
            end if;
         end loop;

         return Gint (Count);
      end if;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Classic_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
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
      use type Ada.Containers.Count_Type;

      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Path := Self.Create_Path (Node);

      --  Verify that the parent node exists.
      --  The parent might not exist in the following case:
      --     - the category/file nodes do not exist
      --     - we insert a message visible only in the editors
      --     - we then insert a message visible in the locations view

      --  ??? Possible optimization: instead of doing this lookup for all
      --  messages, we could do this at the higher level: whenever a message is
      --  added and this causes a change of flags in the category or file level
      --  then re-add the category or file node to the models that need it

      declare
         Parent  : Gtk_Tree_Path;
      begin
         Parent := Copy (Path);

         if Up (Parent) then
            if Get_Iter (Self, Path) = Null_Iter then
               if Node.Parent /= null then
                  Node_Added (Self, Node.Parent);
               end if;
            end if;
         end if;

         Path_Free (Parent);
      end;

      Self.Stamp := Self.Stamp + 1;
      Iter := Self.Create_Iter (Node);
      Self.Row_Inserted (Path, Iter);

      Dummy := Up (Path);
      Iter := Self.Parent (Iter);

      if Node.Parent /= null
        and then Node.Parent.Children.Length = 1
      then
         Self.Row_Has_Child_Toggled (Path, Iter);
         --  ??? We need to emit this also if it is the first child inserted
         --  that matches flags
      end if;

      --  J326-003: when new row at message level is inserted weight of the
      --  file node can be changed, so we need to notify upper model/view
      --  about such change.

      if Get_Depth (Path) = 2 then
         Self.Row_Changed (Path, Iter);
      end if;

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
      Node  : Node_Access := Self.Get_Node (Parent);
      Count : Gint := 0;

   begin
      if Node = null then
         if N < Gint (Self.Container.Categories.Length) then
            for J in Self.Container.Categories.First_Index
              .. Self.Container.Categories.Last_Index
            loop
               if Match (Self.Flags,
                         Self.Container.Categories.Element (J).Flags)
               then
                  if Count = N then
                     Node := Self.Container.Categories.Element (J);
                     exit;
                  end if;
                  Count := Count + 1;
               end if;
            end loop;
         else
            Node := null;
         end if;

      else
         if N < Gint (Node.Children.Length) then
            for J in Node.Children.First_Index .. Node.Children.Last_Index loop
               if Match (Self.Flags,
                         Node.Children.Element (J).Flags) then
                  if Count = N then
                     Node := Node.Children.Element (J);
                     exit;
                  end if;
               end if;
            end loop;

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
