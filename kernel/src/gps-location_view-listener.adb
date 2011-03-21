-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2010-2011, AdaCore                  --
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
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Glib.Object;
with Glib;
with Gtk.Enums;
with Gtk.Tree_Model.Utils;
with Gtk.Widget;

with Basic_Types;
with Commands;
with GNATCOLL.VFS.GtkAda;
with GPS.Editors.GtkAda;
with String_Utils;
with Traces;

package body GPS.Location_View.Listener is

   use Ada.Strings.Fixed;
   use Gdk.Color;
   use Gdk.Pixbuf;
   use Glib;
   use Gtk.Widget;
   use GNATCOLL.VFS;
   use GPS.Editors;
   use GPS.Editors.GtkAda;
   use Node_Vectors;
   use String_Utils;
   use Traces;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Node_Record'Class, Node_Access);

   procedure Recursive_Free (N : in out Node_Access);
   --  Free N and all children of N

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

   function Find_Category_Node
     (Self : not null access Locations_Listener;
      Cat  : Unbounded_String) return Node_Access;
   --  Find the node containing category Cat.
   --  Create it if necessary.

   function New_Category_Node
     (Self : not null access Locations_Listener;
      Cat  : Unbounded_String) return Node_Access;
   --  Create a new category node in the model

   function Find_File_Node
     (Self     : not null access Locations_Listener;
      Cat_Node : Node_Access;
      File     : GNATCOLL.VFS.Virtual_File) return Node_Access;
   --  Find the node containing File in the category node.
   --  Create it if necessary.

   function New_File_Node
     (Self     : not null access Locations_Listener;
      Cat_Node : Node_Access;
      File     : GNATCOLL.VFS.Virtual_File) return Node_Access;
   --  Add a new file node to the model.

   function File_Note_Index
     (Cat_Node : Node_Access;
      File     : GNATCOLL.VFS.Virtual_File) return Natural;
   --  Return the index of the node containing File in the children of Cat_Node

   function Find_Node
     (Self    : not null access Locations_Listener;
      Message : Message_Access) return Node_Access;
   --  Find node corresponding to Message

   package Message_Conversions is
     new System.Address_To_Access_Conversions
       (GPS.Kernel.Messages.Abstract_Message'Class);

   --------------------
   -- Recursive_Free --
   --------------------

   procedure Recursive_Free (N : in out Node_Access) is
      C : Node_Access;
   begin
      for J in N.Children.First_Index .. N.Children.Last_Index loop
         C := N.Children.Element (J);
         Recursive_Free (C);
      end loop;

      Unchecked_Free (N);
   end Recursive_Free;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (Self    : not null access Locations_Listener;
      Message : Message_Access) return Node_Access
   is
      Cat  : constant Unbounded_String := Message.Get_Category;
      File : constant Virtual_File := Message.Get_File;

      Cat_Node : constant Node_Access := Find_Category_Node (Self, Cat);

      Parent      : Message_Access;
      Parent_Node : Node_Access;
   begin
      Parent := Message.Get_Parent;

      if Parent = null then
         Parent_Node := Cat_Node.Children.Element
           (File_Note_Index (Cat_Node, File));
      else
         Parent_Node := Find_Node (Self, Parent);
      end if;

      for J in Parent_Node.Children.First_Index
        .. Parent_Node.Children.Last_Index
      loop
         if Parent_Node.Children.Element (J).Message = Message then
            return Parent_Node.Children.Element (J);
         end if;
      end loop;
      return null;
   end Find_Node;

   --------------------
   -- Find_File_Node --
   --------------------

   function Find_File_Node
     (Self     : not null access Locations_Listener;
      Cat_Node : Node_Access;
      File     : GNATCOLL.VFS.Virtual_File) return Node_Access is
   begin
      for J in Cat_Node.Children.First_Index .. Cat_Node.Children.Last_Index
      loop
         if Cat_Node.Children.Element (J).File = File then
            return Cat_Node.Children.Element (J);
         end if;
      end loop;

      return New_File_Node (Self, Cat_Node, File);
   end Find_File_Node;

   -------------------
   -- New_File_Node --
   -------------------

   function New_File_Node
     (Self     : not null access Locations_Listener;
      Cat_Node : Node_Access;
      File     : GNATCOLL.VFS.Virtual_File) return Node_Access
   is
      Node : Node_Access;
   begin
      Node := new Node_Record (Node_File);
      Node.Parent := Cat_Node;
      Node.Message_Count := 0;
      Node.File := File;
      Cat_Node.Children.Append (Node);
      Self.Model.Node_Added (Node);
      return Node;
   end New_File_Node;

   ------------------------
   -- Find_Category_Node --
   ------------------------

   function Find_Category_Node
     (Self : not null access Locations_Listener;
      Cat  : Unbounded_String) return Node_Access is
   begin
      for J in Self.Model.Categories.First_Index
        .. Self.Model.Categories.Last_Index
      loop
         if Self.Model.Categories.Element (J).Name = Cat then
            return Self.Model.Categories.Element (J);
         end if;
      end loop;

      return New_Category_Node (Self, Cat);
   end Find_Category_Node;

   -----------------------
   -- New_Category_Node --
   -----------------------

   function New_Category_Node
     (Self : not null access Locations_Listener;
      Cat  : Unbounded_String) return Node_Access
   is
      Node : Node_Access;
   begin
      Node := new Node_Record (Node_Category);
      Node.Parent := null;
      Node.Message_Count := 0;
      Node.Name := Cat;
      Self.Model.Categories.Append (Node);
      Self.Model.Node_Added (Node);
      return Node;
   end New_Category_Node;

   ---------------------
   -- File_Note_Index --
   ---------------------

   function File_Note_Index
     (Cat_Node : Node_Access;
      File     : GNATCOLL.VFS.Virtual_File) return Natural is
   begin
      for J in Cat_Node.Children.First_Index .. Cat_Node.Children.Last_Index
      loop
         if Cat_Node.Children.Element (J).File = File then
            return J;
         end if;
      end loop;

      return 0;
   end File_Note_Index;

   ----------------
   -- File_Added --
   ----------------

   overriding procedure File_Added
     (Self    : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Cat_Node : constant Node_Access := Find_Category_Node (Self, Category);
      Node     : Node_Access;
      pragma Unreferenced (Node);
   begin
      Node := New_File_Node (Self, Cat_Node, File);
   end File_Added;

   --------------------
   -- Category_Added --
   --------------------

   overriding procedure Category_Added
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String)
   is
      Node : Node_Access;
      pragma Unreferenced (Node);
   begin
      Node := New_Category_Node (Self, Category);
   end Category_Added;

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
         if Self.Categories.Is_Empty then
            Node := null;

         else
            Node := Self.Categories.First_Element;
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
            Index := Self.Categories.Find_Index (Aux);
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
     (Self     : not null access Locations_Listener;
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Cat_Node  : Node_Access := Find_Category_Node (Self, Category);
      Path  : constant Gtk_Tree_Path := Self.Model.Create_Path (Cat_Node);
      Dummy : Boolean;
      Index : constant Natural := File_Note_Index (Cat_Node, File);
      File_Node : Node_Access;
      pragma Unreferenced (Dummy);

   begin
      --  Find the index of the file node

      Append_Index (Path, Gint (Index) - 1);
      Self.Model.Row_Deleted (Path);

      if Cat_Node.Children.Is_Empty then
         Dummy := Up (Path);
         Self.Model.Row_Has_Child_Toggled
           (Path, Self.Model.Create_Iter (Cat_Node));
      end if;

      File_Node := Cat_Node.Children.Element (Index);
      Cat_Node.Children.Delete (Index);
      Recursive_Free (File_Node);

      --  Remove the category node if it has no entries left

      if Cat_Node.Children.Is_Empty then
         --  Remove the category node
         Dummy := Up (Path);

         Self.Model.Categories.Delete
           (Self.Model.Categories.Find_Index (Cat_Node));

         Recursive_Free (Cat_Node);
         Self.Model.Row_Deleted (Path);

         --  Note: after the call to Row_Deleted above, the locations view
         --  might have been destroyed (if the preference "Auto close Locations
         --  view" is enabled). Therefore, do not make any reference to Self
         --  or Self.Model below this line in this subprogram.
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

         when Message_Column =>
            return Glib.GType_Pointer;

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
            if Indices (J) < Gint (Self.Categories.Length) then
               Node :=
                 Self.Categories.Element (Natural (Indices (J)) + 1);

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
      use Basic_Types;
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
               if Node.File /= No_File then
                  Set_String (Value, String (Node.File.Base_Name));

               else
                  Set_String (Value, "&lt;unknown&gt;");
               end if;

            when Node_Message =>
               if Node.Message.Level = Primary
                 and (Node.Message.Get_Line /= 0
                        or Node.Message.Get_Column /= 0)
               then
                  --  For primary messages, output line:column information and
                  --  text of the message when line:column information is
                  --  available.

                  declare
                     Location : constant String :=
                       Image (Node.Message.Get_Line)
                         & ':' & Image (Natural (Node.Message.Get_Column));
                     Length   : constant Natural :=
                       Integer'Max (0, Location_Padding - Location'Length);

                  begin
                     Set_String
                       (Value,
                        "<b>" & Location & "</b>" & (Length * ' ')
                        & To_String (Node.Message.Get_Markup));
                  end;

               else
                  --  Otherwise output message text only.

                  Set_String
                    (Value,
                     (Location_Padding * ' ')
                     & To_String (Node.Message.Get_Markup));
               end if;
         end case;
      end Set_Node_Markup;

      Node : constant Node_Access := Self.Get_Node (Iter);

      use Commands;
      Action : Action_Item;
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
                    (Value, To_String (Node.Message.Get_Category));
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
                             Element (Position).Message.Get_Weight);
                        Next (Position);
                     end loop;

                     Set_Int (Value, Gint (Maximum));
                  end;

               when Node_Message =>
                  case Node.Message.Level is
                     when Primary =>
                        Set_Int (Value, Gint (Node.Message.Get_Weight));

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
                    (Value, Node.Message.Get_File);
            end case;

         when Line_Column =>
            Init (Value, Glib.GType_Int);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Int (Value, -1);

               when Node_Message =>
                  Set_Int (Value, Gint (Node.Message.Get_Line));
            end case;

         when Column_Column =>
            Init (Value, Glib.GType_Int);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Int (Value, -1);

               when Node_Message =>
                  Set_Int (Value, Gint (Node.Message.Get_Column));
            end case;

         when Text_Column =>
            Init (Value, GType_String);

            case Node.Kind is
               when Node_Category =>
                  Set_String (Value, To_String (Node.Name));

               when Node_File =>
                  if Node.File /= No_File then
                     Set_String (Value, String (Node.File.Base_Name));

                  else
                     Set_String (Value, "<unknown>");
                  end if;

               when Node_Message =>
                  Set_String (Value, To_String (Node.Message.Get_Text));
            end case;

         when Node_Icon_Column =>
            Init (Value, Gdk.Pixbuf.Get_Type);

            --  Create puxbufs for category and file nodes. It is down here
            --  because icon factory is not customized at message container
            --  initialization time.

            if Self.Category_Pixbuf = null then
               Self.Category_Pixbuf :=
                 Self.Kernel.Get_Main_Window.Render_Icon
                   ("gps-box", Gtk.Enums.Icon_Size_Menu);
            end if;

            if Self.File_Pixbuf = null then
               Self.File_Pixbuf :=
                 Self.Kernel.Get_Main_Window.Render_Icon
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
                        M := N.Message;

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
                         & ":" & Image (N.Message.Get_Line)
                         & ':' & Image (Integer (N.Message.Get_Column))
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
                  Set_Mark (Value, Node.Message.Get_Editor_Mark);
            end case;

         when Action_Pixbuf_Column =>
            Init (Value, Gdk.Pixbuf.Get_Type);

            case Node.Kind is
               when Node_Category | Node_File =>
                  null;

               when Node_Message =>
                  Action := Node.Message.Get_Action;
                  if Action /= null
                    and then Action.Associated_Command /= null
                  then
                     Set_Object (Value, Glib.Object.GObject (Action.Image));
                  end if;
            end case;

         when Action_Command_Column =>
            Init (Value, Glib.GType_Pointer);

            case Node.Kind is
               when Node_Category | Node_File =>
                  null;

               when Node_Message =>
                  Set_Address (Value, To_Address (Node.Message.Get_Action));
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
               Set_Int
                 (Value,
                  Sort_Order_Hint'Pos
                    (Get_Sort_Order_Hint
                       (GPS.Kernel.Messages.Get_Messages_Container
                          (Self.Kernel),
                        To_String (Category_Node.Name))));
            end;

         when Message_Column =>
            Init (Value, Glib.GType_Pointer);

            case Node.Kind is
               when Node_Category | Node_File =>
                  Set_Address (Value, System.Null_Address);

               when Node_Message =>
                  Set_Address
                    (Value,
                     Message_Conversions.To_Address
                       (Message_Conversions.Object_Pointer (Node.Message)));
            end case;

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
      Kernel    : Kernel_Handle) is
   begin
      Object := new Classic_Tree_Model_Record;
      Object.Kernel := Kernel;
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
         return not Self.Categories.Is_Empty;
      else
         return not Node.Children.Is_Empty;
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
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class)
   is
      Category_Node  : constant Node_Access :=
                         Find_Category_Node (Self, Message.Get_Category);
      Parent_Message : constant Message_Access := Get_Parent (Message);
      Node           : Node_Access;
      Iter           : Gtk_Tree_Iter;
      Path           : Gtk_Tree_Path;
      Dummy          : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Node := new Node_Record (Node_Message);

      if Parent_Message = null then
         Node.Parent := Find_File_Node (Self, Category_Node, Message.Get_File);
         Node.Parent.Message_Count := Node.Parent.Message_Count + 1;
         Node.Parent.Parent.Message_Count :=
           Node.Parent.Parent.Message_Count + 1;

         --  Notify view about changes in category and file nodes.

         Iter := Self.Model.Create_Iter (Node.Parent);
         Path := Self.Model.Create_Path (Node.Parent);
         Self.Model.Row_Changed (Path, Iter);

         Iter := Self.Model.Parent (Iter);
         Dummy := Up (Path);
         Self.Model.Row_Changed (Path, Iter);
         Path_Free (Path);

      else
         Node.Parent := Find_Node (Self, Parent_Message);
      end if;

      Node.Message := Message_Access (Message);
      Node.Parent.Children.Append (Node);

      Node_Added (Self.Model, Node);
   end Message_Added;

   ------------------------------
   -- Message_Property_Changed --
   ------------------------------

   overriding procedure Message_Property_Changed
     (Self     : not null access Locations_Listener;
      Message  : not null access Abstract_Message'Class;
      Property : String)
   is
      pragma Unreferenced (Property);
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter;

      Node : constant Node_Access :=
        Find_Node (Self, Message_Access (Message));
   begin
      Self.Model.Stamp := Self.Model.Stamp + 1;
      Iter := Self.Model.Create_Iter (Node);
      Path := Self.Model.Create_Path (Node);
      Self.Model.Row_Changed (Path, Iter);
      Path_Free (Path);
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class)
   is
      Node  : Node_Access := Find_Node (Self, Message_Access (Message));
      Path  : constant Gtk_Tree_Path := Self.Model.Create_Path (Node);
      Iter  : Gtk_Tree_Iter;
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Node.Parent.Children.Delete (Node.Parent.Children.Find_Index (Node));

      Self.Model.Row_Deleted (Path);

      Dummy := Up (Path);
      Iter := Self.Model.Create_Iter (Node.Parent);

      if Node.Parent.Kind = Node_File then
         Node.Parent.Message_Count := Node.Parent.Message_Count - 1;
         Node.Parent.Parent.Message_Count :=
           Node.Parent.Parent.Message_Count - 1;

         if Node.Parent.Children.Is_Empty then
            Self.Model.Row_Has_Child_Toggled (Path, Iter);
         end if;

         --  Notify view about changes in category and file nodes.

         Self.Model.Row_Changed (Path, Iter);

         Iter := Self.Model.Parent (Iter);
         Dummy := Up (Path);
         Self.Model.Row_Changed (Path, Iter);

      elsif Node.Parent.Children.Is_Empty then
         Self.Model.Row_Has_Child_Toggled (Path, Iter);
      end if;

      Recursive_Free (Node);
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
   begin
      if Node = null then
         return Gint (Self.Categories.Length);

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
      Position : Node_Vectors.Cursor;
      Node     : Node_Access := Self.Get_Node (Iter);

   begin
      if Node /= null then
         if Node.Parent = null then
            if Node.Kind /= Node_Category then
               raise Program_Error;
            end if;

            Position := Self.Categories.Find (Node);

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

      Self.Stamp := Self.Stamp + 1;
      Iter := Self.Create_Iter (Node);
      Self.Row_Inserted (Path, Iter);

      Dummy := Up (Path);
      Iter := Self.Parent (Iter);

      if Node.Parent /= null
        and then Node.Parent.Children.Length = 1
      then
         Self.Row_Has_Child_Toggled (Path, Iter);
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

   begin
      if Node = null then
         if N < Gint (Self.Categories.Length) then
            Node := Self.Categories.Element (Integer (N));
         end if;
      else
         if N < Gint (Node.Children.Length) then
            Node := Node.Children.Element (Integer (N));
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

   --------------
   -- Register --
   --------------

   function Register
     (Kernel : Kernel_Handle) return Locations_Listener_Access
   is
      L : Locations_Listener_Access;
   begin
      L := new Locations_Listener;
      Gtk_New (L.Model, Kernel);
      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (L), (Editor_Side => False,
                               GPS.Kernel.Messages.Locations => True));
      return L;
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Kernel : Kernel_Handle;
      L      : in out Locations_Listener_Access)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Locations_Listener'Class, Locations_Listener_Access);

      Cat_Node  : Node_Access;

   begin
      --  Unregister the listener
      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (L));

      --  Free memory stored in the container

      for Cat_Ind in L.Model.Categories.First_Index ..
        L.Model.Categories.Last_Index
      loop
         Cat_Node := L.Model.Categories.Element (Cat_Ind);

         Recursive_Free (Cat_Node);
      end loop;

      --  Destroy the model
      Unref (L.Model);

      Unchecked_Free (L);
   end Unregister;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (L : Locations_Listener_Access)
      return Gtk.Tree_Model.Gtk_Tree_Model is
   begin
      return Gtk_Tree_Model (L.Model);
   end Get_Model;

end GPS.Location_View.Listener;
