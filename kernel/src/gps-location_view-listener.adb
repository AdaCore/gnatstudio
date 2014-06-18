------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;
with System.Address_To_Access_Conversions;

with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Glib.Object;
with Glib.Values;
with Gtk.Enums;

with Commands;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS;
with GNATCOLL.Xref;
with GPS.Editors.GtkAda;
with GPS.Editors.Line_Information;  use GPS.Editors.Line_Information;
with String_Utils;                  use String_Utils;

package body GPS.Location_View.Listener is

   use type Commands.Command_Access;
   use type Glib.Gint;
   use type GNAT.Strings.String_Access;
   use type GNATCOLL.Xref.Visible_Column;

   package Message_Conversions is
     new System.Address_To_Access_Conversions
       (GPS.Kernel.Messages.Abstract_Message'Class);

   function To_Address is
     new Ada.Unchecked_Conversion
       (GPS.Kernel.Messages.Action_Item, System.Address);

   procedure Find_Category
     (Self     : not null access Locations_Listener'Class;
      Category : String;
      Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Lookup for specified category and set Iter to it's iterator in tree.

   procedure Find_File
     (Self          : not null access Locations_Listener'Class;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Lookup for specified file in specified category.

   procedure Find_Message
     (Self          : not null access Locations_Listener'Class;
      Message       : not null access Abstract_Message'Class;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Lookup for specified message.

   procedure Set
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      To     : GNATCOLL.VFS.Virtual_File);
   --  Sets value of underling model's cell

   function Get
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return GNATCOLL.VFS.Virtual_File;
   --  Gets value of specifid cells

   procedure Set
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      To     : GPS.Editors.Editor_Mark'Class);
   --  Sets value of underling model's cell

   Location_Padding : constant := 10;
   --  Size of field for line:column location information in view

   Non_Leaf_Color_Name : constant String := "blue";
   --  Name of the color to be used for category and file names

   procedure Insert_With_Values
     (Tree_Store : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record;
      Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Position   : Glib.Gint;
      Columns    : Glib.Gint_Array;
      Values     : Glib.Values.GValue_Array);
   --  ??? Must be moved to GtkAda

   --------------------
   -- Category_Added --
   --------------------

   overriding procedure Category_Added
     (Self                     : not null access Locations_Listener;
      Category                 : Ada.Strings.Unbounded.Unbounded_String;
      Allow_Auto_Jump_To_First : Boolean)
   is
      pragma Unreferenced (Allow_Auto_Jump_To_First);

      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      --  Create puxbufs for category nodes. It is down here because icon
      --  factory is not customized at message container initialization time.

      if Self.Category_Pixbuf = null then
         Self.Category_Pixbuf :=
           Self.Kernel.Get_Main_Window.Render_Icon
             ("gps-box", Gtk.Enums.Icon_Size_Menu);
      end if;

      Self.Model.Append (Iter, Gtk.Tree_Model.Null_Iter);

      Self.Model.Set (Iter, Category_Column, To_String (Category));
      Self.Model.Set (Iter, Weight_Column, 0);
      Self.Model.Set (Iter, File_Column, No_File);
      Self.Model.Set (Iter, Line_Column, -1);
      Self.Model.Set (Iter, Column_Column, -1);
      Self.Model.Set (Iter, Text_Column, To_String (Category));
      Self.Model.Set
        (Iter, Node_Icon_Column, Glib.Object.GObject (Self.Category_Pixbuf));
      Self.Model.Set (Iter, Node_Markup_Column, To_String (Category));
      Self.Model.Set (Iter, Node_Tooltip_Column, To_String (Category));
      Self.Model.Set (Iter, Node_Mark_Column, GPS.Editors.Nil_Editor_Mark);
      Self.Model.Set (Iter, Action_Pixbuf_Column, Glib.Object.GObject'(null));
      Self.Model.Set (Iter, Action_Command_Column, System.Null_Address);
      Self.Model.Set (Iter, Action_Tooltip_Column, To_String (Category));
      Self.Model.Set (Iter, Number_Of_Children_Column, 0);
      Self.Model.Set
        (Iter,
         Sort_Order_Hint_Column,
         Sort_Order_Hint'Pos
           (GPS.Kernel.Messages.Get_Messages_Container
              (Self.Kernel).Get_Sort_Order_Hint (To_String (Category))));
      Self.Model.Set (Iter, Message_Column, System.Null_Address);
   end Category_Added;

   ----------------------
   -- Category_Removed --
   ----------------------

   overriding procedure Category_Removed
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String)
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Self.Find_Category (To_String (Category), Iter);
      Self.Model.Remove (Iter);
   end Category_Removed;

   ----------------
   -- File_Added --
   ----------------

   overriding procedure File_Added
     (Self     : not null access Locations_Listener;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      --  Create puxbufs for category and file nodes. It is down here because
      --  icon factory is not customized at message container initialization
      --  time.

      if Self.File_Pixbuf = null then
         Self.File_Pixbuf :=
           Self.Kernel.Get_Main_Window.Render_Icon
             ("gps-file", Gtk.Enums.Icon_Size_Menu);
      end if;

      --  Lookup for iter for parent category row.

      Self.Find_Category (To_String (Category), Category_Iter);

      --  Append row for file.

      Self.Model.Append (Iter, Category_Iter);

      Self.Model.Set (Iter, Category_Column, To_String (Category));
      Self.Model.Set (Iter, Weight_Column, 0);
      Self.Model.Set (Iter, File_Column, File);
      Self.Model.Set (Iter, Line_Column, -1);
      Self.Model.Set (Iter, Column_Column, -1);

      if File /= No_File then
         Self.Model.Set (Iter, Text_Column, String (File.Base_Name));

      else
         Self.Model.Set (Iter, Text_Column, "<unknown>");
      end if;

      Self.Model.Set
        (Iter, Node_Icon_Column, Glib.Object.GObject (Self.File_Pixbuf));

      if File /= No_File then
         Self.Model.Set (Iter, Node_Markup_Column, String (File.Base_Name));

      else
         Self.Model.Set (Iter, Node_Markup_Column, "&lt;unknown&gt;");
      end if;

      Self.Model.Set (Iter, Node_Tooltip_Column, String (File.Base_Name));
      Self.Model.Set (Iter, Node_Mark_Column, GPS.Editors.Nil_Editor_Mark);
      Self.Model.Set (Iter, Action_Pixbuf_Column, Glib.Object.GObject'(null));
      Self.Model.Set (Iter, Action_Command_Column, System.Null_Address);
      Self.Model.Set (Iter, Action_Tooltip_Column, String (File.Base_Name));
      Self.Model.Set (Iter, Number_Of_Children_Column, 0);
      Self.Model.Set
        (Iter,
         Sort_Order_Hint_Column,
         Self.Model.Get_Int (Category_Iter, Sort_Order_Hint_Column));
      Self.Model.Set (Iter, Message_Column, System.Null_Address);
   end File_Added;

   ------------------
   -- File_Removed --
   ------------------

   overriding procedure File_Removed
     (Self     : not null access Locations_Listener;
      Category : Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Self.Find_File (To_String (Category), File, Category_Iter, File_Iter);
      Self.Model.Remove (File_Iter);
   end File_Removed;

   -------------------
   -- Find_Category --
   -------------------

   procedure Find_Category
     (Self     : not null access Locations_Listener'Class;
      Category : String;
      Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Iter := Self.Model.Get_Iter_First;

      while Iter /= Null_Iter loop
         exit when Self.Model.Get_String (Iter, Category_Column) = Category;

         Self.Model.Next (Iter);
      end loop;
   end Find_Category;

   ---------------
   -- Find_File --
   ---------------

   procedure Find_File
     (Self          : not null access Locations_Listener'Class;
      Category      : String;
      File          : GNATCOLL.VFS.Virtual_File;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Self.Find_Category (Category, Category_Iter);

      File_Iter := Self.Model.Children (Category_Iter);

      while File_Iter /= Null_Iter loop
         exit when Self.Model.Get (File_Iter, File_Column) = File;

         Self.Model.Next (File_Iter);
      end loop;
   end Find_File;

   ------------------
   -- Find_Message --
   ------------------

   procedure Find_Message
     (Self          : not null access Locations_Listener'Class;
      Message       : not null access Abstract_Message'Class;
      Category_Iter : out Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type System.Address;

      Parent_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Message.Level = Primary then
         Self.Find_File
           (To_String (Message.Get_Category),
            Message.Get_File,
            Category_Iter,
            File_Iter);
         Parent_Iter := File_Iter;

      else
         Self.Find_Message
           (Message.Get_Parent, Category_Iter, File_Iter, Parent_Iter);
      end if;

      Iter := Self.Model.Children (Parent_Iter);

      while Iter /= Null_Iter loop
         exit when Self.Model.Get_Address (Iter, Message_Column) =
           Message_Conversions.To_Address
             (Message_Conversions.Object_Pointer (Message));

         Self.Model.Next (Iter);
      end loop;
   end Find_Message;

   ---------
   -- Get --
   ---------

   function Get
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return GNATCOLL.VFS.Virtual_File is
   begin
      return GNATCOLL.VFS.GtkAda.Get_File (Self, Iter, Column);
   end Get;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (L : Locations_Listener_Access) return Gtk.Tree_Model.Gtk_Tree_Model is
   begin
      return To_Interface (L.Model);
   end Get_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Object : out Classic_Tree_Model) is
   begin
      Object := new Classic_Tree_Model_Record;
      Initialize (Object);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Classic_Tree_Model_Record'Class) is
      Types : constant Glib.GType_Array :=
        (Glib.Guint (Category_Column)           => Glib.GType_String,
         Glib.Guint (Weight_Column)             => Glib.GType_Int,
         Glib.Guint (File_Column)               =>
           GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type,
         Glib.Guint (Line_Column)               => Glib.GType_Int,
         Glib.Guint (Column_Column)             => Glib.GType_Int,
         Glib.Guint (Text_Column)               => Glib.GType_String,
         Glib.Guint (Node_Icon_Column)          => Gdk.Pixbuf.Get_Type,
         Glib.Guint (Node_Markup_Column)        => Glib.GType_String,
         Glib.Guint (Node_Tooltip_Column)       => Glib.GType_String,
         Glib.Guint (Node_Mark_Column)          =>
           GPS.Editors.GtkAda.Get_Editor_Mark_Type,
         Glib.Guint (Action_Pixbuf_Column)      => Gdk.Pixbuf.Get_Type,
         Glib.Guint (Action_Command_Column)     => Glib.GType_Pointer,
         Glib.Guint (Action_Tooltip_Column)     => Glib.GType_String,
         Glib.Guint (Number_Of_Children_Column) => Glib.GType_Int,
         Glib.Guint (Sort_Order_Hint_Column)    => Glib.GType_Int,
         Glib.Guint (Message_Column)            => Glib.GType_Pointer);

   begin
      Gtk.Tree_Store.Initialize (Self, Types);
   end Initialize;

   ------------------------
   -- Insert_With_Values --
   ------------------------

   procedure Insert_With_Values
      (Tree_Store : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record;
       Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
       Parent     : Gtk.Tree_Model.Gtk_Tree_Iter;
       Position   : Glib.Gint;
       Columns    : Glib.Gint_Array;
       Values     : Glib.Values.GValue_Array)
   is
      procedure Internal
         (Tree_Store : System.Address;
          Iter       : out Gtk.Tree_Model.Gtk_Tree_Iter;
          Parent     : System.Address;
          Position   : Glib.Gint;
          Columns    : not null access Glib.Gint;
          Values     : not null access Glib.Values.GValue;
          N_Values   : Glib.Gint);
      pragma Import (C, Internal, "gtk_tree_store_insert_with_valuesv");
      Tmp_Iter : aliased Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Internal
        (Glib.Object.Get_Object (Tree_Store),
         Tmp_Iter,
         Iter_Or_Null (Parent'Address),
         Position,
         Columns (Columns'First)'Unrestricted_Access,
         Values (Values'First)'Unrestricted_Access,
         Values'Length);
      Iter := Tmp_Iter;
   end Insert_With_Values;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Parent_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;

      Columns : Glib.Gint_Array (1 .. Natural (Total_Columns));
      Values  : Glib.Values.GValue_Array (1 .. Total_Columns);
      Last    : Glib.Gint := 0;

   begin
      if Message.Get_Parent /= null then
         Self.Find_Message
           (Message.Get_Parent, Category_Iter, File_Iter, Parent_Iter);

      else
         Self.Find_File
           (To_String (Message.Get_Category),
            Message.Get_File,
            Category_Iter,
            File_Iter);
         Parent_Iter := File_Iter;
      end if;

      Last := Last + 1;
      Columns (Natural (Last)) := Category_Column;
      Glib.Values.Init (Values (Last), Glib.GType_String);
      Glib.Values.Set_String (Values (Last), To_String (Message.Get_Category));

      Last := Last + 1;
      Columns (Natural (Last)) := Weight_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);

      case Message.Level is
         when Primary =>
            Glib.Values.Set_Int
              (Values (Last), Glib.Gint (Message.Get_Weight));
            Self.Model.Set
              (File_Iter,
               Weight_Column,
               Glib.Gint'Max
                 (Self.Model.Get_Int (File_Iter, Weight_Column),
                  Glib.Gint (Message.Get_Weight)));

         when Secondary =>
            Glib.Values.Set_Int (Values (Last), 0);
      end case;

      Last := Last + 1;
      Columns (Natural (Last)) := File_Column;
      Glib.Values.Init
        (Values (Last), GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type);
      GNATCOLL.VFS.GtkAda.Set_File (Values (Last), Message.Get_File);

      Last := Last + 1;
      Columns (Natural (Last)) := Line_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);
      Glib.Values.Set_Int (Values (Last), Glib.Gint (Message.Get_Line));

      Last := Last + 1;
      Columns (Natural (Last)) := Column_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);
      Glib.Values.Set_Int (Values (Last), Glib.Gint (Message.Get_Column));

      Last := Last + 1;
      Columns (Natural (Last)) := Text_Column;
      Glib.Values.Init (Values (Last), Glib.GType_String);
      Glib.Values.Set_String (Values (Last), To_String (Message.Get_Text));

      Last := Last + 1;
      Columns (Natural (Last)) := Node_Icon_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Object);
      Glib.Values.Set_Object (Values (Last), null);

      Last := Last + 1;
      Columns (Natural (Last)) := Node_Markup_Column;
      Glib.Values.Init (Values (Last), Glib.GType_String);

      if Message.Level = Primary
        and (Message.Get_Line /= 0 or Message.Get_Column /= 0)
      then
         --  For primary messages, output line:column information and text of
         --  the message when line:column information is available.

         declare
            Location : constant String :=
              Image (Message.Get_Line)
                & ':'
                & Image (Natural (Message.Get_Column));
            Length   : constant Natural :=
              Integer'Max (0, Location_Padding - Location'Length);

         begin
            Glib.Values.Set_String
              (Values (Last),
               "<b>" & Location & "</b>" & (Length * ' ')
               & To_String (Message.Get_Markup));
         end;

      else
         --  Otherwise output message text only.

         Glib.Values.Set_String
           (Values (Last),
            (Location_Padding * ' ') & To_String (Message.Get_Markup));
      end if;

      declare
         Markup : Unbounded_String;
         M      : Message_Access := Message_Access (Message);

      begin
         loop
            case M.Level is
               when Primary =>
                  Markup := ASCII.LF & M.Get_Markup & Markup;

                  exit;

               when Secondary =>
                  Markup := ASCII.LF & "  " & M.Get_Markup & Markup;
            end case;

            M := M.Get_Parent;
         end loop;

         Markup :=
           M.Get_Category
           & ASCII.LF
           & String (M.Get_File.Base_Name)
           & ":" & Image (M.Get_Line)
           & ':' & Image (Integer (M.Get_Column))
           & To_String (Markup);

         Last := Last + 1;
         Columns (Natural (Last)) := Node_Tooltip_Column;
         Glib.Values.Init (Values (Last), Glib.GType_String);
         Glib.Values.Set_String (Values (Last), To_String (Markup));
      end;

      Last := Last + 1;
      Columns (Natural (Last)) := Node_Mark_Column;
      Glib.Values.Init
        (Values (Last), GPS.Editors.GtkAda.Get_Editor_Mark_Type);
      GPS.Editors.GtkAda.Set_Mark (Values (Last), Message.Get_Editor_Mark);

      Last := Last + 1;
      Columns (Natural (Last)) := Action_Pixbuf_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Object);

      if Message.Get_Action /= null
        and then Message.Get_Action.Associated_Command /= null
      then
         Glib.Values.Set_Object
           (Values (Last), Glib.Object.GObject (Message.Get_Action.Image));

      else
         Glib.Values.Set_Object (Values (Last), null);
      end if;

      Last := Last + 1;
      Columns (Natural (Last)) := Action_Command_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Pointer);
      Glib.Values.Set_Address (Values (Last), To_Address (Message.Get_Action));

      Last := Last + 1;
      Columns (Natural (Last)) := Action_Tooltip_Column;
      Glib.Values.Init (Values (Last), Glib.GType_String);

      if Message.Get_Action /= null
        and then Message.Get_Action.Tooltip_Text /= null
      then
         Glib.Values.Set_String
           (Values (Last), Message.Get_Action.Tooltip_Text.all);

      else
         Glib.Values.Set_String (Values (Last), "");
      end if;

      Last := Last + 1;
      Columns (Natural (Last)) := Number_Of_Children_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);
                              Glib.Values.Set_Int (Values (Last), 0);

      Last := Last + 1;
      Columns (Natural (Last)) := Sort_Order_Hint_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Int);
      Glib.Values.Set_Int
        (Values (Last),
         Sort_Order_Hint'Pos
           (GPS.Kernel.Messages.Get_Messages_Container
                (Self.Kernel).Get_Sort_Order_Hint
                (To_String (Message.Get_Category))));
      --  XXX Can it be changed dynamically?

      Last := Last + 1;
      Columns (Natural (Last)) := Message_Column;
      Glib.Values.Init (Values (Last), Glib.GType_Pointer);
      Glib.Values.Set_Address
        (Values (Last),
         Message_Conversions.To_Address
           (Message_Conversions.Object_Pointer (Message)));

      --  Create row for the message using prepared data

      Insert_With_Values
        (Gtk.Tree_Store.Gtk_Tree_Store_Record (Self.Model.all)'Access,
         Iter,
         Parent_Iter,
         -1,
         Columns (1 .. Natural (Last)),
         Values (1 .. Last));

      --  Update message counts for category and file row when message is
      --  primary.

      if Message.Level = Primary then
         Self.Model.Set
           (Category_Iter,
            Number_Of_Children_Column,
            Self.Model.Get_Int (Category_Iter, Number_Of_Children_Column) + 1);
         Self.Model.Set
           (File_Iter,
            Number_Of_Children_Column,
            Self.Model.Get_Int (File_Iter, Number_Of_Children_Column) + 1);
      end if;
   end Message_Added;

   ------------------------------
   -- Message_Property_Changed --
   ------------------------------

   overriding procedure Message_Property_Changed
     (Self     : not null access Locations_Listener;
      Message  : not null access Abstract_Message'Class;
      Property : String)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Property = "action" then
         Self.Find_Message (Message, Category_Iter, File_Iter, Iter);

         if Message.Get_Action /= null
           and then Message.Get_Action.Associated_Command /= null
         then
            Self.Model.Set
              (Iter,
               Action_Pixbuf_Column,
               Glib.Object.GObject (Message.Get_Action.Image));

         else
            Self.Model.Set
              (Iter, Action_Pixbuf_Column, Glib.Object.GObject'(null));
         end if;

         Self.Model.Set
           (Iter, Action_Command_Column, To_Address (Message.Get_Action));

         if Message.Get_Action /= null
           and then Message.Get_Action.Tooltip_Text /= null
         then
            Self.Model.Set
              (Iter,
               Action_Tooltip_Column,
               Message.Get_Action.Tooltip_Text.all);

         else
            Self.Model.Set (Iter, Action_Tooltip_Column, "");
         end if;
      end if;
   end Message_Property_Changed;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Locations_Listener;
      Message : not null access Abstract_Message'Class)
   is
      Category_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      File_Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      Self.Find_Message (Message, Category_Iter, File_Iter, Iter);

      Self.Model.Remove (Iter);

      if Message.Level = Primary then
         --  Update message counters

         Self.Model.Set
           (Category_Iter,
            Number_Of_Children_Column,
            Self.Model.Get_Int (Category_Iter, Number_Of_Children_Column) - 1);
         Self.Model.Set
           (File_Iter,
            Number_Of_Children_Column,
            Self.Model.Get_Int (File_Iter, Number_Of_Children_Column) - 1);
      end if;
   end Message_Removed;

   --------------
   -- Register --
   --------------

   function Register
     (Kernel : Kernel_Handle) return Locations_Listener_Access
   is
      Success : Boolean;
      Self    : Locations_Listener_Access;

   begin
      Self := new Locations_Listener;
      Self.Kernel := Kernel;

      --  Allocate foreground color for category and file nodes

      Gdk.RGBA.Parse (Self.Non_Leaf_Color, Non_Leaf_Color_Name, Success);

      --  Create GtkTreeModel

      Gtk_New (Self.Model);

      --  Register listener

      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (Self),
         (Editor_Side => False, GPS.Kernel.Messages.Locations => True));

      return Self;
   end Register;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      To     : GNATCOLL.VFS.Virtual_File)
   is
      Value : Glib.Values.GValue;

   begin
      Glib.Values.Init (Value, GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type);
      GNATCOLL.VFS.GtkAda.Set_File (Value, To);
      Self.Set_Value (Iter, Column, Value);
      Glib.Values.Unset (Value);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : not null access Classic_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      To     : GPS.Editors.Editor_Mark'Class)
   is
      Value : Glib.Values.GValue;

   begin
      Glib.Values.Init (Value, GPS.Editors.GtkAda.Get_Editor_Mark_Type);
      GPS.Editors.GtkAda.Set_Mark (Value, To);
      Self.Set_Value (Iter, Column, Value);
      Glib.Values.Unset (Value);
   end Set;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Kernel : Kernel_Handle;
      Self   : in out Locations_Listener_Access)
   is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation
          (Locations_Listener'Class, Locations_Listener_Access);

   begin
      --  Unregister the listener

      Get_Messages_Container (Kernel).Unregister_Listener
        (Listener_Access (Self));

      --  Destroy the model

      Self.Model.Clear;
      Unref (Self.Model);

      --  Release pixbufs.

      if Self.Category_Pixbuf /= null then
         Unref (Self.Category_Pixbuf);
      end if;

      if Self.File_Pixbuf /= null then
         Unref (Self.File_Pixbuf);
      end if;

      Unchecked_Free (Self);
   end Unregister;

end GPS.Location_View.Listener;
