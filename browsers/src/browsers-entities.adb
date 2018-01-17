------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Xref;             use GNATCOLL.Xref;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Window;                use Gdk.Window;

with Glib;                      use Glib;
with Glib.Main;
with Glib.Object;               use Glib.Object;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Enums;               use Pango.Enums;
with Pango.Font;                use Pango.Font;

with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views;  use Gtkada.Canvas_View.Views;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;

with Basic_Types;               use Basic_Types;
with Browsers.Canvas;           use Browsers.Canvas;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Generic_Views;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;
with Xref;                      use Xref;

package body Browsers.Entities is

   package Entity_Arrays is new Ada.Containers.Indefinite_Doubly_Linked_Lists
      (Root_Entity'Class);
   use Entity_Arrays;

   UML_Abstract : constant String := "{Abstract}";
   UML_Generic  : constant String := "{Generic}";
   --  String used in UML to indicate that an entity is abstract

   Show_Qualified_Name : Boolean_Preference;

   ------------------
   -- Type browser --
   ------------------

   type Type_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Primitive_Button : Gdk.Pixbuf.Gdk_Pixbuf;
      Idle_Id          : Glib.Main.G_Source_Id := 0;

      Compartment_Title : Drawing_Style;
   end record;
   --  See inherited documentation

   overriding procedure Create_Menu
     (View    : not null access Type_Browser_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding function Load_From_XML
     (Self : not null access Type_Browser_Record;
      Node : XML_Utils.Node_Ptr) return access GPS_Item_Record'Class;
   overriding procedure Load_From_XML
     (Self     : not null access Type_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class);
   overriding procedure Preferences_Changed
     (Self : not null access Type_Browser_Record;
      Pref : Default_Preferences.Preference);

   function Initialize
     (View   : access Type_Browser_Record'Class)
      return Gtk_Widget;
   --  Creates the dependency browser and returns the focus widget

   package Entities_Views is new Generic_Views.Simple_Views
     (Module_Name            => Entity_Browser_Module_Name,
      View_Name              => -"Entity Browser",
      Formal_View_Record     => Type_Browser_Record,
      Formal_MDI_Child       => Browser_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Default);
   subtype Type_Browser is Entities_Views.View_Access;

   type Compartment_Item_Record is
     new Rect_Item_Record and Clickable_Item
   with record
      Folded : Boolean := False;

      Col1   : Items_Lists.List;  --  will have the same width
      Col2   : Items_Lists.List;  --  will have the same width
      --  The contents of these lists are not direct children of the
      --  compartment, but are contained within rect_item (one per line)

      W1, W2 : Gdouble;  --  size request for each column
   end record;
   type Compartment_Item is access all Compartment_Item_Record'Class;

   overriding procedure On_Click
     (Self    : not null access Compartment_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);
   overriding procedure Size_Request
     (Self    : not null access Compartment_Item_Record;
      Context : Draw_Context);

   ---------------
   -- Type item --
   ---------------

   type Type_Item_Record is new GPS_Item_Record with record
      Entity     : Root_Entity_Ref;

      Attrs, Ops : Compartment_Item;
      --  Those are children of the type_item, so destroyed automatically when
      --  the item is destroyed.
   end record;
   type Type_Item is access all Type_Item_Record'Class;

   overriding function Save_To_XML
     (Self : not null access Type_Item_Record)
      return XML_Utils.Node_Ptr;
   overriding procedure Set_Context
     (Item    : not null access Type_Item_Record;
      Context : in out Selection_Context);

   procedure Add_Attrs
     (Self   : not null access Type_Item_Record'Class;
      Child  : access Container_Item_Record'Class;
      Folded : Boolean := False);
   procedure Add_Ops
     (Self   : not null access Type_Item_Record'Class;
      Child  : access Container_Item_Record'Class;
      Folded : Boolean := True);
   --  Add a child to each of the compartments.
   --  The compartment is created as needed, using the specified Folded state.
   --  If the Child is null, the compartment is created but no child added.

   procedure Reset (Self : not null access Type_Item_Record'Class);
   --  Recompute what an item should display

   procedure Reset_All_Items (Browser : access Gtk_Widget_Record'Class);
   --  Redraw all items

   -----------
   -- Links --
   -----------

   type Entity_Link_Record is new GPS_Link_Record with record
      Parent_Link  : Boolean := False;
      Name         : Unbounded_String;
   end record;
   type Entity_Link is access all Entity_Link_Record'Class;

   overriding procedure Save_To_XML
     (Self : not null access Entity_Link_Record;
      Node : not null XML_Utils.Node_Ptr);

   ----------
   -- Misc --
   ----------

   procedure Find_Or_Create_Item
     (Browser     : not null access Type_Browser_Record'Class;
      Entity      : Root_Entity'Class;
      Item        : out Type_Item;
      Newly_Added : out Boolean);
   --  Create (or return an existing) item displaying the information for
   --  Entity. The position of the item is not computed

   procedure Add_And_Layout
     (Self   : not null access Type_Browser_Record'Class;
      Entity : Root_Entity'Class);
   --  Similar to Find_Or_Create_Item, but recompute the whole layout of the
   --  canvas to add the item.

   procedure Add_Link
     (Item         : not null access Type_Item_Record'Class;
      Item2        : not null access Type_Item_Record'Class;
      Link_Name    : String;
      Parent_Link  : Boolean);
   --  Create a new item displaying the information for Entity, and link it
   --  with Item.
   --  If Parent_Link is true, then the link used is a Parent_Link_Record.

   type Entity_Ref_Record is new Text_Item_Record and Clickable_Item
   with record
      Entity    : Root_Entity_Ref;
      Link_Name : Unbounded_String;
   end record;
   type Entity_Ref is access Entity_Ref_Record'Class;
   function Gtk_New_Entity
     (Self      : not null access Type_Browser_Record'Class;
      Entity    : Root_Entity'Class;
      Link_Name : String := "") return Entity_Ref;
   overriding procedure On_Click
     (Self    : not null access Entity_Ref_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);
   --  A text item that represents an entity. Clicking on it will display the
   --  entity in the browser, with a link from

   procedure Show_Entity_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for this module (in the shell window)

   procedure Add_Primitive_Operations
     (Item   : access Type_Item_Record'Class);
   --  Add the sorted list of primitive operations for Entity at the end of
   --  Meth_Layout.

   procedure Add_Parameters (Item : access Type_Item_Record'Class);
   --  Add the list of parameters for a subprogram entity to the end of
   --  an cross-reference list.

   procedure Add_Fields (Item : not null access Type_Item_Record'Class);
   --  Add the list of fields for a record-like entity to the end of
   --  a cross-reference list.
   --  This is also usable to get the enumeration literals for an enumeration
   --  type.

   procedure Add_Package_Contents
     (Item : not null access Type_Item_Record'Class);
   --  Add the parent package information for an entity at the end of its
   --  cross-reference lists.

   type Show_Parents_Button is new Left_Arrow_Record with null record;
   overriding procedure On_Click
     (Self    : not null access Show_Parents_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   type Show_Children_Button is new Right_Arrow_Record with null record;
   overriding procedure On_Click
     (Self    : not null access Show_Children_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   procedure Add_Type
     (Item   : not null access Type_Item_Record'Class;
      Entity : Root_Entity'Class;
      Prefix : String);
   --  Add a new line in the item, starting with "prefix : " and followed by an
   --  hyper link for the type of Entity.

   procedure Add_Array_Type
     (Item       : not null access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Prefix     : String);
   --  Add the information for an array type

   procedure Add_Access_Type
     (Item       : not null access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Prefix     : String);
   --  Add the information for an access type

   procedure Add_Subprogram
     (Item   : not null access Type_Item_Record'Class;
      Entity : Root_Entity'Class);
   --  Add a line that describes the subprogram Entity.

   procedure Sort
     (Db  : access General_Xref_Database_Record'Class;
      Arr : in out Entity_Arrays.List);
   --  Sort the array alphabetically

   type Examine_Entity_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Examine_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -----------------
   -- Save_To_XML --
   -----------------

   overriding function Save_To_XML
     (Self : not null access Type_Item_Record)
      return XML_Utils.Node_Ptr
   is
      Decl : constant General_Entity_Declaration :=
        Self.Entity.Element.Get_Declaration;
      N    : constant XML_Utils.Node_Ptr := new XML_Utils.Node;

   begin
      N.Tag := new String'("entity");
      XML_Utils.Set_Attribute (N, "name", To_String (Decl.Name));
      XML_Utils.Set_Attribute (N, "file", Decl.Loc.File.Display_Full_Name);
      XML_Utils.Set_Attribute (N, "line", Decl.Loc.Line'Img);
      XML_Utils.Set_Attribute (N, "col",  Decl.Loc.Column'Img);

      return N;
   end Save_To_XML;

   -----------------
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (Self : not null access Entity_Link_Record;
      Node : not null XML_Utils.Node_Ptr) is
   begin
      XML_Utils.Set_Attribute (Node, "name", To_String (Self.Name));

      if Self.Parent_Link then
         XML_Utils.Set_Attribute (Node, "parent", "1");
      end if;
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding function Load_From_XML
     (Self : not null access Type_Browser_Record;
      Node : XML_Utils.Node_Ptr) return access GPS_Item_Record'Class
   is
      E : constant Root_Entity'Class :=
        Self.Kernel.Databases.Get_Entity
          (Name => XML_Utils.Get_Attribute (Node, "name"),
           Loc  =>
             (File    => Create (+XML_Utils.Get_Attribute (Node, "file")),
              Project_Path => <>,
              Line    =>
                Integer'Value (XML_Utils.Get_Attribute (Node, "line")),
              Column  =>
                Visible_Column_Type'Value
                  (XML_Utils.Get_Attribute (Node, "col"))));
      It : Type_Item;
      Newly_Added : Boolean;
   begin
      Self.Find_Or_Create_Item (E, It, Newly_Added);
      return It;
   end Load_From_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (Self     : not null access Type_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class)
   is
      pragma Unreferenced (Self);

   begin
      Add_Link
        (Type_Item (From), Type_Item (To),
         Link_Name   => XML_Utils.Get_Attribute (Node, "name"),
         Parent_Link => XML_Utils.Get_Attribute (Node, "parent") = "1");
   end Load_From_XML;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Type_Browser_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Sep   : Gtk_Separator_Menu_Item;
   begin
      General_Browser_Record (View.all).Create_Menu (Menu);  --  inherited

      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, View.Kernel, Show_Qualified_Name);
   end Create_Menu;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Compartment_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      Item : constant Type_Item := Type_Item (Details.Toplevel_Item);
   begin
      if Details.Event_Type = Double_Click then
         Self.Folded := not Self.Folded;
         Item.Reset;
         View.Model.Refresh_Layout;  --  position of links
      end if;
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Entity_Ref_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (View);
      Item     : constant Type_Item := Type_Item (Details.Toplevel_Item);
      B        : constant Type_Browser := Type_Browser (Item.Browser);
      New_Item : Type_Item;
      Added    : Boolean;
      Items    : Items_Lists.List;
   begin
      Find_Or_Create_Item (B, Self.Entity.Element, New_Item, Added);
      Add_Link
        (Item        => Item,
         Item2       => New_Item,
         Link_Name   => To_String (Self.Link_Name),
         Parent_Link => False);

      if Added then
         Items.Append (Abstract_Item (New_Item));
         Insert_And_Layout_Items
           (B.Get_View,
            Ref       => Item,
            Items     => Items,
            Direction => (if B.Horizontal_Layout then Right else Down),
            Space_Between_Items  => Default_Space_Between_Items,
            Space_Between_Layers => Default_Space_Between_Layers,
            Duration             => 0.3);
      else
         B.Get_View.Model.Refresh_Layout;  --  for the link
      end if;
   end On_Click;

   --------------------
   -- Add_And_Layout --
   --------------------

   procedure Add_And_Layout
     (Self   : not null access Type_Browser_Record'Class;
      Entity : Root_Entity'Class)
   is
      Ignore  : Type_Item;
      Added   : Boolean;
      pragma Unreferenced (Ignore);
   begin
      Self.Find_Or_Create_Item
        (Entity      => Entity,
         Item        => Ignore,
         Newly_Added => Added);

      if Added then
         Self.Refresh_Layout
           (Rescale              => False,
            Space_Between_Items  => Default_Space_Between_Items,
            Space_Between_Layers => Default_Space_Between_Layers);
      end if;
   end Add_And_Layout;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Examine_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      View : constant Type_Browser := Entities_Views.Get_Or_Create_View
        (Get_Kernel (Context.Context));
      pragma Unreferenced (Command);
   begin
      Add_And_Layout (View, Get_Entity (Context.Context));
      return Commands.Success;
   end Execute;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   overriding procedure Preferences_Changed
     (Self : not null access Type_Browser_Record;
      Pref : Default_Preferences.Preference)
   is
   begin
      if Pref = null
        or else Pref = Preference (Show_Qualified_Name)
      then
         Reset_All_Items (Self);
      end if;
   end Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Entities_Views.Register_Module (Kernel);

      Show_Qualified_Name := Kernel.Get_Preferences.Create_Invisible_Pref
        ("browser-entities-qualified-names", False,
         Label => -"Show qualified names",
         Doc  => -"Show a fully qualified name for title bars.");

      Register_Action
        (Kernel, "Browser: examine entity",
         Command     => new Examine_Entity_Command,
         Description =>
           "Open the entity Browser to show details on the selected entity",
         Filter     => not Create (Module => Entities_Views.M_Name)
             and Lookup_Filter (Kernel, "Entity"),
         Category  => -"Views");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Browsers/Examine entity %e",
         Action => "Browser: examine entity");

      Kernel.Scripts.Register_Command
        ("show",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("documentation",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Show_Entity_Command_Handler'Access,
         Params       => (2 => Param ("include_inherited", Optional => True)));
   end Register_Module;

   ---------------------------------
   -- Show_Entity_Command_Handler --
   ---------------------------------

   procedure Show_Entity_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Entity : constant Root_Entity'Class := Get_Data (Data, 1);
   begin
      if Entity /= No_Root_Entity then
         if Command = "show" then
            Add_And_Layout
              (Entities_Views.Get_Or_Create_View (Kernel, Focus => True),
               Entity => Entity);

         elsif Command = "documentation" then
            declare
               Extended : constant Boolean := Nth_Arg (Data, 2, False);
            begin
               Set_Return_Value
                 (Data,
                  Documentation
                    (Kernel.Databases,
                     Kernel.Get_Language_Handler,
                     Entity => Entity,
                     Raw_Format => not Extended));
            end;
         end if;
      end if;
   end Show_Entity_Command_Handler;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Type_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Browsers.Canvas.Initialize (View);
      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View);

      View.Compartment_Title := Gtk_New
        (Font  => (Name   => From_String ("sans bold 9"),
                   Halign => Pango.Enums.Pango_Align_Center,
                   others => <>));

      return Gtk_Widget (View.Get_View);
   end Initialize;

   ----------
   -- Sort --
   ----------

   procedure Sort
     (Db  : access General_Xref_Database_Record'Class;
      Arr : in out Entity_Arrays.List)
   is
      pragma Unreferenced (Db);
      function Lt (Op1, Op2 : Root_Entity'Class) return Boolean;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Root_Entity'Class) return Boolean is
      begin
         return Cmp (Op1, Op2) < 0;
      end Lt;

      package Do_Sort is new Entity_Arrays.Generic_Sorting (Lt);
   begin
      Do_Sort.Sort (Arr);
   end Sort;

   ------------------------------
   -- Add_Primitive_Operations --
   ------------------------------

   procedure Add_Primitive_Operations (Item : access Type_Item_Record'Class) is
      B    : constant Type_Browser := Type_Browser (Item.Browser);

      use Entity_Arrays;
      Methods : Xref.Entity_Array :=
        Item.Entity.Element.Methods (Include_Inherited => False);

      Arr  : Entity_Arrays.List;
   begin
      --  Store all primitive operations in an array, so that we can display
      --  them sorted, and possibly filter them out.
      --  ??? Not very efficient, since we already have such an array.

      for M in Methods'Range loop
         Append (Arr, Methods (M).all);
      end loop;

      Free (Methods);

      Sort (B.Get_Kernel.Databases, Arr);

      for A of Arr loop
         if A /= No_Root_Entity then
            Add_Subprogram (Item, A);
         end if;
      end loop;
   end Add_Primitive_Operations;

   --------------------
   -- Gtk_New_Entity --
   --------------------

   function Gtk_New_Entity
     (Self      : not null access Type_Browser_Record'Class;
      Entity    : Root_Entity'Class;
      Link_Name : String := "") return Entity_Ref
   is
      S   : constant access Browser_Styles := Self.Get_View.Get_Styles;
      Ent : constant Entity_Ref := new Entity_Ref_Record;
   begin
      Ent.Entity.Replace_Element (Entity);
      Ent.Link_Name := To_Unbounded_String (Link_Name);
      Ent.Initialize_Text (S.Hyper_Link, Entity.Get_Name);
      return Ent;
   end Gtk_New_Entity;

   ---------------
   -- Add_Attrs --
   ---------------

   procedure Add_Attrs
     (Self   : not null access Type_Item_Record'Class;
      Child  : access Container_Item_Record'Class;
      Folded : Boolean := False)
   is
      B : constant Type_Browser := Type_Browser (Self.Browser);
   begin
      if Self.Attrs = null then
         Self.Attrs := new Compartment_Item_Record;
         Self.Attrs.Folded := Folded;
         Self.Attrs.Initialize_Rect (Self.Browser.Get_View.Get_Styles.Nested);
         Self.Add_Child (Self.Attrs);

         if Folded then
            Self.Attrs.Add_Child
              (Gtk_New_Text
                 (B.Compartment_Title, "attributes (double-click to view)"));
         end if;
      end if;

      if Child /= null and then not Self.Attrs.Folded then
         Self.Attrs.Add_Child
           (Child, Margin => (1.0, 4.0, 0.0, 10.0));
      end if;
   end Add_Attrs;

   -------------
   -- Add_Ops --
   -------------

   procedure Add_Ops
     (Self   : not null access Type_Item_Record'Class;
      Child  : access Container_Item_Record'Class;
      Folded : Boolean := True)
   is
      B : constant Type_Browser := Type_Browser (Self.Browser);
   begin
      if Self.Ops = null then
         Self.Ops := new Compartment_Item_Record;
         Self.Ops.Folded := Folded;
         Self.Ops.Initialize_Rect (Self.Browser.Get_View.Get_Styles.Nested);
         Self.Add_Child (Self.Ops);

         if Folded then
            Self.Ops.Add_Child
              (Gtk_New_Text
                 (B.Compartment_Title, "operations (double-click to view)"));
         end if;
      end if;

      if Child /= null and then not Self.Ops.Folded then
         Self.Ops.Add_Child
           (Child, Margin => (1.0, 4.0, 0.0, 10.0));
      end if;
   end Add_Ops;

   --------------------
   -- Add_Subprogram --
   --------------------

   procedure Add_Subprogram
     (Item   : not null access Type_Item_Record'Class;
      Entity : Root_Entity'Class)
   is
      B    : constant Type_Browser := Type_Browser (Item.Browser);
      S    : constant access Browser_Styles := B.Get_View.Get_Styles;
      Rect : constant Rect_Item := Gtk_New_Rect (S.Invisible);
   begin
      Rect.Set_Child_Layout (Horizontal_Stack);
      Rect.Add_Child (Gtk_New_Entity (B, Entity));
      Rect.Add_Child
        (Gtk_New_Text (S.Text_Font, " (" & Entity.Get_Display_Kind & ")"));
      Item.Add_Ops (Rect);
   end Add_Subprogram;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters (Item : access Type_Item_Record'Class) is
      B        : constant Type_Browser := Type_Browser (Item.Browser);
      S        : constant access Browser_Styles := B.Get_View.Get_Styles;
      Params   : Parameter_Array := Item.Entity.Element.Parameters;
      Returned : constant Root_Entity'Class :=
        Item.Entity.Element.Returned_Type;
      Rect  : Rect_Item;
   begin
      for P in Params'Range loop
         declare
            Parameter : constant Root_Entity'Class := Params (P).Parameter.all;
            Name      : constant String := Parameter.Get_Name;
         begin
            Rect := Gtk_New_Rect (S.Invisible);
            Rect.Set_Child_Layout (Horizontal_Stack);
            Rect.Add_Child
              (Gtk_New_Text
                 (S.Text_Font, Name & " : " & Image (Params (P).Kind)));
            Item.Add_Attrs (Rect);

            --  In some cases, access parameters reference their pointed type
            --  through Pointed_Type. However, if that access type is a
            --  renaming of another type, we'll need to try Get_Variable_Type
            --  if Pointed_Type didn't return anything.

            if Params (P).Kind = Access_Parameter then
               declare
                  E : constant Root_Entity'Class := Parameter.Pointed_Type;
               begin
                  if E = No_Root_Entity then
                     Rect.Add_Child
                       (Gtk_New_Entity (B, Parameter.Get_Type_Of));
                  else
                     Rect.Add_Child (Gtk_New_Entity (B, E));
                  end if;
               end;
            else
               Rect.Add_Child (Gtk_New_Entity (B, Parameter.Get_Type_Of));
            end if;

         end;
      end loop;

      Free (Params);

      if Returned /= No_Root_Entity then
         Rect := Gtk_New_Rect (S.Invisible);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, "return "));
         Rect.Add_Child (Gtk_New_Entity (B, Returned));
         Item.Add_Attrs (Rect);
      end if;
   end Add_Parameters;

   --------------------------
   -- Add_Package_Contents --
   --------------------------

   procedure Add_Package_Contents
     (Item : not null access Type_Item_Record'Class)
   is
      use Entity_Arrays;
      B      : constant Type_Browser := Type_Browser (Item.Browser);
      S      : constant access Browser_Styles := B.Get_View.Get_Styles;
      Parent : constant Root_Entity'Class :=
        Item.Entity.Element.Parent_Package;
      Arr    : Entity_Arrays.List;
      Rect  : Rect_Item;
   begin
      if Parent /= No_Root_Entity then
         Rect := Gtk_New_Rect (S.Invisible);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, "Parent: "));
         Rect.Add_Child (Gtk_New_Entity (B, Parent));
         Item.Add_Attrs (Rect);
      end if;

      declare
         Iter : Abstract_Entities_Cursor'Class := Get_All_Called_Entities
           (Item.Entity.Element);
      begin
         while not At_End (Iter) loop
            declare
               Called : constant Root_Entity'Class := Get (Iter);
            begin
               if Called /= No_Root_Entity then
                  Append (Arr, Called);
               end if;

               Next (Iter);
            end;
         end loop;
      end;

      Sort (B.Get_Kernel.Databases, Arr);

      for Current of Arr loop
         if Is_Subprogram (Current) then
            Add_Subprogram (Item, Current);

         elsif Is_Type (Current) then
            Rect := Gtk_New_Rect (S.Invisible);
            Rect.Set_Child_Layout (Horizontal_Stack);
            Rect.Add_Child (Gtk_New_Entity (B, Current));
            Rect.Add_Child (Gtk_New_Text (S.Text_Font, " (type)"));
            Item.Add_Attrs (Rect);

         --  We want to show variables declared in this package, but not the
         --  parameters to subprograms.

         else
            declare
               Subp : constant Root_Entity'Class := Is_Parameter_Of (Current);
            begin
               if Subp = No_Root_Entity
                 and then Current.Caller_At_Declaration = Item.Entity.Element
               then
                  Item.Add_Type (Current, Prefix => Current.Get_Name);
               end if;
            end;
         end if;
      end loop;
   end Add_Package_Contents;

   ----------------
   -- Add_Fields --
   ----------------

   procedure Add_Fields (Item : not null access Type_Item_Record'Class) is
      B        : constant Type_Browser := Type_Browser (Item.Browser);
      S        : constant access Browser_Styles := B.Get_View.Get_Styles;
      Literals : Xref.Entity_Array := Item.Entity.Element.Literals;

   begin
      if Literals'Length /= 0 then
         for F in Literals'Range loop
            Item.Add_Attrs
              (Gtk_New_Text (S.Text_Font, Get_Name (Literals (F).all)));
         end loop;

      else
         declare
            Fields : Xref.Entity_Array := Item.Entity.Element.Fields;
            Discrs : Xref.Entity_Array := Item.Entity.Element.Discriminants;
         begin
            for D in Discrs'Range loop
               Item.Add_Type
                 (Discrs (D).all,
                  -"Discriminant: " & Get_Name (Discrs (D).all));
            end loop;

            for F in Fields'Range loop
               Item.Add_Type (Fields (F).all, Get_Name (Fields (F).all));
            end loop;

            Free (Fields);
            Free (Discrs);
         end;
      end if;

      Free (Literals);
   end Add_Fields;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type
     (Item   : not null access Type_Item_Record'Class;
      Entity : Root_Entity'Class;
      Prefix : String)
   is
      B     : constant Type_Browser := Type_Browser (Item.Browser);
      S     : constant access Browser_Styles := B.Get_View.Get_Styles;
      Typ   : constant Root_Entity'Class := Entity.Get_Type_Of;
      Rect  : Rect_Item;
      E1, E2 : access Container_Item_Record'Class;
   begin
      if Typ = No_Root_Entity then
         --  Special handling for anonymous types, as generated by GNAT

         if Entity.Is_Array then
            Item.Add_Array_Type (Entity, Prefix);
         elsif Entity.Is_Access then
            Item.Add_Access_Type (Entity, Prefix & " : ");
         else
            Item.Add_Attrs
              (Gtk_New_Text (S.Text_Font, Prefix & " : <anonymous>"));
         end if;

      else
         Rect := Gtk_New_Rect (S.Invisible);
         Rect.Set_Child_Layout (Horizontal_Stack);

         E1 := Gtk_New_Text (S.Text_Font, Prefix & " : ");
         Rect.Add_Child (E1);
         E2 := Gtk_New_Entity (B, Typ, Prefix);
         Rect.Add_Child (E2);
         Item.Add_Attrs (Rect);
         Item.Attrs.Col1.Append (E1);
         Item.Attrs.Col2.Append (E2);
      end if;
   end Add_Type;

   --------------------
   -- Add_Array_Type --
   --------------------

   procedure Add_Array_Type
     (Item       : not null access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Prefix     : String)
   is
      B       : constant Type_Browser := Type_Browser (Item.Browser);
      S       : constant access Browser_Styles := B.Get_View.Get_Styles;
      Typ     : constant Root_Entity'Class := Entity.Component_Type;
      Indexes : Xref.Entity_Array := Entity.Index_Types;
      Rect    : Rect_Item;
   begin
      if Typ /= No_Root_Entity then
         Rect := Gtk_New_Rect (S.Invisible);
         Item.Add_Attrs (Rect);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " : array "));

         for Ind in Indexes'Range loop
            if Ind = Indexes'First then
               Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " ("));
            else
               Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " "));
            end if;

            Rect.Add_Child (Gtk_New_Entity (B, Indexes (Ind).all));

            if Ind = Indexes'Last then
               Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & ")"));
            else
               Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & ","));
            end if;
         end loop;

         Free (Indexes);

         Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " of "));
         Rect.Add_Child (Gtk_New_Entity (B, Typ));
      else
         Item.Add_Attrs (Gtk_New_Text (S.Text_Font, Prefix & " : ???"));
      end if;
   end Add_Array_Type;

   ---------------------
   -- Add_Access_Type --
   ---------------------

   procedure Add_Access_Type
     (Item       : not null access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Prefix     : String)
   is
      B   : constant Type_Browser := Type_Browser (Item.Browser);
      S   : constant access Browser_Styles := B.Get_View.Get_Styles;
      Typ : constant Root_Entity'Class := Pointed_Type (Entity);
      Rect    : Rect_Item;
   begin
      if Typ /= No_Root_Entity then
         Rect := Gtk_New_Rect (S.Invisible);
         Item.Add_Attrs (Rect);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & "access "));
         Rect.Add_Child (Gtk_New_Entity (B, Typ));
      else
         Item.Add_Attrs (Gtk_New_Text (S.Text_Font, Prefix & "???"));
      end if;
   end Add_Access_Type;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Item         : not null access Type_Item_Record'Class;
      Item2        : not null access Type_Item_Record'Class;
      Link_Name    : String;
      Parent_Link  : Boolean)
   is
      Browser : constant Type_Browser := Type_Browser (Item.Browser);
      Styles  : constant access Browser_Styles := Browser.Get_View.Get_Styles;
      Link     : Entity_Link;
      Label    : Text_Item;
   begin
      if not Browser.Has_Link (Item, Item2) then
         if Link_Name /= "" then
            Label := Gtk_New_Text (Styles.Label, Link_Name);
         end if;

         Link := new Entity_Link_Record;
         Link.Parent_Link := Parent_Link;
         Link.Name := To_Unbounded_String (Link_Name);
         Link.Default_Style :=
           (if Parent_Link then Styles.Link2 else Styles.Link);
         Browser_Model (Browser.Get_View.Model).Add (Link);
         Initialize
           (Link,
            From    => Item,
            To      => Item2,
            Routing => (if Parent_Link then Orthogonal else Curve),
            Label   => Label,
            Style   => Link.Default_Style);
      end if;
   end Add_Link;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Show_Parents_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Self);
      Item     : constant Type_Item := Type_Item (Details.Toplevel_Item);
      B        : constant Type_Browser := Type_Browser (Item.Browser);
      Parents  : Xref.Entity_Array :=
        Parent_Types (Item.Entity.Element, Recursive => False);
      It       : Type_Item;
      Added    : Boolean;
      Items    : Items_Lists.List;
   begin
      for P in Parents'Range loop
         Find_Or_Create_Item (B, Parents (P).all, It, Added);
         Add_Link (It, Item, "", Parent_Link => True);
         if Added then
            Items.Append (Abstract_Item (It));
         end if;
      end loop;

      Insert_And_Layout_Items
        (View,
         Ref       => Item,
         Items     => Items,
         Direction => (if Item.Browser.Horizontal_Layout then Up else Left),
         Space_Between_Items  => Default_Space_Between_Items,
         Space_Between_Layers => Default_Space_Between_Layers,
         Duration  => 0.3);

      Free (Parents);
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Show_Children_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Self);
      Item     : constant Type_Item := Type_Item (Details.Toplevel_Item);
      B        : constant Type_Browser := Type_Browser (Item.Browser);
      Children : Xref.Entity_Array :=
        Child_Types (Item.Entity.Element, Recursive => False);
      Items    : Items_Lists.List;
      It       : Type_Item;
      Added    : Boolean;
   begin
      for C in Children'Range loop
         Find_Or_Create_Item (B, Children (C).all, It, Added);
         Add_Link (Item, It, "", Parent_Link => True);
         if Added then
            Items.Append (Abstract_Item (It));
         end if;
      end loop;

      Insert_And_Layout_Items
        (View,
         Ref       => Item,
         Items     => Items,
         Direction => (if Item.Browser.Horizontal_Layout then Down else Right),
         Space_Between_Items  => Default_Space_Between_Items,
         Space_Between_Layers => Default_Space_Between_Layers,
         Duration             => 0.3);

      Free (Children);
   end On_Click;

   ---------------------
   -- Reset_All_Items --
   ---------------------

   procedure Reset_All_Items (Browser : access Gtk_Widget_Record'Class) is
      B : constant Type_Browser := Type_Browser (Browser);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         Type_Item (Item).Reset;
      end On_Item;
   begin
      B.Get_View.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);
      B.Get_View.Model.Refresh_Layout;
   end Reset_All_Items;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : not null access Type_Item_Record'Class) is
      B : constant Type_Browser := Type_Browser (Self.Browser);
      E : constant Root_Entity'Class := Self.Entity.Element;

      Has_Attrs    : constant Boolean := Self.Attrs /= null;
      Attrs_Folded : constant Boolean :=
        Self.Attrs /= null and then Self.Attrs.Folded;
      Has_Ops      : constant Boolean := Self.Ops /= null;
      Ops_Folded   : constant Boolean :=
        Self.Ops = null or else Self.Ops.Folded;

      Title : Unbounded_String;
   begin
      Self.Clear (B.Get_View.Model);
      Self.Attrs := null;  --  were destroyed
      Self.Ops   := null;

      --  Now compute the contents of the item

      if Show_Qualified_Name.Get_Pref then
         Title := To_Unbounded_String (E.Qualified_Name);
      else
         Title := To_Unbounded_String (E.Get_Name);
      end if;

      if E.Is_Abstract then
         Title :=  UML_Abstract & ASCII.LF & Title;
      end if;
      if E.Is_Generic then
         Title := UML_Generic & ASCII.LF & Title;
      end if;

      --  ??? This meta-type does not provide much information to the user,
      --  since the granularity is that of the ALI file, too coarse.
      --     Append (Title, ASCII.LF & "{"
      --       & Get_Display_Kind (Item.Entity.Element) & "}");

      declare
         Left   : constant access Left_Arrow_Record'Class :=
           new Show_Parents_Button;
         Right  : constant access Right_Arrow_Record'Class :=
           new Show_Children_Button;
      begin
         Setup_Titlebar
           (Self, B,
            Name  => To_String (Title),
            Left  => Left,
            Right => Right);
      end;

      if Has_Attrs then
         Self.Add_Attrs (null, Folded => Attrs_Folded);
      end if;

      if Has_Ops then
         Self.Add_Ops (null, Folded => Ops_Folded);
      end if;

      if not Is_Type (E) then
         Self.Add_Type (E, "of type");

      elsif Is_Access (E) then
         Self.Add_Access_Type (E, "");

      elsif Is_Array (E) then
         Self.Add_Array_Type (E, "");

      elsif Is_Subprogram (E) then
         Self.Add_Parameters;

      elsif Has_Methods (E) then
         Self.Add_Fields;
         Self.Add_Primitive_Operations;

      elsif Is_Container (E) then
         Self.Add_Package_Contents;
         Self.Add_Fields;

      else
         --  Enumerations, in particular
         Self.Add_Fields;
      end if;
   end Reset;

   -------------------------
   -- Find_Or_Create_Item --
   -------------------------

   procedure Find_Or_Create_Item
     (Browser     : not null access Type_Browser_Record'Class;
      Entity      : Root_Entity'Class;
      Item        : out Type_Item;
      Newly_Added : out Boolean)
   is
      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
      begin
         if Item = null and then Type_Item (It).Entity.Element = Entity then
            Item := Type_Item (It);
         end if;
      end On_Item;

      S : constant access Browser_Styles := Browser.Get_View.Get_Styles;
   begin
      Item := null;
      Newly_Added := False;
      Browser.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);

      if Item = null and then Entity /= No_Root_Entity then
         Newly_Added := True;
         Item := new Type_Item_Record;
         Item.Browser := General_Browser (Browser);
         Item.Entity.Replace_Element (Entity);

         Browser_Model (Browser.Get_View.Model).Add (Item);

         Item.Initialize_Rect (Style => S.Item, Radius => 5.0);
         Item.Reset;
      end if;
   end Find_Or_Create_Item;

   -----------------
   -- Set_Context --
   -----------------

   overriding procedure Set_Context
     (Item    : not null access Type_Item_Record;
      Context : in out Selection_Context)
   is
      Loc   : constant General_Location :=
        Get_Declaration (Item.Entity.Element).Loc;
   begin
      Set_Entity_Information
        (Context       => Context,
         Entity        => Item.Entity.Element);
      Set_File_Information
        (Context     => Context,
         Files       => (1 => Loc.File),
         Line        => Loc.Line,
         Column      => Loc.Column);
      --  We need to set the file information, even though it will also display
      --  some contextual menus (file dependencies,...), otherwise the call
      --  graph will not work.
   end Set_Context;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Compartment_Item_Record;
      Context : Draw_Context)
   is
      R      : Item_Rectangle;
   begin
      Rect_Item_Record (Self.all).Size_Request (Context);  --  inherited

      Self.W1 := 0.0;
      Self.W2 := 0.0;

      --  The size of all the children has been computed, we can now compute
      --  the size of the two columns

      for C of Self.Col1 loop
         R := C.Bounding_Box;
         Self.W1 := Gdouble'Max (Self.W1, R.Width);
      end loop;

      for C of Self.Col2 loop
         R := C.Bounding_Box;
         Self.W2 := Gdouble'Max (Self.W2, R.Width);
      end loop;

      R := Self.Bounding_Box;
      Self.Set_Size_Request
        (Width => Gdouble'Max (R.Width, Self.W1 + Self.W2));

      --  And now have each child request the common width

      for C of Self.Col1 loop
         Container_Item (C).Set_Size_Request (Width => Self.W1);
      end loop;

      for C of Self.Col2 loop
         Container_Item (C).Set_Size_Request (Width => Self.W2);
      end loop;
   end Size_Request;

end Browsers.Entities;
