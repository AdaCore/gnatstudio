------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Xref;             use GNATCOLL.Xref;

with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Window;                use Gdk.Window;

with Glib;                      use Glib;
with Glib.Main;
with Glib.Object;               use Glib.Object;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views;  use Gtkada.Canvas_View.Views;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;

with Browsers.Canvas;           use Browsers.Canvas;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Generic_Views;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;
with Xref;                      use Xref;

package body Browsers.Entities is
   Me : constant Trace_Handle := Create ("Browser.Entities");

   package Entity_Arrays is new Ada.Containers.Indefinite_Doubly_Linked_Lists
      (Root_Entity'Class);
   use Entity_Arrays;

   UML_Abstract : constant String := "{Abstract}";
   UML_Generic  : constant String := "{Generic}";
   --  String used in UML to indicate that an entity is abstract

   ------------------
   -- Type browser --
   ------------------

   type Type_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Primitive_Button : Gdk.Pixbuf.Gdk_Pixbuf;
      Idle_Id          : Glib.Main.G_Source_Id := 0;
   end record;
   --  See inherited documentation

   function Initialize
     (View   : access Type_Browser_Record'Class)
      return Gtk_Widget;
   --  Creates the dependency browser and returns the focus widget

   package Entities_Views is new Generic_Views.Simple_Views
     (Module_Name            => Entity_Browser_Module_Name,
      View_Name              => -"Entity Browser",
      Formal_View_Record     => Type_Browser_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Default);
   subtype Type_Browser is Entities_Views.View_Access;

   ---------------
   -- Type item --
   ---------------

   type Type_Item_Record is new GPS_Item_Record with record
      Entity                : Root_Entity_Ref;
      Might_Have_Primitives : Boolean := True;
      Inherited_Primitives  : Boolean := False;
   end record;
   type Type_Item is access all Type_Item_Record'Class;

   overriding procedure Set_Context
     (Item    : not null access Type_Item_Record;
      Context : in out Selection_Context);

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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      Entities_Views.Register_Module (Kernel);

      Command := new Examine_Entity_Command;

      Register_Contextual_Menu
        (Kernel, "Examine entity",
         Label      => -"Browsers/Examine entity %e",
         Action     => Command,
         Ref_Item   => "Entity called by in browser",
         Add_Before => False,
         Filter     => not Create (Module => Entities_Views.M_Name));
      Register_Command
        (Kernel, "show",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "discriminants",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "fields",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "literals",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "is_predefined",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "parameters",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "methods",
         Class   => Get_Entity_Class (Kernel),
         Params  => (2 => Param ("include_inherited", Optional => True)),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "documentation",
         Class       => Get_Entity_Class (Kernel),
         Handler     => Show_Entity_Command_Handler'Access,
         Params      => (2 => Param ("include_inherited", Optional => True)));
      Register_Command
        (Kernel, "return_type",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "pointed_type",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "derived_types",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "parent_types",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "primitive_of",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "type",
         Class   => Get_Entity_Class (Kernel),
         Handler => Show_Entity_Command_Handler'Access);
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
      Ignore : Type_Item;
      pragma Unreferenced (Ignore);
   begin
      if Entity /= No_Root_Entity then
         if Command = "show" then
            Add_And_Layout
              (Entities_Views.Get_Or_Create_View (Kernel, Focus => True),
               Entity => Entity);

         elsif Command = "discriminants" then
            declare
               Discrs : Xref.Entity_Array := Discriminants (Entity);
            begin
               Set_Return_Value_As_List (Data);

               for D in Discrs'Range loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), Discrs (D).all));
               end loop;

               Free (Discrs);
            end;

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

         elsif Command = "parameters" then
            declare
               Params : constant Parameter_Array := Parameters (Entity);
            begin
               Set_Return_Value_As_List (Data);
               for P in Params'Range loop
                  Set_Return_Value
                    (Data, Create_Entity
                       (Get_Script (Data), Params (P).Parameter));
               end loop;
            end;

         elsif Command = "methods" then
            declare
               Methods : Xref.Entity_Array :=
                 Entity.Methods
                   (Include_Inherited => Nth_Arg (Data, 2, False));
            begin
               Set_Return_Value_As_List (Data);

               for M in Methods'Range loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), Methods (M).all));
               end loop;

               Free (Methods);
            end;

         elsif Command = "return_type" then
            Set_Return_Value
              (Data, Create_Entity (Get_Script (Data), Entity.Returned_Type));

         elsif Command = "primitive_of" then
            declare
               Arr : Xref.Entity_Array := Entity.Is_Primitive_Of;
            begin
               Set_Return_Value_As_List (Data);
               for A in Arr'Range loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), Arr (A).all));
               end loop;

               Free (Arr);
            end;

         elsif Command = "pointed_type" then
            declare
               Result : Root_Entity'Class := Entity.Pointed_Type;
            begin
               if Result = No_Root_Entity then
                  Result := Entity.Get_Type_Of;
                  if Result /= No_Root_Entity then
                     Result := Result.Pointed_Type;
                  end if;
               end if;
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), Result));
            end;

         elsif Command = "type" then
            Set_Return_Value
              (Data, Create_Entity (Get_Script (Data), Entity.Get_Type_Of));

         elsif Command = "is_predefined" then
            Set_Return_Value
              (Data, Entity.Is_Predefined_Entity);

         elsif Command = "fields" then
            declare
               F : Xref.Entity_Array := Entity.Fields;
            begin
               Set_Return_Value_As_List (Data);

               for F2 in F'Range loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), F (F2).all));
               end loop;

               Free (F);
            end;

         elsif Command = "literals" then
            declare
               F : Xref.Entity_Array := Entity.Literals;
            begin
               Set_Return_Value_As_List (Data);

               for F2 in F'Range loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), F (F2).all));
               end loop;

               Free (F);
            end;

         elsif Command = "derived_types" then
            declare
               Children : Xref.Entity_Array :=
                 Entity.Child_Types (Recursive => False);
            begin
               Set_Return_Value_As_List (Data);

               for C in Children'Range loop
                  Set_Return_Value
                    (Data, Create_Entity
                       (Get_Script (Data), Children (C).all));
               end loop;

               Free (Children);
            end;

         elsif Command = "parent_types" then
            declare
               Parents : Xref.Entity_Array :=
                 Entity.Parent_Types (Recursive => False);
            begin
               Set_Return_Value_As_List (Data);

               for C in Parents'Range loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), Parents (C).all));
               end loop;

               Free (Parents);
            end;

         end if;
      end if;

   exception
      when E : others => Trace (Me, E);
         Set_Error_Msg (Data, -"Internal error");
   end Show_Entity_Command_Handler;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Type_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Browsers.Canvas.Initialize (View);
      Register_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Entities_Views.Get_Module,
         Context_Func    => Default_Browser_Context_Factory'Access);
      return Gtk_Widget (View);
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
        Item.Entity.Element.Methods
          (Include_Inherited => Item.Inherited_Primitives);

      Arr  : Entity_Arrays.List;
   begin
      Trace (Me, "Add_Primitive_Operations: Inherited_Primitives="
             & Item.Inherited_Primitives'Img);

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
      Item.Add_Child (Rect);
   end Add_Subprogram;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters (Item : access Type_Item_Record'Class) is
      B        : constant Type_Browser := Type_Browser (Item.Browser);
      S        : constant access Browser_Styles := B.Get_View.Get_Styles;
      Params   : constant Parameter_Array := Item.Entity.Element.Parameters;
      Returned : constant Root_Entity'Class :=
        Item.Entity.Element.Returned_Type;
      Rect  : Rect_Item;
   begin
      for P in Params'Range loop
         declare
            Parameter : constant Root_Entity'Class := Params (P).Parameter;
            Name      : constant String := Parameter.Get_Name;
         begin
            Rect := Gtk_New_Rect (S.Invisible);
            Rect.Set_Child_Layout (Horizontal_Stack);
            Rect.Add_Child
              (Gtk_New_Text
                 (S.Text_Font, Name & " : " & Image (Params (P).Kind)));
            Item.Add_Child (Rect);

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

      if Returned /= No_Root_Entity then
         Rect := Gtk_New_Rect (S.Invisible);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, "return "));
         Rect.Add_Child (Gtk_New_Entity (B, Returned));
         Item.Add_Child (Rect);
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
         Item.Add_Child (Rect);
      end if;

      declare
         Iter : Calls_Iterator'Class := Get_All_Called_Entities
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
            Item.Add_Child (Rect);

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

   procedure Add_Fields (Item : not null  access Type_Item_Record'Class) is
      use Entity_Arrays;
      B     : constant Type_Browser := Type_Browser (Item.Browser);
      S     : constant access Browser_Styles := B.Get_View.Get_Styles;
      Literals : Xref.Entity_Array := Item.Entity.Element.Literals;
   begin
      if Literals'Length /= 0 then
         for F in Literals'Range loop
            Item.Add_Child
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
   begin
      if Typ = No_Root_Entity then
         --  Special handling for anonymous types, as generated by GNAT

         if Entity.Is_Array then
            Item.Add_Array_Type (Entity, Prefix);
         elsif Entity.Is_Access then
            Item.Add_Access_Type (Entity, Prefix);
         else
            Item.Add_Child
              (Gtk_New_Text (S.Text_Font, Prefix & " : <anonymous>"));
         end if;

      else
         Rect := Gtk_New_Rect (S.Invisible);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " : "));
         Rect.Add_Child (Gtk_New_Entity (B, Typ, Prefix));
         Item.Add_Child (Rect);
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
         Item.Add_Child (Rect);
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
         Item.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " : ???"));
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
         Item.Add_Child (Rect);
         Rect.Set_Child_Layout (Horizontal_Stack);
         Rect.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " : access "));
         Rect.Add_Child (Gtk_New_Entity (B, Typ));
      else
         Item.Add_Child (Gtk_New_Text (S.Text_Font, Prefix & " : ???"));
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
      Link     : GPS_Link;
      Label    : Text_Item;
   begin
      if not Browser.Has_Link (Item, Item2) then
         if Link_Name /= "" then
            Label := Gtk_New_Text (Styles.Label, Link_Name);
         end if;

         Link := new GPS_Link_Record;
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
      Title : Unbounded_String;
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

         Title := To_Unbounded_String (Entity.Qualified_Name);
         if Entity.Is_Abstract then
            Append (Title, ASCII.LF & "   " & UML_Abstract);
         end if;
         if Entity.Is_Generic then
            Append (Title, ASCII.LF & "   " & UML_Generic);
         end if;

         Setup_Titlebar
           (Item, Browser,
            Name  => To_String (Title),
            Left  => new Show_Parents_Button,
            Right => new Show_Children_Button);

         Item.Add_Child
           (Gtk_New_Text
              (S.Text_Font, Get_Display_Kind (Item.Entity.Element)));

         if not Is_Type (Entity) then
            Item.Add_Type (Entity, "of type");

         elsif Is_Access (Entity) then
            Item.Add_Access_Type (Entity, "");

         elsif Is_Array (Entity) then
            Item.Add_Array_Type (Entity, "");

         elsif Is_Subprogram (Entity) then
            Item.Add_Parameters;

         elsif Has_Methods (Entity) then
            Item.Might_Have_Primitives := True;
            Item.Add_Fields;
            Item.Add_Primitive_Operations;

         elsif Is_Container (Entity) then
            Item.Add_Package_Contents;
            Item.Add_Fields;

         else
            --  Enumerations, in particular
            Item.Add_Fields;
         end if;
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

end Browsers.Entities;
