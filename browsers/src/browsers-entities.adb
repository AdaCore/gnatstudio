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
with Ada.Numerics.Generic_Elementary_Functions;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Xref;             use GNATCOLL.Xref;
with GNAT.Strings;              use GNAT.Strings;

with Cairo;                     use Cairo;
with Cairo.Region;              use Cairo.Region;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Window;                use Gdk.Window;

with Glib;                      use Glib;
with Glib.Main;
with Glib.Object;               use Glib.Object;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Canvas;             use Gtkada.Canvas;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;
with Gtkada.Types;

with Pango.Layout;              use Pango.Layout;

with Basic_Types;

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
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Xref;           use GPS.Kernel.Xref;
with Xref;                      use Xref;
with Ada.Unchecked_Deallocation;

package body Browsers.Entities is
   Me : constant Trace_Handle := Create ("Browser.Entities");

   package Num is new Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Num;

   package Entity_Arrays is new Ada.Containers.Indefinite_Doubly_Linked_Lists
      (Root_Entity'Class);
   use Entity_Arrays;

   type Entity_Browser_Module_Record is new Module_ID_Record with null record;
   Entity_Browser_Module : Module_ID;

   Left_Margin : constant := 20;
   --  Indentation for the attributes and methods layouts

   UML_Abstract : constant String := "{Abstract}";
   --  String used in UML to indicate that an entity is abstract

   Generic_Item_Box_Width_Right : constant := 10;
   Generic_Item_Box_Width      : constant := Generic_Item_Box_Width_Right + 30;
   --  The position of the templates parameters box for generic items.
   --  Right refers to the position from the right side of the item.

   Generic_Item_Box_Height_Top : constant := 10;
   Generic_Item_Box_Height     : constant := Generic_Item_Box_Height_Top + 17;
   --  Height of the top-rigth box for generic items

   Include_Inherited : aliased constant String := "include_inherited";
   Methods_Cmd_Parameters : constant Cst_Argument_List :=
     (2 => Include_Inherited'Access);

   overriding procedure Default_Context_Factory
     (Module  : access Entity_Browser_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

   type Entity_Browser_Action_Context is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Entity_Browser_Action_Context;
      Ctxt : GPS.Kernel.Selection_Context) return Boolean;
   --  A context that matches if the current widget is an entity browser

   ------------------
   -- Type browser --
   ------------------

   type Type_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Primitive_Button : Gdk.Pixbuf.Gdk_Pixbuf;
      Idle_Id          : Glib.Main.G_Source_Id := 0;
   end record;
   overriding procedure Refresh_Layout_Orientation
     (Browser : access Type_Browser_Record);
   --  See inherited documentation

   function Initialize
     (View   : access Type_Browser_Record'Class)
      return Gtk_Widget;
   --  Creates the dependency browser and returns the focus widget

   package Entities_Views is new Generic_Views.Simple_Views
     (Module_Name            => "Entity_Browser",
      View_Name              => -"Entity Browser",
      Formal_View_Record     => Type_Browser_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Graphs);
   subtype Type_Browser is Entities_Views.View_Access;

   ---------------
   -- Type item --
   ---------------

   type Type_Item_Record is new Browsers.Canvas.Arrow_Item_Record with record
      Entity                : Root_Entity_Ref;
      Might_Have_Primitives : Boolean := True;
      Inherited_Primitives  : Boolean := False;
      General_Lines,
      Attr_Lines,
      Meth_Lines           : Xref_List;

      Layout_W1, Meth_Layout_W1 : Gint;
      Layout_H, Meth_Layout_H : Gint;
      --  Width of the column in the two areas of the item
   end record;
   type Type_Item is access all Type_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Type_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Root_Entity'Class);
   --  Open a new item in the browser that represents Entity.
   --  A copy of Entity is made, thus the caller should free Entity.

   procedure Initialize
     (Item    : access Type_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Root_Entity'Class);
   --  Internal initialization function

   overriding procedure Destroy (Item : in out Type_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   overriding procedure Compute_Size
     (Item          : not null access Type_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo.Region.Cairo_Rectangle_Int);
   overriding procedure Resize_And_Draw
     (Item             : access Type_Item_Record;
      Cr               : Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   overriding procedure Contextual_Factory
     (Item    : access Type_Item_Record;
      Context : in out Selection_Context;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu);
   overriding function Get_Last_Button_Number
     (Item : access Type_Item_Record) return Glib.Gint;
   overriding procedure Redraw_Title_Bar
     (Item : access Type_Item_Record;
      Cr   : Cairo_Context);
   --  See doc for inherited subprograms

   ------------------
   -- Generic item --
   ------------------
   --  This type is used to represent generic items

   type Generic_Item_Record is new Type_Item_Record with null record;

   overriding procedure Compute_Size
     (Item          : not null access Generic_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo.Region.Cairo_Rectangle_Int);
   overriding procedure Resize_And_Draw
     (Item             : access Generic_Item_Record;
      Cr               : Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   overriding function Point_In_Item
     (Item   : access Generic_Item_Record;
      X, Y   : Glib.Gint) return Boolean;
   overriding procedure Draw
     (Item   : access Generic_Item_Record;
      Cr     : Cairo_Context);
   overriding procedure Clip_Line
     (Src   : access Generic_Item_Record;
      Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      To_X  : Gint;
      To_Y  : Gint;
      X_Pos : Gfloat;
      Y_Pos : Gfloat;
      Side  : out Item_Side;
      X_Out : out Gint;
      Y_Out : out Gint);
   --  See doc for inherited subprograms

   -----------------
   -- Parent link --
   -----------------

   type Parent_Link_Record is new Browsers.Canvas.Browser_Link_Record
     with null record;

   overriding procedure Draw_Straight_Line
     (Link        : access Parent_Link_Record;
      Cr          : Cairo.Cairo_Context;
      Parent_Side : Gtkada.Canvas.Item_Side;
      X1, Y1      : Glib.Gdouble;
      Child_Side  : Gtkada.Canvas.Item_Side;
      X2, Y2      : Glib.Gdouble);
   --  See doc for inherited subprogram

   ----------
   -- Misc --
   ----------

   function Add_Or_Select_Item
     (Browser : access Type_Browser_Record'Class;
      Entity  : Root_Entity'Class) return Type_Item;
   --  Create (or return an existing) item displaying the information for
   --  Entity.

   procedure Add_Item_And_Link
     (Item         : access Type_Item_Record'Class;
      Entity       : Root_Entity'Class;
      Link_Name    : String;
      Parent_Link  : Boolean;
      Reverse_Link : Boolean := False);
   --  Create a new item displaying the information for Entity, and link it
   --  with Item. If Reverse_Link is False, link goes from Item to Entity,
   --  otherwise it goes in the opposite direction.
   --  If Parent_Link is true, then the link used is a Parent_Link_Record.

   type Show_Entity_Callback is new Active_Area_Callback with record
      Item      : Browser_Item;
      Entity    : Root_Entity_Ref;
      Link_Name : GNAT.Strings.String_Access;
   end record;

   overriding function Call
     (Callback : Show_Entity_Callback;
      Event    : Gdk.Event.Gdk_Event_Button) return Boolean;
   overriding procedure Destroy (Callback : in out Show_Entity_Callback);
   --  See inherated doc

   function Build
     (Item      : access Browser_Item_Record'Class;
      Entity    : Root_Entity'Class;
      Link_Name : String := "") return Active_Area_Cb;
   --  Build a new callback to display entities

   procedure Show_Entity_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for this module (in the shell window)

   procedure Add_Primitive_Operations
     (Kernel : access Kernel_Handle_Record'Class;
      List   : in out Xref_List;
      Item   : access Type_Item_Record'Class);
   --  Add the sorted list of primitive operations for Entity at the end of
   --  Meth_Layout.

   procedure Add_Parameters
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class);
   --  Add the list of parameters for a subprogram entity to the end of
   --  an cross-reference list.

   procedure Add_Fields
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class);
   --  Add the list of fields for a record-like entity to the end of
   --  a cross-reference list.
   --  This is also usable to get the enumeration literals for an enumeration
   --  type.

   procedure Add_Package_Contents
     (Kernel       : access Kernel_Handle_Record'Class;
      General_List : in out Xref_List;
      Attr_List    : in out Xref_List;
      Meth_List    : in out Xref_List;
      Item         : access Type_Item_Record'Class);
   --  Add the parent package information for an entity at the end of its
   --  cross-reference lists.

   procedure On_Show_Source
     (Browser : access Gtk_Widget_Record'Class; Item : Browser_Item);
   --  Display a source editor to show the declaration of the entity

   procedure Find_Parent_Types (Item : access Arrow_Item_Record'Class);
   --  Display the parent types for the item

   procedure Find_Child_Types (Item : access Arrow_Item_Record'Class);
   --  Display the children types for the item

   procedure Find_Parent_Or_Child_Types
     (Item    : access Arrow_Item_Record'Class;
      Members : Xref.Entity_Array;
      Parents : Boolean);
   --  Display the parent/child types for the item

   procedure Hide_Show_Inherited
     (Event : Gdk_Event_Button;
      Item  : access Browser_Item_Record'Class);
   --  Change the status of inherited primitive operations (shown or hidden)

   procedure Add_Type
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Entity   : Root_Entity'Class;
      Prefix   : String);
   --  Add a new line in List, starting with prefix and followed by an
   --  hyper link for the type of Entity.

   procedure Add_Array_Type
     (List       : in out Xref_List;
      Item       : access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Info_Added : out Boolean);
   --  Add the information for an array type

   procedure Add_Access_Type
     (List       : in out Xref_List;
      Item       : access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Info_Added : out Boolean);
   --  Add the information for an access type

   function Entity_As_Link
     (Kernel : access Kernel_Handle_Record'Class;
      Ent    : Root_Entity'Class) return String;
   --  Return a string that contains the entity name, as an hyper link. If
   --  Entity is in fact a predefined entity, no link is setup.

   procedure Add_Subprogram
     (Kernel : access Kernel_Handle_Record'Class;
      List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Root_Entity'Class);
   --  Add a line that describes the subprogram Entity.
   --  Entity should not be freed by the caller

   procedure Sort
     (Db  : access General_Xref_Database_Record'Class;
      Arr : in out Entity_Arrays.List);
   --  Sort the array alphabetically

   --------------
   -- Commands --
   --------------

   type Examine_Entity_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Examine_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   --------------------------------
   -- Refresh_Layout_Orientation --
   --------------------------------

   overriding procedure Refresh_Layout_Orientation
     (Browser : access Type_Browser_Record)
   is
   begin
      --  Always force a vertical layout
      Set_Layout_Orientation (Get_Canvas (Browser), Vertical_Layout => True);
   end Refresh_Layout_Orientation;

   ----------
   -- Call --
   ----------

   overriding function Call
     (Callback : Show_Entity_Callback;
      Event    : Gdk.Event.Gdk_Event_Button) return Boolean is
   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Press
      then
         Add_Item_And_Link
           (Type_Item (Callback.Item), Callback.Entity.Element,
            Callback.Link_Name.all, Parent_Link => False);
         Layout (Get_Browser (Callback.Item), Force => False);
         return True;
      end if;
      return False;
   end Call;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Callback : in out Show_Entity_Callback)
   is
      V : Root_Entity'Class := Callback.Entity.Element;
   begin
      Unref (V);
      Free (Callback.Link_Name);
   end Destroy;

   -----------
   -- Build --
   -----------

   function Build
     (Item      : access Browser_Item_Record'Class;
      Entity    : Root_Entity'Class;
      Link_Name : String := "") return Active_Area_Cb
   is
      H : Root_Entity_Ref;
   begin
      Ref (Entity);
      H.Replace_Element (Entity);
      return new Show_Entity_Callback'
        (Active_Area_Callback with
         Item      => Browser_Item (Item),
         Link_Name => new String'(Link_Name),
         Entity    => H);
   end Build;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Examine_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Type_Browser := Entities_Views.Get_Or_Create_View
        (Get_Kernel (Context.Context));
      Ignore  : Type_Item;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Add_Or_Select_Item
        (Browser => View,
         Entity  => Get_Entity (Context.Context));
      Layout (View, Force => False);
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
      Entity_Browser_Context  : constant Action_Filter :=
                                  new Entity_Browser_Action_Context;
   begin
      Entity_Browser_Module := new Entity_Browser_Module_Record;
      Entities_Views.Register_Module (Kernel, Entity_Browser_Module);

      Command := new Examine_Entity_Command;

      Register_Contextual_Menu
        (Kernel, "Examine entity",
         Label      => -"Browsers/Examine entity %e",
         Action     => Command,
         Ref_Item   => "Entity called by in browser",
         Add_Before => False,
         Filter     => not Entity_Browser_Context);
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
        (Kernel, "methods",
         Class   => Get_Entity_Class (Kernel),
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler => Show_Entity_Command_Handler'Access);
      Register_Command
        (Kernel, "documentation",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Show_Entity_Command_Handler'Access,
         Minimum_Args => 0,
         Maximum_Args => 1);
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
      View   : Type_Browser;
      Ignore : Type_Item;
      pragma Unreferenced (Ignore);
   begin
      if Entity /= No_Root_Entity then
         if Command = "show" then
            View := Entities_Views.Get_Or_Create_View (Kernel, Focus => True);
            Ignore := Add_Or_Select_Item
              (Browser => View,
               Entity  => Entity);

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
            Name_Parameters (Data, Methods_Cmd_Parameters);

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
      return Gtk_Widget
   is
      i_page_xpm : aliased Gtkada.Types.Chars_Ptr_Array (0 .. 0);
      pragma Import (C, i_page_xpm, "i_page_xpm");

   begin
      Initialize
        (View,
         Create_Toolbar  => False,
         Parents_Pixmap  => Stock_Go_Up,
         Children_Pixmap => Stock_Go_Down);

      --  ??? Should be freed when browser is destroyed
      View.Primitive_Button := Gdk_New_From_Xpm_Data (i_page_xpm);

      Register_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Entities_Views.Get_Module,
         Context_Func    => Default_Browser_Context_Factory'Access);

      return Gtk_Widget (View);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Type_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Root_Entity'Class) is
   begin
      if Entity.Is_Generic then
         Item := new Generic_Item_Record;
      else
         Item := new Type_Item_Record;
      end if;
      Initialize (Item, Browser, Entity);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Type_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Root_Entity'Class)
   is
      Name : constant String := Entity.Qualified_Name;
   begin
      if Entity.Is_Abstract then
         Initialize
           (Item, Browser,
            Name & ASCII.LF & "   " & UML_Abstract,
            Find_Parent_Types'Access, Find_Child_Types'Access);
      else
         Initialize
           (Item, Browser, Name,
            Find_Parent_Types'Access, Find_Child_Types'Access);
      end if;
      Ref (Entity);
      Item.Entity.Replace_Element (Entity);
      Recompute_Size (Item);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Item : in out Type_Item_Record) is
   begin
      Free (Item.General_Lines);
      Free (Item.Attr_Lines);
      Free (Item.Meth_Lines);
   end Destroy;

   --------------------
   -- Entity_As_Link --
   --------------------

   function Entity_As_Link
     (Kernel : access Kernel_Handle_Record'Class;
      Ent    : Root_Entity'Class) return String
   is
      pragma Unreferenced (Kernel);
      Name : constant String := Ent.Get_Name;
   begin
      if Ent.Is_Predefined_Entity then
         return Name;
      else
         return '@' & Name & '@';
      end if;
   end Entity_As_Link;

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

   procedure Add_Primitive_Operations
     (Kernel : access Kernel_Handle_Record'Class;
      List   : in out Xref_List;
      Item   : access Type_Item_Record'Class)
   is
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

      Sort (Kernel.Databases, Arr);

      if not Item.Inherited_Primitives then
         --  For each inherited operation, remove it from the list of
         --  operations to display.
         --  ??? Would be more efficient if GNAT simply didn't give that
         --  information initially, we can easily get it from the
         --  parent's list.
         --  This step isn't needed for C++.

         declare
            Parents     : constant Xref.Entity_Array :=
              Item.Entity.Element.Parent_Types (Recursive => False);
            Op          : Root_Entity_Access;
         begin
            for Parent in Parents'Range loop
               declare
                  Parent_Methods : Xref.Entity_Array :=
                    Parents (Parent).Methods (Include_Inherited => False);
               begin
                  for M in Parent_Methods'Range loop
                     Op := Parent_Methods (M);

                     for A of Arr loop
                        if A = Op.all then
                           A := No_Root_Entity;
                           exit;
                        end if;
                     end loop;
                  end loop;

                  Free (Parent_Methods);
               end;
            end loop;
         end;
      end if;

      for A of Arr loop
         if A /= No_Root_Entity then
            Add_Subprogram (Kernel, List, Item, A);
         end if;
      end loop;
   end Add_Primitive_Operations;

   --------------------
   -- Add_Subprogram --
   --------------------

   procedure Add_Subprogram
     (Kernel : access Kernel_Handle_Record'Class;
      List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Root_Entity'Class) is
   begin
      Add_Line
        (List,
         Entity_As_Link (Kernel, Entity) & " ("
         & Entity.Get_Display_Kind & ')',
         Callback => (1 => Build (Item, Entity)));
      --  Do not free Entity, it's needed for callbacks
   end Add_Subprogram;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Params : constant Parameter_Array :=
        Item.Entity.Element.Parameters;

      Typ : Root_Entity_Access;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Root_Entity'Class, Root_Entity_Access);

      Returned    : constant Root_Entity'Class :=
        Item.Entity.Element.Returned_Type;
   begin
      for P in Params'Range loop
         declare
            Parameter : constant Root_Entity'Class := Params (P).Parameter;
         begin

            --  In some cases, access parameters reference their pointed type
            --  through Pointed_Type. However, if that access type is a
            --  renaming of another type, we'll need to try Get_Variable_Type
            --  if Pointed_Type didn't return anything.

            if Params (P).Kind = Access_Parameter then
               Typ := new Root_Entity'Class'(Parameter.Pointed_Type);
            else
               Typ := new Root_Entity'Class'(No_Root_Entity);
            end if;

            if Typ.all = No_Root_Entity then
               Unchecked_Free (Typ);
               Typ := new Root_Entity'Class'(Parameter.Get_Type_Of);
            end if;

            declare
               Name : constant String := Parameter.Get_Name;
            begin
               Add_Line
                 (List,
                  Name & " : " &
                    Image (Params (P).Kind) & " " &
                    Entity_As_Link (Kernel, Typ.all),
                  Length1  => Name'Length + 1,
                  Callback => (1 => Build (Item, Typ.all)));
            end;

            Unchecked_Free (Typ);

            --  Do not free Typ, it is needed for callbacks
         end;
      end loop;

      if Returned /= No_Root_Entity then
         Add_Line
           (List,
            "return " & Entity_As_Link (Kernel, Returned),
            Length1  => 7,
            Callback => (1 => Build (Item, Returned)));
         --  Do not free Returned, it is needed for callbacks
      end if;
   end Add_Parameters;

   --------------------------
   -- Add_Package_Contents --
   --------------------------

   --------------------------
   -- Add_Package_Contents --
   --------------------------

   procedure Add_Package_Contents
     (Kernel       : access Kernel_Handle_Record'Class;
      General_List : in out Xref_List;
      Attr_List    : in out Xref_List;
      Meth_List    : in out Xref_List;
      Item         : access Type_Item_Record'Class)
   is
      use Entity_Arrays;
      Parent : constant Root_Entity'Class :=
        Item.Entity.Element.Parent_Package;
      Arr    : Entity_Arrays.List;

   begin
      if Parent /= No_Root_Entity then
         Add_Line (General_List, "Parent: " & Entity_As_Link (Kernel, Parent),
                   Callback => (1 => Build (Item, Parent)));
         --  Do not destroy parent, needed for callbacks
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

      Sort (Kernel.Databases, Arr);

      for Current of Arr loop
         if Is_Subprogram (Current) then
            Add_Subprogram (Kernel, Meth_List, Item, Current);

         elsif Is_Type (Current) then
            declare
               Name : constant String := Entity_As_Link (Kernel, Current);
            begin
               Add_Line
                 (Attr_List, Name & "(type)",
                  Length1 => Name'Length,
                  Callback => (1 => Build (Item, Current, "")));
            end;

         --  We want to show variables declared in this package, but not the
         --  parameters to subprograms.

         else
            declare
               Subp : constant Root_Entity'Class := Is_Parameter_Of (Current);
            begin
               if Subp = No_Root_Entity
                 and then Current.Caller_At_Declaration = Item.Entity.Element
               then
                  Add_Type
                    (Attr_List, Item, Current,
                     Current.Get_Name);
               end if;
            end;
         end if;
      end loop;
   end Add_Package_Contents;

   ----------------
   -- Add_Fields --
   ----------------

   procedure Add_Fields
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class)
   is
      use Entity_Arrays;
      Literals : Xref.Entity_Array := Item.Entity.Element.Literals;

   begin
      if Literals'Length /= 0 then
         for F in Literals'Range loop
            Add_Line (List, Get_Name (Literals (F).all));
         end loop;

      else
         declare
            Fields : Xref.Entity_Array := Item.Entity.Element.Fields;
            Discrs : Xref.Entity_Array := Item.Entity.Element.Discriminants;
         begin
            for D in Discrs'Range loop
               Add_Type (List, Item, Discrs (D).all,
                         -"Discriminant: "
                         & Get_Name (Discrs (D).all));
            end loop;

            for F in Fields'Range loop
               Add_Type
                 (List, Item, Fields (F).all,
                  Get_Name (Fields (F).all));
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
     (List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Root_Entity'Class;
      Prefix : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Typ   : constant Root_Entity'Class := Entity.Get_Type_Of;
      Info_Added : Boolean := False;
   begin
      if Typ = No_Root_Entity then
         --  Special handling for anonymous types, as generated by GNAT

         if Entity.Is_Array then
            Add_Array_Type (List, Item, Entity, Info_Added);
         elsif Entity.Is_Access then
            Add_Access_Type (List, Item, Entity, Info_Added);
         end if;

         if Info_Added then
            Add_Line
              (List, Prefix & " <anonymous>",
               Length1 => Prefix'Length + 12);
         else
            Add_Line (List, Prefix & " ???", Length1 => Prefix'Length + 1);
         end if;
      else
         Add_Line
           (List,
            Prefix & ": " & Entity_As_Link (Kernel, Typ),
            Length1  => Prefix'Length + 2,
            Callback => (1 => Build (Item, Typ, Prefix)));
         --  Do not free Typ, needed for callbacks
      end if;
   end Add_Type;

   --------------------
   -- Add_Array_Type --
   --------------------

   procedure Add_Array_Type
     (List       : in out Xref_List;
      Item       : access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Info_Added : out Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Typ : constant Root_Entity'Class := Entity.Component_Type;
      Indexes : Xref.Entity_Array := Entity.Index_Types;
   begin
      if Typ /= No_Root_Entity then
         Add_Line (List, "array ");
         for Ind in Indexes'Range loop
            if Ind = Indexes'First then
               if Ind = Indexes'Last then
                  Add_Line
                    (List, "  ("
                     & Entity_As_Link (Kernel, Indexes (Ind).all) & ")",
                     Callback => (1 => Build (Item, Indexes (Ind).all)));
               else
                  Add_Line
                    (List, "  ("
                     & Entity_As_Link (Kernel, Indexes (Ind).all) & ",",
                     Callback => (1 => Build (Item, Indexes (Ind).all)));
               end if;
            elsif Ind = Indexes'Last then
               Add_Line
                 (List, "   "
                  & Entity_As_Link (Kernel, Indexes (Ind).all) & ")",
                  Callback => (1 => Build (Item, Indexes (Ind).all)));
            else
               Add_Line
                 (List, "   "
                  & Entity_As_Link (Kernel, Indexes (Ind).all) & ",",
                  Callback => (1 => Build (Item, Indexes (Ind).all)));
            end if;
         end loop;

         Free (Indexes);

         Add_Line (List, "of " & Entity_As_Link (Kernel, Typ),
                   Callback => (1 => Build (Item, Typ)));
         Info_Added := True;
      else
         Info_Added := False;
      end if;
   end Add_Array_Type;

   ---------------------
   -- Add_Access_Type --
   ---------------------

   procedure Add_Access_Type
     (List       : in out Xref_List;
      Item       : access Type_Item_Record'Class;
      Entity     : Root_Entity'Class;
      Info_Added : out Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Typ : constant Root_Entity'Class := Pointed_Type (Entity);
   begin
      if Typ /= No_Root_Entity then
         Add_Line
           (List, "access to " & Entity_As_Link (Kernel, Typ),
            Callback => (1 => Build (Item, Typ)));
         --  Do not destroy Typ, needed for callbacks
         Info_Added := True;
      else
         Info_Added := False;
      end if;
   end Add_Access_Type;

   -----------------------
   -- Add_Item_And_Link --
   -----------------------

   procedure Add_Item_And_Link
     (Item         : access Type_Item_Record'Class;
      Entity       : Root_Entity'Class;
      Link_Name    : String;
      Parent_Link  : Boolean;
      Reverse_Link : Boolean := False)
   is
      Canvas   : constant Interactive_Canvas :=
                   Get_Canvas (Get_Browser (Item));
      New_Item : Type_Item;
      Link     : Canvas_Link;
   begin
      New_Item := Add_Or_Select_Item
        (Type_Browser (Get_Browser (Item)), Entity);

      if Parent_Link then
         if Reverse_Link then
            if not Has_Link (Canvas, Item, New_Item, Link_Name) then
               Link := new Parent_Link_Record;
               Add_Link (Canvas, Link, Item, New_Item, Descr => Link_Name,
                         Arrow => No_Arrow);

               --  Force the link to be connected to the bottom of the parent,
               --  and the top of the child
               --  Set_Src_Pos  (Link, 0.5, Y_Pos => 0.0);
               --  Set_Dest_Pos (Link, 0.5, Y_Pos => 1.0);
            end if;
         elsif not Has_Link (Canvas, New_Item, Item, Link_Name) then
            Link := new Parent_Link_Record;
            Add_Link (Canvas, Link, New_Item, Item, Descr => Link_Name,
                      Arrow => No_Arrow);
         end if;

      elsif Reverse_Link then
         if not Has_Link (Canvas, New_Item, Item, Link_Name) then
            Link := new Browser_Link_Record;
            Browser_Link (Link).Orthogonal := True;
            Add_Link (Canvas, Link, New_Item, Item, Descr => Link_Name);
         end if;

      elsif not Has_Link (Canvas, Item, New_Item, Link_Name) then
         Link := new Browser_Link_Record;
         Browser_Link (Link).Orthogonal := True;
         Add_Link (Canvas, Link, Item, New_Item, Descr => Link_Name);
      end if;

      Canvas.Refresh (New_Item);
   end Add_Item_And_Link;

   --------------------------------
   -- Find_Parent_Or_Child_Types --
   --------------------------------

   procedure Find_Parent_Or_Child_Types
     (Item    : access Arrow_Item_Record'Class;
      Members : Xref.Entity_Array;
      Parents : Boolean)
   is
      B  : constant Type_Browser := Type_Browser (Get_Browser (Item));
      It : constant Type_Item := Type_Item (Item);
   begin
      for P in Members'Range loop
         Add_Item_And_Link
           (It, Members (P).all, "",
            Parent_Link => True,
            Reverse_Link => not Parents);
      end loop;

      if Parents then
         Set_Parents_Shown (It, True);
      else
         Set_Children_Shown (It, True);
      end if;

      Layout (B, Force => False);
      Refresh_Canvas (B.Get_Canvas);

   exception
      when E : others => Trace (Me, E);
   end Find_Parent_Or_Child_Types;

   -----------------------
   -- Find_Parent_Types --
   -----------------------

   procedure Find_Parent_Types (Item  : access Arrow_Item_Record'Class) is
   begin
      Find_Parent_Or_Child_Types
        (Item,
         Parent_Types (Type_Item (Item).Entity.Element, Recursive => False),
         Parents => True);
   exception
      when E : others => Trace (Me, E);
   end Find_Parent_Types;

   ----------------------
   -- Find_Child_Types --
   ----------------------

   procedure Find_Child_Types (Item  : access Arrow_Item_Record'Class) is
      Children : Xref.Entity_Array :=
        Child_Types (Type_Item (Item).Entity.Element, Recursive => False);
   begin
      for C in Children'Range loop
         Add_Item_And_Link
           (Type_Item (Item), Children (C).all, "",
            Parent_Link => True,
            Reverse_Link => True);
      end loop;

      Free (Children);

      Set_Children_Shown (Type_Item (Item), True);
      Layout (Type_Browser (Get_Browser (Item)), Force => False);
      Refresh_Canvas (Get_Canvas (Get_Browser (Item)));

   exception
      when E : others => Trace (Me, E);
   end Find_Child_Types;

   -------------------------
   -- Hide_Show_Inherited --
   -------------------------

   procedure Hide_Show_Inherited
     (Event : Gdk_Event_Button;
      Item  : access Browser_Item_Record'Class)
   is
      It : constant Type_Item := Type_Item (Item);
      Canvas : constant Interactive_Canvas :=
        Get_Canvas (Get_Browser (It));
   begin
      if Event.Button = 1
        and then Event.The_Type = Button_Release
      then
         It.Inherited_Primitives := not It.Inherited_Primitives;
         Trace (Me, "Hide_Show_Inherited => "
                & It.Inherited_Primitives'Img);

         Canvas.Refresh (It);
         Item_Updated (Canvas, It);
      end if;
   end Hide_Show_Inherited;

   ------------------
   -- Compute_Size --
   ------------------

   overriding procedure Compute_Size
     (Item          : not null access Type_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo.Region.Cairo_Rectangle_Int)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Xoffset : constant Gint := 0;
      W, H                           : Gint;
      Layout_H, Layout_W1, Layout_W2 : Gint;
      Meth_Layout_W1, Meth_Layout_W2 : Gint;
      Meth_Layout_H                  : Gint;
      General_Lines                  : Xref_List;
      Attr_Lines                     : Xref_List;
      Meth_Lines                     : Xref_List;
      Added                          : Boolean;
   begin
      Add_Line (General_Lines, Get_Display_Kind (Item.Entity.Element));

      if not Is_Type (Item.Entity.Element) then
         Add_Type (Attr_Lines, Item, Item.Entity.Element, "of type");

      elsif Is_Access (Item.Entity.Element) then
         Add_Access_Type (Attr_Lines, Item, Item.Entity.Element, Added);

      elsif Is_Array (Item.Entity.Element) then
         Add_Array_Type (Attr_Lines, Item, Item.Entity.Element, Added);

      elsif Is_Subprogram (Item.Entity.Element) then
         Add_Parameters (Attr_Lines, Item);
         Set_Children_Shown (Item, True);

      elsif Has_Methods (Item.Entity.Element) then
         Item.Might_Have_Primitives := True;
         Add_Primitive_Operations (Kernel, Meth_Lines, Item);
         Add_Fields (Attr_Lines, Item);

      elsif Is_Container (Item.Entity.Element) then
         Add_Package_Contents
           (Kernel, General_Lines, Attr_Lines, Meth_Lines, Item);
         Add_Fields (Attr_Lines, Item);

      else
         --  Enumerations, in particular
         Add_Fields (Attr_Lines, Item);
      end if;

      if not Parents_Shown (Item) then
         declare
            Parents : Xref.Entity_Array :=
              Parent_Types (Item.Entity.Element, Recursive => False);
         begin
            Set_Parents_Shown (Item, Parents'Length = 0);
            Free (Parents);
         end;
      end if;

      Get_Pixel_Size
        (Get_Browser (Item), General_Lines, Layout_W1, Layout_W2, Layout_H,
         Layout);
      W := Gint'Max (Width, Layout_W1 + Layout_W2);
      H := Layout_H;

      Get_Pixel_Size
        (Get_Browser (Item), Attr_Lines, Layout_W1, Layout_W2, Layout_H,
         Layout);
      W := Gint'Max
        (W, Layout_W1 + Layout_W2 + Left_Margin + Xoffset + 2 * Margin);

      Get_Pixel_Size
        (Get_Browser (Item), Meth_Lines, Meth_Layout_W1, Meth_Layout_W2,
         Meth_Layout_H, Layout);
      W := Gint'Max
        (W, Meth_Layout_W1 + Meth_Layout_W2
         + 2 * Margin + Xoffset + Left_Margin);
      H := H + Layout_H + 2 * Margin + Meth_Layout_H;

      Item.Layout_W1 := Layout_W1;
      Item.Layout_H  := Layout_H;
      Item.Meth_Layout_W1 := Meth_Layout_W1;
      Item.Meth_Layout_H  := Meth_Layout_H;

      Width := Gint'Max (W, Title_Box.Width);
      Title_Box.Width := Width;
      Height := H;

      Free (Item.General_Lines);
      Free (Item.Attr_Lines);
      Free (Item.Meth_Lines);

      Item.General_Lines := General_Lines;
      Item.Attr_Lines    := Attr_Lines;
      Item.Meth_Lines    := Meth_Lines;
   end Compute_Size;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Type_Item_Record;
      Cr               : Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      Y : Gint;
      Style_Context : Gtk_Style_Context;
      Border : Gtk.Style.Gtk_Border;

   begin
      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access, Cr, Width, Height,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Margin + Yoffset;

      Display_Lines
        (Item, Cr, Item.General_Lines, Margin + Xoffset, Y, 0, Layout);
      Display_Lines
        (Item, Cr, Item.Attr_Lines, Margin + Xoffset + Left_Margin, Y,
         Item.Layout_W1, Layout);

      if Item.Layout_H /= 0 and then Item.Meth_Layout_H /= 0 then
         Y := Y + 2;

         Style_Context := Get_Style_Context (Get_Browser (Item));
         Style_Context.Get_Border (Gtk_State_Flag_Normal, Border);

         --  We used to use Gtk.Style.X_Thickness to determine the width of
         --  the line.  Now we'll use the thickness of the bottom border
         --  instead.

         Draw_Line
           (Cr,
            Color    => Shade (White_RGBA, 0.3),
            X1       => Gint (Border.Bottom),
            Y1       => Y,
            X2       => Get_Coord (Item).Width - Gint (Border.Bottom) - 1,
            Y2       => Y);
         Y := Y + 1;
      end if;

      Display_Lines
        (Item, Cr, Item.Meth_Lines, Margin + Xoffset + Left_Margin, Y,
         Item.Meth_Layout_W1, Layout);
   end Resize_And_Draw;

   ----------------------
   -- Redraw_Title_Bar --
   ----------------------

   overriding procedure Redraw_Title_Bar
     (Item : access Type_Item_Record;
      Cr   : Cairo_Context)
   is
   begin
      Redraw_Title_Bar (Arrow_Item_Record (Item.all)'Access, Cr);

      if Item.Might_Have_Primitives
        and then Is_Type (Item.Entity.Element)
      then
         --  ??? Should use a different icon depending on
         --  Item.Inherited_Primitives.
         Draw_Title_Bar_Button
           (Item, Cr,
            Num    => Get_Last_Button_Number (Item),
            Pixbuf => Type_Browser (Get_Browser (Item)).Primitive_Button,
            Cb     => Build (Hide_Show_Inherited'Access, Item));
      end if;
   end Redraw_Title_Bar;

   ------------------------
   -- Add_Or_Select_Item --
   ------------------------

   function Add_Or_Select_Item
     (Browser : access Type_Browser_Record'Class;
      Entity  : Root_Entity'Class) return Type_Item
   is
      Found : Type_Item;
      Iter  : Item_Iterator := Start (Get_Canvas (Browser));
   begin
      --  Check if there is already an item displaying Entity
      loop
         Found := Type_Item (Get (Iter));
         exit when Found = null
           or else Found.Entity.Element = Entity;
         Next (Iter);
      end loop;

      if Found = null and then Entity /= No_Root_Entity then
         Gtk_New (Found, Browser, Entity);
         Put (Get_Canvas (Browser), Found);
      end if;

      --  Need to always refresh the canvas so that the links are correctly
      --  displayed.
      Refresh_Canvas (Get_Canvas (Browser));

      return Found;
   end Add_Or_Select_Item;

   --------------------
   -- On_Show_Source --
   --------------------

   procedure On_Show_Source
     (Browser : access Gtk_Widget_Record'Class;
      Item    : Browser_Item)
   is
      B  : constant Type_Browser := Type_Browser (Browser);
      It : constant Type_Item    := Type_Item (Item);
      Loc : constant General_Location :=
        Get_Declaration (It.Entity.Element).Loc;
      Name : constant String := Get_Name (It.Entity.Element);

      use Basic_Types;
   begin
      Add_Navigation_Location (Get_Kernel (B), -"Entity Browser");

      Open_File_Editor
        (Kernel     => Get_Kernel (B),
         Filename   => Loc.File,
         Project    => Loc.Project,
         Line       => Loc.Line,
         Column     => Loc.Column,
         Column_End => Loc.Column + Basic_Types.Visible_Column_Type
             (Name'Length));
   end On_Show_Source;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   overriding procedure Contextual_Factory
     (Item    : access Type_Item_Record;
      Context : in out Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event);
      Mitem : Gtk_Menu_Item;
      Loc   : constant General_Location := Get_Declaration
        (Item.Entity.Element).Loc;
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

      if Menu /= null then
         Gtk_New (Mitem, -"Show source");
         Add (Menu, Mitem);
         Item_Cb.Object_Connect
           (Mitem, Signal_Activate, On_Show_Source'Access,
            Slot_Object => Browser,
            User_Data   => Browser_Item (Item));
      end if;
   end Contextual_Factory;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Entity_Browser_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject)
   is
      pragma Unreferenced (Module);
      Browser : constant Type_Browser :=
        Entities_Views.View_From_Widget (Child);
      Iter    : constant Item_Iterator :=
        Start (Get_Canvas (Browser), Selected_Only => True);
   begin
      --  If there is no selection, or more than one item, nothing we can do
      if Get (Iter) /= null
        and then Get (Next (Iter)) = null
      then
         Contextual_Factory
           (Browser_Item (Get (Iter)), Context, Browser, null, null);
      end if;
   end Default_Context_Factory;

   ----------------------------
   -- Get_Last_Button_Number --
   ----------------------------

   overriding function Get_Last_Button_Number (Item : access Type_Item_Record)
      return Gint is
   begin
      return Get_Last_Button_Number (Arrow_Item_Record (Item.all)'Access) + 1;
   end Get_Last_Button_Number;

   ------------------------
   -- Draw_Straight_Line --
   ------------------------

   overriding procedure Draw_Straight_Line
     (Link        : access Parent_Link_Record;
      Cr          : Cairo_Context;
      Parent_Side : Gtkada.Canvas.Item_Side;
      X1, Y1      : Glib.Gdouble;
      Child_Side  : Gtkada.Canvas.Item_Side;
      X2, Y2      : Glib.Gdouble)
   is
      Width : constant := 5.0;
      pragma Unreferenced (Link, Child_Side, Parent_Side);
      Length : constant Gdouble := Sqrt ((X1 - X2) ** 2 + (Y1 - Y2) ** 2);
   begin
      Cairo.Save (Cr);
      Cairo.Translate (Cr, X2, Y2);
      if Y1 /= Y2 then
         Cairo.Rotate (Cr, Arctan (Y1 - Y2, X1 - X2));
      end if;

      Move_To (Cr, 0.5, 0.5);
      Line_To (Cr, Length - Width * 2.0 - 0.5, 0.0);
      Cairo.Stroke (Cr);

      --  Draw the arrow head
      Move_To (Cr, Length - Width * 2.0 - 0.5, -Width + 0.5);
      Line_To (Cr, Length - 0.5, 0.5);
      Line_To (Cr, Length - Width * 2.0 - 0.5, Width + 0.5);
      Close_Path (Cr);
      Cairo.Stroke (Cr);
      Cairo.Restore (Cr);
   end Draw_Straight_Line;

   ------------------
   -- Compute_Size --
   ------------------

   overriding procedure Compute_Size
     (Item          : not null access Generic_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo_Rectangle_Int)
   is
   begin
      Compute_Size
        (Type_Item_Record (Item.all)'Access, Layout, Width, Height,
         Title_Box);
      Height := Height + Generic_Item_Box_Height_Top;

      Width := Gint'Max (Width, Title_Box.Width);
      Title_Box.Width := Width;
      Width := Width + 4;  --  for the extra top-right rectangle
      Title_Box.Y := Title_Box.Y + Generic_Item_Box_Height_Top;
   end Compute_Size;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Generic_Item_Record;
      Cr               : Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
   begin
      Set_Source_Color (Cr, White_RGBA);
      Cairo.Rectangle
        (Cr,
         X      =>
           Gdouble (Get_Coord (Item).Width - Generic_Item_Box_Width + 1),
         Y      => 1.0,
         Width  => Gdouble (Generic_Item_Box_Width - 2),
         Height => Gdouble (Generic_Item_Box_Height - 2));
      Fill_Preserve (Cr);
      Set_Source_Color (Cr, Shade (White_RGBA, 0.3));
      Stroke (Cr);

      Yoffset := 0;
      Xoffset := 0;
      Resize_And_Draw
        (Type_Item_Record (Item.all)'Access, Cr,
         Width, Height,
         Width_Offset  => Width_Offset + 4,
         Height_Offset => Height_Offset,
         Xoffset       => Xoffset,
         Yoffset       => Yoffset,
         Layout        => Layout);
   end Resize_And_Draw;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Item   : access Generic_Item_Record;
      Cr     : Cairo_Context)
   is
      Item_Width : constant Gint := Get_Coord (Item).Width;
      Item_Height : constant Gint := Get_Coord (Item).Height;
   begin
      Cairo.Set_Fill_Rule (Cr, Cairo_Fill_Rule_Winding);
      Cairo.Rectangle
        (Cr,
         0.0,
         Gdouble (Generic_Item_Box_Height_Top),
         Gdouble (Item_Width - Generic_Item_Box_Width_Right),
         Gdouble (Item_Height));
      Cairo.Rectangle
        (Cr,
         Gdouble (Item_Width - Generic_Item_Box_Width),
         0.0,
         Gdouble (Generic_Item_Box_Width),
         Gdouble (Generic_Item_Box_Height));
      Cairo.Clip (Cr);
      Draw (Type_Item_Record (Item.all)'Access, Cr);
   end Draw;

   -------------------
   -- Point_In_Item --
   -------------------

   overriding function Point_In_Item
     (Item : access Generic_Item_Record;
      X, Y : Glib.Gint) return Boolean is
   begin
      if (Y < Get_Coord (Item).Y + Generic_Item_Box_Height_Top
          and then X - Get_Coord (Item).X <
            Get_Coord (Item).Width - Generic_Item_Box_Width)
        or else (X - Get_Coord (Item).X >
                   Get_Coord (Item).Width - Generic_Item_Box_Width_Right
                 and then Y - Get_Coord (Item).Y > Generic_Item_Box_Height)
      then
         return False;
      else
         return Point_In_Item (Canvas_Item_Record (Item.all)'Access, X, Y);
      end if;
   end Point_In_Item;

   ---------------
   -- Clip_Line --
   ---------------

   overriding procedure Clip_Line
     (Src   : access Generic_Item_Record;
      Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      To_X  : Gint;
      To_Y  : Gint;
      X_Pos : Gfloat;
      Y_Pos : Gfloat;
      Side  : out Item_Side;
      X_Out : out Gint;
      Y_Out : out Gint)
   is
      Coord : constant Gdk_Rectangle := Get_Coord (Src);
   begin
      Clip_Line (Type_Item_Record (Src.all)'Access, Canvas,
                 To_X, To_Y, X_Pos, Y_Pos, Side, X_Out, Y_Out);

      case Side is
         when North =>
            if X_Out < Coord.X + Coord.Width - Generic_Item_Box_Width then
               Y_Out := Y_Out + Generic_Item_Box_Height_Top;
            end if;

         when East =>
            if Y_Out > Coord.Y + Generic_Item_Box_Height then
               X_Out := X_Out - Generic_Item_Box_Width_Right;
            end if;

         when others =>
            null;
      end case;
   end Clip_Line;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Entity_Browser_Action_Context;
      Ctxt    : Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      --  Do not check the current focus widget ourselves. Instead, we know
      --  it has been properly checked when the context was created, and we
      --  just check the current module from there.
      return GPS.Kernel.Modules.Module_ID (Get_Creator (Ctxt)) =
        Entity_Browser_Module;
   end Filter_Matches_Primitive;

end Browsers.Entities;
