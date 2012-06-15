------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada.Numerics.Generic_Elementary_Functions;

with GNAT.Heap_Sort_G;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNAT.Strings;              use GNAT.Strings;

with Cairo;                     use Cairo;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Window;                use Gdk.Window;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Canvas;             use Gtkada.Canvas;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;
with Gtkada.Types;

with Pango.Layout;              use Pango.Layout;

with Basic_Types;

with Browsers.Canvas;           use Browsers.Canvas;
with Entities.Queries;          use Entities, Entities.Queries;
with Commands.Interactive;      use Commands, Commands.Interactive;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Traces;                    use Traces;
with XML_Utils;                 use XML_Utils;
with Xref;                      use Xref;

package body Browsers.Entities is

   Me : constant Debug_Handle := Create ("Browser.Entities");

   package Num is new Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Num;

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
      Idle_Id          : Gtk.Main.Idle_Handler_Id := 0;
   end record;
   type Type_Browser is access all Type_Browser_Record'Class;

   overriding procedure Refresh_Layout_Orientation
     (Browser : access Type_Browser_Record);
   --  See inherited documentation

   ---------------
   -- Type item --
   ---------------

   type Type_Item_Record is new Browsers.Canvas.Arrow_Item_Record with record
      Entity               : Entity_Information;
      Inherited_Primitives : Boolean := False;
      General_Lines,
      Attr_Lines,
      Meth_Lines           : Xref_List;
   end record;
   type Type_Item is access all Type_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Type_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Entity_Information);
   --  Open a new item in the browser that represents Entity.
   --  A copy of Entity is made, thus the caller should free Entity.

   procedure Initialize
     (Item    : access Type_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Entity_Information);
   --  Internal initialization function

   overriding procedure Destroy (Item : in out Type_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   overriding function Get_Item_Style
     (Item : access Type_Item_Record) return Gtk.Style.Gtk_Style;
   overriding procedure Resize_And_Draw
     (Item             : access Type_Item_Record;
      Cr               : in out Cairo_Context;
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
   overriding procedure Highlight (Item : access Type_Item_Record);
   --  See doc for inherited subprograms

   ------------------
   -- Generic item --
   ------------------
   --  This type is used to represent generic items

   type Generic_Item_Record is new Type_Item_Record with null record;

   overriding procedure Resize_And_Draw
     (Item             : access Generic_Item_Record;
      Cr               : in out Cairo_Context;
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

   procedure On_Type_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Entity browser menu

   function Open_Type_Browser_Child
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Create (or return an existing) type browser, and insert it directly into
   --  the MDI.
   --  If a new browser is created, it is initially empty.
   --  If the browser already existed, it is raised.

   function Open_Type_Browser
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Type_Browser;
   --  Create (or return an existing) type browser. The newly created browser
   --  is not inserted into the MDI.

   function Add_Or_Select_Item
     (Browser : access Type_Browser_Record'Class;
      Entity  : Entity_Information) return Type_Item;
   --  Create (or return an existing) item displaying the information for
   --  Entity.

   procedure Add_Item_And_Link
     (Item         : access Type_Item_Record'Class;
      Entity       : Entity_Information;
      Link_Name    : String;
      Parent_Link  : Boolean;
      Reverse_Link : Boolean := False);
   --  Create a new item displaying the information for Entity, and link it
   --  with Item. If Reverse_Link is False, link goes from Item to Entity,
   --  otherwise it goes in the opposite direction.
   --  If Parent_Link is true, then the link used is a Parent_Link_Record.

   type Show_Entity_Callback is new Active_Area_Callback with record
      Item      : Browser_Item;
      Entity    : Entity_Information;
      Link_Name : GNAT.Strings.String_Access;
   end record;

   overriding function Call
     (Callback : Show_Entity_Callback;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   overriding procedure Destroy (Callback : in out Show_Entity_Callback);
   --  See inherated doc

   function Build
     (Item      : access Browser_Item_Record'Class;
      Entity    : Entity_Information;
      Link_Name : String := "") return Active_Area_Cb;
   --  Build a new callback to display entities

   procedure Show_Entity_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Command handler for this module (in the shell window)

   procedure Add_Primitive_Operations
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class);
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
     (General_List : in out Xref_List;
      Attr_List    : in out Xref_List;
      Meth_List    : in out Xref_List;
      Item         : access Type_Item_Record'Class);
   --  Add the parent package information for an entity at the end of its
   --  cross-reference lists.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Support functions for the MDI

   procedure On_Show_Source
     (Browser : access Gtk_Widget_Record'Class; Item : Browser_Item);
   --  Display a source editor to show the declaration of the entity

   procedure Find_Parent_Types (Item : access Arrow_Item_Record'Class);
   --  Display the parent types for the item

   procedure Find_Child_Types (Item : access Arrow_Item_Record'Class);
   --  Display the children types for the item

   procedure Find_Parent_Or_Child_Types
     (Item    : access Arrow_Item_Record'Class;
      Members : Entity_Information_Array;
      Parents : Boolean);
   --  Display the parent/child types for the item

   procedure Hide_Show_Inherited
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class);
   --  Change the status of inherited primitive operations (shown or hidden)

   procedure Add_Type
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Entity   : Entity_Information;
      Prefix   : String);
   --  Add a new line in List, starting with prefix and followed by an
   --  hyper link for the type of Entity.

   procedure Add_Array_Type
     (List       : in out Xref_List;
      Item       : access Type_Item_Record'Class;
      Entity     : Entity_Information;
      Info_Added : out Boolean);
   --  Add the information for an array type

   procedure Add_Access_Type
     (List       : in out Xref_List;
      Item       : access Type_Item_Record'Class;
      Entity     : Entity_Information;
      Info_Added : out Boolean);
   --  Add the information for an access type

   function Entity_As_Link (Ent : Entity_Information) return String;
   --  Return a string that contains the entity name, as an hyper link. If
   --  Entity is in fact a predefined entity, no link is setup.

   procedure Add_Subprogram
     (List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Entity_Information);
   --  Add a line that describes the subprogram Entity.
   --  Entity should not be freed by the caller

   procedure Sort (Arr : in out Entity_Information_Arrays.Instance);
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
      Event    : Gdk.Event.Gdk_Event) return Boolean is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         Add_Item_And_Link
           (Type_Item (Callback.Item), Callback.Entity,
            Callback.Link_Name.all, Parent_Link => False);
         Layout (Get_Browser (Callback.Item), Force => False);
         return True;
      end if;
      return False;
   end Call;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Callback : in out Show_Entity_Callback) is
   begin
      Unref (Callback.Entity);
      Free (Callback.Link_Name);
   end Destroy;

   -----------
   -- Build --
   -----------

   function Build
     (Item      : access Browser_Item_Record'Class;
      Entity    : Entity_Information;
      Link_Name : String := "") return Active_Area_Cb is
   begin
      Ref (Entity);
      return new Show_Entity_Callback'
        (Active_Area_Callback with
         Item      => Browser_Item (Item),
         Link_Name => new String'(Link_Name),
         Entity    => Entity);
   end Build;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Examine_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Child : constant MDI_Child :=
                Open_Type_Browser_Child (Get_Kernel (Context.Context));
      Ignore  : Type_Item;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Add_Or_Select_Item
        (Browser => Type_Browser (Get_Widget (Child)),
         Entity  => Get_Entity (Context.Context, Ask_If_Overloaded => True));
      Layout (Type_Browser (Get_Widget (Child)), Force => False);
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

      Register_Module
        (Module      => Entity_Browser_Module,
         Kernel      => Kernel,
         Module_Name => "Entity_Browser");

      Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Command := new Examine_Entity_Command;

      Register_Contextual_Menu
        (Kernel, "Examine entity",
         Label      => -"Browsers/Examine entity %e",
         Action     => Command,
         Ref_Item   => "Entity called by in browser",
         Add_Before => False,
         Filter     => not Entity_Browser_Context);
      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & (-"Tools") & '/' & (-"Browsers"),
         Text        => -"_Entity",
         Callback    => On_Type_Browser'Access);
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
      Entity : constant Entity_Information := Get_Data (Data, 1);
      Child  : MDI_Child;
      Result : Entity_Information;
      Ignore : Type_Item;
      pragma Unreferenced (Ignore);
   begin
      if Entity /= null then
         if Command = "show" then
            Child := Open_Type_Browser_Child (Kernel);
            Ignore := Add_Or_Select_Item
              (Browser => Type_Browser (Get_Widget (Child)),
               Entity  => Entity);

         elsif Command = "discriminants" then
            declare
               Iter  : Entity_Reference_Iterator;
               Discr : Entity_Information;
            begin
               Find_All_References
                 (Iter   => Iter,
                  Entity => Entity,
                  Filter => (Discriminant => True, others => False));

               Set_Return_Value_As_List (Data);

               while not At_End (Iter) loop
                  Discr := Get_Entity (Iter);
                  if Discr /= null then
                     Set_Return_Value
                       (Data, Create_Entity (Get_Script (Data), Discr));
                  end if;

                  Next (Iter);
               end loop;

               Destroy (Iter);
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
                     Entity => General_Entity'
                       (Old_Entity => Entity, others => <>),
                     Raw_Format => not Extended));
            end;

         elsif Command = "parameters" then
            declare
               Iter  : Subprogram_Iterator;
               Param : Entity_Information;
            begin
               Iter := Get_Subprogram_Parameters
                 (Subprogram            => Entity,
                  File_Has_No_LI_Report => null);

               Set_Return_Value_As_List (Data);
               loop
                  Get (Iter, Param);
                  exit when Param = null;

                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), Param));
                  Next (Iter);
               end loop;
            end;

         elsif Command = "methods" then
            Name_Parameters (Data, Methods_Cmd_Parameters);
            declare
               Iter : Primitive_Operations_Iterator;
            begin
               Find_All_Primitive_Operations
                 (Iter, Entity, Include_Inherited => Nth_Arg (Data, 2, False));
               Set_Return_Value_As_List (Data);

               while not At_End (Iter) loop
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data), Get (Iter)));
                  Next (Iter);
               end loop;
               Destroy (Iter);
            end;

         elsif Command = "return_type" then
            Set_Return_Value
              (Data,
               Create_Entity (Get_Script (Data), Returned_Type (Entity)));

         elsif Command = "primitive_of" then
            Set_Return_Value
              (Data,
               Create_Entity
                 (Get_Script (Data), Is_Primitive_Operation_Of (Entity)));

         elsif Command = "pointed_type" then
            Result := Pointed_Type (Entity);
            if Result = null
              and then Get_Type_Of (Entity) /= null
            then
               Result := Pointed_Type (Get_Type_Of (Entity));
            end if;
            Set_Return_Value (Data, Create_Entity (Get_Script (Data), Result));

         elsif Command = "type" then
            Set_Return_Value
              (Data, Create_Entity (Get_Script (Data), Get_Type_Of (Entity)));

         elsif Command = "fields" then
            declare
               Iter  : Calls_Iterator;
               Field : Entity_Information;
            begin
               Set_Return_Value_As_List (Data);

               Iter := Get_All_Called_Entities (Entity);
               while not At_End (Iter) loop
                  Field := Get (Iter);

                  if In_Range (Get_Declaration_Of (Field), Entity) then
                     if not Is_Discriminant (Field, Entity) then
                        Set_Return_Value
                          (Data, Create_Entity (Get_Script (Data), Field));
                     end if;
                  end if;

                  Next (Iter);
               end loop;

               Destroy (Iter);
            end;

         elsif Command = "derived_types" then
            declare
               Children : Children_Iterator := Get_Child_Types (Entity);
               Child    : Entity_Information;
            begin
               Set_Return_Value_As_List (Data);

               while not At_End (Children) loop
                  Child := Get (Children);
                  if Child /= null then
                     Set_Return_Value
                       (Data, Create_Entity (Get_Script (Data), Child));
                  end if;
                  Next (Children);
               end loop;
               Destroy (Children);
            end;
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
         Set_Error_Msg (Data, -"Internal error");
   end Show_Entity_Command_Handler;

   -----------------------
   -- Open_Type_Browser --
   -----------------------

   function Open_Type_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Type_Browser
   is
      Browser    : constant Type_Browser := new Type_Browser_Record;
      i_page_xpm : aliased Gtkada.Types.Chars_Ptr_Array (0 .. 0);
      pragma Import (C, i_page_xpm, "i_page_xpm");

   begin
      Initialize
        (Browser, Kernel,
         Create_Toolbar  => False,
         Parents_Pixmap  => Stock_Go_Up,
         Children_Pixmap => Stock_Go_Down);

      --  ??? Should be freed when browser is destroyed
      Browser.Primitive_Button := Gdk_New_From_Xpm_Data (i_page_xpm);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Entity_Browser_Module,
         Context_Func    => Default_Browser_Context_Factory'Access);

      return Browser;
   end Open_Type_Browser;

   -----------------------------
   -- Open_Type_Browser_Child --
   -----------------------------

   function Open_Type_Browser_Child
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : GPS_MDI_Child;
      Browser : Type_Browser;
      Title   : constant String := -"Entity Browser";
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Type_Browser_Record'Tag));

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Open_Type_Browser (Kernel);
         Gtk_New (Child, Browser,
                  Focus_Widget   => Gtk_Widget (Get_Canvas (Browser)),
                  Default_Width  => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height => Gint (Default_Widget_Height.Get_Pref),
                  Group          => Group_Graphs,
                  Module         => Entity_Browser_Module);
         Set_Title (Child, Title);
         Put (Get_MDI (Kernel), Child);
         Set_Focus_Child (Child);
      end if;

      Add_Navigation_Location (Kernel, Title);

      return MDI_Child (Child);
   end Open_Type_Browser_Child;

   ---------------------
   -- On_Type_Browser --
   ---------------------

   procedure On_Type_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Child   : MDI_Child;
      Entity  : Entity_Information;
      Ignore   : Type_Item;
      pragma Unreferenced (Widget, Ignore);

   begin
      Child := Open_Type_Browser_Child (Kernel);

      if Context /= No_Context then
         Entity := Get_Entity (Context, Ask_If_Overloaded => True);
         if Entity /= null then
            Ignore := Add_Or_Select_Item
              (Browser => Type_Browser (Get_Widget (Child)),
               Entity  => Entity);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Type_Browser;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Type_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Entity_Information) is
   begin
      if Get_Kind (Entity).Is_Generic then
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
      Entity  : Entity_Information) is
   begin
      if Get_Kind (Entity).Is_Abstract then
         Initialize
           (Item, Browser,
            Get_Full_Name (Entity) & ASCII.LF & "   " & UML_Abstract,
            Find_Parent_Types'Access, Find_Child_Types'Access);
      else
         Initialize
           (Item, Browser, Get_Full_Name (Entity),
            Find_Parent_Types'Access, Find_Child_Types'Access);
      end if;
      Ref (Entity);
      Item.Entity := Entity;
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

   function Entity_As_Link (Ent : Entity_Information) return String is
   begin
      if Is_Predefined_Entity (Ent) then
         return Get (Get_Name (Ent)).all;
      else
         return '@' & Get (Get_Name (Ent)).all & '@';
      end if;
   end Entity_As_Link;

   ----------
   -- Sort --
   ----------

   procedure Sort (Arr : in out Entity_Information_Arrays.Instance) is
      use type Entity_Information_Arrays.Index_Type;
      First : constant Integer :=
                Integer (Entity_Information_Arrays.First) - 1;

      procedure Move (From, To : Natural);
      function Lt (Op1, Op2 : Natural) return Boolean;
      --  See GNAT.Heap_Sort_G

      Tmp : Entity_Information;

      ----------
      -- Move --
      ----------

      procedure Move (From, To : Natural) is
      begin
         if From = 0 then
            Arr.Table (Entity_Information_Arrays.Index_Type (To + First)) :=
              Tmp;
         elsif To = 0 then
            Tmp :=
              Arr.Table (Entity_Information_Arrays.Index_Type (From + First));
         else
            Arr.Table (Entity_Information_Arrays.Index_Type (To + First)) :=
              Arr.Table (Entity_Information_Arrays.Index_Type (From + First));
         end if;
      end Move;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         if Op1 = 0 then
            return Tmp <
              Arr.Table (Entity_Information_Arrays.Index_Type (Op2 + First));
         elsif Op2 = 0 then
            return
              Arr.Table (Entity_Information_Arrays.Index_Type (Op1 + First)) <
              Tmp;
         else
            return
              Arr.Table (Entity_Information_Arrays.Index_Type (Op1 + First)) <
              Arr.Table (Entity_Information_Arrays.Index_Type (Op2 + First));
         end if;
      end Lt;

      package Entity_Sort is new GNAT.Heap_Sort_G (Move, Lt);
   begin
      Entity_Sort.Sort
        (Integer (Entity_Information_Arrays.Last (Arr)) - First);
   end Sort;

   ------------------------------
   -- Add_Primitive_Operations --
   ------------------------------

   procedure Add_Primitive_Operations
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class)
   is
      use Entity_Information_Arrays;
      Prim : Primitive_Operations_Iterator;
      Arr  : Entity_Information_Arrays.Instance;
   begin
      Trace (Me, "Add_Primitive_Operations: Inherited_Primitives="
             & Item.Inherited_Primitives'Img);

      Find_All_Primitive_Operations
        (Iter              => Prim,
         Entity            => Item.Entity,
         Include_Inherited => Item.Inherited_Primitives);

      --  Store all primitive operations in an array, so that we can display
      --  them sorted, and possibly filter them out.

      while not At_End (Prim) loop
         Append (Arr, Get (Prim));
         Next (Prim);
      end loop;

      Destroy (Prim);

      Sort (Arr);

      if not Item.Inherited_Primitives then
         --  For each inherited operation, remove it from the list of
         --  operations to display.
         --  ??? Would be more efficient if GNAT simply didn't give that
         --  information initially, we can easily get it from the
         --  parent's list.
         --  This step isn't needed for C++.

         declare
            Parents     : constant Entity_Information_Array :=
                            Get_Parent_Types (Item.Entity);
            Parent_Prim : Primitive_Operations_Iterator;
            Op          : Entity_Information;
         begin
            for Parent in Parents'Range loop
               Find_All_Primitive_Operations
                 (Iter              => Parent_Prim,
                  Entity            => Parents (Parent),
                  Include_Inherited => False);

               while not At_End (Parent_Prim) loop
                  Op := Get (Parent_Prim);

                  for A in Entity_Information_Arrays.First .. Last (Arr) loop
                     if Arr.Table (A) = Op then
                        Arr.Table (A) := null;
                        exit;
                     end if;
                  end loop;

                  Next (Parent_Prim);
               end loop;

               Destroy (Parent_Prim);
            end loop;
         end;
      end if;

      for A in Entity_Information_Arrays.First .. Last (Arr) loop
         if Arr.Table (A) /= null then
            Add_Subprogram (List, Item, Arr.Table (A));
         end if;
      end loop;

      Free (Arr);
      Destroy (Prim);
   end Add_Primitive_Operations;

   --------------------
   -- Add_Subprogram --
   --------------------

   procedure Add_Subprogram
     (List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Entity_Information) is
   begin
      Add_Line
        (List,
         Entity_As_Link (Entity) & " ("
         & (-Kind_To_String (Get_Kind (Entity))) & ')',
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
      Subs           : Subprogram_Iterator;
      Typ, Parameter : Entity_Information;
      Returned    : constant Entity_Information := Returned_Type (Item.Entity);
   begin
      Subs := Get_Subprogram_Parameters (Item.Entity);

      loop
         Get (Subs, Parameter);
         exit when Parameter = null;

         --  In some cases, access parameters reference their pointed type
         --  through Pointed_Type. However, if that access type is a renaming
         --  of another type, we'll need to try Get_Variable_Type if
         --  Pointed_Type didn't return anything.

         if Get_Kind (Parameter).Kind = Access_Kind then
            Typ := Pointed_Type (Parameter);
         else
            Typ := null;
         end if;

         if Typ = null then
            Typ := Get_Variable_Type (Parameter);
         end if;

         Add_Line
           (List,
            Get (Get_Name (Parameter)).all & ": " &
              Image (Get_Type (Subs)) & " " & Entity_As_Link (Typ),
            Length1  => Get (Get_Name (Parameter)).all'Length + 1,
            Callback => (1 => Build (Item, Typ)));
         --  Do not free Typ, it is needed for callbacks

         Next (Subs);
      end loop;

      if Returned /= null then
         Add_Line
           (List,
            "return " & Entity_As_Link (Returned),
            Length1  => 7,
            Callback => (1 => Build (Item, Returned)));
         --  Do not free Returned, it is needed for callbacks
      end if;
   end Add_Parameters;

   --------------------------
   -- Add_Package_Contents --
   --------------------------

   procedure Add_Package_Contents
     (General_List : in out Xref_List;
      Attr_List    : in out Xref_List;
      Meth_List    : in out Xref_List;
      Item         : access Type_Item_Record'Class)
   is
      use Entity_Information_Arrays;
      Parent : constant Entity_Information := Get_Parent_Package (Item.Entity);
      Iter   : Calls_Iterator;
      Arr    : Entity_Information_Arrays.Instance;
      Called : Entity_Information;

   begin
      if Parent /= null then
         Add_Line (General_List, "Parent: " & Entity_As_Link (Parent),
                   Callback => (1 => Build (Item, Parent)));
         --  Do not destroy parent, needed for callbacks
      end if;

      Iter := Get_All_Called_Entities (Item.Entity);
      while not At_End (Iter) loop
         Called := Get (Iter);

         if Called /= null then
            if In_Range (Get_Declaration_Of (Called), Item.Entity) then
               Append (Arr, Called);
            end if;
         end if;

         Next (Iter);
      end loop;

      Sort (Arr);

      for A in Entity_Information_Arrays.First .. Last (Arr) loop
         if Is_Subprogram (Arr.Table (A)) then
            Trace (Me, "Container: " & Debug_Name (Arr.Table (A)));
            Add_Subprogram (Meth_List, Item, Arr.Table (A));

         elsif Get_Kind (Arr.Table (A)).Is_Type then
            Trace (Me, "Type: " & Debug_Name (Arr.Table (A)));
            declare
               Name : constant String := Entity_As_Link (Arr.Table (A));
            begin
               if Is_Subtype (Arr.Table (A)) then
                  Add_Line
                    (Attr_List, Name & "(subtype)",
                     Length1 => Name'Length,
                     Callback => (1 => Build (Item, Arr.Table (A), "")));
               else
                  Add_Line
                    (Attr_List, Name & "(type)",
                     Length1 => Name'Length,
                     Callback => (1 => Build (Item, Arr.Table (A), "")));
               end if;
            end;

         --  We want to show variables declared in this package, but not the
         --  parameters to subprograms.

         elsif Is_Parameter_Of (Arr.Table (A)) = null
           and then Get_Caller (Declaration_As_Reference (Arr.Table (A))) =
             Item.Entity
         then
            Trace (Me, "Variable: " & Debug_Name (Arr.Table (A)));
            Add_Type
              (Attr_List, Item, Arr.Table (A),
               Get (Get_Name (Arr.Table (A))).all);
         end if;
      end loop;

      Free (Arr);
   end Add_Package_Contents;

   ----------------
   -- Add_Fields --
   ----------------

   procedure Add_Fields
     (List : in out Xref_List;
      Item : access Type_Item_Record'Class)
   is
      use Entity_Information_Arrays;

      Field         : Entity_Information;
      Is_Enum       : constant Boolean :=
                        Get_Kind (Item.Entity).Kind = Enumeration_Kind;
      Discriminants : Entity_Reference_Iterator;
      Iter          : Calls_Iterator;
      Fields        : Entity_Information_List;

   begin
      Find_All_References
        (Iter    => Discriminants,
         Entity  => Item.Entity,
         In_File => Get_File (Get_Declaration_Of (Item.Entity)),
         Filter  => (Discriminant => True, others => False));

      while not At_End (Discriminants) loop
         Field := Get_Entity (Discriminants);
         if Field /= null then
            Add_Type (List, Item, Field,
                      -"Discriminant: " & Get (Get_Name (Field)).all);
         end if;

         Next (Discriminants);
      end loop;

      Iter := Get_All_Called_Entities (Item.Entity);
      while not At_End (Iter) loop
         Field := Get (Iter);

         if Field /= null then
            --  Hide discriminants (already displayed) and subprograms
            --  (would happen in C++, but these are primitive operations in
            --  this case)

            if not Get_Kind (Field).Is_Type   --  only variables
              and then In_Range (Get_Declaration_Of (Field), Item.Entity)
              and then not Is_Discriminant (Field, Item.Entity)
              and then (Get_Kind (Field).Kind /= Function_Or_Operator
                        and then Get_Kind (Field).Kind /= Procedure_Kind
                        and then Get_Kind (Field).Kind /= Package_Kind)
            then
               Entity_Information_Arrays.Append (Fields, Field);
            end if;
         end if;

         Next (Iter);
      end loop;

      Sort (Fields, Sort_By => Sort_Source_Order);

      for F in Entity_Information_Arrays.First .. Last (Fields) loop
         if Is_Enum then
            Add_Line (List, Get (Get_Name (Fields.Table (F))).all);
         else
            Add_Type (List, Item, Fields.Table (F),
                      Get (Get_Name (Fields.Table (F))).all);
         end if;
      end loop;

   end Add_Fields;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type
     (List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Entity_Information;
      Prefix : String)
   is
      Typ        : constant Entity_Information := Get_Variable_Type (Entity);
      Info_Added : Boolean := False;
   begin
      if Typ = null then
         --  Special handling for anonymous types, as generated by GNAT

         if Get_Kind (Entity).Kind = Array_Kind then
            Add_Array_Type (List, Item, Entity, Info_Added);
         elsif Get_Kind (Entity).Kind = Access_Kind then
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
            Prefix & ": " & Entity_As_Link (Typ),
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
      Entity     : Entity_Information;
      Info_Added : out Boolean)
   is
      Typ : constant Entity_Information := Array_Contents_Type (Entity);
      Indexes : constant Entity_Information_Array :=
        Array_Index_Types (Entity);
   begin
      if Typ /= null then
         Add_Line (List, "array ");
         for Ind in Indexes'Range loop
            if Ind = Indexes'First then
               if Ind = Indexes'Last then
                  Add_Line (List, "  (" & Entity_As_Link (Indexes (Ind)) & ")",
                            Callback => (1 => Build (Item, Indexes (Ind))));
               else
                  Add_Line (List, "  (" & Entity_As_Link (Indexes (Ind)) & ",",
                            Callback => (1 => Build (Item, Indexes (Ind))));
               end if;
            elsif Ind = Indexes'Last then
               Add_Line (List, "   " & Entity_As_Link (Indexes (Ind)) & ")",
                         Callback => (1 => Build (Item, Indexes (Ind))));
            else
               Add_Line (List, "   " & Entity_As_Link (Indexes (Ind)) & ",",
                         Callback => (1 => Build (Item, Indexes (Ind))));
            end if;
         end loop;

         Add_Line (List, "of " & Entity_As_Link (Typ),
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
      Entity     : Entity_Information;
      Info_Added : out Boolean)
   is
      Typ : constant Entity_Information := Pointed_Type (Entity);
   begin
      if Typ /= null then
         Add_Line
           (List, "access to " & Entity_As_Link (Typ),
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
      Entity       : Entity_Information;
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

      Highlight (Browser_Item (New_Item));
   end Add_Item_And_Link;

   --------------------------------
   -- Find_Parent_Or_Child_Types --
   --------------------------------

   procedure Find_Parent_Or_Child_Types
     (Item    : access Arrow_Item_Record'Class;
      Members : Entity_Information_Array;
      Parents : Boolean)
   is
      It : constant Type_Item := Type_Item (Item);
      Cr : Cairo_Context;
   begin
      for P in Members'Range loop
         Add_Item_And_Link
           (It, Members (P), "",
            Parent_Link => True,
            Reverse_Link => not Parents);
      end loop;

      if Parents then
         Set_Parents_Shown (It, True);
      else
         Set_Children_Shown (It, True);
      end if;

      Cr := Create (Item);
      Redraw_Title_Bar (Item, Cr);
      Destroy (Cr);

      Layout (Type_Browser (Get_Browser (Item)), Force => False);
      Refresh_Canvas (Get_Canvas (Get_Browser (Item)));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Find_Parent_Or_Child_Types;

   -----------------------
   -- Find_Parent_Types --
   -----------------------

   procedure Find_Parent_Types (Item  : access Arrow_Item_Record'Class) is
   begin
      Find_Parent_Or_Child_Types
        (Item, Get_Parent_Types (Type_Item (Item).Entity), Parents => True);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Find_Parent_Types;

   ----------------------
   -- Find_Child_Types --
   ----------------------

   procedure Find_Child_Types (Item  : access Arrow_Item_Record'Class) is
      Iter  : Children_Iterator := Get_Child_Types (Type_Item (Item).Entity);
      Child : Entity_Information;
      Cr    : Cairo_Context;
   begin
      --  ??? Should be done in background
      while not At_End (Iter) loop
         Child := Get (Iter);
         if Child /= null then
            Add_Item_And_Link
              (Type_Item (Item), Child, "",
               Parent_Link => True,
               Reverse_Link => True);
         end if;
         Next (Iter);
      end loop;
      Destroy (Iter);

      Set_Children_Shown (Type_Item (Item), True);
      Cr := Create (Item);
      Redraw_Title_Bar (Item, Cr);
      Destroy (Cr);
      Layout (Type_Browser (Get_Browser (Item)), Force => False);
      Refresh_Canvas (Get_Canvas (Get_Browser (Item)));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Find_Child_Types;

   -------------------------
   -- Hide_Show_Inherited --
   -------------------------

   procedure Hide_Show_Inherited
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class)
   is
      It : constant Type_Item := Type_Item (Item);
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
         It.Inherited_Primitives := not It.Inherited_Primitives;
         Trace (Me, "Hide_Show_Inherited => "
                & It.Inherited_Primitives'Img);
         Refresh (It);
         Item_Updated (Get_Canvas (Get_Browser (It)), It);
      end if;
   end Hide_Show_Inherited;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Type_Item_Record;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      W, H                           : Gint;
      Layout_H, Layout_W1, Layout_W2 : Gint;
      Meth_Layout_W1, Meth_Layout_W2 : Gint;
      Meth_Layout_H                  : Gint;
      Y                              : Gint;
      General_Lines                  : Xref_List;
      Attr_Lines                     : Xref_List;
      Meth_Lines                     : Xref_List;
      Parent                         : Entity_Information;
      Added                          : Boolean;
      Style                          : Gtk_Style;

   begin
      Trace (Me, "Resize_And_Draw: " & Get_Full_Name (Item.Entity));
      Update_Xref (Get_File (Get_Declaration_Of (Item.Entity)));

      Add_Line (General_Lines, -Kind_To_String (Get_Kind (Item.Entity)));

      if not Get_Kind (Item.Entity).Is_Type then
         Add_Type (Attr_Lines, Item, Item.Entity, "of type");

      elsif Is_Subtype (Item.Entity) then
         Parent := Get_Variable_Type (Item.Entity);
         Add_Line
           (Attr_Lines, -"subtype of " & Entity_As_Link (Parent),
            Callback => (1 => Build (Item, Parent)));
         --  Do not destroy Parent, needed for callbacks

      else
         case Get_Kind (Item.Entity).Kind is
            when Overloaded_Entity
              | Unresolved_Entity
              | Private_Type
              | Private_Object
              | Label_On_Block
              | Label_On_Loop
              | Label_On_Statement
              =>
               null;

            when Boolean_Kind
              | Decimal_Fixed_Point
              | Enumeration_Literal
              | Exception_Entity
              | Floating_Point
              | Include_File
              | Macro
              | Function_Macro
              | Modular_Integer
              | Named_Number
              | Ordinary_Fixed_Point
              | Reference
              | Signed_Integer
              | String_Kind =>
               null;

            when Enumeration_Kind =>
               Add_Fields (Attr_Lines, Item);

            when Access_Kind =>
               Add_Access_Type (Attr_Lines, Item, Item.Entity, Added);

            when Array_Kind =>
               Add_Array_Type (Attr_Lines, Item, Item.Entity, Added);

            when Class_Wide
              | Class
              | Union
              | Record_Kind
              | Interface_Kind
              | Protected_Kind
              | Task_Kind =>
               Add_Primitive_Operations (Meth_Lines, Item);
               Add_Fields (Attr_Lines, Item);

            when Entry_Or_Entry_Family
              | Function_Or_Operator
              | Procedure_Kind =>
               Add_Parameters (Attr_Lines, Item);
               Set_Children_Shown (Item, True);

            when Package_Kind =>
               Add_Package_Contents
                 (General_Lines, Attr_Lines, Meth_Lines, Item);
         end case;
      end if;

      if not Parents_Shown (Item) then
         declare
            Parents : constant Entity_Information_Array :=
                        Get_Parent_Types (Item.Entity);
         begin
            Set_Parents_Shown (Item, Parents'Length = 0);
         end;
      end if;

      Get_Pixel_Size
        (Get_Browser (Item), General_Lines, Layout_W1, Layout_W2, Layout_H,
         Layout);
      W := Gint'Max (Width, Layout_W1 + Layout_W2);
      H := Height + Layout_H;

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

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access, Cr, W, H,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Margin + Yoffset;

      Display_Lines (Item, Cr, General_Lines, Margin + Xoffset, Y, 0, Layout);

      Display_Lines (Item, Cr, Attr_Lines, Margin + Xoffset + Left_Margin, Y,
                     Layout_W1, Layout);

      if Layout_H /= 0 and then Meth_Layout_H /= 0 then
         Y := Y + 2;
         Style := Get_Item_Style (Item);
         Draw_Line
           (Cr, Get_Fg (Style, State_Normal),
            X1       => X_Thickness (Style),
            Y1       => Y,
            X2       => Get_Coord (Item).Width - X_Thickness (Style) - 1,
            Y2       => Y);
         Y := Y + 1;
      end if;

      Display_Lines (Item, Cr, Meth_Lines, Margin + Xoffset + Left_Margin, Y,
                     Meth_Layout_W1, Layout);

      Free (Item.General_Lines);
      Free (Item.Attr_Lines);
      Free (Item.Meth_Lines);

      Item.General_Lines := General_Lines;
      Item.Attr_Lines    := Attr_Lines;
      Item.Meth_Lines    := Meth_Lines;

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

      if Get_Kind (Item.Entity).Is_Type then
         case Get_Kind (Item.Entity).Kind is
            when Class_Wide
              | Class
              | Record_Kind
              | Protected_Kind
              | Task_Kind
              =>
               --  ??? Should use a different icon depending on
               --  Item.Inherited_Primitives.
               Draw_Title_Bar_Button
                 (Item, Cr,
                  Num    => Get_Last_Button_Number (Item),
                  Pixbuf => Type_Browser (Get_Browser (Item)).Primitive_Button,
                  Cb     => Build (Hide_Show_Inherited'Access, Item));

            when others =>
               null;
         end case;
      end if;
   end Redraw_Title_Bar;

   ------------------------
   -- Add_Or_Select_Item --
   ------------------------

   function Add_Or_Select_Item
     (Browser : access Type_Browser_Record'Class;
      Entity  : Entity_Information) return Type_Item
   is
      Found : Type_Item;
      Iter  : Item_Iterator := Start (Get_Canvas (Browser));
   begin
      --  Check if there is already an item displaying Entity
      loop
         Found := Type_Item (Get (Iter));
         exit when Found = null
           or else Found.Entity = Entity;
         Next (Iter);
      end loop;

      if Found = null and then Entity /= null then
         Gtk_New (Found, Browser, Entity);
         Put (Get_Canvas (Browser), Found);
         Refresh (Found);
      end if;

      --  Need to always refresh the canvas so that the links are correctly
      --  displayed.
      Refresh_Canvas (Get_Canvas (Browser));

      return Found;
   end Add_Or_Select_Item;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      Child : GPS_MDI_Child;
   begin
      if Node.Tag.all = "Entities_Browser" then
         Gtk_New (Child, Open_Type_Browser (User),
                  Default_Width  => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height => Gint (Default_Widget_Height.Get_Pref),
                  Group          => Group_Graphs,
                  Module         => Entity_Browser_Module);
         Set_Title (Child, -"Entity Browser");
         Put (MDI, Child);

         return MDI_Child (Child);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Type_Browser_Record'Class then
         N := new Node;
         N.Tag := new String'("Entities_Browser");
         return N;
      end if;

      return null;
   end Save_Desktop;

   --------------------
   -- On_Show_Source --
   --------------------

   procedure On_Show_Source
     (Browser : access Gtk_Widget_Record'Class;
      Item    : Browser_Item)
   is
      B  : constant Type_Browser := Type_Browser (Browser);
      It : constant Type_Item    := Type_Item (Item);

      use Basic_Types;
   begin
      Add_Navigation_Location (Get_Kernel (B), -"Entity Browser");

      Open_File_Editor
        (Kernel     => Get_Kernel (B),
         Filename   => Get_Filename
           (Get_File (Get_Declaration_Of (It.Entity))),
         Line       => Get_Line (Get_Declaration_Of (It.Entity)),
         Column     => Get_Column (Get_Declaration_Of (It.Entity)),
         Column_End => Get_Column (Get_Declaration_Of (It.Entity)) +
           Basic_Types.Visible_Column_Type
             (Get (Get_Name (It.Entity))'Length));
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
   begin
      Set_Entity_Information
        (Context       => Context,
         Entity_Name   => Get_Name (Item.Entity),
         Entity_Column => Get_Column (Get_Declaration_Of (Item.Entity)));
      Set_File_Information
        (Context     => Context,
         Files       =>
           (1 => Get_Filename (Get_File (Get_Declaration_Of (Item.Entity)))),
         Line        => Get_Line (Get_Declaration_Of (Item.Entity)),
         Column      => Get_Column (Get_Declaration_Of (Item.Entity)));
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
      Browser : constant Type_Browser := Type_Browser (Child);
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

   ---------------
   -- Highlight --
   ---------------

   overriding procedure Highlight (Item : access Type_Item_Record) is
      Cr : constant Cairo_Context := Create (Item);
   begin
      Redraw_Title_Bar (Browser_Item (Item), Cr);
      Destroy (Cr);
   end Highlight;

   -----------------------
   -- Get_Background_GC --
   -----------------------

   overriding function Get_Item_Style
     (Item : access Type_Item_Record) return Gtk.Style.Gtk_Style is
   begin
      return Get_Default_Item_Style (Item);
   end Get_Item_Style;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Generic_Item_Record;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class) is
   begin
      Yoffset := Yoffset + Generic_Item_Box_Height_Top;
      Resize_And_Draw
        (Type_Item_Record (Item.all)'Access, Cr,
         Width, Height + Generic_Item_Box_Height_Top,
         Width_Offset + Generic_Item_Box_Width_Right,
         Height_Offset, Xoffset, Yoffset, Layout);

      Draw_Rectangle
        (Cr, Get_Base (Get_Item_Style (Item), State_Normal),
         Filled => True,
         X      => Get_Coord (Item).Width - Generic_Item_Box_Width + 1,
         Y      => 1,
         Width  => Generic_Item_Box_Width - 2,
         Height => Generic_Item_Box_Height - 2);
      Draw_Shadow
        (Cr,
         Style       => Get_Style (Get_Browser (Item)),
         Shadow_Type => Shadow_Out,
         X           => Get_Coord (Item).Width - Generic_Item_Box_Width,
         Y           => 0,
         Width       => Generic_Item_Box_Width,
         Height      => Generic_Item_Box_Height);
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
