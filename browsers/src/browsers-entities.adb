-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Ada.Exceptions;       use Ada.Exceptions;
with GNAT.Heap_Sort_G;
with GNAT.Strings;         use GNAT.Strings;
with GNAT.OS_Lib;

with Src_Info.Queries;     use Src_Info, Src_Info.Queries;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Browsers.Canvas;      use Browsers.Canvas;
with Traces;               use Traces;
with Glide_Intl;           use Glide_Intl;
with Shell;                use Shell;

with Glib;          use Glib;
with Glib.Convert;  use Glib.Convert;
with Glib.Object;   use Glib.Object;
with Glib.Xml_Int;  use Glib.Xml_Int;
with Gdk.GC;        use Gdk.GC;
with Gdk.Event;     use Gdk.Event;
with Gdk.Drawable;  use Gdk.Drawable;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Region;    use Gdk.Region;
with Gdk.Window;    use Gdk.Window;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Main;      use Gtk.Main;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Stock;     use Gtk.Stock;
with Gtk.Style;     use Gtk.Style;
with Gtk.Widget;    use Gtk.Widget;
with Gtkada.Canvas; use Gtkada.Canvas;
with Gtkada.MDI;    use Gtkada.MDI;
with Pango.Layout;  use Pango.Layout;
with Gtkada.Types;

with String_Utils;  use String_Utils;

package body Browsers.Entities is

   Me : constant Debug_Handle := Create ("Browser.Entities");

   Entity_Browser_Module : Module_ID;

   Left_Margin : constant := 20;
   --  Indentation for the attributes and methods layouts.

   UML_Abstract : constant String := "{Abstract}";
   --  String used in UML to indicate that an entity is abstract

   Generic_Item_Box_Width_Right : constant := 10;
   Generic_Item_Box_Width      : constant := Generic_Item_Box_Width_Right + 30;
   --  The position of the templates parameters box for generic items.
   --  Right refers to the position from the right side of the item.

   Generic_Item_Box_Height_Top : constant := 10;
   Generic_Item_Box_Height     : constant := Generic_Item_Box_Height_Top + 17;
   --  Height of the top-rigth box for generic items.

   ------------------
   -- Type browser --
   ------------------

   type Type_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Primitive_Button : Gdk.Pixbuf.Gdk_Pixbuf;
      Idle_Id          : Gtk.Main.Idle_Handler_Id;
   end record;
   type Type_Browser is access all Type_Browser_Record'Class;

   ---------------
   -- Type item --
   ---------------

   type Type_Item_Record is new Browsers.Canvas.Arrow_Item_Record with record
      Entity : Src_Info.Queries.Entity_Information;
      Inherited_Primitives : Boolean := False;
   end record;
   type Type_Item is access all Type_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Type_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Open a new item in the browser that represents Entity.
   --  A copy of Entity is made, thus the caller should free Entity.

   procedure Initialize
     (Item    : access Type_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information);
   --  Internal initialization function

   function Get_Background_GC
     (Item : access Type_Item_Record) return Gdk.GC.Gdk_GC;
   procedure Resize_And_Draw
     (Item             : access Type_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   function Contextual_Factory
     (Item  : access Type_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   function Get_Last_Button_Number (Item : access Type_Item_Record)
      return Glib.Gint;
   procedure Redraw_Title_Bar (Item : access Type_Item_Record);
   procedure Highlight (Item : access Type_Item_Record);
   --  See doc for inherited subprograms

   ------------------
   -- Generic item --
   ------------------
   --  This type is used to represent generic items

   type Generic_Item_Record is new Type_Item_Record with null record;

   procedure Resize_And_Draw
     (Item             : access Generic_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   function Point_In_Item
     (Item   : access Generic_Item_Record;
      X, Y   : Glib.Gint) return Boolean;
   procedure Draw
     (Item   : access Generic_Item_Record;
      Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      Xdest  : Glib.Gint;
      Ydest  : Glib.Gint);
   procedure Clip_Line
     (Src   : access Generic_Item_Record;
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

   procedure Draw_Straight_Line
     (Link : access Parent_Link_Record;
      Window : Gdk.Window.Gdk_Window;
      GC : Gdk.GC.Gdk_GC;
      Parent_Side : Gtkada.Canvas.Item_Side;
      X1, Y1 : Glib.Gint;
      Child_Side : Gtkada.Canvas.Item_Side;
      X2, Y2 : Glib.Gint);
   --  See doc for inherited subprogram

   ----------
   -- Misc --
   ----------

   procedure On_Type_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Entity browser menu

   function Open_Type_Browser_Child
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Create (or return an existing) type browser, and insert it directly into
   --  the MDI.
   --  If a new browser is created, it is initially empty.
   --  If the browser already existed, it is raised.

   function Open_Type_Browser
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Type_Browser;
   --  Create (or return an existing) type browser. The newly created browser
   --  is not inserted into the MDI.

   function Add_Or_Select_Item
     (Browser : access Type_Browser_Record'Class;
      Entity  : Entity_Information) return Type_Item;
   --  Create (or return an existing) item displaying the information for
   --  Entity.

   procedure Add_Or_Select_Item
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Same as above, but adapted for contextual menus

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

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries in the contextual menus of other modules

   type Show_Entity_Callback is new Active_Area_Callback with record
      Item    : Browser_Item;
      Entity  : Entity_Information;
      Link_Name : GNAT.Strings.String_Access;
   end record;

   function Call (Callback : Show_Entity_Callback;
                  Event    : Gdk.Event.Gdk_Event) return Boolean;
   procedure Destroy (Callback : in out Show_Entity_Callback);
   --  See inherated doc

   function Build
     (Item   : access Browser_Item_Record'Class;
      Entity : Entity_Information;
      Link_Name : String := "") return Active_Area_Cb;
   --  Build a new callback to display entities.

   function Show_Entity_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Command handler for this module (in the shell window)

   function "<" (E1, E2 : Entity_Information) return Boolean;

   procedure Add_Primitive_Operations
     (List     : in out Xref_List;
      Kernel   : access Kernel_Handle_Record'Class;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr);
   --  Add the sorted list of primitive operations for Entity at the end of
   --  Meth_Layout.

   procedure Add_Parameters
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr);
   --  Add the list of parameters for Entity (a subprogram) to the end of
   --  Attr_Layout.

   procedure Add_Fields
     (Kernel   : access Kernel_Handle_Record'Class;
      List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr);
   --  Add the list of fields for a record-like entity to the end of
   --  Attr_Layout.
   --  This is also usable to get the enumeration literals for an enumeration
   --  type.

   procedure Add_Package_Contents
     (General_List : in out Xref_List;
      Attr_List    : in out Xref_List;
      Meth_List    : in out Xref_List;
      Item         : access Type_Item_Record'Class;
      Lib_Info     : LI_File_Ptr);
   --  Add the parent package for Entity at the end of Attr_Layout

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create a current kernel context, based on the currently selected item

   procedure On_Show_Source
     (Browser : access Gtk_Widget_Record'Class; Item : Browser_Item);
   --  Display a source editor to show the declaration of the entity

   procedure Find_Parent_Types (Item  : access Arrow_Item_Record'Class);
   --  Display the parent types for the item

   procedure Find_Children_Types (Item  : access Arrow_Item_Record'Class);
   --  Display the children types for the item

   procedure Hide_Show_Inherited
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class);
   --  Change the status of inherited primitive operations (shown or hidden)

   procedure Add_Type
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information;
      Prefix   : String);
   --  Add a new line in List, starting with prefix and followed by an
   --  hyper link for the type of Entity.

   function Entity_As_Link (Ent : Entity_Information) return String;
   --  Return a string that contains the entity name, as an hyper link. If
   --  Entity is in fact a predefined entity, no link is setup.

   procedure Add_Subprogram
     (List   : in out Xref_List;
      Item   : access Type_Item_Record'Class;
      Entity : Entity_Information);
   --  Add a line that describes the subprogram Entity.
   --  Entity should not be freed by the caller

   procedure Sort (Arr : in out Entity_Information_Array);
   --  Sort the array alphabetically

   type Find_Children_Types_Data is record
      Iter    : Entity_Reference_Iterator_Access;
      Item    : Type_Item;
      Kernel  : Kernel_Handle;
      Browser : Type_Browser;
   end record;

   package Children_Types_Idle is new Gtk.Main.Idle (Find_Children_Types_Data);

   procedure Destroy_Idle (Data : in out Find_Children_Types_Data);
   function Find_Children_Types_Idle
     (Data : Find_Children_Types_Data) return Boolean;
   --  Subprograms used for the lazy computation of children entities

   ----------
   -- Call --
   ----------

   function Call
     (Callback : Show_Entity_Callback;
      Event    : Gdk.Event.Gdk_Event) return Boolean is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         Add_Item_And_Link
           (Type_Item (Callback.Item), Callback.Entity,
            Callback.Link_Name.all, Parent_Link => False);
         return True;
      end if;
      return False;
   end Call;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Callback : in out Show_Entity_Callback) is
   begin
      Destroy (Callback.Entity);
      Free (Callback.Link_Name);
   end Destroy;

   -----------
   -- Build --
   -----------

   function Build
     (Item   : access Browser_Item_Record'Class;
      Entity : Entity_Information;
      Link_Name : String := "") return Active_Area_Cb is
   begin
      return new Show_Entity_Callback'
        (Active_Area_Callback with
         Item => Browser_Item (Item),
         Link_Name => new String'(Link_Name),
         Entity => Entity);
   end Build;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Register_Module
        (Module                  => Entity_Browser_Module,
         Kernel                  => Kernel,
         Module_Name             => "Entity_Browser",
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access,
         MDI_Child_Tag           => Type_Browser_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & (-"Tools"),
         Text        => -"Entity Browser",
         Callback    => On_Type_Browser'Access);
      Register_Command
        (Kernel,
         Command      => "entity.show",
         Usage        => "entity.show entity_name file_name [line] [column}",
         Description  =>
           -("Display in the type browser the informations known about the"
             & " entity: list of fields for records, list of primitive"
             & " subprograms or methods, list of parameters, ..."),
         Minimum_Args => 2,
         Maximum_Args => 4,
         Handler      => Show_Entity_Command_Handler'Access);
   end Register_Module;

   ---------------------------------
   -- Show_Entity_Command_Handler --
   ---------------------------------

   function Show_Entity_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      Entity : Entity_Information;
      Child  : MDI_Child;
      Status : Find_Decl_Or_Body_Query_Status;
      Name, File : String_Access;
      L, C : Positive := 1;
      Item  : Type_Item;
      pragma Unreferenced (Item);

   begin
      if Command = "entity.show" then
         Child := Open_Type_Browser_Child (Kernel);

         Name   := Args (Args'First);
         File   := Args (Args'First + 1);

         if Args'First + 2 <= Args'Last then
            L := Positive'Value (Args (Args'First + 2).all);

            if Args'First + 3 <= Args'Last then
               C := Positive'Value (Args (Args'First + 3).all);
            end if;
         end if;

         Find_Declaration_Or_Overloaded
           (Kernel      => Kernel,
            Lib_Info    => Locate_From_Source_And_Complete (Kernel, File.all),
            File_Name   => File.all,
            Entity_Name => Name.all,
            Line        => L,
            Column      => C,
            Entity      => Entity,
            Status      => Status);

         if Status /= Success and then Status /= Fuzzy_Match then
            return "Entity not found";
         end if;

         Item := Add_Or_Select_Item
           (Browser => Type_Browser (Get_Widget (Child)), Entity  => Entity);
         Destroy (Entity);
      end if;

      return "";

   exception
      when E : others =>
         Trace (Me, "Show_Entity_Command_Handler, unexpected exception "
                & Exception_Information (E));
         return "error";
   end Show_Entity_Command_Handler;

   -----------------------------
   -- Browser_Contextual_Menu --
   -----------------------------

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Item           : Gtk_Menu_Item;
      Entity_Context : Entity_Selection_Context_Access;

   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity_Context := Entity_Selection_Context_Access (Context);

         if Has_Entity_Name_Information (Entity_Context) then
            Gtk_New
              (Item, Label => (-"Examine entity ")
               & Locale_To_UTF8
                 (Krunch (Entity_Name_Information (Entity_Context))));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Add_Or_Select_Item'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Browser_Contextual_Menu;

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
      Initialize (Browser, Kernel,
                  Create_Toolbar  => False,
                  Parents_Pixmap  => Stock_Go_Up,
                  Children_Pixmap => Stock_Go_Down);

      --  ??? Should be freed when browser is destroyed.
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child : MDI_Child;
      Browser : Type_Browser;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Type_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Open_Type_Browser (Kernel);
         Child := Put
           (Get_MDI (Kernel), Browser,
            Focus_Widget => Gtk_Widget (Get_Canvas (Browser)),
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height));
         Set_Focus_Child (Child);
         Set_Title (Child, -"Entity Browser");
      end if;

      return Child;
   end Open_Type_Browser_Child;

   ---------------------
   -- On_Type_Browser --
   ---------------------

   procedure On_Type_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Context : Selection_Context_Access :=
        Get_Current_Context (Kernel);
      Child : MDI_Child;
      Item  : Type_Item;
      pragma Unreferenced (Widget, Item);

   begin
      Ref (Context);
      Child := Open_Type_Browser_Child (Kernel);

      if Context /= null
        and then Context.all in Entity_Selection_Context'Class
      then
         Item := Add_Or_Select_Item
           (Browser => Type_Browser (Get_Widget (Child)),
            Entity  => Get_Entity (Entity_Selection_Context_Access (Context)));
      end if;

      Unref (Context);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_Type_Browser "
                & Exception_Information (E));
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
            Get_Name (Entity) & ASCII.LF & "   " & UML_Abstract,
            Find_Parent_Types'Access, Find_Children_Types'Access);
      else
         Initialize
           (Item, Browser, Get_Name (Entity),
            Find_Parent_Types'Access, Find_Children_Types'Access);
      end if;
      Item.Entity := Copy (Entity);
   end Initialize;

   --------
   -- E1 --
   --------

   function "<" (E1, E2 : Entity_Information) return Boolean is
   begin
      return Get_Name (E1) < Get_Name (E2);
   end "<";

   --------------------
   -- Entity_As_Link --
   --------------------

   function Entity_As_Link (Ent : Entity_Information) return String is
   begin
      if Is_Predefined_Entity (Ent) then
         return Get_Name (Ent);
      else
         return '@' & Get_Name (Ent) & '@';
      end if;
   end Entity_As_Link;

   ----------
   -- Sort --
   ----------

   procedure Sort (Arr : in out Entity_Information_Array) is
      procedure Move (From, To : Natural);
      function Lt (Op1, Op2 : Natural) return Boolean;
      --  See GNAT.Heap_Sort_G

      procedure Move (From, To : Natural) is
      begin
         Arr (To) := Arr (From);
      end Move;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Arr (Op1) < Arr (Op2);
      end Lt;

      package Entity_Sort is new GNAT.Heap_Sort_G (Move, Lt);
   begin
      Entity_Sort.Sort (Arr'Last);
   end Sort;

   ------------------------------
   -- Add_Primitive_Operations --
   ------------------------------

   procedure Add_Primitive_Operations
     (List     : in out Xref_List;
      Kernel   : access Kernel_Handle_Record'Class;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr)
   is
      Prim : Primitive_Iterator := Get_Primitive_Operations
        (Lib_Info => Lib_Info, Entity => Item.Entity);
      L : constant Natural := Length (Prim);

      Arr : Entity_Information_Array (0 .. L);
      --  Index 0 is needed for heap sort

      Parent_Lib_Info : LI_File_Ptr;
      Parent_Iter : Parent_Iterator;
      Parent : Entity_Information;
      Parent_Prim : Primitive_Iterator;
      Op    : Entity_Information;

   begin
      for Index in 1 .. Arr'Last loop
         Arr (Index) := Get (Prim);
         Next (Prim);
      end loop;

      Sort (Arr);

      Trace (Me, "Add_Primitive_Operations: Inherited_Primitives="
             & Item.Inherited_Primitives'Img);

      if not Item.Inherited_Primitives then
         --  For each inherited operation, remove it from the list of
         --  operations to display.
         Parent_Iter := Get_Parent_Types (Lib_Info, Item.Entity);
         loop
            Parent := Get (Parent_Iter);
            exit when Parent = No_Entity_Information;

            Parent_Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Parent));
            Parent_Prim := Get_Primitive_Operations
              (Parent_Lib_Info, Parent);

            loop
               Op := Get (Parent_Prim);
               exit when Op = No_Entity_Information;

               for A in 1 .. L loop
                  if Arr (A) /= No_Entity_Information
                    and then Is_Equal (Arr (A), Op)
                  then
                     Destroy (Arr (A));
                     exit;
                  end if;
               end loop;

               Destroy (Op);
               Next (Parent_Prim);
            end loop;

            Destroy (Parent);
            Next (Parent_Iter);
         end loop;
      end if;

      for E in 1 .. L loop
         if Arr (E) /= No_Entity_Information then
            Add_Subprogram (List, Item, Arr (E));
         end if;
      end loop;
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
         Callback => Build (Item, Entity));
      --  Do not free Entity, it's needed for callbacks
   end Add_Subprogram;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr)
   is
      Subs : Subprogram_Iterator := Get_Subprogram_Parameters
        (Lib_Info   => Lib_Info, Subprogram => Item.Entity);
      Typ, Parameter : Entity_Information;
      Returned : constant Entity_Information := Returned_Type
        (Lib_Info, Item.Entity);
   begin
      loop
         Parameter := Get (Subs);
         exit when Parameter = No_Entity_Information;

         --  In some cases, access parameters reference their pointed type
         --  through Pointed_Type. However, if that access type is a renaming
         --  of another type, we'll need to try Get_Variable_Type if
         --  Pointed_Type didn't return anything.

         Typ := No_Entity_Information;

         if Get_Kind (Parameter).Kind = Access_Kind then
            Typ := Pointed_Type (Lib_Info, Parameter);
         end if;

         if Typ = No_Entity_Information then
            Typ := Get_Variable_Type (Lib_Info, Parameter);
         end if;

         Add_Line
           (List,
            Get_Name (Parameter) & ": "
            & Image (Get_Type (Subs)) & " " & Entity_As_Link (Typ),
            Length1 => Get_Name (Parameter)'Length + 1,
            Callback => Build (Item, Typ));
         --  Do not free Typ, it is needed for callbacks

         Destroy (Parameter);
         Next (Subs);
      end loop;

      if Returned /= No_Entity_Information then
         Add_Line
           (List,
            "return " & Entity_As_Link (Returned),
            Length1 => 7,
            Callback => Build (Item, Returned));
         --  Do not free Returned, it is needed for calbacks.
      end if;
   end Add_Parameters;

   --------------------------
   -- Add_Package_Contents --
   --------------------------

   procedure Add_Package_Contents
     (General_List : in out Xref_List;
      Attr_List    : in out Xref_List;
      Meth_List    : in out Xref_List;
      Item         : access Type_Item_Record'Class;
      Lib_Info     : LI_File_Ptr)
   is
      Parent : constant Entity_Information :=
        Get_Parent_Package (Lib_Info, Item.Entity);

      Tree : Scope_Tree := Create_Tree (Lib_Info);
      Iter, Iter2 : Scope_Tree_Node_Iterator;
      Count : Natural := 0;
   begin
      if Parent /= No_Entity_Information then
         Add_Line (General_List, "Parent: " & Entity_As_Link (Parent),
                   Callback => Build (Item, Parent));
         --  Do not destroy parent, needed for callbacks
      end if;

      Iter := Start (Find_Entity_Scope (Tree, Item.Entity));
      Iter2 := Iter;

      while Get (Iter2) /= Null_Scope_Tree_Node loop
         if Is_Declaration (Get (Iter2)) then
            Count := Count + 1;
         end if;
         Next (Iter2);
      end loop;

      declare
         --  Index 0 is needed for heap sort
         Arr : Entity_Information_Array (0 .. Count);
      begin
         Count := 1;

         while Get (Iter) /= Null_Scope_Tree_Node loop
            if Is_Declaration (Get (Iter)) then
               Arr (Count) := Get_Entity (Get (Iter));
               Count := Count + 1;
            end if;
            Next (Iter);
         end loop;

         Sort (Arr);

         for A in Arr'First + 1 .. Arr'Last loop
            if Is_Subprogram (Arr (A)) then
               Add_Subprogram (Meth_List, Item, Arr (A));

            elsif Get_Kind (Arr (A)).Is_Type then
               declare
                  Name : constant String := Entity_As_Link (Arr (A));
               begin
                  if Is_Subtype (Lib_Info, Arr (A)) then
                     Add_Line
                       (Attr_List, Name & "(subtype)",
                        Length1 => Name'Length,
                        Callback => Build (Item, Arr (A), ""));
                  else
                     Add_Line
                       (Attr_List, Name & "(type)",
                        Length1 => Name'Length,
                        Callback => Build (Item, Arr (A), ""));
                  end if;
               end;

            else
               Add_Type (Attr_List, Item, Lib_Info, Arr (A),
                         Get_Name (Arr (A)));
            end if;
         end loop;
      end;

      Free (Tree);
   end Add_Package_Contents;

   ----------------
   -- Add_Fields --
   ----------------

   procedure Add_Fields
     (Kernel      : access Kernel_Handle_Record'Class;
      List        : in out Xref_List;
      Item        : access Type_Item_Record'Class;
      Lib_Info    : LI_File_Ptr)
   is
      Tree : Scope_Tree;
      Node : Scope_Tree_Node;
      Iter : Scope_Tree_Node_Iterator;
      Field : Entity_Information;
      Is_Enum : constant Boolean :=
        Get_Kind (Item.Entity).Kind = Enumeration_Kind;
   begin
      Get_Scope_Tree (Kernel, Item.Entity, Tree, Node);

      if Node /= Null_Scope_Tree_Node then
         Iter := Start (Node);

         loop
            Node := Get (Iter);
            exit when Node = Null_Scope_Tree_Node;

            if Is_Declaration (Node) then
               Field := Get_Entity (Node);

               if Is_Enum then
                  Add_Line (List, Get_Name (Field));
               else
                  Add_Type (List, Item, Lib_Info, Field, Get_Name (Field));
               end if;
               Destroy (Field);
            end if;

            Next (Iter);
         end loop;
      end if;

      Free (Tree);
   end Add_Fields;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information;
      Prefix   : String)
   is
      Typ : constant Entity_Information :=
        Get_Variable_Type (Lib_Info, Entity);
   begin
      if Typ = No_Entity_Information then
         Add_Line (List, Prefix & " ???", Length1 => Prefix'Length + 1);
      else
         Add_Line
           (List,
            Prefix & ": " & Entity_As_Link (Typ),
            Length1 => Prefix'Length + 2,
            Callback => Build (Item, Typ, Prefix));
         --  Do not free Typ, needed for callbacks
      end if;
   end Add_Type;

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
            Add_Link (Canvas, Link, New_Item, Item, Descr => Link_Name);
         end if;

      elsif not Has_Link (Canvas, Item, New_Item, Link_Name) then
         Link := new Browser_Link_Record;
         Add_Link (Canvas, Link, Item, New_Item, Descr => Link_Name);
      end if;

      Highlight (Browser_Item (New_Item));
   end Add_Item_And_Link;

   -----------------------
   -- Find_Parent_Types --
   -----------------------

   procedure Find_Parent_Types (Item  : access Arrow_Item_Record'Class) is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Iter : Parent_Iterator;
      Lib_Info : LI_File_Ptr;
      It : constant Type_Item := Type_Item (Item);
      Parent : Entity_Information;
   begin
      Push_State (Kernel, Busy);
      Lib_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Declaration_File_Of (It.Entity));
      Iter := Get_Parent_Types (Lib_Info, It.Entity);

      loop
         Parent := Get (Iter);
         exit when Parent = No_Entity_Information;

         Add_Item_And_Link (It, Parent, "", Parent_Link => True);

         Destroy (Parent);
         Next (Iter);
      end loop;

      Set_Parents_Shown (It, True);
      Redraw_Title_Bar (Item);
      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Find_Parent_Types: unexpected exception "
                & Exception_Information (E));
   end Find_Parent_Types;

   ------------------------------
   -- Find_Children_Types_Idle --
   ------------------------------

   function Find_Children_Types_Idle
     (Data : Find_Children_Types_Data) return Boolean
   is
      LI    : LI_File_Ptr;
      Child : Child_Type_Iterator;
      C     : Entity_Information;
   begin
      if Get (Data.Iter.all) = No_Reference then
         return False;

      else
         LI := Get_LI (Data.Iter.all);

         --  The following loop is fast enough that we should do it all at once
         --  in the idle callback. It only acts on a single LI file
         Child := Get_Children_Types (LI, Data.Item.Entity);

         loop
            C := Get (Child);
            exit when C = No_Entity_Information;

            Add_Item_And_Link
              (Data.Item, C, "", Parent_Link => True,
               Reverse_Link => True);
            Destroy (C);
            Next (Child);
         end loop;

         Destroy (Child);

         Next (Get_Language_Handler (Data.Kernel), Data.Iter.all,
               Get_LI_File_List (Data.Kernel));
         return True;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Find_Children_Types_Idle;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Find_Children_Types_Data) is
   begin
      Layout (Data.Browser, Force => False);
      Destroy (Data.Iter);
      Pop_State (Data.Kernel);
      Data.Browser.Idle_Id := 0;
   end Destroy_Idle;

   -------------------------
   -- Find_Children_Types --
   -------------------------

   procedure Find_Children_Types  (Item  : access Arrow_Item_Record'Class) is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Data   : Find_Children_Types_Data :=
        (Kernel  => Kernel,
         Browser => Type_Browser (Get_Browser (Item)),
         Item    => Type_Item (Item),
         Iter    => new Entity_Reference_Iterator);
   begin
      Push_State (Kernel, Busy);
      Find_All_References
        (Root_Project => Get_Project (Kernel),
         Lang_Handler => Get_Language_Handler (Kernel),
         Entity       => Type_Item (Item).Entity,
         List         => Get_LI_File_List (Kernel),
         Iterator     => Data.Iter.all,
         LI_Once      => True);

      Data.Browser.Idle_Id := Children_Types_Idle.Add
        (Cb       => Find_Children_Types_Idle'Access,
         D        => Data,
         Priority => Priority_Low_Idle,
         Destroy  => Destroy_Idle'Access);

      Set_Children_Shown (Item, True);
      Redraw_Title_Bar (Item);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Find_Children_Types: unexpected exception "
                & Exception_Information (E));
   end Find_Children_Types;

   -------------------------
   -- Hide_Show_Inherited --
   -------------------------

   procedure Hide_Show_Inherited
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class)
   is
      It : Type_Item := Type_Item (Item);
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
         It.Inherited_Primitives := not It.Inherited_Primitives;
         Refresh (It);
         Item_Updated (Get_Canvas (Get_Browser (It)), It);
      end if;
   end Hide_Show_Inherited;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Type_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      W, H, Layout_H, Layout_W1, Layout_W2,
        Meth_Layout_W1, Meth_Layout_W2, Meth_Layout_H : Gint;
      Y : Gint;
      General_Lines, Attr_Lines, Meth_Lines : Xref_List;
      Parent : Entity_Information;
      Lib_Info : LI_File_Ptr;
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
   begin
      Trace (Me, "Resize_And_Draw: " & Get_Name (Item.Entity));

      Lib_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Declaration_File_Of (Item.Entity));

      Add_Line (General_Lines, -Kind_To_String (Get_Kind (Item.Entity)));

      if not Get_Kind (Item.Entity).Is_Type then
         Add_Type (Attr_Lines, Item, Lib_Info, Item.Entity, "of type");

      elsif Is_Subtype (Lib_Info, Item.Entity) then
         Parent := Get_Variable_Type (Lib_Info, Item.Entity);
         Add_Line
           (Attr_Lines, -"subtype of " & Entity_As_Link (Parent),
            Callback => Build (Item, Parent));

      else
         case Get_Kind (Item.Entity).Kind is
            when Overloaded_Entity
              | Unresolved_Entity
              | Private_Type
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
              | Modular_Integer
              | Named_Number
              | Ordinary_Fixed_Point
              | Signed_Integer
              | String_Kind =>
               null;

            when Enumeration_Kind =>
               Add_Fields (Kernel, Attr_Lines, Item, Lib_Info);

            when Access_Kind =>
               Parent := Pointed_Type (Lib_Info, Item.Entity);

               --  Could be null if the access type is really a subtyping
               if Parent /= No_Entity_Information then
                  Add_Line
                    (Attr_Lines, "access to " & Entity_As_Link (Parent),
                     Callback => Build (Item, Parent));
                  --  Do not destroy Parent, needed for callbacks
               end if;

            when Array_Kind =>
               Parent := Array_Contents_Type (Lib_Info, Item.Entity);

               --  Could be null if the array type is really a subtyping
               if Parent /= No_Entity_Information then
                  Add_Line
                    (Attr_Lines, "array of " & Entity_As_Link (Parent),
                     Callback => Build (Item, Parent));
                     --  Do not destroy Parent, needed for callbacks
               end if;

            when Class_Wide
              | Class
              | Record_Kind
              | Protected_Kind
              | Task_Kind =>
               Add_Primitive_Operations (Meth_Lines, Kernel, Item, Lib_Info);
               Add_Fields (Kernel, Attr_Lines, Item, Lib_Info);

            when Entry_Or_Entry_Family
              | Function_Or_Operator
              | Procedure_Kind =>
               Add_Parameters (Attr_Lines, Item, Lib_Info);
               Set_Children_Shown (Item, True);

            when Package_Kind =>
               Add_Package_Contents
                 (General_Lines, Attr_Lines, Meth_Lines, Item, Lib_Info);
         end case;
      end if;

      if not Parents_Shown (Item) then
         declare
            Parent : Entity_Information := Get
              (Get_Parent_Types (Lib_Info, Item.Entity));
         begin
            Set_Parents_Shown (Item, Parent = No_Entity_Information);
            Destroy (Parent);
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
        (Arrow_Item_Record (Item.all)'Access, W, H,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Margin + Yoffset;

      Display_Lines (Item, General_Lines, Margin + Xoffset, Y, 0, Layout);

      Display_Lines (Item, Attr_Lines, Margin + Xoffset + Left_Margin, Y,
                     Layout_W1, Layout);

      if Layout_H /= 0 and then Meth_Layout_H /= 0 then
         Y := Y + 2;
         Draw_Line
           (Drawable => Pixmap (Item),
            GC       => Get_Black_GC (Get_Style (Get_Browser (Item))),
            X1       => 0,
            Y1       => Y,
            X2       => Get_Coord (Item).Width,
            Y2       => Y);
         Y := Y + 1;
      end if;

      Display_Lines (Item, Meth_Lines, Margin + Xoffset + Left_Margin, Y,
                     Meth_Layout_W1, Layout);

      Free (Attr_Lines);
      Free (Meth_Lines);
   end Resize_And_Draw;

   ----------------------
   -- Redraw_Title_Bar --
   ----------------------

   procedure Redraw_Title_Bar (Item : access Type_Item_Record) is
   begin
      Redraw_Title_Bar (Arrow_Item_Record (Item.all)'Access);

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
                 (Item,
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
           or else Is_Equal (Found.Entity, Entity);
         Next (Iter);
      end loop;

      if Found = null and then Entity /= No_Entity_Information then
         Gtk_New (Found, Browser, Entity);
         Put (Get_Canvas (Browser), Found);
         Refresh (Found);
         Layout (Browser, Force => False);
      end if;

      --  Need to always refresh the canvas so that the links are correctly
      --  displayed.
      Refresh_Canvas (Get_Canvas (Browser));

      return Found;
   end Add_Or_Select_Item;

   ------------------------
   -- Add_Or_Select_Item --
   ------------------------

   procedure Add_Or_Select_Item
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Child : constant MDI_Child :=
        Open_Type_Browser_Child (Get_Kernel (Context));
      Item : Type_Item;
      pragma Unreferenced (Item, Widget);
   begin
      Item := Add_Or_Select_Item
        (Browser => Type_Browser (Get_Widget (Child)),
         Entity  => Get_Entity (Entity_Selection_Context_Access (Context)));
   end Add_Or_Select_Item;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child is
   begin
      if Node.Tag.all = "Entities_Browser" then
         return Put
           (MDI, Gtk_Widget (Open_Type_Browser (User)),
            Default_Width  => Get_Pref (User, Default_Widget_Width),
            Default_Height => Get_Pref (User, Default_Widget_Height));
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
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
   begin
      Open_File_Editor
        (Kernel     => Get_Kernel (B),
         Filename   => Get_Declaration_File_Of (It.Entity),
         Line       => Get_Declaration_Line_Of (It.Entity),
         Column     => Get_Declaration_Column_Of (It.Entity),
         Column_End => Get_Declaration_Column_Of (It.Entity)
           + Get_Name (It.Entity)'Length,
         From_Path => True);
   end On_Show_Source;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access Type_Item_Record;
      Browser : access General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event);
      Context : Entity_Selection_Context_Access;
      Mitem : Gtk_Menu_Item;
   begin
      Context := new Entity_Selection_Context;
      Set_Entity_Information
        (Context     => Context,
         Entity_Name => Get_Name (Item.Entity),
         Line        => Get_Declaration_Line_Of (Item.Entity),
         Column      => Get_Declaration_Column_Of (Item.Entity));
      Set_File_Information
        (Context     => Context,
         File_Name   => Get_Declaration_File_Of (Item.Entity));
      --  We need to set the file information, even though it will also display
      --  some contextual menus (file dependencies,...), otherwise the call
      --  graph will not work.

      if Menu /= null then
         Gtk_New (Mitem, -"Show source");
         Add (Menu, Mitem);
         Item_Cb.Object_Connect
           (Mitem, "activate",
            Item_Cb.To_Marshaller (On_Show_Source'Access),
            Slot_Object => Browser,
            User_Data   => Browser_Item (Item));
      end if;

      return Selection_Context_Access (Context);
   end Contextual_Factory;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel);
      Browser : constant Type_Browser := Type_Browser (Child);
      Iter    : constant Selection_Iterator := Start (Get_Canvas (Browser));
   begin
      --  If there is no selection, or more than one item, nothing we can do
      if Get (Iter) = null
        or else Get (Next (Iter)) /= null
      then
         return null;
      end if;

      return Contextual_Factory
        (Browser_Item (Get (Iter)), Browser, null, null);
   end Default_Factory;

   ----------------------------
   -- Get_Last_Button_Number --
   ----------------------------

   function Get_Last_Button_Number (Item : access Type_Item_Record)
      return Gint is
   begin
      return Get_Last_Button_Number (Arrow_Item_Record (Item.all)'Access) + 1;
   end Get_Last_Button_Number;

   ------------------------
   -- Draw_Straight_Line --
   ------------------------

   procedure Draw_Straight_Line
     (Link : access Parent_Link_Record;
      Window : Gdk.Window.Gdk_Window;
      GC : Gdk.GC.Gdk_GC;
      Parent_Side : Gtkada.Canvas.Item_Side;
      X1, Y1 : Glib.Gint;
      Child_Side : Gtkada.Canvas.Item_Side;
      X2, Y2 : Glib.Gint)
   is
      Depth : constant := 10;
      Width : constant := 5;
      pragma Unreferenced (Link, Child_Side);
   begin
      case Parent_Side is
         when West =>
            Draw_Polygon
              (Window,
               GC,
               Filled => False,
               Points => ((X => X1, Y => Y1),
                          (X => X1 - Depth, Y => Y1 - Width),
                          (X => X1 - Depth, Y => Y1 + Width)));
            Draw_Line (Window, GC, X1 - Depth, Y1, X2, Y2);

         when East =>
            Draw_Polygon
              (Window,
               GC,
               Filled => False,
               Points => ((X => X1, Y => Y1),
                          (X => X1 + Depth, Y => Y1 - Width),
                          (X => X1 + Depth, Y => Y1 + Width)));
            Draw_Line (Window, GC, X1 + Depth, Y1, X2, Y2);

         when North =>
            Draw_Polygon
              (Window,
               GC,
               Filled => False,
               Points => ((X => X1, Y => Y1),
                          (X => X1 + Width, Y => Y1 - Depth),
                          (X => X1 - Width, Y => Y1 - Depth)));
            Draw_Line (Window, GC, X1, Y1 - Depth, X2, Y2);

         when South =>
            Draw_Polygon
              (Window,
               GC,
               Filled => False,
               Points => ((X => X1, Y => Y1),
                          (X => X1 + Width, Y => Y1 + Depth),
                          (X => X1 - Width, Y => Y1 + Depth)));
            Draw_Line (Window, GC, X1, Y1 + Depth, X2, Y2);
      end case;
   end Draw_Straight_Line;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Item : access Type_Item_Record) is
   begin
      Redraw_Title_Bar (Browser_Item (Item));
   end Highlight;

   -----------------------
   -- Get_Background_GC --
   -----------------------

   function Get_Background_GC
     (Item : access Type_Item_Record) return Gdk.GC.Gdk_GC is
   begin
      return Get_Default_Item_Background_GC (Get_Browser (Item));
   end Get_Background_GC;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Generic_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class) is
   begin
      Yoffset := Yoffset + Generic_Item_Box_Height_Top;
      Resize_And_Draw
        (Type_Item_Record (Item.all)'Access,
         Width, Height + Generic_Item_Box_Height_Top,
         Width_Offset + Generic_Item_Box_Width_Right,
         Height_Offset, Xoffset, Yoffset, Layout);

      Draw_Rectangle
        (Pixmap (Item),
         GC     => Get_Title_Background_GC (Item),
         Filled => True,
         X      => Get_Coord (Item).Width - Generic_Item_Box_Width + 1,
         Y      => 1,
         Width  => Generic_Item_Box_Width - 2,
         Height => Generic_Item_Box_Height - 2);
      Draw_Shadow
        (Style       => Get_Style (Get_Browser (Item)),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
         X           => Get_Coord (Item).Width - Generic_Item_Box_Width,
         Y           => 0,
         Width       => Generic_Item_Box_Width,
         Height      => Generic_Item_Box_Height);
   end Resize_And_Draw;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Item   : access Generic_Item_Record;
      Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      GC     : Gdk.GC.Gdk_GC;
      Xdest  : Glib.Gint;
      Ydest  : Glib.Gint)
   is
      Region : Gdk_Region;
      Item_Width : constant Gint := Get_Coord (Item).Width;
      Item_Height : constant Gint := Get_Coord (Item).Height;
   begin
      Region := Rectangle
        ((0,
          To_Canvas_Coordinates (Canvas, Generic_Item_Box_Height_Top),
          To_Canvas_Coordinates
             (Canvas, Item_Width - Generic_Item_Box_Width_Right),
          To_Canvas_Coordinates (Canvas, Item_Height)));
      Union_With_Rect
        (Region,
         (To_Canvas_Coordinates (Canvas, Item_Width - Generic_Item_Box_Width),
          0,
          To_Canvas_Coordinates (Canvas, Generic_Item_Box_Width),
          To_Canvas_Coordinates (Canvas, Generic_Item_Box_Height)));

      Set_Clip_Region (GC, Region);
      Set_Clip_Origin (GC, Xdest, Ydest);
      Draw (Type_Item_Record (Item.all)'Access, Canvas, GC,  Xdest, Ydest);
      Set_Clip_Mask (GC, null);

      Destroy (Region);
   end Draw;

   -------------------
   -- Point_In_Item --
   -------------------

   function Point_In_Item
     (Item   : access Generic_Item_Record;
      X, Y   : Glib.Gint) return Boolean is
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

   procedure Clip_Line
     (Src   : access Generic_Item_Record;
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
      Clip_Line (Type_Item_Record (Src.all)'Access,
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

end Browsers.Entities;
