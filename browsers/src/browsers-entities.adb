-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Src_Info.Queries;     use Src_Info, Src_Info.Queries;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Browsers.Canvas;      use Browsers.Canvas;
with Traces;               use Traces;
with Glide_Intl;           use Glide_Intl;
with String_List_Utils;    use String_List_Utils;

with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.Xml_Int;  use Glib.Xml_Int;
with Gdk.GC;        use Gdk.GC;
with Gdk.Event;     use Gdk.Event;
with Gdk.Drawable;  use Gdk.Drawable;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Stock;     use Gtk.Stock;
with Gtk.Style;     use Gtk.Style;
with Gtk.Widget;    use Gtk.Widget;
with Gtkada.Canvas; use Gtkada.Canvas;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtkada.MDI;    use Gtkada.MDI;
with Pango.Layout;  use Pango.Layout;

package body Browsers.Entities is

   Me : constant Debug_Handle := Create ("Browser.Entities");

   Type_Browser_Module : Module_ID;

   Left_Margin : constant := 20;
   --  Indentation for the attributes and methods layouts.

   procedure On_Type_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Type browser menu

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
      Args    : String_List_Utils.String_List.List) return String;
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

   procedure Add_Parent_Package
     (List     : in out Xref_List;
      Item     : access Type_Item_Record'Class;
      Lib_Info : LI_File_Ptr);
   --  Add the parent package for Entity at the end of Attr_Layout

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create a current kernel context, based on the currently selected item

   procedure On_Show_Source (Browser : access Gtk_Widget_Record'Class);
   --  Display a source editor to show the declaration of the entity

   procedure Find_Parent_Types
     (Event : Gdk_Event; Item  : access Browser_Item_Record'Class);
   --  Display the parent types for the item

   procedure Find_Children_Types
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class);
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
   --  Add a new line in List, starting with prefix and followed by the an
   --  hyper link for the type of Entity.

   function Entity_As_Link (Ent : Entity_Information) return String;
   --  Return a string that contains the entity name, as an hyper link. If
   --  Entity is in fact a predefined entity, no link is setup.

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
        (Module                  => Type_Browser_Module,
         Kernel                  => Kernel,
         Module_Name             => "Type_Browser",
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access,
         MDI_Child_Tag           => Type_Browser_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & (-"Tools"),
         Text        => -"Type Browser",
         Callback    => On_Type_Browser'Access);
      Register_Command
        (Kernel, "entity.show",
         (-"Usage:") & ASCII.LF & "  entity.show entity_name file_name [line]"
         & " [column}" & ASCII.LF
         & (-"Display in the type browser the informations known about the"
            & " entity: list of fields for records, list of primitive"
            & " subprograms or methods, list of parameters, ..."),
         Handler => Show_Entity_Command_Handler'Access);
   end Register_Module;

   ---------------------------------
   -- Show_Entity_Command_Handler --
   ---------------------------------

   function Show_Entity_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;
      Entity : Entity_Information;
      Child  : MDI_Child;
      Status : Find_Decl_Or_Body_Query_Status;
      Name, File, Line : List_Node;
      L, C : Positive := 1;
      Item  : Type_Item;
      pragma Unreferenced (Item);

   begin
      if Command = "entity.show" then
         if Length (Args) < 2 then
            return "Not enough arguments";
         end if;

         Child := Open_Type_Browser_Child (Kernel);

         Name   := First (Args);
         File   := Next (Name);

         if Next (File) /= Null_Node then
            Line := Next (File);
            L := Positive'Value (Data (Line));

            if Next (Line) /= Null_Node then
               C := Positive'Value (Data (Next (Line)));
            end if;
         end if;

         Find_Declaration_Or_Overloaded
           (Kernel      => Kernel,
            Lib_Info    => Locate_From_Source_And_Complete
              (Kernel, Data (File)),
            File_Name   => Data (File),
            Entity_Name => Data (Name),
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
            Gtk_New (Item, Label => (-"Examine entity ") &
                     Entity_Name_Information (Entity_Context));
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

   function Open_Type_Browser (Kernel : access Kernel_Handle_Record'Class)
      return Type_Browser
   is
      Browser : constant Type_Browser := new Type_Browser_Record;
   begin
      Initialize (Browser, Kernel, Create_Toolbar => False);

      --  ??? Should be freed when browser is destroyed.
      Browser.Up_Arrow := Render_Icon
        (Browser, Stock_Go_Up, Icon_Size_Menu);
      Browser.Down_Arrow := Render_Icon
        (Browser, Stock_Go_Down, Icon_Size_Menu);
      Browser.Primitive_Button := Render_Icon
        (Browser, Stock_Properties, Icon_Size_Menu);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Type_Browser_Module,
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
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Type_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Child := Put (Get_MDI (Kernel), Open_Type_Browser (Kernel));
         Set_Title (Child, -"Entities Browser");
      end if;

      return Child;
   end Open_Type_Browser_Child;

   ---------------------
   -- On_Type_Browser --
   ---------------------

   procedure On_Type_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      Child : constant MDI_Child := Open_Type_Browser_Child (Kernel);
      Item  : Type_Item;
      pragma Unreferenced (Widget, Item);

   begin
      if Context /= null
        and then Context.all in Entity_Selection_Context'Class
      then
         Item := Add_Or_Select_Item
           (Browser => Type_Browser (Get_Widget (Child)),
            Entity  => Get_Entity (Entity_Selection_Context_Access (Context)));
      end if;

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
      Item := new Type_Item_Record;
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
      Initialize (Item, Browser);
      Set_Title (Item, Get_Name (Entity));
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
      for Index in 1 .. Arr'Last loop
         Arr (Index) := Get (Prim);
         Next (Prim);
      end loop;

      Entity_Sort.Sort (L);

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
            Add_Line
              (List,
               Entity_As_Link (Arr (E)) & " ("
               & (-Kind_To_String (Get_Kind (Arr (E)))) & ')',
               Callback => Build (Item, Arr (E)));
            --  Do not free Arr (E), it's needed for callbacks
         end if;
      end loop;
   end Add_Primitive_Operations;

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

   ------------------------
   -- Add_Parent_Package --
   ------------------------

   procedure Add_Parent_Package
     (List        : in out Xref_List;
      Item        : access Type_Item_Record'Class;
      Lib_Info    : LI_File_Ptr)
   is
      Parent : constant Entity_Information :=
        Get_Parent_Package (Lib_Info, Item.Entity);
   begin
      if Parent /= No_Entity_Information then
         Add_Line (List, "Parent: " & Entity_As_Link (Parent),
                   Callback => Build (Item, Parent));
         --  Do not destroy parent, needed for callbacks
      end if;
   end Add_Parent_Package;

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

   procedure Find_Parent_Types
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Iter : Parent_Iterator;
      Lib_Info : LI_File_Ptr;
      It : constant Type_Item := Type_Item (Item);
      Parent : Entity_Information;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
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

         It.Parents_Computed := True;
         Redraw_Title_Bar (Item);
      end if;
   end Find_Parent_Types;

   -------------------------
   -- Find_Children_Types --
   -------------------------

   procedure Find_Children_Types
     (Event : Gdk_Event;
      Item  : access Browser_Item_Record'Class)
   is
      Iter   : Entity_Reference_Iterator;
      Child  : Child_Type_Iterator;
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      LI     : LI_File_Ptr;
      C      : Entity_Information;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Release
      then
         Push_State (Kernel, Busy);
         Find_All_References
           (Kernel       => Kernel,
            Entity       => Type_Item (Item).Entity,
            Iterator     => Iter,
            LI_Once      => True);

         while Get (Iter) /= No_Reference loop
            LI := Get_LI (Iter);

            Child := Get_Children_Types (LI, Type_Item (Item).Entity);

            loop
               C := Get (Child);
               exit when C = No_Entity_Information;

               Add_Item_And_Link
                 (Type_Item (Item), C, "", Parent_Link => True,
                  Reverse_Link => True);

               Destroy (C);
               Next (Child);
            end loop;

            Destroy (Child);

            Next (Kernel, Iter);
         end loop;

         Destroy (Iter);

         Type_Item (Item).Children_Computed := True;
         Redraw_Title_Bar (Item);

         Pop_State (Kernel);
      end if;

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
     (Item                        : access Type_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint)
   is
      W, H, Layout_H, Layout_W1, Layout_W2,
        Meth_Layout_W1, Meth_Layout_W2, Meth_Layout_H : Gint;
      Y : Gint;
      General_Lines, Attr_Lines, Meth_Lines : Xref_List;
      Parent : Entity_Information;
      Lib_Info : LI_File_Ptr;
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Layout : Pango_Layout;
   begin
      Trace (Me, "Resize_And_Draw: " & Get_Name (Item.Entity));
      Reset_Active_Areas (Item.all);

      Lib_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Declaration_File_Of (Item.Entity));

      Add_Line (General_Lines, -Kind_To_String (Get_Kind (Item.Entity)));

      if not Get_Kind (Item.Entity).Is_Type then
         Add_Type (Attr_Lines, Item, Lib_Info, Item.Entity, "Of type: ");

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
               Item.Children_Computed := True;

            when Package_Kind =>
               Add_Parent_Package (Attr_Lines, Item, Lib_Info);
         end case;
      end if;

      if Item.Parents_Computed = False then
         declare
            Parent : Entity_Information := Get
              (Get_Parent_Types (Lib_Info, Item.Entity));
         begin
            Item.Parents_Computed := Parent = No_Entity_Information;
            Destroy (Parent);
         end;
      end if;

      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description
        (Layout,
         Get_Pref (Get_Kernel (Get_Browser (Item)), Browsers_Link_Font));

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
        (Browser_Item_Record (Item.all)'Access, W, H,
         Width_Offset, Height_Offset, Xoffset, Yoffset);

      Y := Margin + Yoffset;

      Display_Lines (Item, General_Lines, Margin + Xoffset, Y, 0, Layout);

      Display_Lines (Item, Attr_Lines, Margin + Xoffset + Left_Margin, Y,
                     Layout_W1, Layout);

      if Layout_H /= 0 and then Meth_Layout_H /= 0 then
         Draw_Line
           (Drawable => Pixmap (Item),
            GC       => Get_Black_GC (Get_Style (Get_Browser (Item))),
            X1       => 0,
            Y1       => Y,
            X2       => Get_Coord (Item).Width,
            Y2       => Y);
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
      Redraw_Title_Bar (Browser_Item_Record (Item.all)'Access);

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

      if not Item.Parents_Computed then
         Draw_Title_Bar_Button
           (Item,
            Num    => Get_Last_Button_Number (Item) - 1,
            Pixbuf => Type_Browser (Get_Browser (Item)).Up_Arrow,
            Cb     => Build (Find_Parent_Types'Access, Item));
      end if;

      if not Item.Children_Computed then
         Draw_Title_Bar_Button
           (Item,
            Num    => Get_Last_Button_Number (Item) - 2,
            Pixbuf => Type_Browser (Get_Browser (Item)).Down_Arrow,
            Cb     => Build (Find_Children_Types'Access, Item));
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
         Refresh (Found);
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
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget is
   begin
      if Node.Tag.all = "Entities_Browser" then
         return Gtk_Widget (Open_Type_Browser (User));
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
     (Browser : access Gtk_Widget_Record'Class)
   is
      B : constant Type_Browser := Type_Browser (Browser);
      Item : constant Type_Item := Type_Item (Selected_Item (B));
   begin
      Open_File_Editor
        (Kernel     => Get_Kernel (B),
         Filename   => Get_Declaration_File_Of (Item.Entity),
         Line       => Get_Declaration_Line_Of (Item.Entity),
         Column     => Get_Declaration_Column_Of (Item.Entity),
         Column_End => Get_Declaration_Column_Of (Item.Entity)
           + Get_Name (Item.Entity)'Length,
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
      --  Do not set the file information, we should really limit to
      --  entity-related submenus here.

      if Menu /= null then
         Gtk_New (Mitem, -"Show source");
         Add (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (On_Show_Source'Access),
            Slot_Object => Browser);
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
   begin
      if Selected_Item (Browser) = null then
         return null;
      end if;

      return Contextual_Factory
        (Browser_Item (Selected_Item (Browser)), Browser, null, null);
   end Default_Factory;

   ----------------------------
   -- Get_Last_Button_Number --
   ----------------------------

   function Get_Last_Button_Number (Item : access Type_Item_Record)
      return Gint is
   begin
      return Get_Last_Button_Number (Browser_Item_Record (Item.all)'Access)
        + 3;
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

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Browser : access General_Browser_Record'Class;
      Item : access Type_Item_Record;
      Parent_Removed, Child_Removed : Boolean)
   is
      pragma Unreferenced (Browser);
   begin
      if Parent_Removed then
         Item.Parents_Computed := False;
      end if;

      if Child_Removed then
         Item.Children_Computed := False;
      end if;
   end Reset;

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

   -----------------------------
   -- Get_Title_Background_GC --
   -----------------------------

   function Get_Title_Background_GC
     (Item : access Type_Item_Record) return Gdk.GC.Gdk_GC
   is
      B : constant General_Browser := Get_Browser (Item);
      Selected : constant Canvas_Item := Selected_Item (B);
   begin
      if Canvas_Item (Item) = Selected then
         return Get_Selected_Item_GC (B);

      elsif Selected /= null
        and then Has_Link (Get_Canvas (B), From => Item, To => Selected)
      then
         return Get_Parent_Linked_Item_GC (B);

      elsif Selected /= null
        and then Has_Link (Get_Canvas (B), From => Selected, To => Item)
      then
         return Get_Child_Linked_Item_GC (B);

      else
         --  ??? Should use different color
         return Get_Selected_Item_GC (B);
      end if;
   end Get_Title_Background_GC;

end Browsers.Entities;
