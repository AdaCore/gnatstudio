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
with Ada.Unchecked_Deallocation;
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
with Gdk.GC;        use Gdk.GC;
with Gdk.Event;     use Gdk.Event;
with Gdk.Drawable;  use Gdk.Drawable;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Style;     use Gtk.Style;
with Gtkada.Canvas; use Gtkada.Canvas;
with Gtkada.MDI;    use Gtkada.MDI;
with Pango.Context; use Pango.Context;
with Pango.Font;    use Pango.Font;
with Pango.Layout;  use Pango.Layout;

package body Browsers.Types is

   Me : constant Debug_Handle := Create ("Browser.Type");

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

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries in the contextual menus of other modules

   type Show_Entity_Callback is new Active_Area_Callback with record
      Item    : Browser_Item;
      Entity  : Entity_Information;
   end record;

   procedure Call (Callback : Show_Entity_Callback;
                   Event    : Gdk.Event.Gdk_Event);
   procedure Destroy (Callback : in out Show_Entity_Callback);
   --  See inherated doc

   function Build
     (Item   : access Browser_Item_Record'Class;
      Entity : Entity_Information) return Show_Entity_Callback'Class;
   --  Build a new callback to display entities.

   function Show_Entity_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String;
   --  Command handler for this module (in the shell window)

   function "<" (E1, E2 : Entity_Information) return Boolean;

   type Entity_Information_Array_Access is access Entity_Information_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Entity_Information_Array, Entity_Information_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Strings.String_List, GNAT.Strings.String_List_Access);

   type Xref_List is record
      Lines : GNAT.Strings.String_List_Access;
      Xrefs : Entity_Information_Array_Access;
   end record;

   procedure Add_Primitive_Operations
     (List        : in out Xref_List;
      Lib_Info    : LI_File_Ptr;
      Entity      : Entity_Information);
   --  Add the sorted list of primitive operations for Entity at the end of
   --  Meth_Layout.

   procedure Add_Parameters
     (List        : in out Xref_List;
      Lib_Info    : LI_File_Ptr;
      Entity      : Entity_Information);
   --  Add the list of parameters for Entity (a subprogram) to the end of
   --  Attr_Layout.

   procedure Add_Fields
     (Kernel      : access Kernel_Handle_Record'Class;
      List        : in out Xref_List;
      Lib_Info    : LI_File_Ptr;
      Entity      : Entity_Information);
   --  Add the list of fields for a record-like entity to the end of
   --  Attr_Layout.

   procedure Add_Parent_Package
     (List        : in out Xref_List;
      Lib_Info    : LI_File_Ptr;
      Entity      : Entity_Information);
   --  Add the parent package for Entity at the end of Attr_Layout

   procedure Add_Line
     (List : in out Xref_List; Str : String; Xref : Entity_Information);
   --  Add a new line that will be displayed in a layout.
   --  Str can contain one substring delimited by @...@. When the user
   --  clicks on that zone, a callback will be called to display the
   --  information for the entity Xref.

   procedure Display_Lines
     (Item   : access Browser_Item_Record'Class;
      List   : Xref_List;
      X      : Gint;
      Y      : in out Gint;
      Layout : access Pango_Layout_Record'Class);
   --  Display the lines from List into Pixmap, starting at X, Y, and setup
   --  appropriate callbacks.
   --  Layout is used while drawing the strings.

   procedure Free (List : in out Xref_List);
   --  Free the data in List (but not the xrefs, since they are still used for
   --  the callbacks).

   procedure Get_Pixel_Size
     (Browser : access General_Browser_Record'Class;
      List : Xref_List;
      W, H : out Gint;
      Layout : Pango_Layout);
   --  Compute the approximate pixels size for List.
   --  Layout is used while computing the length

   ----------
   -- Call --
   ----------

   procedure Call
     (Callback : Show_Entity_Callback;
      Event    : Gdk.Event.Gdk_Event)
   is
      Canvas   : Interactive_Canvas;
      New_Item : Type_Item;
      Link     : Canvas_Link;
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Canvas := Get_Canvas (Get_Browser (Callback.Item));
         New_Item := Add_Or_Select_Item
           (Type_Browser (Get_Browser (Callback.Item)), Callback.Entity);

         if not Has_Link (Canvas, Callback.Item, New_Item) then
            Link := new Canvas_Link_Record;
            Configure (Link, Descr => Get_Name (Callback.Entity));
            Add_Link (Canvas, Link, Callback.Item, New_Item);
         end if;
      end if;
   end Call;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Callback : in out Show_Entity_Callback) is
   begin
      Destroy (Callback.Entity);
   end Destroy;

   -----------
   -- Build --
   -----------

   function Build
     (Item   : access Browser_Item_Record'Class;
      Entity : Entity_Information) return Show_Entity_Callback'Class is
   begin
      return Show_Entity_Callback'
        (Active_Area_Callback with
         Item => Browser_Item (Item),
         Entity => Entity);
   end Build;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Xref_List) is
   begin
      Free (List.Lines);
      Unchecked_Free (List.Xrefs);
   end Free;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (List : in out Xref_List; Str : String; Xref : Entity_Information)
   is
      Tmp : GNAT.Strings.String_List_Access := List.Lines;
      Xrefs : Entity_Information_Array_Access := List.Xrefs;
   begin
      if Tmp /= null then
         List.Lines :=
           new GNAT.Strings.String_List'(Tmp.all & new String'(Str));
         Unchecked_Free (Tmp);
      else
         List.Lines := new GNAT.Strings.String_List'(1 => new String'(Str));
      end if;

      if Xrefs /= null then
         List.Xrefs := new Entity_Information_Array'(Xrefs.all & Xref);
         Unchecked_Free (Xrefs);
      else
         List.Xrefs := new Entity_Information_Array'(1 => Xref);
      end if;
   end Add_Line;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   procedure Get_Pixel_Size
     (Browser : access General_Browser_Record'Class;
      List : Xref_List;
      W, H : out Gint;
      Layout : Pango_Layout)
   is
      Descr : constant Pango_Font_Description :=
        Get_Pref (Get_Kernel (Browser), Browsers_Link_Font);
      Font  : Pango_Font;
      Metrics : Pango_Font_Metrics;
      Longest : Gint := 0;
      H2    : Gint;
   begin
      H := 0;

      if List.Lines = null then
         W := 0;
         return;
      end if;

      Font := Load_Font (Get_Pango_Context (Browser), Descr);
      Metrics := Get_Metrics (Font);

      for L in List.Lines'Range loop
         Set_Text (Layout, List.Lines (L).all);
         Get_Pixel_Size (Layout, W, H2);
         H := H + H2;
         Longest := Gint'Max (Longest, W);
      end loop;

      W := Longest;
      Unref (Metrics);
      Unref (Font);
   end Get_Pixel_Size;

   -------------------
   -- Display_Lines --
   -------------------

   procedure Display_Lines
     (Item   : access Browser_Item_Record'Class;
      List   : Xref_List;
      X      : Gint;
      Y      : in out Gint;
      Layout : access Pango_Layout_Record'Class)
   is
      Browser : constant General_Browser := Get_Browser (Item);
      X2     : Gint;
      First, Last : Integer;
      In_Xref : Boolean;
      GC     : Gdk_GC;
      W, H   : Gint;

      procedure Display (L : Natural);
      --  Display the slice First .. Last - 1

      procedure Display (L : Natural) is
      begin
         if First < Last - 1 then
            Set_Text (Layout, List.Lines (L)(First .. Last - 1));

            if In_Xref then
               GC := Get_Text_GC (Browser);
            else
               GC := Get_Black_GC (Get_Style (Browser));
            end if;

            Draw_Layout
              (Drawable => Pixmap (Item),
               GC       => GC,
               X        => X2,
               Y        => Y,
               Layout   => Pango_Layout (Layout));

            Get_Pixel_Size (Layout, W, H);

            if In_Xref then
               Draw_Line (Pixmap (Item), GC, X2, Y + H, X2 + W, Y + H);

               if List.Xrefs (L) /= No_Entity_Information then
                  Add_Active_Area
                    (Item,
                     Gdk_Rectangle'(X2, Y, W, H),
                     Build (Item, List.Xrefs (L)));
               end if;
            end if;

            X2 := X2 + W;
         end if;

         In_Xref := not In_Xref;
      end Display;

   begin
      if List.Lines = null then
         return;
      end if;

      for L in List.Lines'Range loop
         First := List.Lines'First;
         Last := First;
         X2   := X;
         In_Xref := False;

         while Last <= List.Lines (L)'Last loop
            if List.Lines (L)(Last) = '@' then
               Display (L);
               First   := Last + 1;
            end if;

            Last := Last + 1;
         end loop;

         Display (L);

         --  No need to query the size again, we just did

         Y := Y + H;
      end loop;
   end Display_Lines;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Type_Browser_Module : Module_ID;
   begin
      Register_Module
        (Module                  => Type_Browser_Module,
         Kernel                  => Kernel,
         Module_Name             => "Type_Browser",
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access);
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
         Set_Title (Child, -"Type Browser");
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
      Set_Title (Item, Get_Name (Entity)
                 & ": " & (-Kind_To_String (Get_Kind (Entity))));
      Item.Entity := Copy (Entity);
   end Initialize;

   --------
   -- E1 --
   --------

   function "<" (E1, E2 : Entity_Information) return Boolean is
   begin
      return Get_Name (E1) < Get_Name (E2);
   end "<";

   ------------------------------
   -- Add_Primitive_Operations --
   ------------------------------

   procedure Add_Primitive_Operations
     (List     : in out Xref_List;
      Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information)
   is
      Prim : Primitive_Iterator := Get_Primitive_Operations
        (Lib_Info => Lib_Info, Entity => Entity);
      L : constant Natural := Length (Prim);

      Arr : Entity_Information_Array (0 .. L);
      --  Index 0 is needed for heap sort

      Index : Natural := 1;
      Op : Entity_Information;

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
      loop
         Op := Get (Prim);
         exit when Op = No_Entity_Information;
         Arr (Index) := Op;
         Index := Index + 1;
         Next (Prim);
      end loop;

      Entity_Sort.Sort (L);

      for E in 1 .. L loop
         Add_Line
           (List,
            '@' & Get_Name (Arr (E)) & "@ ( "
            & (-Kind_To_String (Get_Kind (Arr (E)))) & ')',
            Arr (E));
         --  Do not free Arr (E), it's needed for callbacks
      end loop;
   end Add_Primitive_Operations;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters
     (List     : in out Xref_List;
      Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information)
   is
      Subs : Subprogram_Iterator := Get_Subprogram_Parameters
        (Lib_Info   => Lib_Info, Subprogram => Entity);
      Typ, Parameter : Entity_Information;
   begin
      loop
         Parameter := Get (Subs);
         exit when Parameter = No_Entity_Information;

         Typ := Get_Variable_Type (Lib_Info, Parameter);

         Add_Line
           (List,
            Get_Name (Parameter) & ": "
            & Image (Get_Type (Subs)) & " @" & Get_Name (Typ) & '@',
            Typ);
         --  Do not free Type, it is needed for callbacks

         Destroy (Parameter);
         Next (Subs);
      end loop;
   end Add_Parameters;

   ------------------------
   -- Add_Parent_Package --
   ------------------------

   procedure Add_Parent_Package
     (List        : in out Xref_List;
      Lib_Info    : LI_File_Ptr;
      Entity      : Entity_Information)
   is
      Parent : constant Entity_Information :=
        Get_Parent_Package (Lib_Info, Entity);
   begin
      if Parent /= No_Entity_Information then
         Add_Line (List, "Parent: @" & Get_Name (Parent) & '@', Parent);
         --  Do not destroy parent, needed for callbacks
      end if;
   end Add_Parent_Package;

   ----------------
   -- Add_Fields --
   ----------------

   procedure Add_Fields
     (Kernel      : access Kernel_Handle_Record'Class;
      List        : in out Xref_List;
      Lib_Info    : LI_File_Ptr;
      Entity      : Entity_Information)
   is
      Tree : Scope_Tree;
      Node : Scope_Tree_Node;
      Iter : Scope_Tree_Node_Iterator;
      Xref, Typ, Field : Entity_Information;
   begin
      Get_Scope_Tree (Kernel, Entity, Tree, Node);

      if Node /= Null_Scope_Tree_Node then
         Iter := Start (Node);

         loop
            Node := Get (Iter);
            exit when Node = Null_Scope_Tree_Node;

            if Is_Declaration (Node) then
               Field := Get_Entity (Node);
               Typ   := Get_Variable_Type (Lib_Info, Field);

               if Is_Predefined_Entity (Typ) then
                  Xref := No_Entity_Information;
               else
                  Xref := Typ;
               end if;

               Add_Line
                 (List,
                  Get_Name (Field) & ": @" & Get_Name (Typ) & '@',
                  Xref);
               --  Do not free Typ, needed for callbacks
               Destroy (Field);
            end if;

            Next (Iter);
         end loop;
      end if;

      Free (Tree);
   end Add_Fields;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item                        : access Type_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint)
   is
      W, H, Layout_H, Layout_W, Meth_Layout_W, Meth_Layout_H : Gint;
      Y : Gint;
      Attr_Lines, Meth_Lines : Xref_List;
      Parent : Entity_Information;
      Lib_Info : LI_File_Ptr;
      Kernel : constant Kernel_Handle := Get_Kernel (Get_Browser (Item));
      Layout : Pango_Layout;
   begin
      Trace (Me, "Resize_And_Draw: " & Get_Name (Item.Entity));

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
           | Enumeration_Kind
           | Exception_Entity
           | Floating_Point
           | Modular_Integer
           | Named_Number
           | Ordinary_Fixed_Point
           | Signed_Integer
           | String_Kind =>
            null;

         when Access_Kind =>
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Item.Entity));
            Parent := Pointed_Type (Lib_Info, Item.Entity);

            --  Could be null if the access type is really a subtyping
            if Parent /= No_Entity_Information then
               Add_Line
                 (Attr_Lines, "access to @" & Get_Name (Parent) & '@', Parent);
               --  Do not destroy Parent, needed for callbacks
            end if;

         when Array_Kind =>
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Item.Entity));
            Parent := Array_Contents_Type (Lib_Info, Item.Entity);

            --  Could be null if the array type is really a subtyping
            if Parent /= No_Entity_Information then
               Add_Line
                 (Attr_Lines, "array of @" & Get_Name (Parent) & '@', Parent);
               --  Do not destroy Parent, needed for callbacks
            end if;

         when Class_Wide
           | Class
           | Record_Kind
           | Protected_Kind
           | Task_Kind =>
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Item.Entity));
            Add_Primitive_Operations (Meth_Lines, Lib_Info, Item.Entity);
            Add_Fields (Kernel, Attr_Lines, Lib_Info, Item.Entity);

         when Entry_Or_Entry_Family
           | Function_Or_Operator
           | Procedure_Kind =>
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Item.Entity));
            Add_Parameters (Attr_Lines, Lib_Info, Item.Entity);

         when Package_Kind =>
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_Declaration_File_Of (Item.Entity));
            Add_Parent_Package (Attr_Lines, Lib_Info, Item.Entity);
      end case;

      Layout := Create_Pango_Layout (Get_Browser (Item), "");
      Set_Font_Description
        (Layout,
         Get_Pref (Get_Kernel (Get_Browser (Item)), Browsers_Link_Font));

      Get_Pixel_Size
        (Get_Browser (Item), Attr_Lines, Layout_W, Layout_H, Layout);
      Get_Pixel_Size
        (Get_Browser (Item), Meth_Lines, Meth_Layout_W, Meth_Layout_H,
         Layout);
      W := Gint'Max (Width, Layout_W);
      W := Gint'Max (W, Meth_Layout_W);
      H := Height + Layout_H + 4 * Margin + Meth_Layout_H;

      Resize_And_Draw
        (Browser_Item_Record (Item.all)'Access, W, H,
         Width_Offset, Height_Offset, Xoffset, Yoffset);

      Y := Margin + Yoffset;

      Display_Lines (Item, Attr_Lines, Margin + Xoffset, Y, Layout);

      if Layout_H /= 0 and then Meth_Layout_H /= 0 then
         Draw_Line
           (Drawable => Pixmap (Item),
            GC       => Get_Black_GC (Get_Style (Get_Browser (Item))),
            X1       => 0,
            Y1       => Y,
            X2       => Get_Coord (Item).Width,
            Y2       => Y);
      end if;

      Display_Lines (Item, Meth_Lines, Margin + Xoffset, Y, Layout);

      Free (Attr_Lines);
      Free (Meth_Lines);
   end Resize_And_Draw;

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
         Refresh_Canvas (Get_Canvas (Browser));
      end if;

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

end Browsers.Types;

