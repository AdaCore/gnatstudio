-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;             use Glib;
with Glib.Xml_Int;     use Glib.Xml_Int;
with Glib.Object;      use Glib.Object;
with Gdk.GC;           use Gdk.GC;
with Gdk.Event;        use Gdk.Event;
with Gdk.Drawable;     use Gdk.Drawable;
with Gtk.Image;            use Gtk.Image;
with Gtk.Image_Menu_Item;  use Gtk.Image_Menu_Item;
with Gtk.Main;         use Gtk.Main;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Pango.Layout;     use Pango.Layout;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtkada.MDI;       use Gtkada.MDI;

with Src_Info;                 use Src_Info;
with Src_Info.Queries;         use Src_Info.Queries;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with String_Utils;             use String_Utils;
with Browsers.Canvas;          use Browsers.Canvas;
with Projects.Registry;        use Projects.Registry;

with Glide_Intl;       use Glide_Intl;
with Browsers.Canvas;  use Browsers.Canvas;
with Language;         use Language;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Traces;           use Traces;
with String_List_Utils; use String_List_Utils;

package body Browsers.Call_Graph is

   Me : constant Debug_Handle := Create ("Browsers.Call_Graph");

   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   All_Refs_Category : constant String := "References for: ";
   --  String used as a category title. This needs to be translated when used,
   --  therefore the following comment is for translation purposes:
   --     -"References for: "

   Automatically_Check_To_Dependencies : constant Boolean := True;
   --  If True, then every time an item is added to the call graph we check,
   --  and if no to dependency exists, the right arrow is not displayed.

   ------------------------
   -- Call graph browser --
   ------------------------

   type Call_Graph_Browser_Record is new
     Browsers.Canvas.General_Browser_Record
   with record
      Idle_Id : Gtk.Main.Idle_Handler_Id;
   end record;
   type Call_Graph_Browser is access all Call_Graph_Browser_Record'Class;

   ------------------
   -- Entity items --
   ------------------

   type Entity_Item_Record is new Browsers.Canvas.Arrow_Item_Record
   with record
      Entity : Src_Info.Queries.Entity_Information;
   end record;
   type Entity_Item is access all Entity_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean);
   --  Create a new entity item.
   --  If May_Have_To_Dependencies is False, the right arrow will not be
   --  displayed in the items.

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean);
   --  Internal initialization function

   procedure Destroy (Item : in out Entity_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   function Contextual_Factory
     (Item  : access Entity_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   procedure Resize_And_Draw
     (Item                        : access Entity_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   --------------------
   -- Renaming links --
   --------------------

   type Renaming_Link_Record is new Browsers.Canvas.Browser_Link_Record
     with null record;
   --  The type of link used between an entity and the entities that rename
   --  it.
   --  A renaming link should always be created from the renaming entity to the
   --  renamed entity.

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Renaming_Link_Record;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint);
   --  Override the default drawing procedure for links

   ----------
   -- Misc --
   ----------

   type Entity_Idle_Data is record
      Kernel : Kernel_Handle;
      Iter   : Entity_Reference_Iterator_Access;
      Entity : Entity_Information;
      Include_Writes : Boolean;
      Include_Reads  : Boolean;
      Category : String_Access;
   end record;
   package Entity_Iterator_Idle is new Gtk.Main.Idle (Entity_Idle_Data);

   type Examine_Ancestors_Idle_Data is record
      Iter    : Entity_Reference_Iterator_Access;
      Browser : Call_Graph_Browser;
      Item    : Entity_Item;
      Entity  : Entity_Information;
   end record;
   package Examine_Ancestors_Idle is new Gtk.Main.Idle
     (Examine_Ancestors_Idle_Data);

   procedure Call_Graph_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries into contextual menus

   procedure Examine_Entity_Call_Graph
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information);
   --  Display the call graph for the node.

   procedure Examine_Ancestors_Call_Graph
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information);
   --  Display the list of subprograms that call Entity.

   function Examine_Ancestors_Call_Graph_Idle
     (Data : Examine_Ancestors_Idle_Data) return Boolean;
   --  Main idle loop for Examine_Ancestors_Call_Graph

   procedure Edit_Entity_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Show the whole call graph for the Entity described in Context.

   procedure Edit_Ancestors_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Show the list of subprograms that call the one described in Context.

   function Find_Entity
     (In_Browser : access General_Browser_Record'Class;
      Entity     : Entity_Information)
      return Canvas_Item;
   --  Return the child that shows Item_Name in the browser, or null if
   --  Item_Name is not already displayed in the canvas.
   --  ??? Should also have line and column information

   function Open_Call_Graph_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Find, or create a new, call graph editor.

   function Create_Call_Graph_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Call_Graph_Browser;
   --  Create a new call graph browser.
   --  It is now added automatically to the MDI

   procedure Edit_Spec_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Open an editor for the entity described in Context.

   procedure Edit_Body_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Open an editor for the entity described in Context.

   procedure Find_All_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  List all the references to the entity

   procedure Find_All_Writes_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  List all the "write to" references to the entity

   procedure Find_All_Reads_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  List all the "reads to" references to the entity

   procedure Find_All_Local_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  List all the references to the entity in the local file (or ALI file).

   procedure Find_All_References_Internal
     (Kernel         : access Kernel_Handle_Record'Class;
      Info           : Entity_Information;
      Category_Title : String;
      Include_Writes : Boolean;
      Include_Reads  : Boolean);
   --  Internal implementation for Find_All_References_From_Contextual,
   --  Find_All_Writes_From_Contextual and Find_All_Reads_From_Contextual.

   function Find_Next_Reference (D : Entity_Idle_Data)
      return Boolean;
   --  Find the next reference to the entity in D.

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Entity_Information) return Entity_Item;
   --  Add a new entity to the browser, if not already there.

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Node    : Scope_Tree_Node) return Entity_Item;
   --  Same as above

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Idle_Data);
   --  Called when the idle loop is destroyed.

   procedure Destroy_Idle (Data : in out Entity_Idle_Data);
   --  Called when the idle loop is destroyed.

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   procedure On_Call_Graph
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Create a callgraph for the entity described in the current kernel
   --  context (if any)

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

   procedure Print_Ref
     (Kernel   : access Kernel_Handle_Record'Class;
      File     : String;
      Line     : Positive;
      Column   : Natural;
      Name     : String;
      Category : String);
   --  Print a reference in the console, after looking for the directory
   --  containing File.
   --  Category corresponds to the purpose of the print. All references
   --  corresponding to the same category will be printed as a group.

   function Call_Graph_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String;
   --  Handle shell commands

   procedure Examine_Ancestors_Call_Graph
     (Item : access Arrow_Item_Record'Class);
   procedure Examine_Entity_Call_Graph
     (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the title bar buttons

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean) is
   begin
      Item := new Entity_Item_Record;
      Initialize (Item, Browser, Entity, May_Have_To_Dependencies);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean) is
   begin
      Item.Entity   := Copy (Entity);
      Initialize (Item, Browser, Get_Name (Entity),
                  Examine_Ancestors_Call_Graph'Access,
                  Examine_Entity_Call_Graph'Access);
      Set_Children_Shown (Item, not May_Have_To_Dependencies);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Entity_Item_Record) is
   begin
      Destroy (Arrow_Item_Record (Item));
      Destroy (Item.Entity);
   end Destroy;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class) is
      B : constant Call_Graph_Browser := Call_Graph_Browser (Browser);
   begin
      if B.Idle_Id /= 0 then
         Idle_Remove (B.Idle_Id);
      end if;
   end On_Destroy;

   -------------------------------
   -- Create_Call_Graph_Browser --
   -------------------------------

   function Create_Call_Graph_Browser
     (Kernel : access Kernel_Handle_Record'Class)
      return Call_Graph_Browser
   is
      Browser : Call_Graph_Browser;
   begin
      Browser := new Call_Graph_Browser_Record;
      Initialize (Browser, Kernel, Create_Toolbar => False);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Call_Graph_Module_Id,
         Context_Func    => Default_Browser_Context_Factory'Access);

      Widget_Callback.Connect
        (Browser, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
      return Browser;
   end Create_Call_Graph_Browser;

   -----------------------------
   -- Open_Call_Graph_Browser --
   -----------------------------

   function Open_Call_Graph_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : MDI_Child;
      Browser : Call_Graph_Browser;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Call_Graph_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Create_Call_Graph_Browser (Kernel);
         Child := Put
           (Get_MDI (Kernel), Browser,
            Focus_Widget => Gtk_Widget (Get_Canvas (Browser)));
         Set_Title (Child, -"Call graph Browser");
      end if;

      return Child;
   end Open_Call_Graph_Browser;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (In_Browser : access General_Browser_Record'Class;
      Entity     : Entity_Information)
      return Canvas_Item
   is
      Found : Canvas_Item := null;
      Iter : Item_Iterator := Start (Get_Canvas (In_Browser));
   begin
      loop
         Found := Get (Iter);
         exit when Found = null
           or else Is_Equal (Entity_Item (Found).Entity, Entity);
         Next (Iter);
      end loop;
      return Found;
   end Find_Entity;

   -------------------------------
   -- Add_Entity_If_Not_Present --
   -------------------------------

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Entity_Information) return Entity_Item
   is
      Child  : Entity_Item;
      May_Have_To_Dependencies : Boolean := True;
      Tree   : Scope_Tree;
      Node   : Scope_Tree_Node;
      Iter   : Scope_Tree_Node_Iterator;

   begin
      Child := Entity_Item (Find_Entity (Browser, Entity));
      if Child = null then

         if Automatically_Check_To_Dependencies then
            Get_Scope_Tree (Get_Kernel (Browser), Entity, Tree, Node);

            if Node /= Null_Scope_Tree_Node then
               Iter := Start (Node);
               May_Have_To_Dependencies := False;

               while Get (Iter) /= Null_Scope_Tree_Node loop
                  if Is_Subprogram (Get (Iter)) then
                     May_Have_To_Dependencies := True;
                     exit;
                  end if;
                  Next (Iter);
               end loop;
            end if;

            Free (Tree);
         end if;

         Gtk_New (Child, Browser, Entity, May_Have_To_Dependencies);
         Put (Get_Canvas (Browser), Child);
         Refresh (Child);
      end if;

      return Child;
   end Add_Entity_If_Not_Present;

   -------------------------------
   -- Add_Entity_If_Not_Present --
   -------------------------------

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Node    : Scope_Tree_Node) return Entity_Item
   is
      Entity : Entity_Information := Get_Entity (Node);
      Child  : Entity_Item;
   begin
      Child := Add_Entity_If_Not_Present (Browser, Entity);
      Destroy (Entity);
      return Child;
   end Add_Entity_If_Not_Present;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel   : access Kernel_Handle_Record'Class;
      Entity   : Entity_Information)
   is
      Item, Child   : Entity_Item;
      Child_Browser : MDI_Child;
      Browser       : Call_Graph_Browser;
      Link          : Browser_Link;
      Tree          : Scope_Tree;
      Node          : Scope_Tree_Node;
      Rename        : Entity_Information;
      Is_Renaming   : Boolean;

      procedure Process_Item (Node : Scope_Tree_Node);
      --  Add the call graph for Node, linking the new items to Item.

      ------------------
      -- Process_Item --
      ------------------

      procedure Process_Item (Node : Scope_Tree_Node) is
         Iter  : Scope_Tree_Node_Iterator := Start (Node);
         Child : Entity_Item;
         Link  : Browser_Link;
      begin
         while Get (Iter) /= Null_Scope_Tree_Node loop
            if Is_Subprogram (Get (Iter)) then
               Child := Add_Entity_If_Not_Present (Browser, Get (Iter));
               if not Has_Link (Get_Canvas (Browser), Item, Child) then
                  Link := new Browser_Link_Record;
                  Add_Link (Get_Canvas (Browser),
                            Link => Link,
                            Src  => Item,
                            Dest => Child);
               end if;

            --  For a label, do not insert it in the browser, but process
            --  its children

            elsif Is_Label (Get (Iter)) then
               Process_Item (Get (Iter));
            end if;

            Next (Iter);
         end loop;
      end Process_Item;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Get_Scope_Tree (Kernel, Entity, Tree, Node);
      if Node = Null_Scope_Tree_Node then
         return;
      end if;

      --  Create the browser if necessary
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      Item := Add_Entity_If_Not_Present (Browser, Node);

      if not Children_Shown (Item) then
         Set_Children_Shown (Item, True);

         --  If we have a renaming, add the entry for the renamed entity
         Renaming_Of
           (Get_LI_File_List (Kernel), Entity, Is_Renaming, Rename);
         if Is_Renaming and then Rename /= No_Entity_Information then
            Child := Add_Entity_If_Not_Present (Browser, Rename);
            if not Has_Link (Get_Canvas (Browser), Item, Child) then
               Link := new Renaming_Link_Record;
               Add_Link
                 (Get_Canvas (Browser), Link => Link,
                  Src => Item, Dest => Child, Arrow => Both_Arrow);
            end if;
            Destroy (Rename);

         elsif Is_Renaming then
            Insert (Kernel,
                    Get_Name (Entity)
                    & (-" is a renaming of an unknown entity"),
                    Mode => Error);

         else
            Process_Item (Node);
         end if;

         Redraw_Title_Bar (Item);
         Layout (Browser, Force => False);
         Refresh_Canvas (Get_Canvas (Browser));
      end if;

      Pop_State (Kernel_Handle (Kernel));
      Free (Tree);

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Free (Tree);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Examine_Entity_Call_Graph;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Idle_Data) is
      procedure Clean;
      --  Clean up before exiting Destroy_Idle

      procedure Clean is
      begin
         Data.Browser.Idle_Id := 0;
         Destroy (Data.Iter);
         Destroy (Data.Entity);
         Pop_State (Get_Kernel (Data.Browser));
      end Clean;

   begin
      Layout (Data.Browser, Force => False);
      Refresh_Canvas (Get_Canvas (Data.Browser));
      Clean;

   exception
      when E : others =>
         Clean;
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Destroy_Idle;

   ---------------------------------------
   -- Examine_Ancestors_Call_Graph_Idle --
   ---------------------------------------

   function Examine_Ancestors_Call_Graph_Idle
     (Data : Examine_Ancestors_Idle_Data) return Boolean
   is
      procedure Add_Item (Node : Scope_Tree_Node; Is_Renaming : Boolean);
      --  Add a new item for the entity declared in Node to the browser

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Node : Scope_Tree_Node; Is_Renaming : Boolean) is
         Child  : Entity_Item;
         Parent : Scope_Tree_Node;
         Link   : Browser_Link;
      begin
         if Is_Subprogram (Node) then

            --  A renaming entity ? Create a special link
            if Is_Renaming then
               Child := Add_Entity_If_Not_Present (Data.Browser, Node);
               if not Has_Link
                 (Get_Canvas (Data.Browser), Child, Data.Item)
               then
                  Link := new Renaming_Link_Record;
                  Add_Link
                    (Get_Canvas (Data.Browser), Link => Link,
                     Src => Child, Dest => Data.Item,
                     Arrow => Both_Arrow);
               end if;

               --  An entity that calls our entity.
            else
               Parent := Get_Parent (Node);
               while Parent /= Null_Scope_Tree_Node
                 and then not Is_Subprogram (Parent)
               loop
                  Parent := Get_Parent (Parent);
               end loop;

               if Parent /= Null_Scope_Tree_Node then
                  Child := Add_Entity_If_Not_Present (Data.Browser, Parent);

                  if not Has_Link
                    (Get_Canvas (Data.Browser), Child, Data.Item)
                  then
                     Link := new Browser_Link_Record;
                     Add_Link
                       (Get_Canvas (Data.Browser), Link => Link,
                        Src => Child, Dest => Data.Item);
                  end if;
               end if;
            end if;
         end if;
      end Add_Item;

      Tree : Scope_Tree;
      Kernel : constant Kernel_Handle := Get_Kernel (Data.Browser);
   begin
      if Get (Data.Iter.all) = No_Reference then
         return False;

      else
         begin
            Tree := Create_Tree (Get_LI (Data.Iter.all));
            if Tree /= Null_Scope_Tree then
               Find_Entity_References
                 (Tree, Data.Entity, Add_Item'Unrestricted_Access);
               Free (Tree);
            end if;

            Next (Get_Language_Handler (Kernel), Data.Iter.all,
                  Get_LI_File_List (Kernel));
            return True;

         exception
            when E : others =>
               Trace (Me, "Unexpected exception: " &
                      Exception_Information (E));
               Free (Tree);
               return False;
         end;
      end if;
   end Examine_Ancestors_Call_Graph_Idle;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel        : access Kernel_Handle_Record'Class;
      Entity        : Entity_Information)
   is
      Browser       : Call_Graph_Browser;
      Item          : Entity_Item;
      Child         : Entity_Item;
      Child_Browser : MDI_Child;
      Data          : Examine_Ancestors_Idle_Data;
      Rename        : Entity_Information;
      Link          : Browser_Link;
      Is_Renaming   : Boolean;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Item := Add_Entity_If_Not_Present (Browser, Entity);
      Set_Parents_Shown (Item, True);
      Redraw_Title_Bar (Item);

      --  If we have a renaming, add the entry for the renamed entity
      Renaming_Of (Get_LI_File_List (Kernel), Entity, Is_Renaming, Rename);
      if Is_Renaming and then Rename /= No_Entity_Information then
         Child := Add_Entity_If_Not_Present (Browser, Rename);
         if not Has_Link (Get_Canvas (Browser), Item, Child) then
            Link := new Renaming_Link_Record;
            Add_Link
              (Get_Canvas (Browser), Link => Link,
               Src => Item, Dest => Child, Arrow => Both_Arrow);
         end if;
         Destroy (Rename);

      elsif Is_Renaming then
         Insert (Kernel,
                 Get_Name (Entity)
                 & (-" is a renaming of an unknown entity"),
                 Mode => Error);
      end if;

      Data := (Iter    => new Entity_Reference_Iterator,
               Browser => Browser,
               Entity  => Copy (Entity),
               Item    => Item);
      Find_All_References
        (Get_Project (Kernel),
         Get_Language_Handler (Kernel),
         Entity,
         Get_LI_File_List (Kernel),
         Data.Iter.all,
         LI_Once => True);

      Browser.Idle_Id := Examine_Ancestors_Idle.Add
        (Cb       => Examine_Ancestors_Call_Graph_Idle'Access,
         D        => Data,
         Priority => Priority_Low_Idle,
         Destroy  => Destroy_Idle'Access);

      --  All memory is freed at the end of
      --  Examine_Ancestors_Call_Graph_Idle

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Edit_Body_From_Contextual --
   -------------------------------

   procedure Edit_Body_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      C : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Location : Src_Info.File_Location;
      Status   : Src_Info.Queries.Find_Decl_Or_Body_Query_Status;
   begin
      Find_Next_Body
        (Get_Kernel (Context),
         Lib_Info    => Locate_From_Source_And_Complete
           (Get_Kernel (C), Source_Filename => File_Information (C)),
         File_Name   => File_Information (C),
         Entity_Name => Entity_Name_Information (C),
         Line        => Line_Information (C),
         Column      => Column_Information (C),
         Location    => Location,
         Status      => Status);

      --  If the body wasn't found then display the specs
      if Status /= Success and then Status /= Fuzzy_Match then
         Open_File_Editor
           (Get_Kernel (Context),
            File_Information (C),
            Line   => Line_Information (C),
            Column => Column_Information (C),
            From_Path => True);
      else
         Open_File_Editor
           (Get_Kernel (Context),
            Filename => Get_File (Location),
            Line     => Get_Line (Location),
            Column   => Get_Column (Location),
            From_Path => True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Edit_Body_From_Contextual;

   -------------------------------
   -- Edit_Spec_From_Contextual --
   -------------------------------

   procedure Edit_Spec_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Entity : constant Entity_Information := Get_Entity (C);

   begin
      --  If the body wasn't found then display the specs
      if Entity = No_Entity_Information then
         Insert (Get_Kernel (Context),
                 (-"Couldn't find cross-reference information for ")
                 & '"' & Entity_Name_Information (C) & '"');
      else
         Open_File_Editor
           (Get_Kernel (Context),
            Get_Declaration_File_Of (Entity),
            Line   => Get_Declaration_Line_Of (Entity),
            Column => Get_Declaration_Column_Of (Entity),
            From_Path => True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Edit_Spec_From_Contextual;

   --------------------------------------------
   -- Edit_Entity_Call_Graph_From_Contextual --
   --------------------------------------------

   procedure Edit_Entity_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Node_Entity : Entity_Information;

   begin
      Push_State (Get_Kernel (Entity), Busy);
      Node_Entity := Get_Entity (Entity);

      if Node_Entity /= No_Entity_Information then
         --  ??? Should check that Decl.Kind is a subprogram

         Examine_Entity_Call_Graph (Get_Kernel (Entity), Node_Entity);

      else
         Insert (Get_Kernel (Entity),
                 -"No call graph available for "
                 & Entity_Name_Information (Entity));
      end if;

      Pop_State (Get_Kernel (Entity));

   exception
      when E : others =>
         Insert (Get_Kernel (Entity),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Entity),
                 Mode => Error);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (Entity));
   end Edit_Entity_Call_Graph_From_Contextual;

   -----------------------------------------------
   -- Edit_Ancestors_Call_Graph_From_Contextual --
   -----------------------------------------------

   procedure Edit_Ancestors_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Info : Entity_Information;

   begin
      Push_State (Get_Kernel (Entity), Busy);
      Info := Get_Entity (Entity);

      if Info /= No_Entity_Information then
         Examine_Ancestors_Call_Graph (Get_Kernel (Entity), Info);
      else
         Insert (Get_Kernel (Entity),
                 -"No information found for the file "
                   & File_Information (Entity),
                 Mode => Error);
      end if;

      Pop_State (Get_Kernel (Entity));

   exception
      when E : others =>
         Insert (Get_Kernel (Entity),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Entity),
                 Mode => Error);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (Entity));
   end Edit_Ancestors_Call_Graph_From_Contextual;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Entity_Idle_Data) is
   begin
      Destroy (Data.Iter);
      Destroy (Data.Entity);
      Free (Data.Category);
      Pop_State (Data.Kernel);
   end Destroy_Idle;

   ---------------
   -- Print_Ref --
   ---------------

   procedure Print_Ref
     (Kernel   : access Kernel_Handle_Record'Class;
      File     : String;
      Line     : Positive;
      Column   : Natural;
      Name     : String;
      Category : String)
   is
      Col : Positive;
   begin
      if Column > 0 then
         Col := Positive (Column);
      else
         Col := 1;
      end if;

      Console.Insert_Result
        (Kernel, Category, File, Name, Line, Col, Name'Length);
   end Print_Ref;

   -------------------------
   -- Find_Next_Reference --
   -------------------------

   function Find_Next_Reference (D : Entity_Idle_Data) return Boolean is
      Location : File_Location;
   begin
      if Get (D.Iter.all) = No_Reference then
         return False;
      else
         if (D.Include_Writes and then Is_Write_Reference (Get (D.Iter.all)))
           or else
           (D.Include_Reads and then Is_Read_Reference (Get (D.Iter.all)))
         then
            Location := Get_Location (Get (D.Iter.all));
            Print_Ref
              (D.Kernel, Get_File (Location),
               Get_Line (Location), Get_Column (Location),
               Get_Name (D.Entity),
               D.Category.all & Get_Name (D.Entity));
         end if;
         Next (Get_Language_Handler (D.Kernel), D.Iter.all,
               Get_LI_File_List (D.Kernel));

         return True;
      end if;
   end Find_Next_Reference;

   ----------------------------------
   -- Find_All_References_Internal --
   ----------------------------------

   procedure Find_All_References_Internal
     (Kernel         : access Kernel_Handle_Record'Class;
      Info           : Entity_Information;
      Category_Title : String;
      Include_Writes : Boolean;
      Include_Reads  : Boolean)
   is
      Data     : Entity_Idle_Data;
      Idle_Id  : Idle_Handler_Id;
      pragma Unreferenced (Idle_Id);

   begin
      Push_State (Kernel_Handle (Kernel), Busy);
      if Info /= No_Entity_Information then
         begin
            Remove_Result_Category
              (Kernel, Category_Title & Get_Name (Info));

            if Include_Reads then
               Print_Ref (Kernel,
                          Get_Declaration_File_Of (Info),
                          Get_Declaration_Line_Of (Info),
                          Get_Declaration_Column_Of (Info),
                          Get_Name (Info),
                          Category_Title & Get_Name (Info));
            end if;

            Data := (Kernel         => Kernel_Handle (Kernel),
                     Iter           => new Entity_Reference_Iterator,
                     Include_Writes => Include_Writes,
                     Include_Reads  => Include_Reads,
                     Category       => new String'(Category_Title),
                     Entity         => Copy (Info));

            Find_All_References
              (Get_Project (Kernel),
               Get_Language_Handler (Kernel),
               Info, Get_LI_File_List (Kernel), Data.Iter.all);

            Idle_Id := Entity_Iterator_Idle.Add
              (Cb       => Find_Next_Reference'Access,
               D        => Data,
               Priority => Priority_Low_Idle,
               Destroy  => Destroy_Idle'Access);
         exception
            when others =>
               Destroy (Data.Iter);
               raise;
         end;
      else
         Pop_State (Kernel_Handle (Kernel));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         Pop_State (Kernel_Handle (Kernel));
   end Find_All_References_Internal;

   -----------------------------------------
   -- Find_All_References_From_Contextual --
   -----------------------------------------

   procedure Find_All_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Find_All_References_Internal
        (Get_Kernel (Context),
         Get_Entity (Entity_Selection_Context_Access (Context)),
         Category_Title => -All_Refs_Category,
         Include_Writes => True,
         Include_Reads  => True);
   end Find_All_References_From_Contextual;

   -------------------------------------
   -- Find_All_Writes_From_Contextual --
   -------------------------------------

   procedure Find_All_Writes_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Find_All_References_Internal
        (Get_Kernel (Context),
         Get_Entity (Entity_Selection_Context_Access (Context)),
         Category_Title => -"Modifications of: ",
         Include_Writes => True,
         Include_Reads  => False);
   end Find_All_Writes_From_Contextual;

   ------------------------------------
   -- Find_All_Reads_From_Contextual --
   ------------------------------------

   procedure Find_All_Reads_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Find_All_References_Internal
        (Get_Kernel (Context),
         Get_Entity (Entity_Selection_Context_Access (Context)),
         Category_Title => -"Read-Only references for: ",
         Include_Writes => False,
         Include_Reads  => True);
   end Find_All_Reads_From_Contextual;

   -----------------------------------------------
   -- Find_All_Local_References_From_Contextual --
   -----------------------------------------------

   procedure Find_All_Local_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Info     : Entity_Information;
      Iter     : Entity_Reference_Iterator;
      Location : File_Location;
      Kernel   : constant Kernel_Handle := Get_Kernel (Entity);

   begin
      Push_State (Kernel, Busy);
      Info := Get_Entity (Entity);

      if Info /= No_Entity_Information then
         --  Print the declaration of the entity, but only if it is in the
         --  current file. Otherwise, this is too surprising for the use

         Remove_Result_Category
           (Kernel, -"References for: " & Get_Name (Info));

         if Get_Declaration_File_Of (Info) = File_Information (Entity) then
            Print_Ref (Kernel,
                       Get_Declaration_File_Of (Info),
                       Get_Declaration_Line_Of (Info),
                       Get_Declaration_Column_Of (Info),
                       Get_Name (Info),
                       -"References for: " & Get_Name (Info));
         end if;

         Find_All_References
           (Get_Project (Kernel),
            Get_Language_Handler (Kernel),
            Info, Get_LI_File_List (Kernel), Iter,
            In_File => File_Information (Entity));

         while Get (Iter) /= No_Reference loop
            Location := Get_Location (Get (Iter));
            Print_Ref
              (Kernel, Get_File (Location),
               Get_Line (Location), Get_Column (Location),
               Get_Name (Info),
               -"References for: " & Get_Name (Info));

            Next
              (Get_Language_Handler (Kernel),
               Iter,
               Get_LI_File_List (Kernel));
         end loop;

         Destroy (Iter);
      end if;

      Pop_State (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         Destroy (Iter);
         Pop_State (Get_Kernel (Entity));
   end Find_All_Local_References_From_Contextual;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Ancestors_Call_Graph
        (Get_Kernel (Get_Browser (Item)), Entity_Item (Item).Entity);
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Entity_Call_Graph
        (Get_Kernel (Get_Browser (Item)), Entity_Item (Item).Entity);
   end Examine_Entity_Call_Graph;

   --------------------------------
   -- Call_Graph_Contextual_Menu --
   --------------------------------

   procedure Call_Graph_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      Submenu        : Gtk_Menu;
      Item           : Gtk_Menu_Item;
      Entity_Context : Entity_Selection_Context_Access;

   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity_Context := Entity_Selection_Context_Access (Context);

         if Has_Entity_Name_Information (Entity_Context) then

            Gtk_New (Item, Label => -"References");
            Gtk_New (Submenu);
            Set_Submenu (Item, Gtk_Widget (Submenu));
            Append (Menu, Item);

            if not Has_Category_Information (Entity_Context)
              or else Category_Information (Entity_Context) in
                Subprogram_Category
            then
               Gtk_New (Item, Label => Entity_Name_Information (Entity_Context)
                        & (-" calls"));
               Append (Submenu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Edit_Entity_Call_Graph_From_Contextual'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => Entity_Name_Information (Entity_Context)
                        & (-" is called by"));
               Append (Submenu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Edit_Ancestors_Call_Graph_From_Contextual'Access),
                  Selection_Context_Access (Context));
            end if;

            Gtk_New (Item, Label => (-"Find all references to ") &
                     Entity_Name_Information (Entity_Context));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Find_All_References_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => (-"Find all local references to ") &
                     Entity_Name_Information (Entity_Context));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Find_All_Local_References_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => (-"Find all writes to ") &
                     Entity_Name_Information (Entity_Context));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Find_All_Writes_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => (-"Find all reads of ") &
                     Entity_Name_Information (Entity_Context));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Find_All_Reads_From_Contextual'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Call_Graph_Contextual_Menu;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access Entity_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access
   is
      pragma Unreferenced (Event);
      Context : constant Selection_Context_Access :=
        new Entity_Selection_Context;
      Mitem : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
   begin
      if Get_Declaration_File_Of (Item.Entity) /= ":" then
         declare
            Filename : constant String := Get_Full_Path_From_File
              (Registry        => Get_Registry (Get_Kernel (Browser)),
               Filename        => Get_Declaration_File_Of (Item.Entity),
               Use_Source_Path => True,
               Use_Object_Path => False);
         begin
            Set_File_Information
              (File_Selection_Context_Access (Context),
               Directory    => Dir_Name (Filename),
               File_Name    => Base_Name (Filename));
         end;
      end if;

      Set_Entity_Information
        (Entity_Selection_Context_Access (Context),
         Entity_Name => Get_Name (Item.Entity),
         Line        => Get_Declaration_Line_Of (Item.Entity),
         Column      => Get_Declaration_Column_Of (Item.Entity),
         Category    => Language.Cat_Procedure);

      if Menu /= null then
         Gtk_New (Mitem, Get_Name (Item.Entity) & (-" calls..."));
         Gtk_New (Pix, Get_Children_Arrow (Get_Browser (Item)));
         Set_Image (Mitem, Pix);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Entity_Call_Graph_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Children_Shown (Item));

         Gtk_New (Mitem, Get_Name (Item.Entity) & (-" is called by..."));
         Gtk_New (Pix, Get_Parents_Arrow (Get_Browser (Item)));
         Set_Image (Mitem, Pix);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Ancestors_Call_Graph_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Parents_Shown (Item));

         Gtk_New (Mitem, -"Go to spec");
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Spec_From_Contextual'Access),
            Context);

         Gtk_New (Mitem, -"Go to body");
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Body_From_Contextual'Access),
            Context);
      end if;

      return Context;
   end Contextual_Factory;

   -------------------
   -- On_Call_Graph --
   -------------------

   procedure On_Call_Graph
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child       : MDI_Child;
      pragma Unreferenced (Widget, Child);

      Context     : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
      Entity      : Entity_Selection_Context_Access;
      Node_Entity : Entity_Information;
   begin
      Child := Open_Call_Graph_Browser (Kernel);

      if Context /= null
        and then Context.all in Entity_Selection_Context'Class
      then
         Entity := Entity_Selection_Context_Access (Context);
         Node_Entity := Get_Entity (Entity);

         if Node_Entity /= No_Entity_Information then
            Examine_Entity_Call_Graph (Kernel, Node_Entity);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_Call_Graph "
                & Exception_Information (E));
   end On_Call_Graph;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel);
      Browser : constant Call_Graph_Browser := Call_Graph_Browser (Child);
      Iter    : constant Selection_Iterator := Start (Get_Canvas (Browser));
   begin
      --  If there is no selection, or more than one item, nothing we can do
      if Get (Iter) = null
        or else Get (Next (Iter)) /= null
      then
         return null;
      end if;

      return Contextual_Factory
        (Item    => Browser_Item (Get (Iter)),
         Browser => Browser,
         Event   => null,
         Menu    => null);
   end Default_Factory;

   --------------------------------
   -- Call_Graph_Command_Handler --
   --------------------------------

   function Call_Graph_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;
      Name, File, Line : List_Node;
      L, C             : Positive := 1;
      Entity           : Entity_Information;
      Status           : Find_Decl_Or_Body_Query_Status;

   begin
      --  ??? This part is copied from
      --  Browsers.Entities.Show_Entity_Command_Handler, would be nice to share
      if Length (Args) < 2 then
         return "Not enough arguments";
      end if;

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
         Lib_Info    => Locate_From_Source_And_Complete (Kernel, Data (File)),
         File_Name   => Data (File),
         Entity_Name => Data (Name),
         Line        => L,
         Column      => C,
         Entity      => Entity,
         Status      => Status);

      if Status /= Success and then Status /= Fuzzy_Match then
         return "Entity not found";
      end if;

      if Command = "entity.find_all_refs" then
         Find_All_References_Internal
           (Kernel, Entity,
            Category_Title => -All_Refs_Category,
            Include_Writes => True,
            Include_Reads  => True);

         Destroy (Entity);
      end if;
      return "";
   end Call_Graph_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools");
   begin
      Register_Module
        (Module                  => Call_Graph_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => Call_Graph_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Call_Graph_Contextual_Menu'Access,
         MDI_Child_Tag           => Call_Graph_Browser_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu (Kernel, Tools, -"Call Graph", "", On_Call_Graph'Access);

      --  ??? File_Name should be optional
      --  ??? Ultimately, we should display the results in the location window,
      --  but return them as a list (Python scripts for instance)
      Register_Command
        (Kernel, "entity.find_all_refs",
         (-"Usage:") & ASCII.LF
         & "  entity.find_all_refs entity_name file_name [line] [column]"
         & ASCII.LF
         & (-"Display in the location window all the references to the"
            & " entity"),
         Handler => Call_Graph_Command_Handler'Access);
   end Register_Module;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget is
   begin
      if Node.Tag.all = "Call_Graph" then
         return Gtk_Widget (Create_Call_Graph_Browser (User));
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
      if Widget.all in Call_Graph_Browser_Record'Class then
         N := new Node;
         N.Tag := new String'("Call_Graph");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Renaming_Link_Record;
      Invert_Mode : Boolean;
      GC          : Gdk.GC.Gdk_GC;
      Edge_Number : Glib.Gint)
   is
   begin
      Set_Line_Attributes
        (GC,
         Line_Width => 0,
         Line_Style => Line_On_Off_Dash,
         Cap_Style  => Cap_Butt,
         Join_Style => Join_Miter);

      Draw_Link
        (Canvas, Browser_Link_Record (Link.all)'Access,
         Invert_Mode, GC, Edge_Number);

      Set_Line_Attributes
        (GC,
         Line_Width => 0,
         Line_Style => Line_Solid,
         Cap_Style  => Cap_Butt,
         Join_Style => Join_Miter);
   end Draw_Link;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item                        : access Entity_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class)
   is
      W, H : Gint;
   begin
      if Get_Declaration_File_Of (Item.Entity) /= "" then
         Set_Text (Layout,
                   Get_Declaration_File_Of (Item.Entity)
                   & ':' & Image (Get_Declaration_Line_Of (Item.Entity)));
      else
         Set_Text (Layout, -"<Unresolved>");
      end if;

      Get_Pixel_Size (Layout, W, H);

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access,
         Gint'Max (Width, W + 2 * Margin),
         Height + H,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Draw_Layout
        (Drawable => Pixmap (Item),
         GC       => Get_Black_GC (Get_Style (Get_Browser (Item))),
         X        => Xoffset + Margin,
         Y        => Yoffset + 1,
         Layout   => Pango_Layout (Layout));
   end Resize_And_Draw;

end Browsers.Call_Graph;
