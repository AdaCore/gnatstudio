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
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.GC;           use Gdk.GC;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Event;        use Gdk.Event;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;         use Gtk.Main;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Stock;        use Gtk.Stock;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtkada.MDI;       use Gtkada.MDI;
with Pango.Layout;     use Pango.Layout;

with Src_Info;                 use Src_Info;
with Src_Info.Queries;         use Src_Info.Queries;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with String_Utils;             use String_Utils;
with Browsers.Canvas;          use Browsers.Canvas;

with Glide_Intl;       use Glide_Intl;
with Browsers.Canvas;  use Browsers.Canvas;
with Language;         use Language;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Traces;           use Traces;

package body Browsers.Call_Graph is

   Me : constant Debug_Handle := Create ("Browsers.Call_Graph");

   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   Margin : constant := 2;

   Automatically_Check_To_Dependencies : constant Boolean := True;
   --  If True, then every time an item is added to the call graph we check,
   --  and if no to dependency exists, the right arrow is not displayed.

   type Entity_Idle_Data is record
      Kernel : Kernel_Handle;
      Iter   : Entity_Reference_Iterator_Access;
      Entity : Entity_Information;
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
     (In_Browser : access Glide_Browser_Record'Class;
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

   procedure Find_All_Local_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  List all the references to the entity in the local file (or ALI file).

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

   procedure Get_Scope_Tree
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      Tree   : out Scope_Tree;
      Node   : out Scope_Tree_Node);
   --  Get the scope tree and node for Entity.

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
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
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information;
      May_Have_To_Dependencies : Boolean)
   is
      B : constant Call_Graph_Browser := Call_Graph_Browser (Browser);
      Width, Height : Gint;
   begin
      Item.Entity   := Copy (Entity);
      Item.Browser  := Glide_Browser (Browser);

      Item.Layout   := Create_Pango_Layout (Browser);
      Set_Font_Description
        (Item.Layout, Get_Pref (Get_Kernel (Browser), Browsers_Link_Font));

      if Get_Declaration_File_Of (Entity) /= "" then
         Set_Text
           (Item.Layout, Get_Name (Entity) & ASCII.LF
            & Get_Declaration_File_Of (Entity)
            & ':' & Image (Get_Declaration_Line_Of (Entity)));

      else
         Set_Text
           (Item.Layout, Get_Name (Entity) & ASCII.LF & (-"<Unresolved>"));
      end if;

      Item.From_Parsed := False;
      Item.To_Parsed   := not May_Have_To_Dependencies;

      Get_Pixel_Size (Item.Layout, Width, Height);
      Width := Width + 2 * Margin
        + Get_Width (B.Left_Arrow) + Get_Width (B.Right_Arrow);

      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Browser), Width, Height + 2 * Margin);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Entity_Item_Record) is
   begin
      Destroy (Item.Entity);
      Unref (Item.Layout);
   end Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Item    : access Entity_Item_Record)
   is
      B : constant Call_Graph_Browser := Call_Graph_Browser (Browser);
   begin
      Draw_Item_Background (Browser, Item);

      Draw_Layout
        (Drawable => Pixmap (Item),
         GC       => Get_Text_GC (Browser),
         X        => Margin + Get_Width (B.Left_Arrow),
         Y        => Margin,
         Layout   => Item.Layout);

      if not Item.From_Parsed then
         Render_To_Drawable_Alpha
           (Pixbuf          => B.Left_Arrow,
            Drawable        => Pixmap (Item),
            Src_X           => 0,
            Src_Y           => 0,
            Dest_X          => Margin,
            Dest_Y          => Margin,
            Width           => -1,
            Height          => -1,
            Alpha           => Alpha_Full,
            Alpha_Threshold => 128);
      end if;

      if not Item.To_Parsed then
         Render_To_Drawable_Alpha
           (Pixbuf          => B.Right_Arrow,
            Drawable        => Pixmap (Item),
            Src_X           => 0,
            Src_Y           => 0,
            Dest_X          => Gint (Get_Coord (Item).Width)
              - Margin - Get_Width (B.Right_Arrow),
            Dest_Y          => Margin,
            Width           => -1,
            Height          => -1,
            Alpha           => Alpha_Full,
            Alpha_Threshold => 128);
      end if;
   end Refresh;

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
      Initialize (Browser, Kernel);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Call_Graph_Module_Id,
         Context_Func    => Default_Browser_Context_Factory'Access);
      Browser.Left_Arrow := Render_Icon
        (Browser, Stock_Go_Back, Icon_Size_Menu);
      Browser.Right_Arrow := Render_Icon
        (Browser, Stock_Go_Forward, Icon_Size_Menu);

      Widget_Callback.Connect
        (Browser, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));

      Set_Size_Request
        (Browser,
         Get_Pref (Kernel, Default_Widget_Width),
         Get_Pref (Kernel, Default_Widget_Height));
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
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Call_Graph_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Child := Put (Get_MDI (Kernel), Create_Call_Graph_Browser (Kernel));
         Set_Title (Child, -"Call graph Browser");
      end if;

      return Child;
   end Open_Call_Graph_Browser;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (In_Browser : access Glide_Browser_Record'Class;
      Entity     : Entity_Information)
      return Canvas_Item
   is
      Found : Canvas_Item := null;

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Check whether Item contains File

      ----------------
      -- Check_Item --
      ----------------

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         pragma Unreferenced (Canvas);
         Info : Entity_Information;
      begin
         if Item.all in Entity_Item_Record'Class then
            Info := Entity_Item (Item).Entity;
            --  ??? Should we check the file name as well
            if Get_Name (Info)  = Get_Name (Entity)
              and then Get_Declaration_Line_Of (Info) =
                Get_Declaration_Line_Of (Entity)
              and then Get_Declaration_Column_Of (Info) =
                Get_Declaration_Column_Of (Entity)
            then
               Found := Canvas_Item (Item);
               return False;
            end if;
         end if;

         return True;
      end Check_Item;

   begin
      For_Each_Item (Get_Canvas (In_Browser), Check_Item'Unrestricted_Access);
      return Found;
   end Find_Entity;

   --------------------
   -- Get_Scope_Tree --
   --------------------

   procedure Get_Scope_Tree
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information;
      Tree   : out Scope_Tree;
      Node   : out Scope_Tree_Node)
   is
      Lib_Info : LI_File_Ptr;
      Location : File_Location;
      Status   : Find_Decl_Or_Body_Query_Status;
   begin
      Lib_Info := Locate_From_Source_And_Complete
        (Kernel, Get_Declaration_File_Of (Entity));

      if Lib_Info /= No_LI_File then
         --  We need to find the body of the entity in fact. In Ada, this
         --  will always be the same LI as the spec, but this is no
         --  longer true for C or C++.
         Find_Next_Body
           (Kernel             => Kernel,
            Lib_Info           => Lib_Info,
            File_Name          => Get_Declaration_File_Of (Entity),
            Entity_Name        => Get_Name (Entity),
            Line               => Get_Declaration_Line_Of (Entity),
            Column             => Get_Declaration_Column_Of (Entity),
            Location           => Location,
            Status             => Status);

         --  In case there is no body, do nothing.
         if Location /= Null_File_Location then
            Lib_Info := Locate_From_Source_And_Complete
              (Kernel, Get_File (Location));
         end if;
      end if;

      if Lib_Info = No_LI_File then
         Insert (Kernel,
                 -"LI file not found for " & Get_Declaration_File_Of (Entity));
         Tree := Null_Scope_Tree;
         Node := Null_Scope_Tree_Node;
         return;
      end if;

      Tree := Create_Tree (Lib_Info);

      if Tree = Null_Scope_Tree then
         Trace (Me, "Couldn't create scope tree for "
                & Get_LI_Filename (Lib_Info));
         Node := Null_Scope_Tree_Node;
         return;
      end if;

      Node := Find_Entity_Scope (Tree, Entity);

      if Node = Null_Scope_Tree_Node then
         Insert (Kernel,
                 -"Couldn't find the call graph for " & Get_Name (Entity));
         Trace (Me, "Couldn't find entity "
                & Get_Name (Entity) & " in "
                & Get_LI_Filename (Lib_Info)
                & " at line" & Get_Declaration_Line_Of (Entity)'Img
                & " column"  & Get_Declaration_Column_Of (Entity)'Img);
         Free (Tree);
         Node := Null_Scope_Tree_Node;
      end if;
   end Get_Scope_Tree;

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
      Link          : Glide_Browser_Link;
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
         Link  : Glide_Browser_Link;
      begin
         while Get (Iter) /= Null_Scope_Tree_Node loop
            if Is_Subprogram (Get (Iter)) then
               Child := Add_Entity_If_Not_Present (Browser, Get (Iter));
               if not Has_Link (Get_Canvas (Browser), Item, Child) then
                  Link := new Glide_Browser_Link_Record;
                  Add_Link (Get_Canvas (Browser),
                            Link => Link,
                            Src  => Item,
                            Dest => Child);
               end if;
               Refresh (Browser, Child);

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

      --  For efficiency, do not recompute the layout for each item
      Set_Auto_Layout (Get_Canvas (Browser), False);

      Item := Add_Entity_If_Not_Present (Browser, Node);

      if not Item.To_Parsed then
         Item.To_Parsed := True;
         Refresh (Browser, Item);

         --  If we have a renaming, add the entry for the renamed entity
         Renaming_Of (Kernel, Entity, Is_Renaming, Rename);
         if Is_Renaming and then Rename /= No_Entity_Information then
            Child := Add_Entity_If_Not_Present (Browser, Rename);
            if not Has_Link (Get_Canvas (Browser), Item, Child) then
               Link := new Renaming_Link_Record;
               Add_Link
                 (Get_Canvas (Browser), Link => Link,
                  Src => Item, Dest => Child, Arrow => Both_Arrow);
            end if;
            Destroy (Rename);
            Refresh (Browser, Child);

         elsif Is_Renaming then
            Insert (Kernel,
                    Get_Name (Entity)
                    & (-" is a renaming of an unknown entity"),
                    Mode => Error);

         else
            Process_Item (Node);
         end if;

         Set_Auto_Layout (Get_Canvas (Browser), True);
         Layout
           (Get_Canvas (Browser),
            Force => False,
            Vertical_Layout => Get_Pref (Kernel, Browsers_Vertical_Layout));
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
      Set_Auto_Layout (Get_Canvas (Data.Browser), True);
      Layout (Get_Canvas (Data.Browser),
              Force => False,
              Vertical_Layout =>
                Get_Pref (Get_Kernel (Data.Browser),
                          Browsers_Vertical_Layout));
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
         Link   : Glide_Browser_Link;
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
               Refresh (Data.Browser, Child);

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
                     Link := new Glide_Browser_Link_Record;
                     Add_Link
                       (Get_Canvas (Data.Browser), Link => Link,
                        Src => Child, Dest => Data.Item);
                  end if;
                  Refresh (Data.Browser, Child);
               end if;
            end if;
         end if;
      end Add_Item;

      Tree : Scope_Tree;
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

            Next (Get_Kernel (Data.Browser), Data.Iter.all);
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
      Link          : Glide_Browser_Link;
      Is_Renaming   : Boolean;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Item := Add_Entity_If_Not_Present (Browser, Entity);
      Item.From_Parsed := True;
      Refresh (Browser, Item);

      --  For efficiency, do not recompute the layout for each item
      Set_Auto_Layout (Get_Canvas (Browser), False);

      --  If we have a renaming, add the entry for the renamed entity
      Renaming_Of (Kernel, Entity, Is_Renaming, Rename);
      if Is_Renaming and then Rename /= No_Entity_Information then
         Child := Add_Entity_If_Not_Present (Browser, Rename);
         if not Has_Link (Get_Canvas (Browser), Item, Child) then
            Link := new Renaming_Link_Record;
            Add_Link
              (Get_Canvas (Browser), Link => Link,
               Src => Item, Dest => Child, Arrow => Both_Arrow);
         end if;
         Refresh (Browser, Child);
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
      Find_All_References (Kernel, Entity, Data.Iter.all, LI_Once => True);

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
            Directory_Information (C) & File_Information (C),
            Line   => Line_Information (C),
            Column => Column_Information (C));
      else
         Open_File_Editor
           (Get_Kernel (Context),
            Filename => Get_File (Location),
            Line     => Get_Line (Location),
            Column   => Get_Column (Location));
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
            Column => Get_Declaration_Column_Of (Entity));
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
         Location := Get_Location (Get (D.Iter.all));
         Print_Ref
           (D.Kernel, Get_File (Location),
            Get_Line (Location), Get_Column (Location),
            Get_Name (D.Entity),
            -"References for: " & Get_Name (D.Entity));
         Next (D.Kernel, D.Iter.all);

         return True;
      end if;
   end Find_Next_Reference;

   -----------------------------------------
   -- Find_All_References_From_Contextual --
   -----------------------------------------

   procedure Find_All_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Data     : Entity_Idle_Data;
      Info     : Entity_Information;
      Idle_Id  : Idle_Handler_Id;

   begin
      Push_State (Get_Kernel (Entity), Busy);
      Info := Get_Entity (Entity);

      if Info /= No_Entity_Information then
         begin
            Remove_Result_Category
              (Get_Kernel (Entity), -"References for: " & Get_Name (Info));

            Print_Ref (Get_Kernel (Entity),
                       Get_Declaration_File_Of (Info),
                       Get_Declaration_Line_Of (Info),
                       Get_Declaration_Column_Of (Info),
                       Get_Name (Info),
                       -"References for: " & Get_Name (Info));

            Data := (Kernel => Get_Kernel (Entity),
                     Iter   => new Entity_Reference_Iterator,
                     Entity => Copy (Info));

            Find_All_References (Get_Kernel (Entity), Info, Data.Iter.all);

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
         Pop_State (Get_Kernel (Entity));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         Pop_State (Get_Kernel (Entity));
   end Find_All_References_From_Contextual;

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

   begin
      Push_State (Get_Kernel (Entity), Busy);
      Info := Get_Entity (Entity);

      if Info /= No_Entity_Information then
         --  Print the declaration of the entity, but only if it is in the
         --  current file. Otherwise, this is too surprising for the use

         Remove_Result_Category
           (Get_Kernel (Entity), -"References for: " & Get_Name (Info));

         if Get_Declaration_File_Of (Info) = File_Information (Entity) then
            Print_Ref (Get_Kernel (Entity),
                       Get_Declaration_File_Of (Info),
                       Get_Declaration_Line_Of (Info),
                       Get_Declaration_Column_Of (Info),
                       Get_Name (Info),
                       -"References for: " & Get_Name (Info));
         end if;

         Find_All_References
           (Get_Kernel (Entity), Info, Iter,
            In_File => File_Information (Entity));

         while Get (Iter) /= No_Reference loop
            Location := Get_Location (Get (Iter));
            Print_Ref
              (Get_Kernel (Entity), Get_File (Location),
               Get_Line (Location), Get_Column (Location),
               Get_Name (Info),
               -"References for: " & Get_Name (Info));

            Next (Get_Kernel (Entity), Iter);
         end loop;

         Destroy (Iter);
      end if;

      Pop_State (Get_Kernel (Entity));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         Destroy (Iter);
         Pop_State (Get_Kernel (Entity));
   end Find_All_Local_References_From_Contextual;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access Entity_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         --  Should we display the ancestors ?
         if Gint (Get_X (Event)) < Get_Coord (Item).Width / 2 then
            Examine_Ancestors_Call_Graph
              (Get_Kernel (Item.Browser), Item.Entity);
            Item.From_Parsed := True;

         --  Or the subprograms we are calling ?
         else
            Examine_Entity_Call_Graph (Get_Kernel (Item.Browser), Item.Entity);
            Item.To_Parsed := True;
         end if;

         --  Make sure that the item we clicked on is still visible
         Show_Item (Get_Canvas (Item.Browser), Item);

      elsif Get_Event_Type (Event) = Button_Press then
         Select_Item (Item.Browser, Item, True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Button_Click;

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
                        & (-" calls..."));
               Append (Submenu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Edit_Entity_Call_Graph_From_Contextual'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => Entity_Name_Information (Entity_Context)
                        & (-" is called by..."));
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
         end if;
      end if;
   end Call_Graph_Contextual_Menu;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access Entity_Item_Record;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access
   is
      pragma Unreferenced (Event);
      Context : constant Selection_Context_Access :=
        new Entity_Selection_Context;
      Mitem : Gtk_Menu_Item;
   begin
      if Get_Declaration_File_Of (Item.Entity) /= ":" then
         declare
            Filename : constant String := Find_Source_File
              (Kernel                 => Get_Kernel (Browser),
               Short_File_Name        => Get_Declaration_File_Of (Item.Entity),
               Use_Predefined_Source_Path => True);
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
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Entity_Call_Graph_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Item.To_Parsed);

         Gtk_New (Mitem, Get_Name (Item.Entity) & (-" is called by..."));
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Ancestors_Call_Graph_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Item.From_Parsed);

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
      pragma Unreferenced (Widget);
      Child       : MDI_Child;
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
   begin
      if Selected_Item (Browser) = null then
         return null;
      end if;

      return Contextual_Factory
        (Item    => Glide_Browser_Item (Selected_Item (Browser)),
         Browser => Browser,
         Event   => null,
         Menu    => null);
   end Default_Factory;

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

   -----------
   -- Reset --
   -----------

   procedure Reset (Browser : access Glide_Browser_Record'Class;
                    Item : access Entity_Item_Record)
   is
      pragma Unreferenced (Browser);
   begin
      Item.To_Parsed := False;
      Item.From_Parsed := False;
   end Reset;

   ---------------
   -- Draw_Link --
   ---------------

   procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Renaming_Link_Record;
      Window      : Gdk.Window.Gdk_Window;
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
        (Canvas, Glide_Browser_Link_Record (Link.all)'Access,
         Window, Invert_Mode, GC, Edge_Number);

      Set_Line_Attributes
        (GC,
         Line_Width => 0,
         Line_Style => Line_Solid,
         Cap_Style  => Cap_Butt,
         Join_Style => Join_Miter);
   end Draw_Link;

end Browsers.Call_Graph;
