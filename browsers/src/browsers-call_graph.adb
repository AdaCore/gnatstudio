-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Glib.Object;      use Glib.Object;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Main;         use Gtk.Main;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Stock;        use Gtk.Stock;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Gtkada.MDI;       use Gtkada.MDI;

with Src_Info;                 use Src_Info;
with Src_Info.Queries;         use Src_Info.Queries;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with String_Utils;             use String_Utils;

with Glide_Intl;       use Glide_Intl;
with Browsers.Canvas;  use Browsers.Canvas;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Language;         use Language;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Traces;           use Traces;

package body Browsers.Call_Graph is

   Me : Debug_Handle := Create ("Browsers.Call_Graph");

   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   Margin : constant := 2;

   type Entity_Idle_Data is record
      Kernel : Kernel_Handle;
      Iter   : Entity_Reference_Iterator_Access;
      Decl   : E_Declaration_Info;
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
      Lib_Info      : LI_File_Ptr;
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

   procedure Edit_Source_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Open an editor for the entity described in Context.

   procedure Find_All_References_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  List all the references to the entity

   function Find_Next_Reference (D : Entity_Idle_Data)
      return Boolean;
   --  Find the next reference to the entity in D.

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Node : Scope_Tree_Node) return Entity_Item;
   --  Add a new entity to the browser, if not already there.

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Idle_Data);
   --  Called when the idle loop is destroyed.

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information) is
   begin
      Item := new Entity_Item_Record;
      Initialize (Item, Browser, Entity);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information)
   is
      B : Call_Graph_Browser := Call_Graph_Browser (Browser);
      Font : Gdk_Font;
      Width, Height : Gint;
   begin
      Item.Entity   := Copy (Entity);
      Item.Browser  := Glide_Browser (Browser);

      Font := Get_Text_Font (Browser);

      Width  := String_Width (Font, Get_Name (Entity)) + 4 * Margin
        + Get_Width (B.Left_Arrow) + Get_Width (B.Right_Arrow);
      Height := (Get_Ascent (Font) + Get_Descent (Font));
      Height := Gint'Max (Height, Get_Height (B.Left_Arrow));
      Height := Height + 2 * Margin;

      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Browser), Width, Height);

      --  Set_Orthogonal_Links (Get_Canvas (Browser), True);

      Refresh (Browser, Item);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Entity_Item_Record) is
   begin
      Destroy (Item.Entity);
   end Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Item    : access Entity_Item_Record)
   is
      B : Call_Graph_Browser := Call_Graph_Browser (Browser);
      Font : Gdk_Font := Get_Text_Font (Browser);
   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Font  => Font,
         GC    => Get_Text_GC (Browser),
         X     => Margin + Get_Width (B.Left_Arrow),
         Y     => Margin + Get_Ascent (Font),
         Text  => Get_Name (Item.Entity));

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
      B : Call_Graph_Browser := Call_Graph_Browser (Browser);
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

   -------------------------------
   -- Add_Entity_If_Not_Present --
   -------------------------------

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Node : Scope_Tree_Node) return Entity_Item
   is
      Entity : Entity_Information;
      Child  : Entity_Item;
   begin
      Entity := Get_Entity (Node);
      Child := Entity_Item (Find_Entity (Browser, Entity));
      if Child = null then
         Gtk_New (Child, Browser, Entity => Entity);
         Put (Get_Canvas (Browser), Child);
      end if;

      Destroy (Entity);
      return Child;
   end Add_Entity_If_Not_Present;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel   : access Kernel_Handle_Record'Class;
      Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information)
   is
      Iter : Scope_Tree_Node_Iterator;
      Item, Child : Entity_Item;
      Child_Browser : MDI_Child;
      Browser : Call_Graph_Browser;
      Link : Glide_Browser_Link;
      Tree : Scope_Tree;
      Node : Scope_Tree_Node;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Tree := Create_Tree (Lib_Info);
      if Tree = Null_Scope_Tree then
         Trace (Me, "Couldn't create scope tree for "
                & Get_LI_Filename (Lib_Info));
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      Node := Find_Entity_Scope (Tree, Entity);

      if Node = Null_Scope_Tree_Node then
         Insert (Kernel,
                 -"Couldn't find the call graph for "
                 & Get_Name (Entity));
         Trace (Me, "Couldn't find entity "
                & Get_Name (Entity) & " in "
                & Get_LI_Filename (Lib_Info)
                & " at line" & Get_Declaration_Line_Of (Entity)'Img
                & " column" & Get_Declaration_Column_Of (Entity)'Img);
         Free (Tree);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      --  Create the browser if necessary
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  For efficiency, do not recompute the layout for each item
      Set_Auto_Layout (Get_Canvas (Browser), False);

      Item := Add_Entity_If_Not_Present (Browser, Node);
      Item.To_Parsed := True;
      Refresh (Browser, Item);

      Iter := Start (Node);
      while Get (Iter) /= Null_Scope_Tree_Node loop
         if Is_Subprogram (Get (Iter)) then
            Child := Add_Entity_If_Not_Present (Browser, Get (Iter));
            if not Has_Link (Get_Canvas (Browser), Item, Child) then
               Link := new Glide_Browser_Link_Record;
               Add_Link (Get_Canvas (Browser),
                         Link => Link,
                         Src => Item,
                         Dest => Child);
            end if;
         end if;

         Next (Iter);
      end loop;

      Set_Auto_Layout (Get_Canvas (Browser), True);
      Layout (Get_Canvas (Browser),
              Force => False,
              Vertical_Layout => Get_Pref (Kernel, Browsers_Vertical_Layout));
      Refresh_Canvas (Get_Canvas (Browser));

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
   begin
      Data.Browser.Idle_Id := 0;
      Destroy (Data.Iter);
      Destroy (Data.Entity);
      Pop_State (Get_Kernel (Data.Browser));
   end Destroy_Idle;

   ---------------------------------------
   -- Examine_Ancestors_Call_Graph_Idle --
   ---------------------------------------

   function Examine_Ancestors_Call_Graph_Idle
     (Data : Examine_Ancestors_Idle_Data) return Boolean
   is
      procedure Add_Item (Node : Scope_Tree_Node);
      --  Add a new item for the entity declared in Node to the browser

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Node : Scope_Tree_Node) is
         Child : Entity_Item;
         Link : Glide_Browser_Link;
      begin
         if Is_Subprogram (Node)
           and then Get_Parent (Node) /= Null_Scope_Tree_Node
         then
            Child := Add_Entity_If_Not_Present
              (Data.Browser, Get_Parent (Node));

            if not Has_Link (Get_Canvas (Data.Browser), Child, Data.Item) then
               Link := new Glide_Browser_Link_Record;
               Add_Link
                 (Get_Canvas (Data.Browser), Link => Link,
                  Src => Child, Dest => Data.Item);
            end if;
         end if;
      end Add_Item;

      Tree : Scope_Tree;
   begin
      if Get (Data.Iter.all) = No_Reference then
         Set_Auto_Layout (Get_Canvas (Data.Browser), True);
         Layout (Get_Canvas (Data.Browser),
                 Force => False,
                 Vertical_Layout =>
                   Get_Pref (Get_Kernel (Data.Browser),
                             Browsers_Vertical_Layout));
         Refresh_Canvas (Get_Canvas (Data.Browser));
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
               Trace (Me, "Unexpected exception: "
                      & Exception_Information (E));
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
      Child_Browser : MDI_Child;
      Data          : Examine_Ancestors_Idle_Data;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Item := Entity_Item (Find_Entity (Browser, Entity));
      if Item = null then
         Gtk_New (Item, Browser,  Entity => Entity);
         Put (Get_Canvas (Browser), Item);
      end if;

      Item.From_Parsed := True;
      Refresh (Browser, Item);

      --  For efficiency, do not recompute the layout for each item
      Set_Auto_Layout (Get_Canvas (Browser), False);

      --  Look for all the parents.

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

      --  All memory is freed at the end of Examine_Ancestors_Call_Graph_Idle

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Examine_Ancestors_Call_Graph;

   ---------------------------------
   -- Edit_Source_From_Contextual --
   ---------------------------------

   procedure Edit_Source_From_Contextual
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      C : Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
   begin
      Open_File_Editor
        (Get_Kernel (Context),
         Directory_Information (C) & File_Information (C),
         Line   => Line_Information (C),
         Column => Column_Information (C));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Edit_Source_From_Contextual;

   --------------------------------------------
   -- Edit_Entity_Call_Graph_From_Contextual --
   --------------------------------------------

   procedure Edit_Entity_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity      : Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Lib_Info    : LI_File_Ptr;
      Decl        : E_Declaration_Info;
      Node_Entity : Entity_Information;

   begin
      Push_State (Get_Kernel (Entity), Busy);
      Decl := Get_Declaration (Entity);

      if Decl /= No_Declaration_Info then
         --  ??? Should check that Decl.Kind is a subprogram

         Lib_Info := Locate_From_Source_And_Complete
           (Get_Kernel (Entity), Get_File (Get_Location (Decl)));

         if Lib_Info /= No_LI_File then
            Node_Entity := Get_Entity (Decl);
            Examine_Entity_Call_Graph
              (Get_Kernel (Entity), Lib_Info, Node_Entity);
            Destroy (Node_Entity);
         end if;

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

      Entity   : Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Decl     : E_Declaration_Info;
      Info : Entity_Information;

   begin
      Push_State (Get_Kernel (Entity), Busy);
      Decl := Get_Declaration (Entity);

      Info := Get_Entity (Decl);
      Examine_Ancestors_Call_Graph (Get_Kernel (Entity), Info);
      Destroy (Info);

      Pop_State (Get_Kernel (Entity));

   exception
      when E : others =>
         Insert (Get_Kernel (Entity),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Entity),
                 Mode => Error);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Destroy (Info);
         Pop_State (Get_Kernel (Entity));
   end Edit_Ancestors_Call_Graph_From_Contextual;

   -------------------------
   -- Find_Next_Reference --
   -------------------------

   function Find_Next_Reference (D : Entity_Idle_Data)
      return Boolean
   is
      It : Entity_Reference_Iterator_Access;
   begin
      if Get (D.Iter.all) = No_Reference then
         It := D.Iter;
         Destroy (It);
         Pop_State (D.Kernel);
         return False;

      else
         Insert (D.Kernel,
                 Get_File (Get_Location (Get (D.Iter.all)))
                 & ':'
                 & Image (Get_Line (Get_Location (Get (D.Iter.all))))
                 & ':'
                 & Image (Get_Column (Get_Location (Get (D.Iter.all))))
                 & ' '
                 & Get_Entity_Name (D.Decl));
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
      Entity   : Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Decl     : E_Declaration_Info;
      Data     : Entity_Idle_Data;
      Info     : Entity_Information;
      Idle_Id  : Idle_Handler_Id;
   begin
      Push_State (Get_Kernel (Entity), Busy);
      Decl := Get_Declaration (Entity);

      if Decl /= No_Declaration_Info then
         begin
            Insert (Get_Kernel (Entity),
                    Get_File (Get_Location (Decl))
                    & ':'
                    & Image (Get_Line (Get_Location (Decl)))
                    & ':'
                    & Image (Get_Column (Get_Location (Decl)))
                    & ' '
                    & Get_Entity_Name (Decl));

            Data := (Kernel => Get_Kernel (Entity),
                     Iter   => new Entity_Reference_Iterator,
                     Decl   => Decl);

            Info := Get_Entity (Decl);
            Find_All_References (Get_Kernel (Entity), Info, Data.Iter.all);
            Destroy (Info);

            Idle_Id := Entity_Iterator_Idle.Add
              (Find_Next_Reference'Access, Data, Priority_Low_Idle);
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

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access Entity_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button)
   is
      LI : LI_File_Ptr;
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
            LI := Locate_From_Source_And_Complete
              (Get_Kernel (Item.Browser),
               Get_Declaration_File_Of (Item.Entity));

            if LI /= No_LI_File then
               Examine_Entity_Call_Graph
                 (Get_Kernel (Item.Browser), LI, Item.Entity);
            end if;
            Item.To_Parsed := True;
         end if;

         if LI = No_LI_File then
            Trace (Me, "LI file not found for "
                   & Get_Declaration_File_Of (Item.Entity));
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
      Item           : Gtk_Menu_Item;
      Entity_Context : Entity_Selection_Context_Access;
   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity_Context := Entity_Selection_Context_Access (Context);

         if (Has_Category_Information (Entity_Context)
             and then Category_Information (Entity_Context)
             in Subprogram_Category)
           or else not Has_Category_Information (Entity_Context)
         then
            Gtk_New (Item, Label => Entity_Name_Information (Entity_Context)
                     & " calls...");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Edit_Entity_Call_Graph_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => Entity_Name_Information (Entity_Context)
                     & " is called by...");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Edit_Ancestors_Call_Graph_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => (-"Find all references to ") &
                     Entity_Name_Information (Entity_Context));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
                 (Find_All_References_From_Contextual'Access),
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
      Context : Selection_Context_Access := new Entity_Selection_Context;
      Filename : constant String := Find_Source_File
        (Kernel                     => Get_Kernel (Browser),
         Short_File_Name            => Get_Declaration_File_Of (Item.Entity),
         Use_Predefined_Source_Path => True);
      Mitem : Gtk_Menu_Item;
   begin
      Set_File_Name_Information
        (File_Name_Selection_Context_Access (Context),
         Directory    => Dir_Name (Filename),
         File_Name    => Base_Name (Filename));
      Set_Entity_Information
        (Entity_Selection_Context_Access (Context),
         Entity_Name => Get_Name (Item.Entity),
         Line        => Get_Declaration_Line_Of (Item.Entity),
         Column      => Get_Declaration_Column_Of (Item.Entity),
         Category    => Language.Cat_Procedure);

      Gtk_New (Mitem, -"Edit source");
      Append (Menu, Mitem);
      Context_Callback.Connect
        (Mitem, "activate",
         Context_Callback.To_Marshaller
         (Edit_Source_From_Contextual'Access),
         Selection_Context_Access (Context));

      return Context;
   end Contextual_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Call_Graph_Module_Id := Register_Module
        (Kernel                  => Kernel,
         Module_Name             => Call_Graph_Module_Name,
         Priority                => Default_Priority,
         Initializer             => null,
         Contextual_Menu_Handler => Call_Graph_Contextual_Menu'Access);
   end Register_Module;

end Browsers.Call_Graph;
