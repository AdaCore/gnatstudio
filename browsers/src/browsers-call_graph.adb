-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtkada.MDI;       use Gtkada.MDI;

with Src_Info;         use Src_Info;
with Src_Info.Queries; use Src_Info.Queries;
with Glide_Kernel;     use Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Intl;       use Glide_Intl;
with Browsers.Canvas;  use Browsers.Canvas;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Language;         use Language;

with Traces;           use Traces;

package body Browsers.Call_Graph is

   Me : Debug_Handle := Create ("Browsers.Call_Graph");

   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   Default_Browser_Width  : constant := 400;
   Default_Browser_Height : constant := 400;
   --  <preference> Default size for the browsers

   Margin : constant := 2;

   Vertical_Layout : Boolean := True;
   --  <preference> Should the layout of the graph be vertical or horizontal ?

   procedure Call_Graph_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries into contextual menus

   procedure Examine_Entity_Call_Graph
     (Kernel        : access Kernel_Handle_Record'Class;
      Lib_Info      : LI_File_Ptr;
      Entity_Name   : String;
      Entity_Line   : Positive;
      Entity_Column : Natural);
   --  Display the call graph for the node.

   procedure Edit_Entity_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Show the whole call graph for the Entity described in Context.

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

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information) is
   begin
      Item := new Entity_Item_Record;
      Initialize (Item, Browser, Kernel, Entity);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Entity  : Src_Info.Queries.Entity_Information)
   is
      Font : Gdk_Font;
      Width, Height : Gint;
   begin
      Item.Entity := Entity;
      Item.Browser     := Glide_Browser (Browser);

      Font := Get_Text_Font (Browser);

      Width  := String_Width (Font, Get_Name (Entity)) + 4 * Margin;
      Height := (Get_Ascent (Font) + Get_Descent (Font)) + 2 * Margin;

      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Browser), Width, Height);

      Set_Orthogonal_Links (Get_Canvas (Browser), True);

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
      Font : Gdk_Font := Get_Text_Font (Browser);
   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Font  => Font,
         GC    => Get_Text_GC (Browser),
         X     => Margin,
         Y     => Margin + Get_Ascent (Font),
         Text  => Get_Name (Item.Entity));
   end Refresh;

   -------------------------------
   -- Create_Call_Graph_Browser --
   -------------------------------

   function Create_Call_Graph_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
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
      Set_Size_Request
        (Browser, Default_Browser_Width, Default_Browser_Height);
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
         Info : Entity_Information;
      begin
         if Item.all in Entity_Item_Record'Class then
            Info := Entity_Item (Item).Entity;
            --  ??? Should we check the file name as well
            if Get_Name (Info) = Get_Name (Entity)
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
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel        : access Kernel_Handle_Record'Class;
      Lib_Info      : LI_File_Ptr;
      Entity_Name   : String;
      Entity_Line   : Positive;
      Entity_Column : Natural)
   is
      Iter : Scope_Tree_Node_Iterator;
      Item, Child : Entity_Item;
      Child_Browser : MDI_Child;
      Browser : Call_Graph_Browser;
      Link : Glide_Browser_Link;
      Entity : Entity_Information;
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

      Node := Find_Entity_Declaration
        (Tree, Entity_Name, Entity_Line, Entity_Column);

      if Node = Null_Scope_Tree_Node then
         Trace (Me, "Couldn't find entity "
                & Entity_Name & " in "
                & Get_LI_Filename (Lib_Info)
                & " at line" & Entity_Line'Img
                & " column" & Entity_Column'Img);
         Free (Tree);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  For efficiency, do not recompute the layout for each item
      Set_Auto_Layout (Get_Canvas (Browser), False);

      Entity := Get_Entity (Tree, Node);
      Item := Entity_Item (Find_Entity (Browser, Entity));
      if Item = null then
         Gtk_New (Item, Browser,  Kernel, Entity => Entity);
         Put (Get_Canvas (Browser), Item);
      else
         Destroy (Entity);
      end if;

      Iter := Start (Node);
      while Get (Iter) /= Null_Scope_Tree_Node loop
         if Is_Subprogram (Get (Iter)) then
            Entity := Get_Entity (Tree, Get (Iter));
            Child := Entity_Item (Find_Entity (Browser, Entity));
            if Child = null then
               Gtk_New (Child, Browser,  Kernel, Entity => Entity);
               Put (Get_Canvas (Browser), Child);
            else
               Destroy (Entity);
            end if;

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
              Vertical_Layout => Vertical_Layout);
      Refresh_Canvas (Get_Canvas (Browser));

      Pop_State (Kernel_Handle (Kernel));
      Free (Tree);
   end Examine_Entity_Call_Graph;

   --------------------------------------------
   -- Edit_Entity_Call_Graph_From_Contextual --
   --------------------------------------------

   procedure Edit_Entity_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Entity   : Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Lib_Info : LI_File_Ptr;
   begin
      Push_State (Get_Kernel (Entity), Busy);

      Lib_Info := Locate_From_Source_And_Complete
        (Get_Kernel (Entity), File_Information (Entity));

      if Lib_Info = No_LI_File then
         Trace (Me, "Edit_File_Call_Graph_From_Contextual: Couldn't find"
                & " LI file for " & File_Information (Entity));
         Pop_State (Get_Kernel (Entity));
         return;
      end if;

      Examine_Entity_Call_Graph
        (Get_Kernel (Entity),
         Lib_Info,
         Entity_Name_Information (Entity),
         79,  --  Line_Information (Entity),
         14); --  Column_Information (Entity));

      Pop_State (Get_Kernel (Entity));
   end Edit_Entity_Call_Graph_From_Contextual;

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
         Examine_Entity_Call_Graph
           (Get_Kernel (Item.Browser),
            Locate_From_Source_And_Complete
            (Get_Kernel (Item.Browser), Get_Declaration_File_Of (Item.Entity)),
            Get_Name (Item.Entity),
            Get_Declaration_Line_Of (Item.Entity),
            Get_Declaration_Column_Of (Item.Entity));

      elsif Get_Event_Type (Event) = Button_Press then
         Select_Item (Item.Browser, Item, True);
      end if;
   end On_Button_Click;

   --------------------------------
   -- Call_Graph_Contextual_Menu --
   --------------------------------

   procedure Call_Graph_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Item           : Gtk_Menu_Item;
      Entity_Context : Entity_Selection_Context_Access;
   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity_Context := Entity_Selection_Context_Access (Context);

         if Has_Category_Information (Entity_Context)
           and then Category_Information (Entity_Context)
             in Subprogram_Category
         then
            Gtk_New (Item, Label => (-"Examine call graph for ") &
                     Entity_Name_Information (Entity_Context));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Entity_Call_Graph_From_Contextual'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Call_Graph_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Call_Graph_Module_Id := Register_Module
        (Module_Name             => Call_Graph_Module_Name,
         Priority                => Default_Priority,
         Initializer             => null,
         Contextual_Menu_Handler => Call_Graph_Contextual_Menu'Access);
   end Register_Module;

end Browsers.Call_Graph;
