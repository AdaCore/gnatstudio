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

   procedure Edit_File_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Show the whole call graph for the file described in Context.

   function Find_Entity
     (In_Browser : access Glide_Browser_Record'Class; Entity_Name : String)
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
      Entity_Name : String) is
   begin
      Item := new Entity_Item_Record;
      Initialize (Item, Browser, Kernel, Entity_Name);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Entity_Name : String)
   is
      Font : Gdk_Font;
      Width, Height : Gint;
   begin
      Item.Entity_Name := new String' (Entity_Name);
      Item.Browser := Glide_Browser (Browser);

      Font := Get_Text_Font (Browser);

      Width  := String_Width (Font, Entity_Name) + 4 * Margin;
      Height := (Get_Ascent (Font) + Get_Descent (Font)) + 2 * Margin;

      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Browser), Width, Height);

      Refresh (Browser, Item);
   end Initialize;

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
         Text  => Item.Entity_Name.all);
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
     (In_Browser : access Glide_Browser_Record'Class; Entity_Name : String)
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
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         if Item.all in Entity_Item_Record'Class
           and then Entity_Item (Item).Entity_Name.all = Entity_Name
         then
            Found := Canvas_Item (Item);
            return False;
         end if;

         return True;
      end Check_Item;

   begin
      For_Each_Item (Get_Canvas (In_Browser), Check_Item'Unrestricted_Access);
      return Found;
   end Find_Entity;

   ------------------------------------------
   -- Edit_File_Call_Graph_From_Contextual --
   ------------------------------------------

   procedure Edit_File_Call_Graph_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File : File_Selection_Context_Access := File_Selection_Context_Access
        (Context);
      Lib_Info : LI_File_Ptr;
      Tree : Scope_Tree;
      Node : Scope_Tree_Node;
      Iter : Scope_Tree_Node_Iterator;
      Item, Child : Entity_Item;
      Child_Browser : MDI_Child;
      Browser : Call_Graph_Browser;
      Link : Glide_Browser_Link;

   begin
      Push_State (Get_Kernel (File), Busy);
      Lib_Info := Locate_From_Source_And_Complete
        (Get_Kernel (File), File_Information (File));

      if Lib_Info = No_LI_File then
         Trace (Me, "Edit_File_Call_Graph_From_Contextual: Couldn't find"
                & " LI file for " & File_Information (File));
         Pop_State (Get_Kernel (File));
         return;
      end if;

      Child_Browser := Open_Call_Graph_Browser (Get_Kernel (Context));
      Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      Tree := Create_Tree (Lib_Info);
      if Tree /= Null_Scope_Tree then
         Node := Find_Entity_Declaration (Tree, "Init_Settings", 79, 14);
         if Node /= Null_Scope_Tree_Node then

            Item := Entity_Item
              (Find_Entity (Browser, Get_Entity_Name (Node)));
            if Item = null then
               Gtk_New
                 (Item, Browser,  Get_Kernel (Context),
                  Entity_Name => Get_Entity_Name (Node));
               Put (Get_Canvas (Browser), Item);
            end if;

            Iter := Start (Node);
            while Get (Iter) /= Null_Scope_Tree_Node loop
               if Is_Subprogram (Get (Iter)) then
                  Child := Entity_Item
                    (Find_Entity (Browser, Get_Entity_Name (Get (Iter))));
                  if Child = null then
                     Gtk_New
                       (Child, Browser,  Get_Kernel (Context),
                        Entity_Name => Get_Entity_Name (Get (Iter)));
                     Put (Get_Canvas (Browser), Child);
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

            Layout (Get_Canvas (Browser),
                    Force => False,
                    Vertical_Layout => Vertical_Layout);
            Refresh_Canvas (Get_Canvas (Browser));

         else
            Trace (Me, "Couldn't find required entity in file "
                   & File_Information (File));
         end if;

         --  Trace_Dump (Me, Tree);
         Free (Tree);

      else
         Trace (Me, "Couldn't create scope tree for file "
                & File_Information (File));
      end if;

      Pop_State (Get_Kernel (File));
   end Edit_File_Call_Graph_From_Contextual;

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
         null;
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
      Item         : Gtk_Menu_Item;
      File_Context : File_Selection_Context_Access;
   begin
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);
         if Has_File_Information (File_Context) then
            Gtk_New (Item, Label => (-"Examine call graph for ") &
                     File_Information (File_Context));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_File_Call_Graph_From_Contextual'Access),
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
