-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2004                      --
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

with Glib;                 use Glib;
with Glib.Xml_Int;         use Glib.Xml_Int;
with Glib.Object;          use Glib.Object;
with Gdk.GC;               use Gdk.GC;
with Gdk.Event;            use Gdk.Event;
with Gtk.Image;            use Gtk.Image;
with Gtk.Image_Menu_Item;  use Gtk.Image_Menu_Item;
with Gtk.Main;             use Gtk.Main;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Object;           use Gtk.Object;
with Gtk.Widget;           use Gtk.Widget;
with Pango.Layout;         use Pango.Layout;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.MDI;           use Gtkada.MDI;

with Entities;                      use Entities;
with Entities.Queries;              use Entities.Queries;
with Glide_Kernel;                  use Glide_Kernel;
with Glide_Kernel.Modules;          use Glide_Kernel.Modules;
with Glide_Kernel.Console;          use Glide_Kernel.Console;
with Glide_Kernel.Contexts;         use Glide_Kernel.Contexts;
with Glide_Kernel.Preferences;      use Glide_Kernel.Preferences;
with Glide_Kernel.Task_Manager;     use Glide_Kernel.Task_Manager;
with Glide_Result_View;             use Glide_Result_View;
with Glide_Kernel.Standard_Hooks;   use Glide_Kernel.Standard_Hooks;
with Commands.Generic_Asynchronous; use Commands;
with String_Utils;                  use String_Utils;
with Browsers.Canvas;               use Browsers.Canvas;
with Glide_Kernel.Scripts;          use Glide_Kernel.Scripts;
with VFS;                           use VFS;

with Glide_Intl;       use Glide_Intl;
with Browsers.Canvas;  use Browsers.Canvas;

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Traces;           use Traces;

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

   Locations_At_A_Time : constant := 300;
   --  Number of locations that will be inserted in the locations view in
   --  each idle processing.

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
      Entity : Entities.Entity_Information;
      Refs   : Xref_List;
   end record;
   type Entity_Item is access all Entity_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Entity_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Entities.Entity_Information;
      May_Have_To_Dependencies : Boolean);
   --  Create a new entity item.
   --  If May_Have_To_Dependencies is False, the right arrow will not be
   --  displayed in the items.

   procedure Initialize
     (Item    : access Entity_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Entity  : Entities.Entity_Information;
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

   function Build
     (Kernel : access Kernel_Handle_Record'Class;
      Parent_Item : access Entity_Item_Record'Class;
      Location : File_Location) return Active_Area_Cb;
   --  Build a callback for links in callgraph items

   type Show_Location_Callback is new Active_Area_Callback with record
      Kernel : Kernel_Handle;
      Parent : Entity_Item;
      Location : File_Location;
   end record;
   type Show_Location_Callback_Access
     is access all Show_Location_Callback'Class;
   function Call (Callback : Show_Location_Callback;
                  Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  See inherated doc

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
      Kernel         : Kernel_Handle;
      Iter           : Entity_Reference_Iterator_Access;
      Entity         : Entity_Information;
      Include_Writes : Boolean;
      Include_Reads  : Boolean;
      Category       : String_Access;
   end record;

   type Examine_Callback is record
      --  The following three fields are only set for graphical callbacks
      Browser        : Call_Graph_Browser;
      Item           : Entity_Item;
      Link_From_Item : Boolean;
   end record;
   type Execute_Callback is access procedure
     (Cb     : Examine_Callback;
      Entity : Entity_Information;
      Ref    : Entity_Reference;
      Is_Renaming : Boolean);

   type Examine_Ancestors_Idle_Data is record
      Iter     : Entity_Reference_Iterator_Access;
      Entity   : Entity_Information;
      Kernel   : Kernel_Handle;
      Callback : Examine_Callback;
      Execute  : Execute_Callback;
   end record;
   package Examine_Ancestors_Idle is new Gtk.Main.Idle
     (Examine_Ancestors_Idle_Data);

   procedure Call_Graph_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries into contextual menus

   procedure Examine_Entity_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information);
   --  Display the call graph for the node.

   procedure Examine_Entity_Call_Graph_Iterator
     (Kernel   : access Kernel_Handle_Record'Class;
      Entity   : Entity_Information;
      Callback : Examine_Callback;
      Execute  : Execute_Callback);
   --  Same as Examine_Entity_Call_Graph, but calls Execute for each matching
   --  entity.

   procedure Examine_Ancestors_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information);
   --  Display the list of subprograms that call Entity.

   procedure Examine_Ancestors_Call_Graph_Iterator
     (Kernel          : access Kernel_Handle_Record'Class;
      Entity          : Entity_Information;
      Callback        : Examine_Callback;
      Execute         : Execute_Callback;
      Background_Mode : Boolean);
   --  Same as Examine_Ancestors_Call_Graph, and calls Callback for each
   --  matching entity. If Background_Mode is true, this is executed in an
   --  idle loop.
   --  If Callback.Browser is null, then Background_Mode is ignored, and the
   --  query is processed synchronously

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
     (Kernel : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Find, or create a new, call graph editor.

   function Create_Call_Graph_Browser
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Create a new call graph browser.

   procedure Edit_Spec_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Open an editor for the entity described in Context.

   procedure Edit_Body_From_Contextual
     (Widget  : access GObject_Record'Class;
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

   procedure Find_Next_Reference
     (Data    : in out Entity_Idle_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Find the next reference to the entity in D.

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Entity_Information) return Entity_Item;
   --  Add a new entity to the browser, if not already there.

   procedure Add_Entity_And_Link
     (Cb          : Examine_Callback;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Is_Renaming : Boolean);
   --  Add Entity, and possibly a link to Cb.Item to Cb.Browser

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Idle_Data);
   --  Called when the idle loop is destroyed.

   procedure Destroy_Idle (Data : in out Entity_Idle_Data);
   --  Called when the idle loop is destroyed.

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Entity_Idle_Data, Destroy_Idle);

   procedure On_Call_Graph
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Create a callgraph for the entity described in the current kernel
   --  context (if any)

   procedure On_Find_All_References
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Find all the references of the current entity.

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

   procedure Print_Ref
     (Kernel   : access Kernel_Handle_Record'Class;
      Location : File_Location;
      Name     : String;
      Category : String);
   --  Print a reference in the console, after looking for the directory
   --  containing File.
   --  Category corresponds to the purpose of the print. All references
   --  corresponding to the same category will be printed as a group.

   procedure Call_Graph_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
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
      Entity  : Entities.Entity_Information;
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
      Entity  : Entities.Entity_Information;
      May_Have_To_Dependencies : Boolean) is
   begin
      Item.Entity   := Entity;
      Ref (Item.Entity);
      Initialize (Item, Browser, Get_Name (Entity),
                  Examine_Ancestors_Call_Graph'Access,
                  Examine_Entity_Call_Graph'Access);
      Set_Children_Shown (Item, not May_Have_To_Dependencies);

      if not Is_Predefined_Entity (Entity) then
         Add_Line
           (Item.Refs,
            "(Decl) @"
            & Base_Name
              (Get_Filename (Get_File (Get_Declaration_Of (Item.Entity))))
            & ':' & Image (Get_Line (Get_Declaration_Of (Item.Entity))) & '@',
            Callback =>
              (1 => Build (Get_Kernel (Get_Browser (Item)),
                           Item,
                           Get_Declaration_Of (Item.Entity))));
      else
         Add_Line (Item.Refs, "<Unresolved>");
      end if;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Entity_Item_Record) is
      Item2 : constant Entity_Item := Item'Unrestricted_Access;
      Iter : Item_Iterator := Start (Get_Canvas (Get_Browser (Item2)));
      It : Canvas_Item;
      Line : Positive;
      Text : String_Access;
      Callback : Active_Area_Cb;
      Cb : Show_Location_Callback_Access;
      Removed : Boolean;
   begin
      if not Gtk.Object.In_Destruction_Is_Set (Get_Browser (Item2)) then
         --  Remove all references to the current item in other items, to keep
         --  the browser's contents as simple as possible.

         --  We have to iterate over all items, since the item being destroyed
         --  is no longer linked to anything at this point.

         loop
            It := Get (Iter);
            exit when It = null;

            Removed := False;

            --  Skip first line, which is the declaration location
            Line := 2;
            loop
               Get_Line (Entity_Item (It).Refs, Line, 1, Callback, Text);
               exit when Text = null;

               if Callback /= null then
                  Cb := Show_Location_Callback_Access (Callback);
                  if Cb.Parent = Item2 then
                     Remove_Line (Entity_Item (It).Refs, Line);
                     Removed := True;
                  else
                     Line := Line + 1;
                  end if;
               else
                  Line := Line + 1;
               end if;
            end loop;

            if Removed then
               Refresh (Entity_Item (It));
            end if;

            Next (Iter);
         end loop;
      end if;

      Unref (Item.Entity);
      Free (Item.Refs);
      Destroy (Arrow_Item_Record (Item));
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
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child
   is
      Browser : Call_Graph_Browser;
      Child   : MDI_Child;
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

      Child := Put
        (Kernel, Browser,
         Focus_Widget   => Gtk_Widget (Get_Canvas (Browser)),
         Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
         Default_Height => Get_Pref (Kernel, Default_Widget_Height),
         Module => Call_Graph_Module_Id);
      Set_Title (Child, -"Call graph Browser");

      return Child;
   end Create_Call_Graph_Browser;

   -----------------------------
   -- Open_Call_Graph_Browser --
   -----------------------------

   function Open_Call_Graph_Browser
     (Kernel : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Call_Graph_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Child := Create_Call_Graph_Browser (Kernel);
         Set_Focus_Child (Child);
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
           or else Entity_Item (Found).Entity = Entity;
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
      Iter   : Calls_Iterator;

   begin
      Child := Entity_Item (Find_Entity (Browser, Entity));
      if Child = null then

         if Automatically_Check_To_Dependencies then
            Iter := Get_All_Called_Entities (Entity);
            if not At_End (Iter) then
               May_Have_To_Dependencies := False;

               while not At_End (Iter) loop
                  if Is_Subprogram (Get (Iter)) then
                     May_Have_To_Dependencies := True;
                     exit;
                  end if;
                  Next (Iter);
               end loop;

               Destroy (Iter);
            end if;
         end if;

         Gtk_New (Child, Browser, Entity, May_Have_To_Dependencies);
         Put (Get_Canvas (Browser), Child);
         Refresh (Child);
      end if;

      return Child;
   end Add_Entity_If_Not_Present;

   ----------------------------------------
   -- Examine_Entity_Call_Graph_Iterator --
   ----------------------------------------

   procedure Examine_Entity_Call_Graph_Iterator
     (Kernel   : access Kernel_Handle_Record'Class;
      Entity   : Entity_Information;
      Callback : Examine_Callback;
      Execute  : Execute_Callback)
   is
      Rename      : Entity_Information;
      Iter : Calls_Iterator;
      Refs : Entity_Reference_Iterator;
      Ref  : Entity_Reference;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  If we have a renaming, add the entry for the renamed entity
      Rename := Renaming_Of (Entity);
      if Rename /= null then
         Execute (Callback, Rename, No_Entity_Reference, Is_Renaming => True);
      else
         Iter := Get_All_Called_Entities (Entity);
         while not At_End (Iter) loop
            Rename := Get (Iter);

            if Rename = null then
               Trace (Me, "Error : null returned by Get_All_Called_Entities");

            elsif Is_Subprogram (Rename) then
               Find_All_References
                 (Iter     => Refs,
                  Entity   => Rename,
                  In_Scope => Entity);

               while not At_End (Refs) loop
                  Ref := Get (Refs);
                  if Get_Kind (Ref) /= Label
                    and then Get_Kind (Ref) /= Body_Entity
                    and then Get_Caller (Ref) = Entity
                    and then Get_Declaration_Of (Rename) /= Get_Location (Ref)
                  then
                     Execute (Callback, Rename, Ref, Is_Renaming => False);
                  end if;

                  Next (Refs);
               end loop;

               Destroy (Refs);
            end if;

            Next (Iter);
         end loop;

         Destroy (Iter);
      end if;

      Pop_State (Kernel_Handle (Kernel));

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Entity_Call_Graph_Iterator;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information)
   is
      Child_Browser : MDI_Child;
      Cb            : Examine_Callback;
   begin
      --  Create the browser if it doesn't exist
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Cb.Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Cb.Item := Add_Entity_If_Not_Present (Cb.Browser, Entity);

      Cb.Link_From_Item := True;

      if not Children_Shown (Cb.Item) then
         Set_Children_Shown (Cb.Item, True);
         Examine_Entity_Call_Graph_Iterator
           (Kernel, Entity, Cb, Add_Entity_And_Link'Access);

         --  Refresh all linked items, since we have added references in them
         Refresh_Linked_Items (Cb.Item, Refresh_Children => True);
      end if;

      --  We need to do a layout in all cases, so that the newly added item
      --  is put at a correct place.
      Layout (Cb.Browser, Force => False);
      Refresh_Canvas (Get_Canvas (Cb.Browser));
      Show_Item (Get_Canvas (Cb.Browser), Cb.Item);

      Redraw_Title_Bar (Cb.Item);

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Entity_Call_Graph;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Idle_Data) is
      procedure Clean;
      --  Clean up before exiting Destroy_Idle

      procedure Clean is
      begin
         Destroy (Data.Iter);
         Unref (Data.Entity);

         if Data.Callback.Browser /= null then
            Data.Callback.Browser.Idle_Id := 0;
            Pop_State (Get_Kernel (Data.Callback.Browser));
         end if;
      end Clean;

   begin
      if Data.Callback.Browser /= null then
         --  Refresh the item, since we might have added links to it
         Refresh (Data.Callback.Item);

         Layout (Data.Callback.Browser, Force => False);
         Refresh_Canvas (Get_Canvas (Data.Callback.Browser));
         Show_Item (Get_Canvas (Data.Callback.Browser), Data.Callback.Item);
      end if;
      Clean;

   exception
      when E : others =>
         Clean;
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Destroy_Idle;

   ---------------------------------------
   -- Examine_Ancestors_Call_Graph_Idle --
   ---------------------------------------

   function Examine_Ancestors_Call_Graph_Idle
     (Data : Examine_Ancestors_Idle_Data) return Boolean
   is
      procedure Add_Item (Refs : Entity_Reference);
      --  Add a new item for the entity declared in Node to the browser

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Refs : Entity_Reference) is
         Parent : constant Entity_Information := Get_Caller (Refs);
      begin
         if Parent /= null
           and then Get_Kind (Refs) /= Declaration
           and then Get_Kind (Refs) /= Body_Entity
           and then Get_Kind (Refs) /= Completion_Of_Private_Or_Incomplete_Type
         then
            Data.Execute (Data.Callback, Parent, Refs, False);
         end if;
      end Add_Item;

   begin
      if At_End (Data.Iter.all) then
         return False;

      else
         Add_Item (Get (Data.Iter.all));
         Next (Data.Iter.all);
         return True;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return False;
   end Examine_Ancestors_Call_Graph_Idle;

   -------------------------------------------
   -- Examine_Ancestors_Call_Graph_Iterator --
   -------------------------------------------

   procedure Examine_Ancestors_Call_Graph_Iterator
     (Kernel          : access Kernel_Handle_Record'Class;
      Entity          : Entity_Information;
      Callback        : Examine_Callback;
      Execute         : Execute_Callback;
      Background_Mode : Boolean)
   is
      Data          : Examine_Ancestors_Idle_Data;
      Rename        : Entity_Information;
   begin
      --  If we have a renaming, add the entry for the renamed entity
      Rename := Renaming_Of (Entity);
      if Rename /= null then
         Execute (Callback, Rename, No_Entity_Reference, Is_Renaming => False);
      end if;

      Ref (Entity);
      Data := (Iter     => new Entity_Reference_Iterator,
               Entity   => Entity,
               Kernel   => Kernel_Handle (Kernel),
               Callback => Callback,
               Execute  => Execute);
      Find_All_References (Iter => Data.Iter.all, Entity => Entity);

      if Background_Mode and then Callback.Browser /= null then
         Callback.Browser.Idle_Id := Examine_Ancestors_Idle.Add
           (Cb       => Examine_Ancestors_Call_Graph_Idle'Access,
            D        => Data,
            Priority => Priority_Low_Idle,
            Destroy  => Destroy_Idle'Access);
      else
         while Examine_Ancestors_Call_Graph_Idle (Data) loop
            null;
         end loop;
         Destroy_Idle (Data);
      end if;
   end Examine_Ancestors_Call_Graph_Iterator;

   -------------------------
   -- Add_Entity_And_Link --
   -------------------------

   procedure Add_Entity_And_Link
     (Cb          : Examine_Callback;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Is_Renaming : Boolean)
   is
      Child : Entity_Item;
      Link  : Browser_Link;
      Loc   : File_Location;
      Line  : Natural;
      Text  : String_Access;
      New_Cb, Callback : Active_Area_Cb;
   begin
      Child := Add_Entity_If_Not_Present (Cb.Browser, Entity);

      if Cb.Link_From_Item then
         if not Has_Link (Get_Canvas (Cb.Browser), Cb.Item, Child) then
            if Is_Renaming then
               Link := new Renaming_Link_Record;
               Add_Link (Get_Canvas (Cb.Browser), Link => Link,
                         Src => Cb.Item, Dest => Child, Arrow => Both_Arrow);
            else
               Link := new Browser_Link_Record;
               Add_Link (Get_Canvas (Cb.Browser), Link => Link,
                         Src => Cb.Item, Dest => Child);
            end if;
         end if;
      else
         if not Has_Link (Get_Canvas (Cb.Browser), Child, Cb.Item) then
            if Is_Renaming then
               Link := new Renaming_Link_Record;
               Add_Link (Get_Canvas (Cb.Browser), Link => Link,
                         Src => Child, Dest => Cb.Item, Arrow => Both_Arrow);
            else
               Link := new Browser_Link_Record;
               Add_Link (Get_Canvas (Cb.Browser), Link => Link,
                         Src => Child, Dest => Cb.Item);
            end if;
         end if;
      end if;

      Loc := Get_Location (Ref);

      --  Always skip the first line, which is the declaration location
      Line := 2;

      if Cb.Link_From_Item then
         New_Cb := Build (Get_Kernel (Get_Browser (Child)), Cb.Item, Loc);

         loop
            Get_Line (Child.Refs, Line, 1, Callback, Text);
            exit when Text = null;

            if Show_Location_Callback_Access (Callback).Parent = Cb.Item then
               Expand_Line
                 (Child.Refs, Line,
                  " @" & Image (Get_Line (Loc))
                    & ':' & Image (Get_Column (Loc)) & '@',
                  (1 => New_Cb));
               return;
            end if;

            Line := Line + 1;
         end loop;

         Add_Line
           (Child.Refs,
            Get_Full_Name (Cb.Item.Entity)
            & ": @"
            & Image (Get_Line (Loc)) & ':' & Image (Get_Column (Loc)) & '@',
            Callback => (1 => New_Cb));

      else
         New_Cb := Build (Get_Kernel (Get_Browser (Child)), Child, Loc);

         loop
            Get_Line (Cb.Item.Refs, Line, 1, Callback, Text);
            exit when Text = null;

            if Show_Location_Callback_Access (Callback).Parent = Child then
               Expand_Line
                 (Cb.Item.Refs, Line,
                  " @" & Image (Get_Line (Loc))
                    & ':' & Image (Get_Column (Loc)) & '@',
                  (1 => New_Cb));
               return;
            end if;

            Line := Line + 1;
         end loop;

         Add_Line
           (Cb.Item.Refs,
            Get_Full_Name (Child.Entity)
            & ": @"
            & Image (Get_Line (Loc)) & ':' & Image (Get_Column (Loc)) & '@',
            Callback => (1 => New_Cb));
      end if;
   end Add_Entity_And_Link;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Entity_Information)
   is
      Child_Browser : MDI_Child;
      Cb            : Examine_Callback;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Call_Graph_Browser (Kernel);
      Cb.Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Cb.Item := Add_Entity_If_Not_Present (Cb.Browser, Entity);
      Set_Parents_Shown (Cb.Item, True);
      Redraw_Title_Bar (Cb.Item);

      Cb.Link_From_Item := False;

      Examine_Ancestors_Call_Graph_Iterator
        (Kernel, Entity, Cb, Add_Entity_And_Link'Access,
         Background_Mode => True);

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Edit_Body_From_Contextual --
   -------------------------------

   procedure Edit_Body_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      C : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Location : Entities.File_Location;

   begin
      Find_Next_Body
        (Entity   => Get_Entity (C),
         Location => Location);

      if Location /= Entities.No_File_Location then
         Open_File_Editor
           (Get_Kernel (Context),
            Filename => Get_Filename (Get_File (Location)),
            Line     => Get_Line (Location),
            Column   => Get_Column (Location));
      else
         --  If the body wasn't found then display the specs
         Open_File_Editor
           (Get_Kernel (Context),
            File_Information (C),
            Line   => Line_Information (C),
            Column => Entity_Column_Information (C));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Edit_Body_From_Contextual;

   -------------------------------
   -- Edit_Spec_From_Contextual --
   -------------------------------

   procedure Edit_Spec_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Entity : constant Entity_Information := Get_Entity (C);

   begin
      if Entity = null then
         Insert (Get_Kernel (Context),
                 (-"Couldn't find cross-reference information for ")
                 & '"' & Entity_Name_Information (C) & '"');
      else
         Open_File_Editor
           (Get_Kernel (Context),
            Get_Filename (Get_File (Get_Declaration_Of (Entity))),
            Line   => Get_Line (Get_Declaration_Of (Entity)),
            Column => Get_Column (Get_Declaration_Of (Entity)));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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

      if Node_Entity /= null then
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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
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

      if Info /= null then
         Examine_Ancestors_Call_Graph (Get_Kernel (Entity), Info);
      else
         Insert (Get_Kernel (Entity),
                 -"No information found for the file "
                   & Full_Name (File_Information (Entity)).all,
                 Mode => Error);
      end if;

      Pop_State (Get_Kernel (Entity));

   exception
      when E : others =>
         Insert (Get_Kernel (Entity),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Entity),
                 Mode => Error);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (Entity));
   end Edit_Ancestors_Call_Graph_From_Contextual;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Entity_Idle_Data) is
   begin
      Destroy (Data.Iter);
      Unref (Data.Entity);
      Free (Data.Category);
      Pop_State (Data.Kernel);
   end Destroy_Idle;

   ---------------
   -- Print_Ref --
   ---------------

   procedure Print_Ref
     (Kernel   : access Kernel_Handle_Record'Class;
      Location : File_Location;
      Name     : String;
      Category : String)
   is
      Col : Positive;
   begin
      if Get_Column (Location) > 0 then
         Col := Positive (Get_Column (Location));
      else
         Col := 1;
      end if;

      Insert_Result
        (Kernel,
         Category  => Category,
         File      => Get_Filename (Get_File (Location)),
         Text      => Name,
         Line      => Get_Line (Location),
         Column    => Col,
         Length    => Name'Length,
         Highlight => True,
         Highlight_Category => "Search Results",
         Remove_Duplicates => False,
         Enable_Counter    => False);
   end Print_Ref;

   -------------------------
   -- Find_Next_Reference --
   -------------------------

   procedure Find_Next_Reference
     (Data    : in out Entity_Idle_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Location : File_Location;
      Count    : Integer := 0;
   begin
      Result := Execute_Again;

      while Count < Locations_At_A_Time
        and then Result = Execute_Again
      loop
         if At_End (Data.Iter.all) then
            Recount_Category
              (Data.Kernel, Data.Category.all & Get_Name (Data.Entity));
            Result := Success;
         else
            if Get (Data.Iter.all) /= No_Entity_Reference then
               if (Data.Include_Writes
                   and then Is_Write_Reference
                     (Get_Kind (Get (Data.Iter.all))))
                 or else
                   (Data.Include_Reads
                    and then Is_Read_Reference
                      (Get_Kind (Get (Data.Iter.all))))
               then
                  Location := Get_Location (Get (Data.Iter.all));
                  Print_Ref
                    (Data.Kernel, Location,
                     Get_Name (Data.Entity),
                     Data.Category.all & Get_Name (Data.Entity));
               end if;
            end if;

            Next (Data.Iter.all);

            Set_Progress
              (Command,
               (Running,
                Get_Current_Progress (Data.Iter.all),
                Get_Total_Progress (Data.Iter.all)));
         end if;

         Count := Count + 1;
      end loop;
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
      Data : Entity_Idle_Data;
      C    : Xref_Commands.Generic_Asynchronous_Command_Access;

   begin
      if Info /= null then
         begin
            Remove_Result_Category
              (Kernel, Category_Title & Get_Name (Info));

            Ref (Info);
            Data := (Kernel         => Kernel_Handle (Kernel),
                     Iter           => new Entity_Reference_Iterator,
                     Include_Writes => Include_Writes,
                     Include_Reads  => Include_Reads,
                     Category       => new String'(Category_Title),
                     Entity         => Info);

            Find_All_References
              (Iter   => Data.Iter.all,
               Entity => Info);
            Xref_Commands.Create
              (C, -"Find all refs", Data, Find_Next_Reference'Access);
            Set_Progress (Command_Access (C),
                          (Running,
                           Get_Current_Progress (Data.Iter.all),
                           Get_Total_Progress (Data.Iter.all)));
            Launch_Background_Command
              (Kernel, Command_Access (C), True, True, "xrefs");

         exception
            when E : others =>
               Trace (Exception_Handle,
                      "Unexpected exception " & Exception_Information (E));
               Destroy (Data.Iter);
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
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

      if Info /= null then
         --  Print the declaration of the entity, but only if it is in the
         --  current file. Otherwise, this is too surprising for the use

         Remove_Result_Category
           (Kernel, -"References for: " & Get_Name (Info));

         if Get_Filename (Get_File (Get_Declaration_Of (Info))) =
           File_Information (Entity)
         then
            Print_Ref
              (Kernel,
               Get_Declaration_Of (Info),
               Get_Name (Info),
               -"References for: " & Get_Name (Info));
         end if;

         Find_All_References
           (Iter    => Iter,
            Entity  => Info,
            In_File => Get_Or_Create
              (Get_Database (Kernel), File_Information (Entity)));

         while not At_End (Iter) loop
            Location := Get_Location (Get (Iter));
            Print_Ref
              (Kernel,
               Location,
               Get_Name (Info),
               -"References for: " & Get_Name (Info));

            Next (Iter);
         end loop;

         Destroy (Iter);
      end if;

      Pop_State (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
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
            Push_State (Get_Kernel (Context), Busy);

            --  Check the entity right away. This will return False if either
            --  the entity isn't a subprogram, or we couldn't find the
            --  declaration. In both cases, we wouldn't be able to draw the
            --  call graph anyway.

            declare
               Name   : constant String :=
                 Krunch (Entity_Name_Information (Entity_Context));
               Entity : constant Entities.Entity_Information :=
                 Get_Entity (Entity_Context);

            begin
               if Entity /= null then
                  Gtk_New (Item, Label => -"References");
                  Gtk_New (Submenu);
                  Set_Submenu (Item, Gtk_Widget (Submenu));
                  Append (Menu, Item);

                  if Is_Container (Get_Kind (Entity).Kind) then
                     Gtk_New (Item, Label => Name & (-" calls"));
                     Append (Submenu, Item);
                     Context_Callback.Connect
                       (Item, "activate",
                        Context_Callback.To_Marshaller
                          (Edit_Entity_Call_Graph_From_Contextual'Access),
                        Selection_Context_Access (Context));

                     Gtk_New (Item, Label => Name & (-" is called by"));
                     Append (Submenu, Item);
                     Context_Callback.Connect
                       (Item, "activate",
                        Context_Callback.To_Marshaller
                          (Edit_Ancestors_Call_Graph_From_Contextual'Access),
                        Selection_Context_Access (Context));
                  end if;

                  Gtk_New (Item, Label => (-"Find all references to ") & Name);
                  Append (Submenu, Item);
                  Context_Callback.Connect
                    (Item, "activate",
                     Context_Callback.To_Marshaller
                       (Find_All_References_From_Contextual'Access),
                     Selection_Context_Access (Context));

                  Gtk_New
                    (Item, Label => (-"Find all local references to ") & Name);
                  Append (Submenu, Item);
                  Context_Callback.Connect
                    (Item, "activate",
                     Context_Callback.To_Marshaller
                       (Find_All_Local_References_From_Contextual'Access),
                     Selection_Context_Access (Context));

                  Gtk_New (Item, Label => (-"Find all writes to ") & Name);
                  Append (Submenu, Item);
                  Context_Callback.Connect
                    (Item, "activate",
                     Context_Callback.To_Marshaller
                       (Find_All_Writes_From_Contextual'Access),
                     Selection_Context_Access (Context));

                  Gtk_New (Item, Label => (-"Find all reads of ") & Name);
                  Append (Submenu, Item);
                  Context_Callback.Connect
                    (Item, "activate",
                     Context_Callback.To_Marshaller
                       (Find_All_Reads_From_Contextual'Access),
                     Selection_Context_Access (Context));
               end if;
            end;

            Pop_State (Get_Kernel (Context));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Call_Graph_Contextual_Menu;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item    : access Entity_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access
   is
      pragma Unreferenced (Event, Browser);
      Context : constant Selection_Context_Access :=
        new Entity_Selection_Context;
      Mitem   : Gtk_Image_Menu_Item;
      Pix     : Gtk_Image;

   begin
      if not Is_Predefined_Entity (Item.Entity) then
         Set_File_Information
           (File_Selection_Context_Access (Context),
            File => Get_Filename (Get_File (Get_Declaration_Of (Item.Entity))),
            Line => Get_Line (Get_Declaration_Of (Item.Entity)));
      end if;

      Set_Entity_Information
        (Entity_Selection_Context_Access (Context),
         Entity_Name   => Get_Name (Item.Entity),
         Entity_Column => Get_Column (Get_Declaration_Of (Item.Entity)));

      if Menu /= null then
         declare
            Name : constant String := Get_Name (Item.Entity);
         begin
            Gtk_New (Mitem, Name & (-" calls..."));
            Gtk_New (Pix, Get_Children_Arrow (Get_Browser (Item)));
            Set_Image (Mitem, Pix);
            Append (Menu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller
               (Edit_Entity_Call_Graph_From_Contextual'Access),
               Context);
            Set_Sensitive (Mitem, not Children_Shown (Item));

            Gtk_New (Mitem, Name & (-" is called by..."));
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
         end;
      end if;

      return Context;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return null;
   end Contextual_Factory;

   -------------------
   -- On_Call_Graph --
   -------------------

   procedure On_Call_Graph
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child       : MDI_Child;
      pragma Unreferenced (Widget, Child);

      Context     : Selection_Context_Access :=
        Get_Current_Context (Kernel);
      Entity      : Entity_Selection_Context_Access;
      Node_Entity : Entity_Information;
   begin
      Ref (Context);
      Child := Open_Call_Graph_Browser (Kernel);

      if Context /= null
        and then Context.all in Entity_Selection_Context'Class
      then
         Entity := Entity_Selection_Context_Access (Context);
         Node_Entity := Get_Entity (Entity);

         if Node_Entity /= null then
            Examine_Entity_Call_Graph (Kernel, Node_Entity);
         end if;
      end if;

      Unref (Context);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Call_Graph;

   ----------------------------
   -- On_Find_All_References --
   ----------------------------

   procedure On_Find_All_References
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);

   begin
      if Context /= null
        and then Context.all in Entity_Selection_Context'Class
      then
         Find_All_References_Internal
           (Kernel,
            Get_Entity (Entity_Selection_Context_Access (Context)),
            Category_Title => -All_Refs_Category,
            Include_Writes => True,
            Include_Reads  => True);

      else
         Console.Insert
           (Kernel, -"Cannot find references: no entity selected",
            Mode => Error);
      end if;


   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Find_All_References;

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

   procedure Call_Graph_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      procedure Add_To_List
        (Cb     : Examine_Callback;
         Entity : Entity_Information;
         Ref    : Entity_Reference;
         Is_Renaming : Boolean);
      --  Add a new entity to the return value

      procedure Add_To_List
        (Cb     : Examine_Callback;
         Entity : Entity_Information;
         Ref    : Entity_Reference;
         Is_Renaming : Boolean)
      is
         pragma Unreferenced (Cb, Is_Renaming);
         Loc : File_Location;
      begin
         if Ref /= No_Entity_Reference then
            Loc := Get_Location (Ref);
            Set_Return_Value
              (Data,
               Create_File_Location
                 (Get_Script (Data),
                  Create_File (Get_Script (Data),
                               Get_Filename (Get_File (Loc))),
                  Get_Line (Loc),
                  Get_Column (Loc)));
         else
            Set_Return_Value (Data, -"<renaming>");
         end if;

         Set_Return_Value_Key
           (Data, Create_Entity (Get_Script (Data), Entity), Append => True);
      end Add_To_List;

      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Entity     : constant Entity_Information := Get_Data (Data, 1);
      Cb         : constant Examine_Callback := (null, null, False);

   begin
      if Command = "find_all_refs" then
         Find_All_References_Internal
           (Kernel, Entity,
            Category_Title => -All_Refs_Category,
            Include_Writes => True,
            Include_Reads  => True);

      elsif Command = "references" then
         declare
            Iter : Entity_Reference_Iterator;
            Loc  : File_Location;
            Ref  : Entity_Reference;
         begin
            Set_Return_Value_As_List (Data);
            Find_All_References
              (Iter,
               Entity                => Entity,
               File_Has_No_LI_Report => null);

            while not At_End (Iter) loop
               Ref := Get (Iter);
               if Ref /= No_Entity_Reference then
                  Loc := Get_Location (Ref);
                  Set_Return_Value
                    (Data,
                     Create_File_Location
                       (Script => Get_Script (Data),
                        File   => Create_File
                          (Script => Get_Script (Data),
                           File   => Get_Filename (Get_File (Loc))),
                        Line   => Get_Line (Loc),
                        Column => Get_Column (Loc)));
               end if;
               Next (Iter);
            end loop;
         end;

      elsif Command = "calls" then
         Examine_Entity_Call_Graph_Iterator
           (Kernel, Entity, Cb, Add_To_List'Unrestricted_Access);

      elsif Command = "called_by" then
         Examine_Ancestors_Call_Graph_Iterator
           (Kernel, Entity, Cb, Add_To_List'Unrestricted_Access,
            Background_Mode => False);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Call_Graph_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools    : constant String := '/' & (-"Tools");
      Navigate : constant String := "/_" & (-"Navigate");
      Find_All : constant String := -"Find _All References";
      Mitem    : Gtk_Menu_Item;

   begin
      Register_Module
        (Module                  => Call_Graph_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => Call_Graph_Module_Name,
         Priority                => Glide_Kernel.Default_Priority,
         Contextual_Menu_Handler => Call_Graph_Contextual_Menu'Access,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu (Kernel, Tools, -"Call Graph", "", On_Call_Graph'Access);
      Register_Menu
        (Kernel, Navigate, Find_All, "",
         On_Find_All_References'Access);

      Gtk_New (Mitem);
      Register_Menu
        (Kernel, Navigate, Mitem, Ref_Item => Find_All);

      Register_Command
        (Kernel,
         Command      => "find_all_refs",
         Description  =>
           -"Display in the location window all the references to the entity.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "references",
         Description  =>
           -"List all references to the entity in the project tree",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "calls",
         Return_Value => "htable",
         Description  =>
           -("Display the list of entities called by the entity. The returned"
             & " value is a dictionary whose keys are instances of Entity "
             & " called by this entity, and whose value is a list of"
             & " FileLocation instances where the entity is referenced"),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "called_by",
         Return_Value => "htable",
         Description  =>
           -("Display the list of entities that call the entity. The returned"
             & " value is a dictionary whose keys are instances of Entity "
             & " called by this entity, and whose value is a list of"
             & " FileLocation instances where the entity is referenced"),
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
   end Register_Module;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Call_Graph" then
         return Create_Call_Graph_Browser (User);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Node_Ptr
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

   -----------
   -- Build --
   -----------

   function Build
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Item : access Entity_Item_Record'Class;
      Location    : File_Location) return Active_Area_Cb is
   begin
      return new Show_Location_Callback'
        (Active_Area_Callback with
         Kernel   => Kernel_Handle (Kernel),
         Parent   => Entity_Item (Parent_Item),
         Location => Location);
   end Build;

   ----------
   -- Call --
   ----------

   function Call (Callback : Show_Location_Callback;
                  Event    : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Open_File_Editor
        (Callback.Kernel,
         Filename => Get_Filename (Get_File (Callback.Location)),
         Line     => Get_Line (Callback.Location),
         Column   => Get_Column (Callback.Location));
      return True;
   end Call;

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
      Ref_W1, Ref_W2, Ref_H, Y : Gint;
   begin
      Get_Pixel_Size
        (Get_Browser (Item), Item.Refs, Ref_W1, Ref_W2, Ref_H,  Layout);

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access,
         Gint'Max (Ref_W1 + Ref_W2 + 2 * Margin, Width),
         Height + Ref_H,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Yoffset + 1;
      Display_Lines (Item, Item.Refs, Margin + Xoffset, Y, Ref_W1, Layout);
   end Resize_And_Draw;

end Browsers.Call_Graph;
