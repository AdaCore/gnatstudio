-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2005                      --
--                              AdaCore                              --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                   use GNAT.OS_Lib;
with System;                        use System;

with Gdk.GC;                        use Gdk.GC;
with Gdk.Event;                     use Gdk.Event;

with Glib;                          use Glib;
with Glib.Object;                   use Glib.Object;
with Glib.Xml_Int;                  use Glib.Xml_Int;

with Gtk.Box;                       use Gtk.Box;
with Gtk.Button;                    use Gtk.Button;
with Gtk.Check_Button;              use Gtk.Check_Button;
with Gtk.Dialog;                    use Gtk.Dialog;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Frame;                     use Gtk.Frame;
with Gtk.Menu;                      use Gtk.Menu;
with Gtk.Object;                    use Gtk.Object;
with Gtk.Radio_Button;              use Gtk.Radio_Button;
with Gtk.Stock;                     use Gtk.Stock;
with Gtk.Vbutton_Box;               use Gtk.Vbutton_Box;
with Gtk.Widget;                    use Gtk.Widget;

with Gtkada.Canvas;                 use Gtkada.Canvas;
with Gtkada.Handlers;               use Gtkada.Handlers;
with Gtkada.MDI;                    use Gtkada.MDI;

with Pango.Layout;                  use Pango.Layout;

with Browsers.Canvas;               use Browsers.Canvas;
with Commands.Generic_Asynchronous; use Commands;
with Commands.Interactive;          use Commands.Interactive;
with Entities.Debug;                use Entities.Debug;
with Entities.Queries;              use Entities.Queries;
with Entities;                      use Entities;
with GPS.Intl;                      use GPS.Intl;
with GPS.Kernel.Console;            use GPS.Kernel.Console;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Modules;            use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;        use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;     use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;             use GPS.Kernel.Styles;
with GPS.Kernel.Task_Manager;       use GPS.Kernel.Task_Manager;
with GPS.Kernel;                    use GPS.Kernel;
with GPS.Location_View;             use GPS.Location_View;
with Histories;                     use Histories;
with String_Utils;                  use String_Utils;
with Traces;                        use Traces;
with VFS;                           use VFS;

package body Browsers.Call_Graph is

   Me : constant Debug_Handle := Create ("Browsers.Call_Graph");

   type Callgraph_Module_Record is new Module_ID_Record with null record;
   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   function Default_Context_Factory
     (Module : access Callgraph_Module_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  See inherited documentation

   Automatically_Check_To_Dependencies : constant Boolean := True;
   --  If True, then every time an item is added to the call graph we check,
   --  and if no to dependency exists, the right arrow is not displayed.

   Locations_At_A_Time : constant := 20;
   --  Number of locations that will be inserted in the locations view in
   --  each idle processing.

   Include_Implicit_Cst : aliased constant String := "include_implicit";
   References_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Include_Implicit_Cst'Access);

   type Filters_Buttons is array (Reference_Kind) of Gtk_Check_Button;
   type References_Filter_Dialog_Record is new Gtk_Dialog_Record with record
      Filters : Filters_Buttons;
   end record;
   type References_Filter_Dialog is access all
     References_Filter_Dialog_Record'Class;

   function All_Refs_Category
     (Entity             : Entity_Information;
      Local_Only         : Boolean;
      Local_File         : VFS.Virtual_File;
      All_From_Same_File : Boolean) return String;
   --  Return the category title when doing a find all refs on a given entity.
   --  If All_From_Same_File is true, we will in fact list all entities
   --  imported form the same file as Entity.
   --  If Local_Only is true, then the references are only in the current file

   ------------------------
   -- Call graph browser --
   ------------------------

   type Call_Graph_Browser_Record is new
     Browsers.Canvas.General_Browser_Record with null record;

   type Call_Graph_Browser is access all Call_Graph_Browser_Record'Class;

   --------------
   -- Commands --
   --------------

   type Container_Entity_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Container_Entity_Filter;
      Context : access Selection_Context'Class) return Boolean;

   type Entity_Calls_Command is new Interactive_Command with record
      To_Browser : Boolean := True;
   end record;
   function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Entity_Called_By_Command is new Interactive_Command with record
      To_Browser : Boolean := True;
   end record;
   function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Find_All_Refs_Command is new Interactive_Command with record
      Locals_Only     : Boolean := False;
      Recurse_Project : Boolean := True;
      Writes_Only     : Boolean := False;
      Reads_Only      : Boolean := False;
   end record;
   function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Find_Specific_Refs_Command
     is new Interactive_Command with null record;
   function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Edit_Body_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Edit_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Edit_Spec_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Edit_Spec_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

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
     (Item    : access Entity_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu) return GPS.Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   procedure Resize_And_Draw
     (Item                        : access Entity_Item_Record;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   function Build
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Item : access Entity_Item_Record'Class;
      Location    : File_Location) return Active_Area_Cb;
   --  Build a callback for links in callgraph items

   type Show_Location_Callback is new Active_Area_Callback with record
      Kernel   : Kernel_Handle;
      Parent   : Entity_Item;
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
      Kernel             : Kernel_Handle;
      Iter               : Entity_Reference_Iterator_Access;
      Entity             : Entity_Information;
      Filter             : Reference_Kind_Filter;
      Iter_Started       : Boolean;
      Show_Caller        : Boolean;
      Category           : String_Access;
      Include_Overriding : Boolean;
   end record;

   type Examine_Callback is record
      --  The following three fields are only set for graphical callbacks
      Kernel         : Kernel_Handle;
      Browser        : Call_Graph_Browser;
      Entity         : Entity_Information;
      Item           : Entity_Item;
      Link_From_Item : Boolean;
   end record;
   type Execute_Callback is access procedure
     (Cb          : Examine_Callback;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Is_Renaming : Boolean);

   type Examine_Ancestors_Idle_Data is record
      Iter              : Entity_Reference_Iterator_Access;
      Entity            : Entity_Information;
      Kernel            : Kernel_Handle;
      Callback          : Examine_Callback;
      Execute           : Execute_Callback;
      Browser_Destroyed : Boolean;
   end record;
   type Examine_Ancestors_Data_Access is access Examine_Ancestors_Idle_Data;

   procedure Examine_Entity_Call_Graph
     (Kernel     : access Kernel_Handle_Record'Class;
      Entity     : Entity_Information;
      To_Browser : Boolean);
   --  Display the call graph for the node.

   procedure Examine_Entity_Call_Graph_Iterator
     (Kernel     : access Kernel_Handle_Record'Class;
      Entity     : Entity_Information;
      Callback   : Examine_Callback;
      Execute    : Execute_Callback);
   --  Same as Examine_Entity_Call_Graph, but calls Execute for each matching
   --  entity.

   procedure Examine_Ancestors_Call_Graph
     (Kernel     : access Kernel_Handle_Record'Class;
      Entity     : Entity_Information;
      To_Browser : Boolean);
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

   procedure Ancestors_Browser_Destroyed_While_Computed
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address);
   pragma Convention (C, Ancestors_Browser_Destroyed_While_Computed);
   --  Called when a browser is destroyed while its contents is being computed

   procedure Examine_Ancestors_Call_Graph_Idle
     (Data    : in out Examine_Ancestors_Data_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Main idle loop for Examine_Ancestors_Call_Graph

   function Find_Entity
     (In_Browser : access General_Browser_Record'Class;
      Entity     : Entity_Information)
      return Canvas_Item;
   --  Return the child that shows Item_Name in the browser, or null if
   --  Item_Name is not already displayed in the canvas.
   --  ??? Should also have line and column information

   function Open_Call_Graph_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Child;
   --  Find, or create a new, call graph editor.

   function Create_Call_Graph_Browser
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child;
   --  Create a new call graph browser.

   procedure Parse_All_Refs
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Entity_Information;
      Locals_Only        : Boolean;
      Local_File         : VFS.Virtual_File;
      All_From_Same_File : Boolean;
      Show_Caller        : Boolean;
      Filter             : Reference_Kind_Filter;
      Include_Overriding : Boolean := False);
   --  Internal implementation of find_all_references.
   --  If All_From_Same_File is True, then all entities imported from the same
   --  file as Entity and referenced in Local_File, as Entity are
   --  displayed.

   procedure Find_All_References_Internal
     (Kernel         : access Kernel_Handle_Record'Class;
      Info           : Entity_Information;
      Category_Title : String;
      Show_Caller    : Boolean;
      Filter         : Reference_Kind_Filter;
      Include_Overriding : Boolean := False);
   --  Internal implementation for Find_All_References_From_Contextual,
   --  Find_All_Writes_From_Contextual and Find_All_Reads_From_Contextual.
   --  Starts a background search for all references.

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

   procedure Insert_In_Locations_View
     (Cb          : Examine_Callback;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Is_Renaming : Boolean);
   --  Add an entry in locations window to show the call graph for Entity.

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Data_Access);
   --  Called when the idle loop is destroyed.

   procedure Destroy_Idle (Data : in out Entity_Idle_Data);
   --  Called when the idle loop is destroyed.

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Entity_Idle_Data, Destroy_Idle);
   package Ancestor_Commands is new Commands.Generic_Asynchronous
     (Examine_Ancestors_Data_Access, Destroy_Idle);

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
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Support functions for the MDI

   procedure Print_Ref
     (Kernel      : access Kernel_Handle_Record'Class;
      Ref         : Entity_Reference;
      Name        : String;
      Category    : String;
      Show_Caller : Boolean);
   --  Display a reference in the locations tree, after looking for the
   --  directory containing File.
   --  Category corresponds to the purpose of the print. All references
   --  corresponding to the same category will be printed as a group.
   --  If Show_Caller is true, the full name of the caller will also be
   --  displayed.

   procedure Call_Graph_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure Xref_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handle shell commands related to the xref database as a whole

   procedure Examine_Ancestors_Call_Graph
     (Item : access Arrow_Item_Record'Class);
   procedure Examine_Entity_Call_Graph
     (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the title bar buttons

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   --  Select or unselect all filters in "Find references..."

   -----------------------
   -- All_Refs_Category --
   -----------------------

   function All_Refs_Category
     (Entity             : Entity_Information;
      Local_Only         : Boolean;
      Local_File         : VFS.Virtual_File;
      All_From_Same_File : Boolean) return String
   is
      Decl  : constant File_Location := Get_Declaration_Of (Entity);
   begin
      if All_From_Same_File then
         return -"Entities imported from "
           & Krunch (Base_Name (Get_Filename (Decl.File)))
           & (-" into ")
           & Krunch (Base_Name (Local_File));

      elsif Local_Only then
         return -"Local references for " & Get_Name (Entity).all
           & " ("  & Krunch (Base_Name (Get_Filename (Decl.File)))
           & ":" & Image (Decl.Line) & ") " & (-"in ")
           & Krunch (Base_Name (Local_File));

      else
         return -"References for " & Get_Name (Entity).all
           & " ("  & Krunch (Base_Name (Get_Filename (Decl.File)))
           & ":" & Image (Decl.Line) & ")";
      end if;
   end All_Refs_Category;

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
      Initialize (Item, Browser, Get_Name (Entity).all,
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
      Item2    : constant Entity_Item := Item'Unrestricted_Access;
      Iter     : Item_Iterator := Start (Get_Canvas (Get_Browser (Item2)));
      It       : Canvas_Item;
      Line     : Positive;
      Text     : String_Access;
      Callback : Active_Area_Cb;
      Cb       : Show_Location_Callback_Access;
      Removed  : Boolean;
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

      Child := Put
        (Kernel, Browser,
         Focus_Widget   => Gtk_Widget (Get_Canvas (Browser)),
         Default_Width  => Get_Pref (Default_Widget_Width),
         Default_Height => Get_Pref (Default_Widget_Height),
         Module         => Call_Graph_Module_Id);
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
      Child : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Call_Graph_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Child := Create_Call_Graph_Browser (Kernel);
         Set_Focus_Child (Child);
      end if;

      Add_Navigation_Location (Kernel, -"Call graph Browser");

      return Child;
   end Open_Call_Graph_Browser;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (In_Browser : access General_Browser_Record'Class;
      Entity     : Entity_Information) return Canvas_Item
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
     (Kernel     : access Kernel_Handle_Record'Class;
      Entity     : Entity_Information;
      Callback   : Examine_Callback;
      Execute    : Execute_Callback)
   is
      Rename : Entity_Information;
      Iter   : Calls_Iterator;
      Refs   : Entity_Reference_Iterator;
      Ref    : Entity_Reference;

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

                  if Ref /= No_Entity_Reference
                    and then Show_In_Call_Graph (Get_Kind (Ref))
                    and then Get_Caller (Ref) = Entity
                    and then Is_Subprogram (Get_Entity (Refs))
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
     (Kernel     : access Kernel_Handle_Record'Class;
      Entity     : Entity_Information;
      To_Browser : Boolean)
   is
      Child_Browser : MDI_Child;
      Cb            : Examine_Callback;
   begin
      --  Create the browser if it doesn't exist
      if To_Browser then
         Child_Browser := Open_Call_Graph_Browser (Kernel);
         Cb.Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

         --  Look for an existing item corresponding to entity
         Cb.Item := Add_Entity_If_Not_Present (Cb.Browser, Entity);
      end if;

      Cb.Link_From_Item := True;
      Cb.Kernel         := Kernel_Handle (Kernel);
      Cb.Entity         := Entity;
      Ref (Entity);

      if not To_Browser then
         Examine_Entity_Call_Graph_Iterator
           (Kernel, Entity, Cb, Insert_In_Locations_View'Access);

      elsif not Children_Shown (Cb.Item) then
         Set_Children_Shown (Cb.Item, True);
         Examine_Entity_Call_Graph_Iterator
           (Kernel, Entity, Cb, Add_Entity_And_Link'Access);

         --  Refresh all linked items, since we have added references in them
         Refresh_Linked_Items (Cb.Item, Refresh_Children => True);
      end if;

      --  We need to do a layout in all cases, so that the newly added item
      --  is put at a correct place.
      if To_Browser then
         Layout (Cb.Browser, Force => False);
         Refresh_Canvas (Get_Canvas (Cb.Browser));
         Show_Item (Get_Canvas (Cb.Browser), Cb.Item);

         Redraw_Title_Bar (Cb.Item);
      end if;

      Unref (Cb.Entity);

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Entity_Call_Graph;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Ancestors_Data_Access) is

      procedure Clean;
      --  Clean up before exiting Destroy_Idle

      -----------
      -- Clean --
      -----------

      procedure Clean is
      begin
         Destroy (Data.Iter);
         Unref (Data.Entity);
         Unref (Data.Callback.Entity);

         if Data.Callback.Browser /= null then
            Pop_State (Get_Kernel (Data.Callback.Browser));
         end if;
      end Clean;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Examine_Ancestors_Idle_Data, Examine_Ancestors_Data_Access);

   begin
      if Data.Callback.Browser /= null then
         --  Refresh the item, since we might have added links to it
         Refresh (Data.Callback.Item);

         Layout (Data.Callback.Browser, Force => False);
         Refresh_Canvas (Get_Canvas (Data.Callback.Browser));
         Show_Item (Get_Canvas (Data.Callback.Browser), Data.Callback.Item);

         Weak_Unref (Data.Callback.Browser,
                     Ancestors_Browser_Destroyed_While_Computed'Access,
                     Data.all'Address);
      end if;
      Clean;

      Unchecked_Free (Data);

   exception
      when E : others =>
         Clean;
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Destroy_Idle;

   ---------------------------------------
   -- Examine_Ancestors_Call_Graph_Idle --
   ---------------------------------------

   procedure Examine_Ancestors_Call_Graph_Idle
     (Data    : in out Examine_Ancestors_Data_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Parent : Entity_Information;
      Ref    : Entity_Reference;
   begin
      if Data.Browser_Destroyed then
         Result := Success;
      elsif At_End (Data.Iter.all) then
         Result := Success;
      else
         Ref := Get (Data.Iter.all);

         if Ref /= No_Entity_Reference then
            Parent := Get_Caller (Ref);
            if Parent /= null
              and then Show_In_Call_Graph (Get_Kind (Ref))
              and then Is_Container (Get_Kind (Parent).Kind)
            then
               Data.Execute (Data.Callback, Parent, Ref, False);
            end if;
         end if;

         Next (Data.Iter.all);

         if Command /= null then
            Set_Progress
              (Command,
               (Running,
                Get_Current_Progress (Data.Iter.all),
                Get_Total_Progress (Data.Iter.all)));
         end if;

         Result := Execute_Again;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         Result := Failure;
   end Examine_Ancestors_Call_Graph_Idle;

   ------------------------------------------------
   -- Ancestors_Browser_Destroyed_While_Computed --
   ------------------------------------------------

   procedure Ancestors_Browser_Destroyed_While_Computed
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address)
   is
      pragma Unreferenced (Where_The_Object_Was);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Examine_Ancestors_Data_Access);
   begin
      Convert (Data).Browser_Destroyed := True;
      Convert (Data).Callback.Browser := null;
   end Ancestors_Browser_Destroyed_While_Computed;

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
      C      : Ancestor_Commands.Generic_Asynchronous_Command_Access;
      Rename : Entity_Information;
      Data   : Examine_Ancestors_Data_Access;
      Result : Command_Return_Type;
   begin
      --  If we have a renaming, add the entry for the renamed entity
      Rename := Renaming_Of (Entity);
      if Rename /= null then
         Execute (Callback, Rename, No_Entity_Reference, Is_Renaming => False);
      end if;

      Ref (Entity);
      Data := new Examine_Ancestors_Idle_Data'
        (Iter              => new Entity_Reference_Iterator,
         Entity            => Entity,
         Kernel            => Kernel_Handle (Kernel),
         Callback          => Callback,
         Browser_Destroyed => False,
         Execute           => Execute);
      Find_All_References (Iter => Data.Iter.all, Entity => Entity);

      if Callback.Browser /= null then
         Weak_Ref
           (Callback.Browser,
            Ancestors_Browser_Destroyed_While_Computed'Access,
            Data.all'Address);
      end if;

      if Background_Mode then
         Ancestor_Commands.Create
           (C, -"Called by", Data, Examine_Ancestors_Call_Graph_Idle'Access);
         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "call graph");
      else
         loop
            Examine_Ancestors_Call_Graph_Idle
              (Data, Command_Access (C), Result);
            exit when Result /= Execute_Again;
         end loop;
         Destroy_Idle (Data);
      end if;
   end Examine_Ancestors_Call_Graph_Iterator;

   ------------------------------
   -- Insert_In_Locations_View --
   ------------------------------

   procedure Insert_In_Locations_View
     (Cb          : Examine_Callback;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Is_Renaming : Boolean)
   is
      type Access_Cst_String is access constant String;
      Name : constant GNAT.OS_Lib.String_Access := Get_Name (Cb.Entity);
      Loc  : constant File_Location := Get_Location (Ref);
      Category1 : aliased constant String := Name.all & " calls";
      Category2 : aliased constant String := Name.all & " called by";
      Category  : Access_Cst_String;
   begin
      if Cb.Link_From_Item then
         Category := Category1'Unchecked_Access;
      else
         Category := Category2'Unchecked_Access;
      end if;

      if Is_Renaming then
         Insert_Location
           (Cb.Kernel,
            Category  => Category.all,
            File      => Get_Filename (Get_File (Loc)),
            Line      => Get_Line (Loc),
            Column    => Get_Column (Loc),
            Length    => Name'Length,
            Highlight => True,
            Text      => "(renaming)");
      else
         Insert_Location
           (Cb.Kernel,
            Category  => Category.all,
            File      => Get_Filename (Get_File (Loc)),
            Line      => Get_Line (Loc),
            Column    => Get_Column (Loc),
            Length    => Name'Length,
            Highlight => True,
            Text      => Get_Name (Entity).all);
      end if;
   end Insert_In_Locations_View;

   -------------------------
   -- Add_Entity_And_Link --
   -------------------------

   procedure Add_Entity_And_Link
     (Cb          : Examine_Callback;
      Entity      : Entity_Information;
      Ref         : Entity_Reference;
      Is_Renaming : Boolean)
   is
      Child            : Entity_Item;
      Link             : Browser_Link;
      Loc              : File_Location;
      Line             : Natural;
      Text             : String_Access;
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
         New_Cb := Build (Cb.Kernel, Cb.Item, Loc);

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
     (Kernel     : access Kernel_Handle_Record'Class;
      Entity     : Entity_Information;
      To_Browser : Boolean)
   is
      Child_Browser : MDI_Child;
      Cb            : Examine_Callback;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Cb.Link_From_Item := False;
      Cb.Entity := Entity;
      Cb.Kernel := Kernel_Handle (Kernel);
      Ref (Entity);

      --  Create the browser if it doesn't exist
      if To_Browser then
         Child_Browser := Open_Call_Graph_Browser (Kernel);
         Cb.Browser := Call_Graph_Browser (Get_Widget (Child_Browser));

         --  Look for an existing item corresponding to entity
         Cb.Item := Add_Entity_If_Not_Present (Cb.Browser, Entity);
         Set_Parents_Shown (Cb.Item, True);
         Redraw_Title_Bar (Cb.Item);
         Examine_Ancestors_Call_Graph_Iterator
           (Kernel, Entity, Cb, Add_Entity_And_Link'Access,
            Background_Mode => True);
      else
         Examine_Ancestors_Call_Graph_Iterator
           (Kernel, Entity, Cb, Insert_In_Locations_View'Access,
            Background_Mode => True);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Ancestors_Call_Graph;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      C : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Location : Entities.File_Location;
   begin
      Find_Next_Body
        (Entity   => Get_Entity (C),
         Location => Location);

      Add_Navigation_Location (Get_Kernel (C), -"Call graph Browser");

      if Location /= Entities.No_File_Location then
         Open_File_Editor
           (Get_Kernel (C),
            Filename => Get_Filename (Get_File (Location)),
            Line     => Get_Line (Location),
            Column   => Get_Column (Location));
      else
         --  If the body wasn't found then display the specs
         Open_File_Editor
           (Get_Kernel (C),
            File_Information (C),
            Line   => Line_Information (C),
            Column => Entity_Column_Information (C));
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_Spec_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      C      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Entity : constant Entity_Information := Get_Entity (C);
   begin
      if Entity = null then
         Insert (Get_Kernel (Context.Context),
                 (-"Couldn't find cross-reference information for ")
                 & '"' & Entity_Name_Information (C) & '"');
      else
         Add_Navigation_Location
           (Get_Kernel (Context.Context), -"Call graph Browser");

         Open_File_Editor
           (Get_Kernel (Context.Context),
            Get_Filename (Get_File (Get_Declaration_Of (Entity))),
            Line   => Get_Line (Get_Declaration_Of (Entity)),
            Column => Get_Column (Get_Declaration_Of (Entity)));
      end if;
      return Commands.Success;
   end Execute;

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
     (Kernel      : access Kernel_Handle_Record'Class;
      Ref         : Entity_Reference;
      Name        : String;
      Category    : String;
      Show_Caller : Boolean)
   is
      Col  : Integer := Get_Column (Get_Location (Ref));
      Line : constant Integer      := Get_Line (Get_Location (Ref));
      File : constant Virtual_File :=
        Get_Filename (Get_File (Get_Location (Ref)));
   begin
      if Col <= 0 then
         Col := 1;
      end if;

      if Show_Caller and then Get_Caller (Ref) /= null then
         Insert_Location
           (Kernel,
            Category  => Category,
            File      => File,
            Text      => Name & " ["
               & Kind_To_String (Get_Kind (Ref)) & "] in: "
               & Get_Full_Name (Get_Caller (Ref)),
            Line      => Line,
            Column    => Col,
            Length    => Name'Length,
            Highlight => True,
            Highlight_Category => Search_Results_Style,
            Remove_Duplicates => False,
            Enable_Counter    => False);

      else
         Insert_Location
           (Kernel,
            Category  => Category,
            File      => File,
            Text      => Name & " [" & Kind_To_String (Get_Kind (Ref)) & "]",
            Line      => Line,
            Column    => Col,
            Length    => Name'Length,
            Highlight => True,
            Highlight_Category => Search_Results_Style,
            Remove_Duplicates => False,
            Enable_Counter    => False);
      end if;
   end Print_Ref;

   -------------------------
   -- Find_Next_Reference --
   -------------------------

   procedure Find_Next_Reference
     (Data    : in out Entity_Idle_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Count : Integer := 0;
   begin
      Result := Execute_Again;

      if not Data.Iter_Started then
         Find_All_References
           (Iter   => Data.Iter.all,
            Entity => Data.Entity,
            Filter => Data.Filter,
            Include_Overriding => Data.Include_Overriding);

         Data.Iter_Started := True;
         Set_Progress
           (Command,
            (Running,
             Get_Current_Progress (Data.Iter.all),
             Get_Total_Progress (Data.Iter.all)));
         return;
      end if;

      while Count < Locations_At_A_Time loop
         if At_End (Data.Iter.all) then
            Recount_Category (Data.Kernel, Data.Category.all);
            Result := Success;
            exit;

            --  Not done parsing all the files yet
         elsif Get (Data.Iter.all) = No_Entity_Reference then
            Next (Data.Iter.all);
            exit;

         else
            Print_Ref
              (Data.Kernel,
               Get (Data.Iter.all),
               Get_Name (Data.Entity).all,
               Data.Category.all,
               Show_Caller => Data.Show_Caller);

            Count := Count + 1;
         end if;

         Next (Data.Iter.all);
      end loop;

      Set_Progress
        (Command,
         (Running,
          Get_Current_Progress (Data.Iter.all),
          Get_Total_Progress (Data.Iter.all)));
   end Find_Next_Reference;

   ----------------------------------
   -- Find_All_References_Internal --
   ----------------------------------

   procedure Find_All_References_Internal
     (Kernel           : access Kernel_Handle_Record'Class;
      Info             : Entity_Information;
      Category_Title   : String;
      Show_Caller      : Boolean;
      Filter           : Reference_Kind_Filter;
      Include_Overriding : Boolean := False)
   is
      Data : Entity_Idle_Data;
      C    : Xref_Commands.Generic_Asynchronous_Command_Access;

   begin
      if Info /= null then
         begin
            Remove_Location_Category (Kernel, Category_Title);

            Ref (Info);
            Data := (Kernel             => Kernel_Handle (Kernel),
                     Iter               => new Entity_Reference_Iterator,
                     Filter             => Filter,
                     Category           => new String'(Category_Title),
                     Iter_Started       => False,
                     Show_Caller        => Show_Caller,
                     Include_Overriding => Include_Overriding,
                     Entity             => Info);

            Xref_Commands.Create
              (C, -"Find all refs", Data, Find_Next_Reference'Access);
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

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Ancestors_Call_Graph
        (Get_Kernel (Get_Browser (Item)), Entity_Item (Item).Entity,
         To_Browser => True);
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Entity_Call_Graph
        (Get_Kernel (Get_Browser (Item)), Entity_Item (Item).Entity,
         To_Browser => True);
   end Examine_Entity_Call_Graph;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item    : access Entity_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu) return GPS.Kernel.Selection_Context_Access
   is
      pragma Unreferenced (Event, Browser, Menu);
      Context : constant Selection_Context_Access :=
        new Entity_Selection_Context;

   begin
      if not Is_Predefined_Entity (Item.Entity) then
         Set_File_Information
           (File_Selection_Context_Access (Context),
            File => Get_Filename (Get_File (Get_Declaration_Of (Item.Entity))),
            Line => Get_Line (Get_Declaration_Of (Item.Entity)));
      end if;

      Set_Entity_Information
        (Entity_Selection_Context_Access (Context),
         Entity_Name   => Get_Name (Item.Entity).all,
         Entity_Column => Get_Column (Get_Declaration_Of (Item.Entity)));
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
            Examine_Entity_Call_Graph
              (Kernel, Node_Entity, To_Browser => True);
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
      Entity  : Entity_Information;

   begin
      if Context /= null
        and then Context.all in Entity_Selection_Context'Class
      then
         Entity := Get_Entity (Entity_Selection_Context_Access (Context));
         Find_All_References_Internal
           (Kernel,
            Entity,
            Category_Title   => All_Refs_Category
              (Entity             => Entity,
               Local_Only         => False,
               Local_File         => VFS.No_File,
               All_From_Same_File => False),
            Filter           => Read_Reference_Filter
               or Write_Reference_Filter,
            Show_Caller      => False);

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

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   function Default_Context_Factory
     (Module : access Callgraph_Module_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Module);
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
   end Default_Context_Factory;

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
      Cb         : constant Examine_Callback :=
        (null, null, null, null, False);
      Filter     : Reference_Kind_Filter;

   begin
      if Command = "find_all_refs" then
         Name_Parameters (Data, References_Cmd_Parameters);
         Filter := Read_Reference_Filter or Write_Reference_Filter;
         Filter (Implicit) := Nth_Arg (Data, 2, False);
         Find_All_References_Internal
           (Kernel, Entity,
            Category_Title   => All_Refs_Category
              (Entity             => Entity,
               Local_Only         => False,
               Local_File         => VFS.No_File,
               All_From_Same_File => False),
            Show_Caller      => False,
            Filter           => Filter);

      elsif Command = "references" then
         Name_Parameters (Data, References_Cmd_Parameters);
         declare
            Iter : Entity_Reference_Iterator;
            Loc  : File_Location;
            Ref  : Entity_Reference;
         begin
            Filter := Real_References_Filter;
            Filter (Implicit) := Nth_Arg (Data, 2, False);

            Set_Return_Value_As_List (Data);
            Find_All_References
              (Iter,
               Entity                => Entity,
               Filter                => Filter,
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

      elsif Command = "called_by_browser" then
         Examine_Ancestors_Call_Graph
           (Kernel, Entity, To_Browser => True);

      elsif Command = "dump" then
         Dump (Entity, Full => False, Name => "");
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Call_Graph_Command_Handler;

   --------------------------
   -- Xref_Command_Handler --
   --------------------------

   procedure Xref_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Output : Ada.Text_IO.File_Type;

      procedure My_Output (Str : String);
      procedure My_Output_Line (Str : String);
      --  Output a string to the file

      ---------------
      -- My_Output --
      ---------------

      procedure My_Output (Str : String) is
      begin
         Put (Output, Str);
      end My_Output;

      --------------------
      -- My_Output_Line --
      --------------------

      procedure My_Output_Line (Str : String) is
      begin
         Put_Line (Output, Str);
      end My_Output_Line;

   begin
      if Command = "dump_xref_db" then
         Create (Output, Name => Get_Home_Dir (Get_Kernel (Data)) & "db_dump");
         Trace (Me, "Database dumped in "
                & Get_Home_Dir (Get_Kernel (Data)) & "db_dump");
         Entities.Debug.Output      := My_Output'Unrestricted_Access;
         Entities.Debug.Output_Line := My_Output_Line'Unrestricted_Access;
         Dump (Get_Database (Get_Kernel (Data)), Full => True);
         Set_Default_Output;
         Close (Output);

      elsif Command = "reset_xref_db" then
         Entities.Reset (Get_Database (Get_Kernel (Data)));
      end if;
   end Xref_Command_Handler;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Container_Entity_Filter;
      Context : access Selection_Context'Class) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Context.all in Entity_Selection_Context'Class
        and then Has_Entity_Name_Information
          (Entity_Selection_Context_Access (Context))
      then
         Entity := Get_Entity (Entity_Selection_Context_Access (Context));
         return Entity /= null
           and then Is_Container (Get_Kind (Entity).Kind);
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Entity      : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context.Context);
      Node_Entity : Entity_Information;
   begin
      Push_State (Get_Kernel (Entity), Busy);
      Node_Entity := Get_Entity (Entity);

      if Node_Entity /= null then
         --  ??? Should check that Decl.Kind is a subprogram
         Examine_Entity_Call_Graph
           (Get_Kernel (Entity), Node_Entity, Command.To_Browser);
      else
         Insert (Get_Kernel (Entity),
                 -"No call graph available for "
                 & Entity_Name_Information (Entity));
      end if;

      Pop_State (Get_Kernel (Entity));
      return Commands.Success;

   exception
      when E : others =>
         Insert (Get_Kernel (Entity),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Entity),
                 Mode => Error);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (Entity));
      return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Entity   : constant Entity_Selection_Context_Access :=
                   Entity_Selection_Context_Access (Context.Context);
      Info : Entity_Information;
   begin
      Push_State (Get_Kernel (Entity), Busy);
      Info := Get_Entity (Entity);

      if Info /= null then
         Examine_Ancestors_Call_Graph
           (Get_Kernel (Entity), Info, Command.To_Browser);
      else
         Insert (Get_Kernel (Entity),
                 -"No information found for the file "
                   & Full_Name (File_Information (Entity)).all,
                 Mode => Error);
      end if;

      Pop_State (Get_Kernel (Entity));
      return Commands.Success;

   exception
      when E : others =>
         Insert (Get_Kernel (Entity),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Entity),
                 Mode => Error);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (Entity));
      return Commands.Failure;
   end Execute;

   --------------------
   -- Parse_All_Refs --
   --------------------

   procedure Parse_All_Refs
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Entity_Information;
      Locals_Only        : Boolean;
      Local_File         : Virtual_File;
      All_From_Same_File : Boolean;
      Show_Caller        : Boolean;
      Filter             : Reference_Kind_Filter;
      Include_Overriding : Boolean := False)
   is
      Iter     : Entity_Reference_Iterator;
      Iter2    : Entity_Iterator;
      Entity2  : Entity_Information;
      Local : constant Source_File :=
        Get_Or_Create (Get_Database (Kernel), Local_File);
      Title : constant String := All_Refs_Category
        (Entity             => Entity,
         Local_Only         => Locals_Only,
         Local_File         => Local_File,
         All_From_Same_File => All_From_Same_File);
      Entity_Decl : constant Source_File :=
        Get_File (Get_Declaration_Of (Entity));
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      if All_From_Same_File then
         Remove_Location_Category (Kernel, Title);

         Find_All_Entities_In_File (Iter => Iter2, File => Local);
         while not At_End (Iter2) loop
            Entity2 := Get (Iter2);

            if Get_File (Get_Declaration_Of (Entity2)) = Entity_Decl then
               if Show_Caller then
                  Find_All_References
                    (Iter               => Iter,
                     Entity             => Entity2,
                     Filter             => Filter,
                     In_File            => Local,
                     Include_Overriding => Include_Overriding);

                  while not At_End (Iter) loop
                     if Get (Iter) /= No_Entity_Reference
                       and then Get_File (Get_Location (Get (Iter))) = Local
                     then
                        Print_Ref (Kernel,
                                   Get (Iter),
                                   Get_Name (Entity2).all,
                                   Title,
                                   Show_Caller => Show_Caller);
                     end if;
                     Next (Iter);
                  end loop;
                  Destroy (Iter);

               else
                  Insert_Location
                    (Kernel,
                     Category     => Title,
                     File         => Get_Filename
                       (Get_File (Get_Declaration_Of (Entity2))),
                     Text         => Get_Name (Entity2).all,
                     Line         => Get_Line (Get_Declaration_Of (Entity2)),
                     Column       => Get_Column (Get_Declaration_Of (Entity2)),
                     Length       => Get_Name (Entity2)'Length,
                     Highlight    => True,
                     Highlight_Category => Search_Results_Style,
                     Remove_Duplicates  => False,
                     Enable_Counter     => True);
               end if;
            end if;

            Next (Iter2);
         end loop;

         Destroy (Iter2);
         Recount_Category (Kernel, Title);

      elsif Locals_Only then
         --  Print the declaration of the entity, but only if it is in the
         --  current file, as expected by users.

         Remove_Location_Category (Kernel, Title);
         Find_All_References
           (Iter          => Iter,
            Entity        => Entity,
            Filter        => Filter,
            In_File       => Get_Or_Create (Get_Database (Kernel), Local_File),
            Include_Overriding => Include_Overriding);

         while not At_End (Iter) loop
            if Get (Iter) /= No_Entity_Reference then
               Print_Ref (Kernel,
                          Get (Iter),
                          Get_Name (Entity).all,
                          Title,
                          Show_Caller => Show_Caller);
            end if;
            Next (Iter);
         end loop;

         Recount_Category (Kernel, Title);
         Destroy (Iter);

      else
         Find_All_References_Internal
           (Kernel,
            Entity,
            Category_Title     => Title,
            Show_Caller        => Show_Caller,
            Filter             => Filter,
            Include_Overriding => Include_Overriding);
      end if;

      Pop_State (Kernel_Handle (Kernel));
   end Parse_All_Refs;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Entity   : constant Entity_Information := Get_Entity
        (Entity_Selection_Context_Access (Context.Context));
      Filter   : Reference_Kind_Filter := (others => False);
      File     : constant Virtual_File := File_Information
        (File_Selection_Context_Access (Context.Context));
   begin
      if Command.Reads_Only then
         Filter := Read_Reference_Filter;
      elsif Command.Writes_Only then
         Filter := Write_Reference_Filter;
      else
         Filter := Read_Reference_Filter or Write_Reference_Filter;
      end if;

      Parse_All_Refs
        (Kernel            => Kernel,
         Entity            => Entity,
         Locals_Only       => Command.Locals_Only,
         Local_File        => File,
         All_From_Same_File => False,
         Filter            => Filter,
         Show_Caller       => False);
      return Commands.Success;
   end Execute;

   ------------------------
   -- Select_All_Filters --
   ------------------------

   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         if D.Filters (F) /= null then
            Set_Active (D.Filters (F), True);
         end if;
      end loop;
   end Select_All_Filters;

   --------------------------
   -- Unselect_All_Filters --
   --------------------------

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         if D.Filters (F) /= null then
            Set_Active (D.Filters (F), False);
         end if;
      end loop;
   end Unselect_All_Filters;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dialog     : References_Filter_Dialog;
      Box        : Gtk_Box;
      Col        : array (1 .. 2) of Gtk_Box;
      Filter_Box : Gtk_Vbutton_Box;
      Index      : Integer := Col'First;
      Project_And_Recursive,
      File_Only : Gtk_Radio_Button;
      Show_Caller : Gtk_Check_Button;
      From_Same_File : Gtk_Radio_Button;
      Include_Overriding : Gtk_Check_Button;
      Filter    : Reference_Kind_Filter := (others => False);
      Frame     : Gtk_Frame;
      Widget    : Gtk_Widget;
      Entity    : constant Entity_Information :=
        Get_Entity
          (Entity_Selection_Context_Access (Context.Context));
      Current_File     : constant Virtual_File := File_Information
        (File_Selection_Context_Access (Context.Context));
      Button    : Gtk_Button;
      pragma Unreferenced (Command, Widget);

   begin
      Dialog := new References_Filter_Dialog_Record;
      Initialize (Dialog,
                  Title  => -"Find References Options",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal);

      --  Context choice

      Gtk_New (Frame, -"Context");
      Pack_Start (Get_Vbox (Dialog), Frame);
      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New (Project_And_Recursive, Widget_SList.Null_List,
               -"In all projects");
      Pack_Start (Box, Project_And_Recursive);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_Project_Recursive", True);
      Associate (Get_History (Kernel).all, "Find_Prefs_Project_Recursive",
                 Project_And_Recursive);

      Gtk_New (File_Only, Get_Group (Project_And_Recursive),
               -"In current file");
      Pack_Start (Box, File_Only);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_File_Only", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_File_Only", File_Only);

      Gtk_New
        (From_Same_File, Get_Group (Project_And_Recursive),
         -"All entities imported from same file");
      Pack_Start (Box, From_Same_File);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_From_Same_File", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_From_Same_File",
                 From_Same_File);

      --  Filter choice

      Gtk_New (Frame, -"Filter");
      Pack_Start (Get_Vbox (Dialog), Frame);
      Gtk_New_Hbox (Box, Homogeneous => False);
      Add (Frame, Box);

      for C in Col'Range loop
         Gtk_New_Vbox (Col (C), Homogeneous => True);
         Pack_Start (Box, Col (C), Expand => True);
      end loop;

      for F in Dialog.Filters'Range loop
         if Is_Real_Reference (F) or else F = Implicit then
            Gtk_New (Dialog.Filters (F), Kind_To_String (F));
            Pack_Start (Col (Index), Dialog.Filters (F));
            Create_New_Boolean_Key_If_Necessary
              (Get_History (Kernel).all,
               History_Key ("Find_Prefs_Filter_" & F'Img), True);
            Associate (Get_History (Kernel).all,
                       History_Key ("Find_Prefs_Filter_" & F'Img),
                       Dialog.Filters (F));
            Index := Index + 1;
            if Index > Col'Last then
               Index := Col'First;
            end if;
         end if;
      end loop;

      Gtk_New (Filter_Box);
      Set_Layout (Filter_Box, Buttonbox_Spread);
      Pack_Start (Box, Filter_Box, Padding => 5);

      Gtk_New (Button, -"Select all");
      Pack_Start (Filter_Box, Button);
      Widget_Callback.Object_Connect
        (Button, "clicked", Select_All_Filters'Access, Dialog);

      Gtk_New (Button, -"Unselect all");
      Pack_Start (Filter_Box, Button);
      Widget_Callback.Object_Connect
        (Button, "clicked", Unselect_All_Filters'Access, Dialog);

      --  Extra info choice

      Gtk_New (Frame, -"Advanced Search");
      Pack_Start (Get_Vbox (Dialog), Frame);
      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      Gtk_New (Show_Caller, -"Show context");
      Pack_Start (Box, Show_Caller);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_Show_Caller", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_Show_Caller",
                 Show_Caller);

      Gtk_New
        (Include_Overriding, -"Include overriding and overriden operations");
      Pack_Start (Box, Include_Overriding);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, "Find_Prefs_Include_Overriding", False);
      Associate (Get_History (Kernel).all, "Find_Prefs_Include_Overriding",
                 Include_Overriding);

      Widget := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Widget := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         for F in Dialog.Filters'Range loop
            if Dialog.Filters (F) = null then
               Filter (F) := False;
            else
               Filter (F) := Get_Active (Dialog.Filters (F));
            end if;
         end loop;

         Parse_All_Refs
           (Kernel             => Kernel,
            Entity             => Entity,
            Locals_Only        => Get_Active (File_Only),
            Local_File         => Current_File,
            All_From_Same_File => Get_Active (From_Same_File),
            Filter             => Filter,
            Show_Caller        => Get_Active (Show_Caller),
            Include_Overriding => Get_Active (Include_Overriding));

         Destroy (Dialog);

         return Commands.Success;
      else
         Destroy (Dialog);
         return Commands.Failure;
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools    : constant String := '/' & (-"Tools");
      Navigate : constant String := "/_" & (-"Navigate");
      Find_All : constant String := -"Find _All References";
      Command  : Interactive_Command_Access;
      Filter   : Action_Filter;

   begin
      Call_Graph_Module_Id := new Callgraph_Module_Record;
      Register_Module
        (Module                  => Call_Graph_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => Call_Graph_Module_Name,
         Priority                => GPS.Kernel.Modules.Default_Priority);
      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Filter  := new Container_Entity_Filter;
      Command := new Entity_Calls_Command;
      Entity_Calls_Command (Command.all).To_Browser := False;
      Register_Contextual_Menu
        (Kernel, "Entity calls",
         Label  => "References/%e calls (in locations)",
         Filter => Filter,
         Action => Command);

      Command := new Entity_Calls_Command;
      Entity_Calls_Command (Command.all).To_Browser := True;
      Register_Contextual_Menu
        (Kernel, "Entity calls in browser",
         Label  => "References/%e calls (in browser)",
         Filter => Filter,
         Action => Command);

      Command := new Entity_Called_By_Command;
      Entity_Called_By_Command (Command.all).To_Browser := False;
      Register_Contextual_Menu
        (Kernel, "Entity called by",
         Label  => "References/%e is called by (in locations)",
         Filter => Filter,
         Action => Command);

      Command := new Entity_Called_By_Command;
      Entity_Called_By_Command (Command.all).To_Browser := True;
      Register_Contextual_Menu
        (Kernel, "Entity called by in browser",
         Label  => "References/%e is called by (in browser)",
         Filter => Filter,
         Action => Command);

      Command := new Find_All_Refs_Command;
      Register_Contextual_Menu
        (Kernel, "Find all references",
         Label  => "References/Find all references to %e",
         Action => Command);

      Command := new Find_Specific_Refs_Command;
      Register_Contextual_Menu
        (Kernel, "Find references...",
         Label  => "References/Find references to %e...",
         Action => Command);

      Command := new Find_All_Refs_Command;
      Find_All_Refs_Command (Command.all).Locals_Only := True;
      Register_Contextual_Menu
        (Kernel, "Find all local references",
         Label  => "References/Find all local references to %e",
         Action => Command);

      Command := new Edit_Spec_Command;
      Register_Contextual_Menu
        (Kernel, "Go to spec",
         Action => Command,
         Filter => Action_Filter (Create (Module => Call_Graph_Module_Name))
         and Lookup_Filter (Kernel, "Entity"));

      Command := new Edit_Body_Command;
      Register_Contextual_Menu
        (Kernel, "Go to body",
         Action => Command,
         Filter => Action_Filter (Create (Module => Call_Graph_Module_Name))
         and Lookup_Filter (Kernel, "Entity"));

      Register_Menu (Kernel, Tools, -"Call Graph", "", On_Call_Graph'Access);
      Register_Menu
        (Kernel, Navigate, Find_All, "", On_Find_All_References'Access,
         Ref_Item => -"Find Previous", Add_Before => False);

      Register_Command
        (Kernel, "find_all_refs",
         Class        => Get_Entity_Class (Kernel),
         Maximum_Args => 1,
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "references",
         Class        => Get_Entity_Class (Kernel),
         Maximum_Args => 1,
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "calls",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "called_by",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "called_by_browser",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "dump_xref_db",
         Handler      => Xref_Command_Handler'Access);
      Register_Command
        (Kernel, "reset_xref_db",
         Handler      => Xref_Command_Handler'Access);

      Browsers.Canvas.Register_Actions (Kernel);
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
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
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
      Add_Navigation_Location (Callback.Kernel, -"Call graph Browser");

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
