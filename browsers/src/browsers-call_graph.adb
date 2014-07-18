------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.Xref;
with GNAT.Strings;                  use GNAT.Strings;

with Cairo;                         use Cairo;
with Cairo.Region;                  use Cairo.Region;

with Gdk.Event;                     use Gdk.Event;

with Glib;                          use Glib;
with Glib.Object;                   use Glib.Object;

with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Menu;                      use Gtk.Menu;
with Gtk.Widget;                    use Gtk.Widget;

with Gtkada.Canvas;                 use Gtkada.Canvas;
with Gtkada.MDI;                    use Gtkada.MDI;

with Pango.Layout;                  use Pango.Layout;

with Browsers.Canvas;               use Browsers.Canvas;
with Commands.Interactive;          use Commands, Commands.Interactive;
with Generic_Views;
with GPS.Intl;                      use GPS.Intl;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Modules;            use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;         use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;     use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Xref;               use GPS.Kernel.Xref;
with GPS.Kernel;                    use GPS.Kernel;
with Histories;                     use Histories;
with String_Utils;                  use String_Utils;
with Std_Dialogs;                   use Std_Dialogs;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;
with Xref;                          use Xref;

package body Browsers.Call_Graph is
   Me : constant Trace_Handle := Create ("CALL_GRAPH");
   use type GNATCOLL.Xref.Visible_Column;

   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   type Callgraph_Module_Record is new Module_ID_Record with null record;
   overriding procedure Default_Context_Factory
     (Module  : access Callgraph_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

   Automatically_Check_To_Dependencies : constant Boolean := True;
   --  If True, then every time an item is added to the call graph we check,
   --  and if no to dependency exists, the right arrow is not displayed.

   ------------------------
   -- Call graph browser --
   ------------------------

   type Call_Graph_Browser_Record is new
     Browsers.Canvas.General_Browser_Record with null record;

   function Initialize
     (View   : access Call_Graph_Browser_Record'Class)
      return Gtk_Widget;
   --  Initialize the browser, and return the focus widget

   package Callgraph_Views is new Generic_Views.Simple_Views
     (Module_Name            => Call_Graph_Module_Name,
      View_Name              => -"Call Graph Browser",
      Formal_View_Record     => Call_Graph_Browser_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Graphs);
   subtype Call_Graph_Browser is Callgraph_Views.View_Access;

   --------------
   -- Commands --
   --------------

   type Edit_Body_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Edit_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Edit_Spec_Command is new Interactive_Command with record
      Kernel : Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Edit_Spec_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Entity_Calls_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Entity_Calls_All_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Entity_Calls_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Entity_Called_By_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ------------------
   -- Entity items --
   ------------------

   type Entity_Item_Record is new Browsers.Canvas.Arrow_Item_Record
   with record
      Entity : Root_Entity_Ref;
      Refs   : Xref_List;

      Col1_Width : Gint;
   end record;
   type Entity_Item is access all Entity_Item_Record'Class;

   procedure Gtk_New
     (Item                     : out Entity_Item;
      Browser                  : access General_Browser_Record'Class;
      Entity                   : Root_Entity'Class;
      May_Have_To_Dependencies : Boolean);
   --  Create a new entity item.
   --  If May_Have_To_Dependencies is False, the right arrow will not be
   --  displayed in the items.

   procedure Initialize
     (Item                     : access Entity_Item_Record'Class;
      Browser                  : access General_Browser_Record'Class;
      Entity                   : Root_Entity'Class;
      May_Have_To_Dependencies : Boolean);
   --  Internal initialization function

   overriding procedure Destroy (Item : in out Entity_Item_Record);
   --  Free the memory occupied by the item. This is called automatically when
   --  the item is removed from the canvas.

   overriding procedure Contextual_Factory
     (Item    : access Entity_Item_Record;
      Context : in out Selection_Context;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu);
   overriding procedure Compute_Size
     (Item          : not null access Entity_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo_Rectangle_Int);
   overriding procedure Resize_And_Draw
     (Item                        : access Entity_Item_Record;
      Cr                          : Cairo.Cairo_Context;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   function Build
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Item : access Entity_Item_Record'Class;
      Location    : General_Location) return Active_Area_Cb;
   --  Build a callback for links in callgraph items

   type Show_Location_Callback is new Active_Area_Callback with record
      Kernel   : Kernel_Handle;
      Parent   : Entity_Item;
      Location : General_Location;
   end record;
   type Show_Location_Callback_Access
     is access all Show_Location_Callback'Class;
   overriding function Call
     (Callback : Show_Location_Callback;
      Event    : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  See inherited doc

   --------------------
   -- Renaming links --
   --------------------

   type Renaming_Link_Record is new Browsers.Canvas.Browser_Link_Record
     with null record;
   --  The type of link used between an entity and the entities that rename
   --  it.
   --  A renaming link should always be created from the renaming entity to the
   --  renamed entity.

   overriding procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Renaming_Link_Record;
      Cr          : Cairo_Context;
      Edge_Number : Glib.Gint;
      Show_Annotation : Boolean := True);
   --  Override the default drawing procedure for links

   ----------
   -- Misc --
   ----------

   type Examine_Ancestors_Data is new Commands_User_Data_Record with record
      Browser        : Call_Graph_Browser;
      Item           : Entity_Item;
      Link_From_Item : Boolean;
   end record;
   type Examine_Ancestors_Data_Access
     is access all Examine_Ancestors_Data'Class;
   overriding procedure Destroy
     (Data : in out Examine_Ancestors_Data; Cancelled : Boolean);
   overriding function On_Entity_Found
     (Data                : access Examine_Ancestors_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean;
   --  See inherited documentation

   procedure Examine_Entity_Call_Graph
     (Kernel  : access Kernel_Handle_Record'Class;
      Entity  : Root_Entity'Class;
      Refresh : Boolean := True);
   --  Display the call graph for the node.
   --  If Refresh is True, refresh the canvas layout.

   procedure Examine_Ancestors_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class);
   --  Display the list of subprograms that call Entity

   function Find_Entity
     (In_Browser : access General_Browser_Record'Class;
      Entity     : Root_Entity'Class) return Canvas_Item;
   --  Return the child that shows Item_Name in the browser, or null if
   --  Item_Name is not already displayed in the canvas.
   --  ??? Should also have line and column information

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Root_Entity'Class) return Entity_Item;
   --  Add a new entity to the browser, if not already there

   procedure Add_Entity_And_Link
     (Browser             : Call_Graph_Browser;
      Item                : Entity_Item;
      Link_From_Item      : Boolean;
      Entity              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Is_Renaming         : Boolean;
      Through_Dispatching : Boolean);
   --  Add Entity, and possibly a link to Cb.Item to Cb.Browser

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
     (Item                     : out Entity_Item;
      Browser                  : access General_Browser_Record'Class;
      Entity                   : Root_Entity'Class;
      May_Have_To_Dependencies : Boolean) is
   begin
      Item := new Entity_Item_Record;
      Initialize (Item, Browser, Entity, May_Have_To_Dependencies);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item                     : access Entity_Item_Record'Class;
      Browser                  : access General_Browser_Record'Class;
      Entity                   : Root_Entity'Class;
      May_Have_To_Dependencies : Boolean)
   is
      Decl   : constant General_Location := Get_Declaration (Entity).Loc;
      Name   : constant String := Get_Name (Entity);
   begin
      Item.Entity.Replace_Element (Entity);
      Initialize (Item, Browser, Name,
                  Examine_Ancestors_Call_Graph'Access,
                  Examine_Entity_Call_Graph'Access);
      Set_Children_Shown (Item, not May_Have_To_Dependencies);

      if not Is_Predefined_Entity (Entity) then
         Add_Line
           (Item.Refs,
            "(Decl) @"
            & Decl.File.Display_Base_Name
            & ':' & Image (Decl.Line) & '@',
            Callback =>
              (1 => Build (Get_Kernel (Get_Browser (Item)), Item, Decl)));
      else
         Add_Line (Item.Refs, "<Unresolved>");
      end if;

      Recompute_Size (Item);
   end Initialize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Item : in out Entity_Item_Record) is
      Item2    : constant Entity_Item := Item'Unrestricted_Access;
      Browser  : constant Interactive_Canvas :=
        Get_Canvas (Get_Browser (Item2));
      Iter     : Item_Iterator := Start (Browser);
      It       : Canvas_Item;
      Line     : Positive;
      Text     : String_Access;
      Callback : Active_Area_Cb;
      Cb       : Show_Location_Callback_Access;
      Removed  : Boolean;
   begin
      if not Get_Browser (Item2).In_Destruction then
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
               Browser.Refresh (Entity_Item (It));
            end if;

            Next (Iter);
         end loop;
      end if;

      Free (Item.Refs);
      Destroy (Arrow_Item_Record (Item));
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Call_Graph_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Initialize (View, Create_Toolbar => False);
      Register_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Call_Graph_Module_Id,
         Context_Func    => Default_Browser_Context_Factory'Access);
      return Gtk_Widget (View);
   end Initialize;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (In_Browser : access General_Browser_Record'Class;
      Entity     : Root_Entity'Class) return Canvas_Item
   is
      Found : Canvas_Item := null;
      Iter  : Item_Iterator := Start (Get_Canvas (In_Browser));
   begin
      loop
         Found := Get (Iter);

         exit when Found = null
           or else Entity_Item (Found).Entity.Element = Entity;
         Next (Iter);
      end loop;
      return Found;
   end Find_Entity;

   -------------------------------
   -- Add_Entity_If_Not_Present --
   -------------------------------

   function Add_Entity_If_Not_Present
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Root_Entity'Class) return Entity_Item
   is
      Child                    : Entity_Item;
      May_Have_To_Dependencies : Boolean := True;

   begin
      Child := Entity_Item (Find_Entity (Browser, Entity));
      if Child = null then
         if Automatically_Check_To_Dependencies then
            declare
               Iter : Calls_Iterator'Class := Get_All_Called_Entities (Entity);
            begin
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
            end;
         end if;

         Gtk_New (Child, Browser, Entity, May_Have_To_Dependencies);
         Put (Get_Canvas (Browser), Child);
      end if;

      return Child;
   end Add_Entity_If_Not_Present;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel  : access Kernel_Handle_Record'Class;
      Entity  : Root_Entity'Class;
      Refresh : Boolean := True)
   is
      Browser : constant Call_Graph_Browser :=
        Callgraph_Views.Get_Or_Create_View (Kernel, Focus => True);
      Item          : constant Entity_Item :=
                        Add_Entity_If_Not_Present (Browser, Entity);
      Canvas        : constant Interactive_Canvas := Get_Canvas (Browser);

   begin
      if not Children_Shown (Item) then
         Set_Children_Shown (Item, True);

         declare
            Data : constant Examine_Ancestors_Data_Access :=
              new Examine_Ancestors_Data'
                (Commands_User_Data_Record with
                 Browser        => Browser,
                 Item           => Item,
                 Link_From_Item => True);

         begin
            Examine_Entity_Call_Graph
              (Entity            => Entity,
               User_Data         => Data,
               Dispatching_Calls => True,
               Get_All_Refs      => True);

            --  Data is no longer valid now, since it has been destroyed
         end;
      end if;

      if Refresh then
         --  We need to do a layout, so that the newly added item is put at a
         --  correct place.
         Layout (Browser, Force => False);
         Align_Item (Canvas, Item, 0.4, 0.4);
      end if;

      Refresh_Canvas (Canvas);
   end Examine_Entity_Call_Graph;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Data : in out Examine_Ancestors_Data; Cancelled : Boolean)
   is
      pragma Unmodified (Data);
   begin
      if not Cancelled then
         if not Data.Link_From_Item then
            Layout (Data.Browser, Force => False);
         end if;

         Refresh_Canvas (Get_Canvas (Data.Browser));
         Show_Item (Get_Canvas (Data.Browser), Data.Item);
      end if;
   end Destroy;

   -------------------------
   -- Add_Entity_And_Link --
   -------------------------

   procedure Add_Entity_And_Link
     (Browser             : Call_Graph_Browser;
      Item                : Entity_Item;
      Link_From_Item      : Boolean;
      Entity              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Is_Renaming         : Boolean;
      Through_Dispatching : Boolean)
   is
      Child            : Entity_Item;
      Link             : Browser_Link;
      Loc              : General_Location;
      Line             : Natural;
      Text             : String_Access;
      New_Cb, Callback : Active_Area_Cb;
      Changing         : Entity_Item;

      Descr_Dispatching : aliased String := "(dispatch)";
      Descr_Others      : aliased String := "";
      Descr            : String_Access := Descr_Others'Unchecked_Access;
   begin
      Child := Add_Entity_If_Not_Present (Browser, Entity);

      if Through_Dispatching then
         Descr := Descr_Dispatching'Unchecked_Access;
      end if;

      if Link_From_Item then
         if not Has_Link (Get_Canvas (Browser), Item, Child) then
            if Is_Renaming then
               Link := new Renaming_Link_Record;
               Add_Link (Get_Canvas (Browser), Link => Link,
                         Src => Item, Dest => Child, Arrow => Both_Arrow);
            else
               Link := new Browser_Link_Record;
               Add_Link (Get_Canvas (Browser), Link => Link,
                         Src => Item, Dest => Child);
            end if;
         end if;
      else
         if not Has_Link (Get_Canvas (Browser), Child, Item) then
            if Is_Renaming then
               Link := new Renaming_Link_Record;
               Add_Link (Get_Canvas (Browser), Link => Link,
                         Src => Child, Dest => Item, Arrow => Both_Arrow);
            else
               Link := new Browser_Link_Record;
               Add_Link (Get_Canvas (Browser), Link => Link,
                         Src => Child, Dest => Item);
            end if;
         end if;
      end if;

      Loc := Get_Location (Ref);

      --  Always skip the first line, which is the declaration location
      Line := 2;

      if Link_From_Item then
         Changing := Child;
         Child    := Item;
      else
         Changing := Item;
      end if;

      New_Cb := Build (Get_Kernel (Browser), Child, Loc);

      loop
         Get_Line (Changing.Refs, Line, 1, Callback, Text);
         exit when Text = null;

         if Show_Location_Callback_Access (Callback).Parent = Child then
            Expand_Line
              (Changing.Refs, Line,
               " @" & Image (Loc.Line)
               & ':' & Image (Integer (Loc.Column)) & '@' & Descr.all,
               (1 => New_Cb),
               Check_Duplicates => True);
            return;
         end if;

         Line := Line + 1;
      end loop;

      Add_Line
        (Changing.Refs,
         Qualified_Name (Child.Entity.Element)
         & ": @"
         & Image (Loc.Line) & ':'
         & Image (Integer (Loc.Column)) & '@' & Descr.all,
         Callback => (1 => New_Cb));
   end Add_Entity_And_Link;

   ---------------------
   -- On_Entity_Found --
   ---------------------

   overriding function On_Entity_Found
     (Data                : access Examine_Ancestors_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean
   is
   begin
      if Data.Link_From_Item then
         Add_Entity_And_Link
           (Data.Browser, Data.Item, Data.Link_From_Item,
            Entity, Ref, Is_Renaming, Through_Dispatching);
      else
         Add_Entity_And_Link
           (Data.Browser, Data.Item, Data.Link_From_Item,
            Parent, Ref, Is_Renaming, Through_Dispatching);
      end if;

      return True;
   end On_Entity_Found;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class)
   is
      Browser : constant Call_Graph_Browser :=
        Callgraph_Views.Get_Or_Create_View (Kernel, Focus => True);
      Data          : Examine_Ancestors_Data_Access;
   begin
      Data := new Examine_Ancestors_Data'
        (Commands_User_Data_Record with
         Browser        => Browser,
         Item           => null,
         Link_From_Item => False);
      Data.Item := Add_Entity_If_Not_Present (Data.Browser, Entity);
      Set_Parents_Shown (Data.Item, True);

      Examine_Ancestors_Call_Graph
        (Kernel            => Kernel,
         Entity            => Entity,
         User_Data         => Data,
         Background_Mode   => True,
         Dispatching_Calls => True,
         Watch             => Gtk_Widget (Data.Browser));
   end Examine_Ancestors_Call_Graph;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Ancestors_Call_Graph
        (Get_Kernel (Get_Browser (Item)), Entity_Item (Item).Entity.Element);
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Entity_Call_Graph
        (Get_Kernel (Get_Browser (Item)), Entity_Item (Item).Entity.Element);
   end Examine_Entity_Call_Graph;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   overriding procedure Contextual_Factory
     (Item    : access Entity_Item_Record;
      Context : in out Selection_Context;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event, Menu, Browser);
      Loc : General_Location;
   begin
      if not Is_Predefined_Entity (Item.Entity.Element) then
         Loc := Get_Declaration (Item.Entity.Element).Loc;
         Set_File_Information
           (Context,
            Files => (1 => Loc.File),
            Line  => Loc.Line);
      end if;

      Set_Entity_Information (Context, Entity => Item.Entity.Element);

   exception
      when E : others =>
         Trace (Me, E);
   end Contextual_Factory;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Callgraph_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject)
   is
      pragma Unreferenced (Module);
      Browser : constant Call_Graph_Browser :=
        Callgraph_Views.View_From_Widget (Child);
      Iter    : constant Item_Iterator :=
        Start (Get_Canvas (Browser), Selected_Only => True);
   begin
      --  If there is no selection, or more than one item, nothing we can do
      if Get (Iter) /= null
        and then Get (Next (Iter)) = null
      then
         Contextual_Factory
           (Context => Context,
            Item    => Browser_Item (Get (Iter)),
            Browser => Browser,
            Event   => null,
            Menu    => null);
      end if;
   end Default_Context_Factory;

   --------------------------------
   -- Call_Graph_Command_Handler --
   --------------------------------

   procedure Call_Graph_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Data);
      Entity    : constant Root_Entity'Class := Get_Data (Data, 1);
   begin
      if Command = "called_by_browser" then
         Examine_Ancestors_Call_Graph (Kernel, Entity);
      end if;
   end Call_Graph_Command_Handler;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node_Entity : constant Root_Entity'Class := Get_Entity (Context.Context);
   begin
      if Node_Entity /= No_Root_Entity then
         --  ??? Should check that Decl.Kind is a subprogram
         Examine_Entity_Call_Graph (Get_Kernel (Context.Context), Node_Entity);
      else
         Insert (Get_Kernel (Context.Context),
                 -"No call graph available for "
                 & Entity_Name_Information (Context.Context));
      end if;

      return Commands.Success;

   exception
      when E : others =>
         Insert (Get_Kernel (Context.Context),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Context.Context),
                 Mode => Error);
         Trace (Me, E);
      return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Entity_Calls_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Key         : constant Histories.History_Key := "Call_Graph_Limit";
      Kernel      : constant Kernel_Handle := Get_Kernel (Context.Context);
      History     : constant Histories.History := Get_History (Kernel);
      Max_Items   : Natural := 1000;

   begin
      if Histories.Get_History (History.all, Key) = null then
         Histories.Add_To_History (History.all, Key, "1000");
      end if;

      declare
         Str  : constant String :=
           Simple_Entry_Dialog
             (Get_Main_Window (Kernel),
              -"Complete Call Graph",
              -("Computing complete call graph may take a long time." &
                ASCII.LF & "Enter maximum number of items to display: "),
              Win_Pos_Mouse, History, Key);
      begin
         if Str /= "" and then Str (Str'First) /= ASCII.NUL then
            Max_Items := Integer'Value (Str);
         else
            return Commands.Failure;
         end if;
      exception
         when others =>
            return Commands.Failure;
      end;

      declare
         Node_Entity : constant Root_Entity'Class :=
           Get_Entity (Context.Context);
      begin

         if Node_Entity /= No_Root_Entity then
            --  ??? Should check that Decl.Kind is a subprogram

            declare
               Browser   : constant Call_Graph_Browser :=
                 Callgraph_Views.Get_Or_Create_View (Kernel, Focus => True);
               Canvas    : constant Interactive_Canvas := Get_Canvas (Browser);
               Item      : Entity_Item;
               Iter      : Item_Iterator;
               Count     : Integer := 1;
            begin
               Examine_Entity_Call_Graph
                 (Kernel, Node_Entity, Refresh => False);

               Main_Loop : loop
                  Iter := Start (Canvas);

                  loop
                     Item := Entity_Item (Get (Iter));

                     exit Main_Loop when Item = null;

                     if not Children_Shown (Item) then
                        Examine_Entity_Call_Graph
                          (Kernel, Item.Entity.Element, Refresh => False);
                        --  Iter may no longer be valid, so start again
                        exit;
                     end if;

                     Next (Iter);
                  end loop;

                  Count := Count + 1;
                  exit Main_Loop when Count > Max_Items;
               end loop Main_Loop;

               Layout (Browser, Force => True);
               Refresh_Canvas (Canvas);

            exception
               when E    : others =>
                  Insert (Get_Kernel (Context.Context),
                          -"Internal error when creating the call graph for "
                          & Entity_Name_Information (Context.Context),
                          Mode => Error);
                  Trace (Me, E);
                  return Commands.Failure;
            end;

         else
            Insert (Get_Kernel (Context.Context),
                    -"No call graph available for "
                    & Entity_Name_Information (Context.Context));
         end if;
      end;

      return Commands.Success;

   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   overriding function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Info : constant Root_Entity'Class := Get_Entity (Context.Context);
   begin
      if Info /= No_Root_Entity then
         Examine_Ancestors_Call_Graph (Get_Kernel (Context.Context), Info);
      else
         Insert (Get_Kernel (Context.Context),
                 -"No information found for the file "
                 & Display_Full_Name
                   (File_Information (Context.Context)),
                 Mode => Error);
      end if;

      return Commands.Success;

   exception
      when E : others =>
         Insert (Get_Kernel (Context.Context),
                 -"Internal error when creating the call graph for "
                 & Entity_Name_Information (Context.Context),
                 Mode => Error);
         Trace (Me, E);
      return Commands.Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Location : General_Location;
   begin
      Location := Get_Body (Get_Entity (Context.Context));

      Add_Navigation_Location
        (Get_Kernel (Context.Context), -"Call graph Browser");

      if Location /= No_Location then
         Open_File_Editor
           (Get_Kernel (Context.Context),
            Filename => Location.File,
            Project  => Location.Project,
            Line     => Location.Line,
            Column   => Location.Column);
      else
         --  If the body wasn't found then display the specs
         Open_File_Editor
           (Get_Kernel (Context.Context),
            Filename => File_Information (Context.Context),
            Project  => Project_Information (Context.Context),
            Line     => Line_Information (Context.Context),
            Column   => Entity_Column_Information (Context.Context));
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Spec_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Entity : constant Root_Entity'Class := Get_Entity (Context.Context);
      Loc    : General_Location;
   begin
      if Entity = No_Root_Entity then
         Insert (Get_Kernel (Context.Context),
                 (-"Couldn't find cross-reference information for ")
                 & '"' & Entity_Name_Information (Context.Context) & '"');
      else
         Add_Navigation_Location
           (Get_Kernel (Context.Context), -"Call graph Browser");

         Loc := Get_Declaration (Entity).Loc;

         Open_File_Editor
           (Get_Kernel (Context.Context),
            Filename => Loc.File,
            Project  => Loc.Project,
            Line     => Loc.Line,
            Column   => Loc.Column);
      end if;
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command  : Interactive_Command_Access;
   begin
      Call_Graph_Module_Id := new Callgraph_Module_Record;
      Callgraph_Views.Register_Module
        (Kernel,
         ID        => Call_Graph_Module_Id);

      Command := new Entity_Calls_Command;
      Register_Contextual_Menu
        (Kernel, "Entity calls in browser",
         Label  => "Browsers/%e calls",
         Filter => Lookup_Filter (Kernel, "Entity is subprogram"),
         Action => Command);

      Command := new Entity_Calls_All_Command;
      Register_Contextual_Menu
        (Kernel, "Entity calls (recursively) in browser",
         Label  => "Browsers/%e calls (recursively)",
         Filter => Lookup_Filter (Kernel, "Entity is subprogram"),
         Action => Command);

      Command := new Entity_Called_By_Command;
      Register_Contextual_Menu
        (Kernel, "Entity called by in browser",
         Label  => "Browsers/%e is called by",
         Filter => Lookup_Filter (Kernel, "Entity is subprogram"),
         Action => Command);

      Command := new Edit_Spec_Command;
      Edit_Spec_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Contextual_Menu
        (Kernel, "Go to spec",
         Action => Command,
         Filter => Create (Module => Call_Graph_Module_Name)
         and Lookup_Filter (Kernel, "Entity"));

      Command := new Edit_Body_Command;
      Edit_Body_Command (Command.all).Kernel := Kernel_Handle (Kernel);
      Register_Contextual_Menu
        (Kernel, "Go to body",
         Action => Command,
         Filter => Create (Module => Call_Graph_Module_Name)
         and Lookup_Filter (Kernel, "Entity"));

      Register_Command
        (Kernel, "called_by_browser",
         Class   => Get_Entity_Class (Kernel),
         Handler => Call_Graph_Command_Handler'Access);
   end Register_Module;

   ---------------
   -- Draw_Link --
   ---------------

   overriding procedure Draw_Link
     (Canvas      : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Link        : access Renaming_Link_Record;
      Cr          : Cairo_Context;
      Edge_Number : Glib.Gint;
      Show_Annotation : Boolean := True)
   is
   begin
      Cairo.Save (Cr);
      Cairo.Set_Dash (Cr, (1 .. 2 => 2.0), 0.0);
      Cairo.Set_Line_Cap (Cr, Cairo_Line_Cap_Butt);
      Cairo.Set_Line_Join (Cr, Cairo_Line_Join_Miter);

      Draw_Link
        (Canvas, Browser_Link_Record (Link.all)'Access,
         Cr, Edge_Number, Show_Annotation);

      Cairo.Restore (Cr);
   end Draw_Link;

   -----------
   -- Build --
   -----------

   function Build
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Item : access Entity_Item_Record'Class;
      Location    : General_Location) return Active_Area_Cb is
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

   overriding function Call
     (Callback : Show_Location_Callback;
      Event    : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Add_Navigation_Location (Callback.Kernel, -"Call graph Browser");

      Open_File_Editor
        (Callback.Kernel,
         Filename => Callback.Location.File,
         Project  => Callback.Location.Project,
         Line     => Callback.Location.Line,
         Column   => Callback.Location.Column);
      return True;
   end Call;

   ------------------
   -- Compute_Size --
   ------------------

   overriding procedure Compute_Size
     (Item          : not null access Entity_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo_Rectangle_Int)
   is
      Ref_W1, Ref_W2, Ref_H : Gint;
   begin
      Get_Pixel_Size
        (Get_Browser (Item), Item.Refs, Ref_W1, Ref_W2, Ref_H,  Layout);
      Item.Col1_Width := Ref_W1;

      Width := Gint'Max (Ref_W1 + Ref_W2 + 2 * Margin, Title_Box.Width);
      Title_Box.Width := Width;
      Height := Ref_H;
   end Compute_Size;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item                        : access Entity_Item_Record;
      Cr                          : Cairo_Context;
      Width, Height               : Glib.Gint;
      Width_Offset, Height_Offset : Glib.Gint;
      Xoffset, Yoffset            : in out Glib.Gint;
      Layout                  : access Pango.Layout.Pango_Layout_Record'Class)
   is
      Y : Gint;
   begin
      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access, Cr, Width, Height,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Yoffset + 1;
      Display_Lines
        (Item, Cr, Item.Refs, Margin + Xoffset, Y, Item.Col1_Width, Layout);
   end Resize_And_Draw;

end Browsers.Call_Graph;
