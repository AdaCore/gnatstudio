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

with Ada.Unchecked_Deallocation;

with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.Xref;
with GNAT.Strings;                  use GNAT.Strings;

with Cairo;                         use Cairo;
with Cairo.Region;                  use Cairo.Region;

with Gdk.Event;                     use Gdk.Event;

with Glib;                          use Glib;
with Glib.Object;                   use Glib.Object;

with Gtk.Box;                       use Gtk.Box;
with Gtk.Button;                    use Gtk.Button;
with Gtk.Check_Button;              use Gtk.Check_Button;
with Gtk.Dialog;                    use Gtk.Dialog;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Frame;                     use Gtk.Frame;
with Gtk.Menu;                      use Gtk.Menu;
with Gtk.Radio_Button;              use Gtk.Radio_Button;
with Gtk.Stock;                     use Gtk.Stock;
with Gtk.Vbutton_Box;               use Gtk.Vbutton_Box;
with Gtk.Widget;                    use Gtk.Widget;

with Gtkada.Canvas;                 use Gtkada.Canvas;
with Gtkada.Handlers;               use Gtkada.Handlers;
with Gtkada.MDI;                    use Gtkada.MDI;

with Pango.Layout;                  use Pango.Layout;

with Basic_Types;

with Browsers.Canvas;               use Browsers.Canvas;
with Commands.Generic_Asynchronous; use Commands;
with Commands.Interactive;          use Commands.Interactive;
with Generic_Views;
with GPS.Intl;                      use GPS.Intl;
with GPS.Kernel.Actions;            use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Messages;           use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Markup;    use GPS.Kernel.Messages.Markup;
with GPS.Kernel.Messages.Simple;    use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;            use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;         use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;            use GPS.Kernel.Project;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;     use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Styles;             use GPS.Kernel.Styles;
with GPS.Kernel.Task_Manager;       use GPS.Kernel.Task_Manager;
with GPS.Kernel.Xref;               use GPS.Kernel.Xref;
with GPS.Styles;                    use GPS.Styles;
with GPS.Styles.UI;                 use GPS.Styles.UI;
with GPS.Kernel;                    use GPS.Kernel;
with GPS.Scripts.Commands;          use GPS.Scripts.Commands;
with Histories;                     use Histories;
with String_Utils;                  use String_Utils;
with Std_Dialogs;                   use Std_Dialogs;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;
with Xref;                          use Xref;
with UTF8_Utils;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package body Browsers.Call_Graph is
   Me : constant Trace_Handle := Create ("CALL_GRAPH");
   use type GNATCOLL.Xref.Visible_Column;

   References_Command_Class_Name : constant String := "ReferencesCommand";
   --  Name of the class for shell commands associated with this package

   Call_Graph_Message_Flags : constant Message_Flags :=
     (Editor_Side => True,
      Locations   => True);
   --  Visibility of call graph's messages in the system at whole

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

   Locations_At_A_Time : constant := 20;
   --  Number of locations that will be inserted in the locations view in
   --  each idle processing.

   Include_Implicit_Cst : aliased constant String := "include_implicit";
   Synchronous_Cst      : aliased constant String := "synchronous";
   Show_Kind_Cst        : aliased constant String := "show_kind";
   In_File_Cst          : aliased constant String := "in_file";
   Dispatching_Calls_Cst : aliased constant String := "dispatching_calls";
   Kind_In_Cst          : aliased constant String := "kind_in";
   References_Cmd_Parameters : constant Cst_Argument_List :=
     (1 => Include_Implicit_Cst'Access,
      2 => Synchronous_Cst'Access,
      3 => Show_Kind_Cst'Access,
      4 => In_File_Cst'Access,
      5 => Kind_In_Cst'Access);

   type Filters_Buttons is array (Natural range <>) of Gtk_Check_Button;
   type Filters_Buttons_Access is access Filters_Buttons;
   type References_Filter_Dialog_Record is new Gtk_Dialog_Record with record
      Filters : Filters_Buttons_Access;
   end record;
   type References_Filter_Dialog is access all
     References_Filter_Dialog_Record'Class;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Filters_Buttons, Filters_Buttons_Access);

   function All_Refs_Category
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean) return String;
   --  Return the category title when doing a find all refs on a given entity.
   --  If All_From_Same_File is true, we will in fact list all entities
   --  imported form the same file as Entity.
   --  If Local_Only is true, then the references are only in the current file

   type Callback_Data_Access is access all Callback_Data'Class;
   type Add_To_List_User_Data
     is new Commands_User_Data_Record with
      record
         Data : Callback_Data_Access;

         Use_Parent_For_Key : Boolean := True;
         --  If this is True, then Parent should be used as the key for entries
         --  in the list. Otherwise, Entity will be used.
      end record;
   type Add_To_List_User_Data_Access is access all Add_To_List_User_Data'Class;
   overriding function On_Entity_Found
     (D                   : access Add_To_List_User_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean;
   --  See inherited documentation.
   --  Add a new entity to the returned value in D.Data.

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

   -------------
   -- Filters --
   -------------

   type Custom_Filter is record
      Db : General_Xref_Database;

      Ref_Kinds : GNAT.Strings.String_List_Access;
      --  The reference kinds' name that should be displayed, or none for all.
      --  Any null value is ignored in this array.

      Filter : Reference_Kind_Filter;
      --  One of the predefined filters
   end record;
   function Is_Valid
     (Self : Custom_Filter; Ref : Root_Entity_Reference'Class) return Boolean;
   procedure Free (Self : in out Custom_Filter);

   --------------
   -- Commands --
   --------------

   type Subprogram_Entity_Filter is new Action_Filter_Record with record
      Kernel : Kernel_Handle;
   end record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Entity_Filter;
      Context : Selection_Context) return Boolean;

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

   type Find_All_Refs_Command is new Interactive_Command with record
      Locals_Only     : Boolean := False;
      Recurse_Project : Boolean := True;
      Writes_Only     : Boolean := False;
      Reads_Only      : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Find_Specific_Refs_Command
     is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

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

   package Entity_Ref_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Root_Entity_Reference'Class);
   use Entity_Ref_List;

   type References_Command is new Root_Command with record
      Kernel        : Kernel_Handle;
      Iter          : Xref.Root_Reference_Iterator_Ref;
      Locations     : Entity_Ref_List.List;
      Show_Ref_Kind : Boolean;
   end record;
   type References_Command_Access is access all References_Command'Class;
   overriding function Execute
     (Command : access References_Command) return Command_Return_Type;
   overriding procedure Free (Command : in out References_Command);

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

   type Entity_Idle_Data is record
      Kernel             : Kernel_Handle;
      Iter               : Root_Reference_Iterator_Access;
      Entity             : Root_Entity_Ref;
      Filter             : Custom_Filter;
      Iter_Started       : Boolean;
      Show_Caller        : Boolean;
      Category           : String_Access;
      Include_Overriding : Boolean;
      Count              : Natural := 0;
   end record;

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

   procedure Parse_All_Refs
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Locals_Only        : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False);
   --  Internal implementation of find_all_references.
   --  If All_From_Same_File is True, then all entities imported from the same
   --  file as Entity and referenced in Local_File, as Entity are
   --  displayed.
   --  This procedure will free Filter.

   procedure Find_All_References_Internal
     (Kernel             : access Kernel_Handle_Record'Class;
      Info               : Root_Entity'Class;
      Category_Title     : String;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False);
   --  Internal implementation for Find_All_References_From_Contextual,
   --  Find_All_Writes_From_Contextual and Find_All_Reads_From_Contextual.
   --  Starts a background search for all references.
   --  This procedure will free Filter.

   procedure Find_Next_Reference
     (Data    : in out Entity_Idle_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Find the next reference to the entity in D

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

   procedure Destroy_Idle (Data : in out Entity_Idle_Data);
   --  Called when the idle loop is destroyed

   package Xref_Commands is new Commands.Generic_Asynchronous
     (Entity_Idle_Data, Destroy_Idle);

   procedure Print_Ref
     (Kernel       : access Kernel_Handle_Record'Class;
      Ref          : Root_Entity_Reference'Class;
      Name         : String;
      Category     : String;
      Show_Caller  : Boolean;
      Sort_In_File : Boolean);
   --  Display a reference in the locations tree, after looking for the
   --  directory containing File.
   --  Category corresponds to the purpose of the print. All references
   --  corresponding to the same category will be printed as a group.
   --  If Show_Caller is true, the full name of the caller will also be
   --  displayed.
   --  If Sort_In_File is true, then the new entry is inserted before the first
   --  entry with a higher line number

   procedure Call_Graph_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure References_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handle shell commands related of ReferencesCommand class

   procedure Put_Locations_In_Return
     (Command       : access References_Command'Class;
      Data          : in out Callback_Data'Class;
      Show_Ref_Kind : Boolean);
   --  Put on the result of Data the list of entities found in the command

   procedure Examine_Ancestors_Call_Graph
     (Item : access Arrow_Item_Record'Class);
   procedure Examine_Entity_Call_Graph
     (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the title bar buttons

   procedure Unselect_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class);
   --  Select or unselect all filters in "Find references..."

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Custom_Filter) is
   begin
      GNAT.Strings.Free (Self.Ref_Kinds);  --  also frees All_Refs
   end Free;

   -----------------------
   -- All_Refs_Category --
   -----------------------

   function All_Refs_Category
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Local_Only         : Boolean;
      Local_File         : GNATCOLL.VFS.Virtual_File;
      All_From_Same_File : Boolean) return String
   is
      pragma Unreferenced (Kernel);
      Decl : constant General_Location := Get_Declaration (Entity).Loc;
   begin
      if All_From_Same_File then
         return -"Entities imported from "
           & Krunch (+Decl.File.Base_Name)
           & (-" into ")
           & Krunch (+Local_File.Base_Name);

      elsif Local_Only then
         return -"Local references for "
           & Get_Name (Entity)
           & " ("  & Krunch (+Decl.File.Base_Name)
           & ":" & Image (Decl.Line) & ") " & (-"in ")
           & Krunch (+Local_File.Base_Name);

      else
         return -"References for "
           & Get_Name (Entity)
           & " ("  & Krunch (+Decl.File.Base_Name)
           & ":" & Image (Decl.Line) & ")";
      end if;
   end All_Refs_Category;

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
      Ref (Entity);
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

      declare
         V : Root_Entity'Class := Item.Entity.Element;
      begin
         Unref (V);
      end;
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

   ----------
   -- Free --
   ----------

   overriding procedure Free (Command : in out References_Command)
   is
      El : Root_Reference_Iterator'Class := Command.Iter.Element;
   begin
      Destroy (El);
   end Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access References_Command) return Command_Return_Type
   is
      Iter_Ref : constant Root_Reference_Iterator_Refs.Reference_Type :=
        Command.Iter.Reference;
   begin
      for J in 1 .. 15 loop
         exit when At_End (Iter_Ref);
         declare
            Ref : constant Root_Entity_Reference'Class :=
              Get (Iter_Ref);
         begin
            if Ref /= No_Root_Entity_Reference then
               Command.Locations.Append (Ref);
            end if;
         end;

         Next (Iter_Ref);
      end loop;

      Set_Progress
        (Command,
         (Activity => Unknown,
          Current  => Get_Current_Progress (Iter_Ref),
          Total    => Get_Total_Progress (Iter_Ref)));

      if At_End (Iter_Ref) then
         Set_Progress
           (Command,
            (Activity => Unknown,
             Current  => Get_Total_Progress (Iter_Ref),
             Total    => Get_Total_Progress (Iter_Ref)));

         return Success;
      else
         return Execute_Again;
      end if;
   end Execute;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Entity_Idle_Data) is
      V : Root_Entity'Class := Data.Entity.Element;
   begin
      Free (Data.Filter);
      Destroy (Data.Iter);
      Unref (V);
      Free (Data.Category);
   end Destroy_Idle;

   ---------------
   -- Print_Ref --
   ---------------

   procedure Print_Ref
     (Kernel       : access Kernel_Handle_Record'Class;
      Ref          : Root_Entity_Reference'Class;
      Name         : String;
      Category     : String;
      Show_Caller  : Boolean;
      Sort_In_File : Boolean)
   is
      pragma Unreferenced (Sort_In_File);
      use Basic_Types;

      Loc     : constant General_Location := Get_Location (Ref);
      Col     : Basic_Types.Visible_Column_Type := Loc.Column;
      Line    : constant Integer      := Loc.Line;
      File    : constant Virtual_File := Loc.File;
      Message : Markup_Message_Access;

   begin
      if Col <= 0 then
         Col := 1;
      end if;

      if Show_Caller and then Get_Caller (Ref) /= No_Root_Entity then
         Message :=
           Create_Markup_Message
             (Get_Messages_Container (Kernel),
              Category,
              File,
              Line,
              Col,
              "<b>" & Name & "</b> ["
              & Get_Display_Kind (Ref) & "] in: "
              & Qualified_Name (Get_Caller (Ref)),
              0,
              Call_Graph_Message_Flags);

      else
         Message :=
           Create_Markup_Message
             (Get_Messages_Container (Kernel),
              Category,
              File,
              Line,
              Col,
              "<b>" & Name & "</b> [" & Get_Display_Kind (Ref) & "]",
              0,
              Call_Graph_Message_Flags);
      end if;

      Message.Set_Highlighting
        (Get_Or_Create_Style_Copy
           (Kernel_Handle (Kernel),
            Get_Name (Search_Results_Style) & '/' & Category,
            Search_Results_Style),
         UTF8_Utils.UTF8_Length (Name));
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
      Search_Entity : constant Root_Entity'Class := Data.Entity.Element;
      Name  : constant String := Search_Entity.Get_Name;
   begin
      Result := Execute_Again;

      if not Data.Iter_Started then
         Data.Iter.all := Find_All_References
           (Entity             => Data.Entity.Element,
            Include_Overriding => Data.Include_Overriding,
            Include_Overridden => Data.Include_Overriding);

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
            Result := Success;
            exit;

         else
            declare
               Ref : constant Root_Entity_Reference'Class :=
                 Get (Data.Iter.all);
            begin
               --  Not done parsing all the files yet
               if Ref = No_Root_Entity_Reference then
                  Next (Data.Iter.all);
                  exit;

               elsif Is_Valid (Data.Filter, Ref) then
                  Data.Count := Data.Count + 1;
                  Print_Ref
                    (Data.Kernel,
                     Ref,
                     (if Get_Entity (Ref) = Search_Entity
                      then Name
                      else Get_Entity (Ref).Get_Name),
                     Data.Category.all,
                     Show_Caller  => Data.Show_Caller,
                     Sort_In_File => False);

                  if Data.Count = 1 then
                     Raise_Locations_Window (Data.Kernel, Give_Focus => False);
                  end if;
               end if;

               Count := Count + 1;
            end;
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
     (Kernel             : access Kernel_Handle_Record'Class;
      Info               : Root_Entity'Class;
      Category_Title     : String;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False)
   is
      Data : Entity_Idle_Data;
      C    : Xref_Commands.Generic_Asynchronous_Command_Access;
      H    : Root_Entity_Ref;
   begin
      if Info /= No_Root_Entity then
         begin
            Get_Messages_Container (Kernel).Remove_Category
              (Category_Title, Call_Graph_Message_Flags);

            Ref (Info);
            H.Replace_Element (Info);
            Data := (Kernel             => Kernel_Handle (Kernel),
                     Iter               => new Entity_Reference_Iterator,
                     Filter             => Filter,
                     Category           => new String'(Category_Title),
                     Iter_Started       => False,
                     Show_Caller        => Show_Caller,
                     Include_Overriding => Include_Overriding,
                     Count              => 0,
                     Entity             => H);

            Trace (Me, "MAN Find_All_References_Internal, starting bg");
            Xref_Commands.Create  --  Will destroy Data when done
              (C, -"Find all refs", Data, Find_Next_Reference'Access);
            Launch_Background_Command
              (Kernel, Command_Access (C), True, True, "xrefs");

         exception
            when E : others =>
               Trace (Me, E);
               Destroy (Data.Iter);
         end;
      else
         Free (Filter);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Find_All_References_Internal;

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

   -----------------------
   -- Is_Read_Reference --
   -----------------------

   function Is_Read_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
      (Ref.Is_Read_Reference);

   ------------------------
   -- Is_Write_Reference --
   ------------------------

   function Is_Write_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Write_Reference);

   --------------------------------
   -- Is_Read_Or_Write_Reference --
   --------------------------------

   function Is_Read_Or_Write_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Read_Or_Write_Reference);

   -----------------------------------
   -- Is_Read_Or_Implicit_Reference --
   -----------------------------------

   function Is_Read_Or_Implicit_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
      (Ref.Is_Read_Or_Implicit_Reference);

   --------------------------------------------
   -- Is_Read_Or_Write_Or_Implicit_Reference --
   --------------------------------------------

   function Is_Read_Or_Write_Or_Implicit_Reference
     (Ref : Root_Entity_Reference'Class) return Boolean is
     (Ref.Is_Read_Or_Write_Or_Implicit_Reference);

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle :=
        Get_Kernel (Call_Graph_Module_Id.all);
      Filter  : Custom_Filter;
   begin
      if Context.Context /= No_Context then
         declare
            Entity : constant Root_Entity'Class :=
              Get_Entity (Context.Context);
         begin
            if Entity /= No_Root_Entity then
               Filter                  := Custom_Filter'
                 (Db                 => Kernel.Databases,
                  Ref_Kinds          => null,
                  Filter             => null);

               if Command.Reads_Only then
                  Filter.Filter := Is_Read_Reference'Access;
               elsif Command.Writes_Only then
                  Filter.Filter := Is_Write_Reference'Access;
               else
                  Filter.Filter := Is_Read_Or_Write_Reference'Access;
               end if;

               Parse_All_Refs
                 (Kernel             => Kernel,
                  Entity             => Entity,
                  Locals_Only        => Command.Locals_Only,
                  Local_File         => File_Information (Context.Context),
                  All_From_Same_File => False,
                  Filter             => Filter,
                  Show_Caller        => True);
            end if;
            return Commands.Success;
         end;
      else
         Kernel.Insert
           (-"Cannot find references: no entity selected",
            Mode => Error);
         return Commands.Failure;
      end if;
   end Execute;

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

   ---------------------
   -- On_Entity_Found --
   ---------------------

   overriding function On_Entity_Found
     (D                   : access Add_To_List_User_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean
   is
      pragma Unreferenced (Through_Dispatching);
      Loc : General_Location;
   begin
      if not Is_Renaming then
         Loc := Get_Location (Ref);
         Set_Return_Value
           (D.Data.all,
            Create_File_Location
              (Get_Script (D.Data.all),
               Create_File (Get_Script (D.Data.all), Loc.File),
               Loc.Line,
               Loc.Column));
      else
         Set_Return_Value (D.Data.all, -"<renaming>");
      end if;

      if D.Use_Parent_For_Key then
         Set_Return_Value_Key
           (D.Data.all,
            Create_Entity (Get_Script (D.Data.all), Parent), Append => True);
      else
         Set_Return_Value_Key
           (D.Data.all,
            Create_Entity (Get_Script (D.Data.all), Entity), Append => True);
      end if;
      return True;
   end On_Entity_Found;

   --------------------------------
   -- Call_Graph_Command_Handler --
   --------------------------------

   procedure Call_Graph_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Data);
      Entity    : constant Root_Entity'Class := Get_Data (Data, 1);
      Filter    : Custom_Filter;
      User_Data : Add_To_List_User_Data_Access;
   begin
      if Command = "find_all_refs" then
         Name_Parameters (Data, References_Cmd_Parameters);

         Filter := Custom_Filter'
           (Db        => Kernel.Databases,
            Ref_Kinds => null,
            Filter    => Is_Read_Or_Implicit_Reference'Access);

         if Nth_Arg (Data, 2, False) then
            Filter.Filter := Is_Read_Or_Write_Or_Implicit_Reference'Access;
         end if;

         Find_All_References_Internal
           (Kernel, Entity,
            Category_Title   => All_Refs_Category
              (Entity             => Entity,
               Kernel             => Kernel,
               Local_Only         => False,
               Local_File         => GNATCOLL.VFS.No_File,
               All_From_Same_File => False),
            Show_Caller      => False,
            Filter           => Filter);

      elsif Command = "references" then
         Name_Parameters (Data, References_Cmd_Parameters);

         declare
            Ref_Command : References_Command_Access := new References_Command;
            References_Command_Class : constant Class_Type := New_Class
              (Get_Kernel (Data),
               References_Command_Class_Name,
               New_Class (Get_Kernel (Data), "Command"));

            Implicit         : constant Boolean := Nth_Arg (Data, 2, False);
            Synchronous      : constant Boolean := Nth_Arg (Data, 3, True);
            Show_Ref_Type    : constant Boolean := Nth_Arg (Data, 4, False);
            Inst_In_File     : constant Class_Instance :=
              Nth_Arg (Data, 5, Get_File_Class (Get_Kernel (Data)),
                       Allow_Null => True);
            Only_If_Kind     : constant String := Nth_Arg (Data, 6, "");
            In_File          : Virtual_File := No_File;
            Instance         : Class_Instance;
            Launched_Command : Scheduled_Command_Access;
         begin
            Ref_Command.Kernel := Kernel;
            Ref_Command.Show_Ref_Kind := Show_Ref_Type;

            if Inst_In_File /= No_Class_Instance then
               In_File := Get_Data (Inst_In_File);
            end if;

            Ref_Command.Iter.Replace_Element
              (Find_All_References
                 (Entity                => Entity,
                  In_File               => In_File,
                  Include_Implicit      => Implicit,
                  Include_All           => False,
                  Kind                  => Only_If_Kind,
                  File_Has_No_LI_Report => null));

            if Synchronous then
               --  Synchronous, return directly the result

               Launch_Synchronous (Ref_Command);
               Put_Locations_In_Return (Ref_Command, Data, Show_Ref_Type);
               Unref (Command_Access (Ref_Command));

            else
               --  Not synchronous, return a command

               Launched_Command := Launch_Background_Command
                 (Kernel          => Kernel,
                  Command         => Ref_Command,
                  Active          => False,
                  Show_Bar        => False,
                  Destroy_On_Exit => True);

               Set_Progress
                 (Ref_Command,
                  (Activity => Unknown,
                   Current  => Get_Current_Progress (Ref_Command.Iter.Element),
                   Total    => Get_Total_Progress (Ref_Command.Iter.Element)));

               Instance := Get_Instance
                 (Launched_Command,
                  Get_Script (Data),
                  References_Command_Class);

               Set_Return_Value (Data, Instance);
            end if;
         end;

      elsif Command = "calls" then
         Name_Parameters (Data, (1 => Dispatching_Calls_Cst'Access));

         --  The following unchecked_access is safe since
         --  Examine_Entity_Call_Graph is called synchronously
         User_Data := new Add_To_List_User_Data;
         User_Data.Data := Data'Unchecked_Access;
         User_Data.Use_Parent_For_Key := False;
         Examine_Entity_Call_Graph
           (User_Data         => User_Data,
            Entity            => Entity,
            Dispatching_Calls => Nth_Arg (Data, 2, False),
            Get_All_Refs      => True);

      elsif Command = "called_by" then
         Name_Parameters (Data, (1 => Dispatching_Calls_Cst'Access));

         --  The following unchecked_access is safe since
         --  Examine_Ancestors_Call_Graph is called synchronously
         User_Data := new Add_To_List_User_Data;
         User_Data.Data := Data'Unchecked_Access;
         Examine_Ancestors_Call_Graph
           (Kernel          => Kernel,
            User_Data       => User_Data,
            Entity          => Entity,
            Dispatching_Calls => Nth_Arg (Data, 2, False),
            Background_Mode => False);

      elsif Command = "called_by_browser" then
         Examine_Ancestors_Call_Graph (Kernel, Entity);
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Call_Graph_Command_Handler;

   --------------------------------
   -- References_Command_Handler --
   --------------------------------

   procedure References_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      ReferencesCommand_Class : constant Class_Type := New_Class
        (Get_Kernel (Data), References_Command_Class_Name);
      Inst         : constant Class_Instance :=
                                  Nth_Arg (Data, 1, ReferencesCommand_Class);
      Data_Command : constant References_Command_Access :=
                               References_Command_Access
                                 (Get_Command (Get_Data (Inst)));
   begin
      if Command = "get_result" then
         Put_Locations_In_Return
           (Data_Command, Data, Show_Ref_Kind => Data_Command.Show_Ref_Kind);
      end if;
   end References_Command_Handler;

   -----------------------------
   -- Put_Locations_In_Return --
   -----------------------------

   procedure Put_Locations_In_Return
     (Command       : access References_Command'Class;
      Data          : in out Callback_Data'Class;
      Show_Ref_Kind : Boolean)
   is
      Inst : Class_Instance;
   begin
      if not Show_Ref_Kind then
         Set_Return_Value_As_List (Data);
      end if;

      declare
         Loc   : General_Location;
      begin
         for Ref of Command.Locations loop

            Loc := Get_Location (Ref);
            Inst := Create_File_Location
              (Script => Get_Script (Data),
               File   => Create_File
                 (Script => Get_Script (Data),
                  File   => Loc.File),
               Line   => Loc.Line,
               Column => Loc.Column);

            if Show_Ref_Kind then
               Set_Return_Value (Data, Get_Display_Kind (Ref));
               Set_Return_Value_Key (Data, Inst);
            else
               Set_Return_Value (Data, Inst);
            end if;
         end loop;
      end;
   end Put_Locations_In_Return;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Entity_Filter;
      Context : Selection_Context) return Boolean is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Entity /= No_Root_Entity
              and then Is_Subprogram (Entity);
         end;
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

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
               Lock      : Database_Lock := Kernel.Databases.Freeze;

            begin
               Parse_All_LI_Information (Kernel, Get_Project (Kernel), False);
               --  ??? for some reason, calling Freeze (Db) here generates
               --  incomplete call graphs
               --  Freeze (Db);

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

               --  ??? See comment about Freeze above
               --  Thaw (Db);
               Kernel.Databases.Thaw (Lock);

               Layout (Browser, Force => True);
               Refresh_Canvas (Canvas);

            exception
               when E    : others =>
                  Kernel.Databases.Thaw (Lock);
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

   --------------------
   -- Parse_All_Refs --
   --------------------

   procedure Parse_All_Refs
     (Kernel             : access Kernel_Handle_Record'Class;
      Entity             : Root_Entity'Class;
      Locals_Only        : Boolean;
      Local_File         : Virtual_File;
      All_From_Same_File : Boolean;
      Show_Caller        : Boolean;
      Filter             : in out Custom_Filter;
      Include_Overriding : Boolean := False)
   is
      Title       : constant String := All_Refs_Category
        (Entity             => Entity,
         Kernel             => Kernel,
         Local_Only         => Locals_Only,
         Local_File         => Local_File,
         All_From_Same_File => All_From_Same_File);
      Decl : constant General_Location := Get_Declaration (Entity).Loc;
      Decl2 : General_Location;
      Entity_Decl : constant Virtual_File := Decl.File;
      Iter2       : Entities_In_File_Cursor;
      Message     : Simple_Message_Access;
      Project     : Project_Type := No_Project;
      Set         : File_Info_Set;
      Imports     : Boolean;
      Is_Limited_With : Boolean;

   begin
      if All_From_Same_File then
         Get_Messages_Container (Kernel).Remove_Category
           (Title, Call_Graph_Message_Flags);

         --  We use a project that is in the same tree as the entity.

         Set := Get_Registry (Kernel).Tree.Info_Set (Local_File);
         for S of Set loop
            S.Project.Project_Imports
              (Decl.Project,
               Include_Extended => True,
               Imports          => Imports,
               Is_Limited_With  => Is_Limited_With);
            if Imports then
               Project := S.Project;
               exit;
            end if;
         end loop;

         Iter2 := Kernel.Databases.Entities_In_File (Local_File, Project);
         while not At_End (Iter2) loop
            declare
               Entity2        : constant Root_Entity'Class := Get (Iter2);
            begin
               Decl2 := Get_Declaration (Entity2).Loc;

               if Decl2.File = Entity_Decl then
                  if Show_Caller then
                     declare
                        Iter : Root_Reference_Iterator'Class :=
                          Find_All_References
                            (Entity             => Entity2,
                             In_File            => Local_File,
                             Include_Overriding => Include_Overriding,
                             Include_Overridden => Include_Overriding);
                     begin

                        declare
                           Name2 : constant String := Get_Name (Entity2);
                           Loc   : General_Location;
                        begin
                           while not At_End (Iter) loop
                              Loc := Get_Location (Get (Iter));

                              if Get (Iter) /= No_Root_Entity_Reference
                                and then Loc.File = Local_File
                                and then Is_Valid (Filter, Ref => Get (Iter))
                              then
                                 Print_Ref (Kernel,
                                            Get (Iter),
                                            Name2,
                                            Title,
                                            Show_Caller => Show_Caller,
                                            Sort_In_File => True);
                              end if;
                              Next (Iter);
                           end loop;
                        end;

                        Destroy (Iter);
                     end;
                  else
                     declare
                        Name2 : constant String := Get_Name (Entity2);
                     begin
                        Message :=
                          Create_Simple_Message
                            (Get_Messages_Container (Kernel),
                             Title,
                             Decl2.File,
                             Decl2.Line,
                             Decl2.Column,
                             Name2,
                             0,
                             Call_Graph_Message_Flags);
                        Message.Set_Highlighting
                          (Get_Or_Create_Style_Copy
                             (Kernel_Handle (Kernel),
                              Get_Name (Search_Results_Style) & '/' & Title,
                              Search_Results_Style),
                           Name2'Length);
                     end;
                  end if;
               end if;

               Next (Iter2);
            end;
         end loop;

         Free (Filter);

      elsif Locals_Only then
         --  Print the declaration of the entity, but only if it is in the
         --  current file, as expected by users.

         Get_Messages_Container (Kernel).Remove_Category
           (Title, Call_Graph_Message_Flags);

         declare
            Iter : Root_Reference_Iterator'Class :=
              Find_All_References
                (Entity        => Entity,
                 In_File       => Local_File,
                 Include_Overridden => Include_Overriding,
                 Include_Overriding => Include_Overriding);
         begin
            declare
               Name : constant String := Get_Name (Entity);
            begin
               while not At_End (Iter) loop
                  if Get (Iter) /= No_Root_Entity_Reference
                    and then Is_Valid (Filter, Get (Iter))
                  then
                     Print_Ref (Kernel,
                                Get (Iter),
                                Name,
                                Title,
                                Show_Caller => Show_Caller,
                                Sort_In_File => True);
                  end if;
                  Next (Iter);
               end loop;
            end;

            Destroy (Iter);
            Free (Filter);
         end;
      else
         Find_All_References_Internal   --  will destroy filter
           (Kernel,
            Entity,
            Category_Title     => Title,
            Show_Caller        => Show_Caller,
            Filter             => Filter,
            Include_Overriding => Include_Overriding);
      end if;
   end Parse_All_Refs;

   ------------------------
   -- Select_All_Filters --
   ------------------------

   procedure Select_All_Filters (Dialog : access Gtk_Widget_Record'Class) is
      D : constant References_Filter_Dialog :=
        References_Filter_Dialog (Dialog);
   begin
      for F in D.Filters'Range loop
         Set_Active (D.Filters (F), True);
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
         Set_Active (D.Filters (F), False);
      end loop;
   end Unselect_All_Filters;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (Self : Custom_Filter; Ref : Root_Entity_Reference'Class) return Boolean
   is
   begin
      if Self.Filter /= null
        and then not Self.Filter (Ref)
      then
         return False;
      end if;

      if Self.Ref_Kinds /= null then
         declare
            Kind : constant String := Get_Display_Kind (Ref);
         begin
            for R in Self.Ref_Kinds'Range loop
               if Self.Ref_Kinds (R) /= null
                 and then Kind = Self.Ref_Kinds (R).all
               then
                  return True;
               end if;
            end loop;
         end;

         return False;
      else
         return True;
      end if;
   end Is_Valid;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel             : constant Kernel_Handle :=
                             Get_Kernel (Context.Context);
      Dialog             : References_Filter_Dialog;
      Box                : Gtk_Box;
      Col                : array (1 .. 2) of Gtk_Box;
      Filter_Box         : Gtk_Vbutton_Box;
      Index              : Integer := Col'First;
      Project_And_Recursive,
      File_Only          : Gtk_Radio_Button;
      Show_Caller        : Gtk_Check_Button;
      From_Same_File     : Gtk_Radio_Button;
      Include_Overriding : Gtk_Check_Button;
      Frame              : Gtk_Frame;
      Ignore             : Gtk_Widget;
      Entity             : constant Root_Entity'Class :=
                             Get_Entity (Context.Context);
      Current_File       : constant Virtual_File :=
                             File_Information (Context.Context);
      Button             : Gtk_Button;
      pragma Unreferenced (Command, Ignore);

      All_Refs : GNAT.Strings.String_List :=
        Kernel.Databases.All_Real_Reference_Kinds;

   begin
      Dialog := new References_Filter_Dialog_Record;
      Dialog.Filters := new Filters_Buttons (All_Refs'Range);

      Initialize (Dialog,
                  Title  => -"Find References Options",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal);

      --  Context choice

      Gtk_New (Frame, -"Context");
      Pack_Start (Get_Content_Area (Dialog), Frame);
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
      Pack_Start (Get_Content_Area (Dialog), Frame);
      Gtk_New_Hbox (Box, Homogeneous => False);
      Add (Frame, Box);

      for C in Col'Range loop
         Gtk_New_Vbox (Col (C), Homogeneous => True);
         Pack_Start (Box, Col (C), Expand => True);
      end loop;

      for F in Dialog.Filters'Range loop
         Gtk_New (Dialog.Filters (F), All_Refs (F).all);
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
      end loop;

      Gtk_New (Filter_Box);
      Set_Layout (Filter_Box, Buttonbox_Spread);
      Pack_Start (Box, Filter_Box, Padding => 5);

      Gtk_New (Button, -"Select all");
      Pack_Start (Filter_Box, Button);
      Widget_Callback.Object_Connect
        (Button, Signal_Clicked, Select_All_Filters'Access, Dialog);

      Gtk_New (Button, -"Unselect all");
      Pack_Start (Filter_Box, Button);
      Widget_Callback.Object_Connect
        (Button, Signal_Clicked, Unselect_All_Filters'Access, Dialog);

      --  Extra info choice

      Gtk_New (Frame, -"Advanced Search");
      Pack_Start (Get_Content_Area (Dialog), Frame);
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

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         for F in Dialog.Filters'Range loop
            if not Get_Active (Dialog.Filters (F)) then
               Free (All_Refs (F));
            end if;
         end loop;

         declare
            Filter : Custom_Filter :=
              (Db        => Kernel.Databases,
               Ref_Kinds => new GNAT.Strings.String_List'(All_Refs),
               Filter    => null);
         begin
            Parse_All_Refs  --  will destroy filter
              (Kernel             => Kernel,
               Entity             => Entity,
               Locals_Only        => Get_Active (File_Only),
               Local_File         => Current_File,
               All_From_Same_File => Get_Active (From_Same_File),
               Filter             => Filter,
               Show_Caller        => Get_Active (Show_Caller),
               Include_Overriding => Get_Active (Include_Overriding));
         end;

         Unchecked_Free (Dialog.Filters);
         Destroy (Dialog);

         return Commands.Success;
      else
         Unchecked_Free (Dialog.Filters);
         GNATCOLL.Utils.Free (All_Refs);
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
      Command  : Interactive_Command_Access;
      Filter   : Action_Filter;
      ReferencesCommand_Class : constant Class_Type := New_Class
        (Kernel, "ReferencesCommand", New_Class (Kernel, "Command"));
   begin
      Call_Graph_Module_Id := new Callgraph_Module_Record;
      Callgraph_Views.Register_Module
        (Kernel,
         ID        => Call_Graph_Module_Id);

      Filter := new Subprogram_Entity_Filter;
      Subprogram_Entity_Filter (Filter.all).Kernel := Kernel_Handle (Kernel);
      Register_Filter (Kernel, Filter, "Entity is subprogram");

      Command := new Entity_Calls_Command;
      Register_Contextual_Menu
        (Kernel, "Entity calls in browser",
         Label  => "Browsers/%e calls",
         Filter => Filter,
         Action => Command);

      Command := new Entity_Calls_All_Command;
      Register_Contextual_Menu
        (Kernel, "Entity calls (recursively) in browser",
         Label  => "Browsers/%e calls (recursively)",
         Filter => Filter,
         Action => Command);

      Command := new Entity_Called_By_Command;
      Register_Contextual_Menu
        (Kernel, "Entity called by in browser",
         Label  => "Browsers/%e is called by",
         Filter => Filter,
         Action => Command);

      Register_Contextual_Submenu
        (Kernel, "References",
         Ref_Item   => "Goto file spec<->body",
         Add_Before => False);

      Register_Action
         (Kernel, "find all references", new Find_All_Refs_Command,
          -("List all references to the entity under the cursor"
            & " in the Locations window"));
      Register_Contextual_Menu
        (Kernel, "Find all references",
         Label      => "References/Find all references to %e",
         Action     => new Find_All_Refs_Command,
         Ref_Item   => "Entity called by in browser",
         Add_Before => False);

      Command := new Find_Specific_Refs_Command;
      Register_Contextual_Menu
        (Kernel, "Find references...",
         Label      => "References/Find references to %e...",
         Action     => Command);

      Command := new Find_All_Refs_Command;
      Find_All_Refs_Command (Command.all).Locals_Only := True;
      Register_Contextual_Menu
        (Kernel, "Find all local references",
         Label  => "References/Find all local references to %e",
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
        (Kernel, "find_all_refs",
         Class        => Get_Entity_Class (Kernel),
         Maximum_Args => 1,
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "references",
         Class        => Get_Entity_Class (Kernel),
         Maximum_Args => 5,
         Handler      => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "calls",
         Class   => Get_Entity_Class (Kernel),
         Maximum_Args => 1,
         Handler => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "called_by",
         Class   => Get_Entity_Class (Kernel),
         Maximum_Args => 1,
         Handler => Call_Graph_Command_Handler'Access);
      Register_Command
        (Kernel, "called_by_browser",
         Class   => Get_Entity_Class (Kernel),
         Handler => Call_Graph_Command_Handler'Access);

      Browsers.Canvas.Register_Actions (Kernel);

      Register_Command
        (Kernel, "get_result",
         Class   => ReferencesCommand_Class,
         Handler => References_Command_Handler'Access);
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
