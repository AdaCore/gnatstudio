------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Basic_Types;                   use Basic_Types;
with Browsers.Canvas;               use Browsers.Canvas;
with Commands.Interactive;          use Commands, Commands.Interactive;
with GNAT.Strings;                  use GNAT.Strings;
with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;
with GNATCOLL.Xref;
with GPS.Editors;                   use GPS.Editors;
with GPS.Intl;                      use GPS.Intl;
with GPS.Kernel.Actions;            use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;         use GPS.Kernel.Modules.UI;
with GPS.Kernel.Modules;            use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;
with GPS.Kernel.Xref;               use GPS.Kernel.Xref;
with GPS.Kernel;                    use GPS.Kernel;
with Generic_Views;
with Glib.Object;                   use Glib.Object;
with Glib;                          use Glib;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Widget;                    use Gtk.Widget;
with Gtkada.Canvas_View;            use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views;      use Gtkada.Canvas_View.Views;
with Gtkada.MDI;                    use Gtkada.MDI;
with Histories;                     use Histories;
with GPS.Dialogs;                   use GPS.Dialogs;
with String_Utils;                  use String_Utils;
with XML_Utils;                     use XML_Utils;
with Xref;                          use Xref;

package body Browsers.Call_Graph is
   Me : constant Trace_Handle := Create ("GPS.VIEWS.CALL_GRAPH");
   use type GNATCOLL.Xref.Visible_Column;

   Call_Graph_Module_Name : constant String := "Call_Graph";

   ------------------------
   -- Call graph browser --
   ------------------------

   type Call_Graph_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with null record;

   overriding function Load_From_XML
     (Self : not null access Call_Graph_Browser_Record;
      Node : XML_Utils.Node_Ptr) return access GPS_Item_Record'Class;
   overriding procedure Load_From_XML
     (Self     : not null access Call_Graph_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class);

   function Initialize
     (View   : access Call_Graph_Browser_Record'Class)
      return Gtk_Widget;
   --  Initialize the browser, and return the focus widget

   package Callgraph_Views is new Generic_Views.Simple_Views
     (Module_Name            => Call_Graph_Module_Name,
      View_Name              => -"Call Graph Browser",
      Formal_View_Record     => Call_Graph_Browser_Record,
      Formal_MDI_Child       => Browser_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Default);
   subtype Call_Graph_Browser is Callgraph_Views.View_Access;

   --------------
   -- Commands --
   --------------

   type Entity_Calls_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Entity_Calls_All_Command is new Interactive_Command with null record;
   overriding function Name
     (Self    : access Entity_Calls_All_Command) return String
     is ("Call graph browser");
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

   type Entity_Item_Record is new GPS_Item_Record with record
      Entity     : Root_Entity_Ref;
      Circle     : Ellipse_Item;
   end record;
   type Entity_Item is access all Entity_Item_Record'Class;

   type Xref_Text_Record is new Text_Item_Record and Clickable_Item with
      record
         File : Virtual_File;
         Line : Integer;
      end record;
   type Xref_Text is access all Xref_Text_Record'Class;
   overriding procedure On_Click
     (Self    : not null access Xref_Text_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   overriding function Save_To_XML
     (Self : not null access Entity_Item_Record)
      return XML_Utils.Node_Ptr;
   overriding procedure Set_Context
     (Item    : not null access Entity_Item_Record;
      Context : in out Selection_Context);

   procedure Create_Or_Find_Entity
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Root_Entity'Class;
      Item    : out Entity_Item;
      Newly_Created : out Boolean;
      Force_Create  : Boolean := False);
   --  Retrieve an existing item for the entity, or create a new item.
   --  The new item is not added to the model automatically.

   ----------
   -- Misc --
   ----------

   type Item_List_Access is access all Items_Lists.List;

   type Examine_Ancestors_Data;
   type Examine_Ancestors_Data_Access is access all Examine_Ancestors_Data;
   type Examine_Ancestors_Data is new Commands_User_Data_Record with record
      Browser        : Call_Graph_Browser;

      Item           : Entity_Item;
      --  The item we are starting from.
      --  Its position is set to No_Position if it was just added to the
      --  browser. It might not have been added to the model yet.

      Link_From_Item : Boolean;

      Items          : Item_List_Access;
      --  The items to be added. This does not include the items that were
      --  already in the browser.

      Parent         : Examine_Ancestors_Data_Access;
      --  The data for the parent entity (when Recursive is true)

      Max_Count      : Natural := Natural'Last;
      --  Maximum number of entities to add

      Recursive      : Boolean := False;
      --  number of items visited
   end record;
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
     (Kernel    : access Kernel_Handle_Record'Class;
      Entity    : Root_Entity'Class;
      Recursive : Boolean;
      Max_Count : Natural := Natural'Last);
   --  Display the call graph for the node.

   procedure Examine_Ancestors_Call_Graph
     (Kernel : access Kernel_Handle_Record'Class;
      Entity : Root_Entity'Class);
   --  Display the list of subprograms that call Entity

   procedure Add_Link_If_Not_Present
     (Browser             : not null access Call_Graph_Browser_Record'Class;
      Src, Dest           : Entity_Item;
      Is_Renaming         : Boolean);
   --  Add Entity, and possibly a link to Cb.Item to Cb.Browser

   procedure Call_Graph_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   type Show_Ancestors_Button is new Left_Arrow_Record with null record;
   overriding procedure On_Click
     (Self    : not null access Show_Ancestors_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   type Show_Children_Button is new Right_Arrow_Record with null record;
   overriding procedure On_Click
     (Self    : not null access Show_Children_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   -----------------
   -- Save_To_XML --
   -----------------

   overriding function Save_To_XML
     (Self : not null access Entity_Item_Record)
      return XML_Utils.Node_Ptr
   is
      Decl : constant General_Entity_Declaration :=
        Self.Entity.Element.Get_Declaration;
      N    : constant Node_Ptr := new Node;
   begin
      N.Tag := new String'("entity");
      Set_Attribute (N, "name", To_String (Decl.Name));
      Set_Attribute (N, "file", Decl.Loc.File.Display_Full_Name);
      Set_Attribute (N, "line", Decl.Loc.Line'Img);
      Set_Attribute (N, "col",  Decl.Loc.Column'Img);
      return N;
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding function Load_From_XML
     (Self : not null access Call_Graph_Browser_Record;
      Node : XML_Utils.Node_Ptr) return access GPS_Item_Record'Class
   is
      E : constant Root_Entity'Class :=
        Self.Kernel.Databases.Get_Entity
          (Name => Get_Attribute (Node, "name"),
           Loc  =>
             (File         => Create (+Get_Attribute (Node, "file")),
              Project_Path => <>,
              Line    => Integer'Value (Get_Attribute (Node, "line")),
              Column  =>
                Visible_Column_Type'Value (Get_Attribute (Node, "col"))));
      It            : Entity_Item;
      Newly_Created : Boolean;
   begin
      Self.Create_Or_Find_Entity
        (Entity        => E,
         Item          => It,
         Newly_Created => Newly_Created,
         Force_Create  => True);
      return It;
   end Load_From_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (Self     : not null access Call_Graph_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class)
   is
      pragma Unreferenced (Node);
   begin
      Self.Add_Link_If_Not_Present
        (Entity_Item (From), Entity_Item (To), Is_Renaming => False);
   end Load_From_XML;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Call_Graph_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Browsers.Canvas.Initialize (View);
      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View);
      return Gtk_Widget (View.Get_View);
   end Initialize;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Xref_Text_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (View);
      It : constant Entity_Item := Entity_Item (Details.Toplevel_Item);
      Editor : constant Editor_Buffer'Class :=
        Get_Kernel (It.Browser).Get_Buffer_Factory.Get (Self.File);
   begin
      Editor.Current_View.Cursor_Goto
        (Editor.New_Location_At_Line (Self.Line), Raise_View => True);
   end On_Click;

   ---------------------------
   -- Create_Or_Find_Entity --
   ---------------------------

   procedure Create_Or_Find_Entity
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Root_Entity'Class;
      Item    : out Entity_Item;
      Newly_Created : out Boolean;
      Force_Create : Boolean := False)
   is
      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
      begin
         if It.all in Entity_Item_Record'Class
           and then Entity_Item (It).Entity.Element = Entity
         then
            Item := Entity_Item (It);
         end if;
      end On_Item;

   begin
      Item := null;

      if not Force_Create then
         Browser.Get_View.Model.For_Each_Item
           (On_Item'Access, Filter => Kind_Item);
      end if;

      if Item = null then
         declare
            Decl : constant General_Location := Get_Declaration (Entity).Loc;
            Name : constant String := Get_Name (Entity);
            S    : constant access Browser_Styles :=
              Browser.Get_View.Get_Styles;
            Text : Xref_Text;

         begin
            Item := new Entity_Item_Record;
            Item.Browser := General_Browser (Browser);
            Item.Entity.Replace_Element (Entity);

            Browser_Model (Browser.Get_View.Model).Add (Item);

            Item.Initialize_Rect (Style => S.Item, Radius => 5.0);
            Setup_Titlebar
              (Item, Browser,
               Name  => Name,
               Left  => new Show_Ancestors_Button,
               Right => new Show_Children_Button);

            Text := new Xref_Text_Record;
            Text.File := Decl.File;
            Text.Line := Decl.Line;
            Initialize_Text
              (Text,
               S.Hyper_Link,
               Decl.File.Display_Base_Name & ':' & Image (Decl.Line));
            Item.Add_Child (Text, Margin => (2.0, 2.0, 2.0, 2.0));

            Item.Set_Position (No_Position);
         end;
         Newly_Created := True;
      else
         Newly_Created := False;
      end if;
   end Create_Or_Find_Entity;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel    : access Kernel_Handle_Record'Class;
      Entity    : Root_Entity'Class;
      Recursive : Boolean;
      Max_Count : Natural := Natural'Last)
   is
      Browser : constant Call_Graph_Browser :=
        Callgraph_Views.Get_Or_Create_View (Kernel, Focus => True);
      Item        : Entity_Item;
      Newly_Added : Boolean;
      Data        : Examine_Ancestors_Data_Access;
   begin
      Create_Or_Find_Entity (Browser, Entity, Item, Newly_Added);
      Data := new Examine_Ancestors_Data'
        (Commands_User_Data_Record with
         Browser        => Browser,
         Item           => Item,
         Link_From_Item => True,
         Recursive      => Recursive,
         Max_Count      => Max_Count,
         Parent         => null,
         Items          => new Items_Lists.List);
      Examine_Entity_Call_Graph
        (Entity            => Entity,
         User_Data         => Data,
         Dispatching_Calls => True,
         Get_All_Refs      => True);
   end Examine_Entity_Call_Graph;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Data : in out Examine_Ancestors_Data; Cancelled : Boolean)
   is
      pragma Unreferenced (Cancelled);
      Horizontal : constant Boolean := Data.Browser.Horizontal_Layout;
      Dir   : Specific_Direction;
   begin
      --  If we are finishing processing the toplevel entity
      if Data.Parent = null then
         if Horizontal then
            Dir := (if Data.Link_From_Item then Right else Left);
         else
            Dir := (if Data.Link_From_Item then Down else Up);
         end if;

         Data.Browser.Get_View.Model.Clear_Selection;
         Data.Browser.Get_View.Model.Add_To_Selection (Data.Item);

         Insert_And_Layout_Items
           (Data.Browser.Get_View,
            Ref                  => Data.Item,
            Items                => Data.Items.all,
            Direction            => Dir,
            Space_Between_Items  => Default_Space_Between_Items,
            Space_Between_Layers => Default_Space_Between_Layers,
            Duration             => 0.3);
      end if;
   end Destroy;

   -----------------------------
   -- Add_Link_If_Not_Present --
   -----------------------------

   procedure Add_Link_If_Not_Present
     (Browser             : not null access Call_Graph_Browser_Record'Class;
      Src, Dest           : Entity_Item;
      Is_Renaming         : Boolean)
   is
      Styles : constant access Browser_Styles := Browser.Get_View.Get_Styles;
      L      : GPS_Link;
      F, T   : Anchor_Attachment;
   begin
      if not Browser.Has_Link (Src, Dest) then
         if Browser.Horizontal_Layout then
            F := (X => 1.0, Y => 0.5, others => <>);
            T := (X => 0.0, Y => 0.5, others => <>);
         else
            F := (X => 0.5, Y => 1.0, others => <>);
            T := (X => 0.5, Y => 0.0, others => <>);
         end if;

         L := new GPS_Link_Record;
         if Is_Renaming then
            L.Default_Style := Styles.Link2;
            Initialize
              (L,
               From    => Src,
               To      => Dest,
               Routing => Curve,
               Label  => Gtk_New_Text (Styles.Label, "renames"),
               Anchor_From => F,
               Anchor_To   => T,
               Style       => L.Default_Style);
         else
            L.Default_Style := Styles.Link;
            Initialize
              (L,
               From    => Src,
               To      => Dest,
               Routing => Curve,
               Anchor_From => F,
               Anchor_To   => T,
               Style       => L.Default_Style);
         end if;
         Browser_Model (Browser.Get_View.Model).Add (L);
         Src.Set_Right_Arrow (Collapse_Arrow);
         Dest.Set_Left_Arrow (Collapse_Arrow);
      end if;
   end Add_Link_If_Not_Present;

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
      pragma Unreferenced (Ref, Through_Dispatching);
      It : Entity_Item;
      Newly_Added : Boolean;
   begin
      if Data.Link_From_Item then
         Create_Or_Find_Entity (Data.Browser, Entity, It, Newly_Added);
         Add_Link_If_Not_Present
           (Data.Browser, Data.Item, It, Is_Renaming => Is_Renaming);
      else
         Create_Or_Find_Entity (Data.Browser, Parent, It, Newly_Added);
         Add_Link_If_Not_Present
           (Data.Browser, It, Data.Item, Is_Renaming => Is_Renaming);
      end if;

      if Newly_Added then
         Data.Items.Append (Abstract_Item (It));
         if Integer (Data.Items.Length) > Data.Max_Count then
            return False;
         end if;
      end if;

      if Data.Recursive then
         declare
            Data2 : constant Examine_Ancestors_Data_Access :=
              new Examine_Ancestors_Data'
                (Commands_User_Data_Record with
                 Browser        => Data.Browser,
                 Item           => It,
                 Link_From_Item => True,
                 Parent         => Examine_Ancestors_Data_Access (Data),
                 Max_Count      => Data.Max_Count,
                 Recursive      => Data.Recursive,
                 Items          => Data.Items);
         begin
            Examine_Entity_Call_Graph
              (Entity            => Entity,
               User_Data         => Data2,
               Dispatching_Calls => True,
               Get_All_Refs      => True);
         end;
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
      It            : Entity_Item;
      Newly_Added   : Boolean;
   begin
      Create_Or_Find_Entity (Browser, Entity, It, Newly_Added);
      Data := new Examine_Ancestors_Data'
        (Commands_User_Data_Record with
         Browser        => Browser,
         Item           => It,
         Link_From_Item => False,
         Recursive      => False,
         Max_Count      => Natural'Last,
         Parent         => null,
         Items          => new Items_Lists.List);
      Examine_Ancestors_Call_Graph
        (Kernel            => Kernel,
         Entity            => Entity,
         User_Data         => Data,
         Background_Mode   => False,
         Dispatching_Calls => True,
         Watch             => Gtk_Widget (Data.Browser));
   end Examine_Ancestors_Call_Graph;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Show_Ancestors_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Self);
      It    : constant Entity_Item  := Entity_Item (Details.Toplevel_Item);
      Model : constant Canvas_Model := View.Model;
      Set   : Item_Sets.Set;
   begin
      Model.To (It, Set);
      if Set.Is_Empty then
         Examine_Ancestors_Call_Graph
           (Get_Kernel (It.Browser), It.Entity.Element);
         Model.To (It, Set);

         --  The action does nothing for this node so hide the button
         if Set.Is_Empty then
            It.Hide_Left_Arrow;
         else
            It.Set_Left_Arrow (Collapse_Arrow);
         end if;
         Highlight_Related_Items (View, It);
         View.Model.Item_Contents_Changed (It);
      else
         --  In case of circular dependency keep the clicked node
         if Set.Contains (Abstract_Item (It)) then
            Set.Delete (Abstract_Item (It));
         end if;
         Model.Remove (Set);
         Model.Refresh_Layout;
         It.Set_Left_Arrow (Left_Arrow);
      end if;
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Show_Children_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Self);
      It    : constant Entity_Item  := Entity_Item (Details.Toplevel_Item);
      Model : constant Canvas_Model := View.Model;
      Set   : Item_Sets.Set;
   begin
      Model.From (It, Set);
      if Set.Is_Empty then
         Examine_Entity_Call_Graph
           (Get_Kernel (It.Browser), It.Entity.Element, Recursive => False);
         Model.From (It, Set);

         --  The action does nothing for this node so hide the button
         if Set.Is_Empty then
            It.Hide_Right_Arrow;
         else
            It.Set_Right_Arrow (Collapse_Arrow);
         end if;
         Highlight_Related_Items (View, It);
         View.Model.Item_Contents_Changed (It);
      else
         --  In case of circular dependency keep the clicked node
         if Set.Contains (Abstract_Item (It)) then
            Set.Delete (Abstract_Item (It));
         end if;
         Model.Remove (Set);
         Model.Refresh_Layout;
         It.Set_Right_Arrow (Right_Arrow);
      end if;
   end On_Click;

   -----------------
   -- Set_Context --
   -----------------

   overriding procedure Set_Context
     (Item    : not null access Entity_Item_Record;
      Context : in out Selection_Context)
   is
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
   end Set_Context;

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
      if Node_Entity /= No_Root_Entity
        and then Is_Subprogram (Node_Entity)
      then
         --  ??? Should check that Decl.Kind is a subprogram
         Examine_Entity_Call_Graph
           (Get_Kernel (Context.Context), Node_Entity, Recursive => False);
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
      Max_Items   : Natural := 100;

   begin
      if Histories.Get_History (History.all, Key) = null then
         Histories.Add_To_History (History.all, Key, "100");
      end if;

      declare
         Str  : constant String := Display_Text_Input_Dialog
           (Kernel   => Kernel,
            Title    => -"Complete Call Graph",
            Message  => -("Computing complete call graph may take a "
              & "long time." & ASCII.LF
              & "Enter maximum number of items to display: "),
            Key      => Key);
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

         if Node_Entity /= No_Root_Entity
           and then Is_Subprogram (Node_Entity)
         then
            --  ??? Should check that Decl.Kind is a subprogram
            Examine_Entity_Call_Graph
              (Kernel, Node_Entity,
               Recursive => True,
               Max_Count => Max_Items);
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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Info : constant Root_Entity'Class := Get_Entity (Context.Context);
   begin
      if Info /= No_Root_Entity
        and then Is_Subprogram (Info)
      then
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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Callgraph_Views.Register_Module (Kernel);

      Register_Action
        (Kernel, "Browser: entity calls",
         Command     => new Entity_Calls_Command,
         Description =>
           "Open the call graph browser to show all entities referenced in the"
           & " scope of the selected entity",
         Category    => -"Views");

      Register_Action
        (Kernel, "Browser: entity calls (recursive)",
         Command     => new Entity_Calls_All_Command,
         Description =>
           "Open the call graph browser to show all entities referenced in the"
         & " scope of the selected entity (recursively)",
         Category    => -"Views");

      Register_Action
        (Kernel, "Browser: entity called by",
         Command     => new Entity_Called_By_Command,
         Description =>
           "Open the call graph browser to show all entities that call the"
           & " selected entity",
         Category    => -"Views");

      Kernel.Scripts.Register_Command
        ("called_by_browser",
         Class   => Get_Entity_Class (Kernel),
         Handler => Call_Graph_Command_Handler'Access);
   end Register_Module;
end Browsers.Call_Graph;
