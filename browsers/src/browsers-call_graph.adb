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

with Browsers.Canvas;               use Browsers.Canvas;
with Commands.Interactive;          use Commands, Commands.Interactive;
with GNAT.Strings;                  use GNAT.Strings;
with GNATCOLL.Projects;             use GNATCOLL.Projects;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;
with GNATCOLL.Xref;
with GPS.Intl;                      use GPS.Intl;
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
with Gtkada.Canvas_View.Models.Layers; use Gtkada.Canvas_View.Models.Layers;
with Gtkada.MDI;                    use Gtkada.MDI;
with Histories;                     use Histories;
with Std_Dialogs;                   use Std_Dialogs;
with String_Utils;                  use String_Utils;
with Xref;                          use Xref;

package body Browsers.Call_Graph is
   Me : constant Trace_Handle := Create ("CALL_GRAPH");
   use type GNATCOLL.Xref.Visible_Column;

   Call_Graph_Module_Id : Module_ID;
   Call_Graph_Module_Name : constant String := "Call_Graph";

   ------------------------
   -- Call graph browser --
   ------------------------

   type Call_Graph_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with null record;

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

   overriding procedure Set_Context
     (Item    : not null access Entity_Item_Record;
      Context : in out Selection_Context);

   procedure Create_Or_Find_Entity
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Root_Entity'Class;
      At_Pos  : Gtkada.Style.Point;
      Item    : out Entity_Item;
      Newly_Created : out Boolean);
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
     (Browser             : Call_Graph_Browser;
      Src, Dest           : Entity_Item;
      Is_Renaming         : Boolean);
   --  Add Entity, and possibly a link to Cb.Item to Cb.Browser

   procedure Call_Graph_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   type Show_Ancestors_Button is new Left_Arrow_Record with null record;
   overriding procedure On_Click
     (Self : not null access Show_Ancestors_Button;
      View : not null access GPS_Canvas_View_Record'Class);

   type Show_Children_Button is new Right_Arrow_Record with null record;
   overriding procedure On_Click
     (Self : not null access Show_Children_Button;
      View : not null access GPS_Canvas_View_Record'Class);

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Call_Graph_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Initialize (View, Use_Canvas_View => True);
      Register_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Call_Graph_Module_Id,
         Context_Func    => Default_Browser_Context_Factory'Access);
      return Gtk_Widget (View);
   end Initialize;

   ---------------------------
   -- Create_Or_Find_Entity --
   ---------------------------

   procedure Create_Or_Find_Entity
     (Browser : access Call_Graph_Browser_Record'Class;
      Entity  : Root_Entity'Class;
      At_Pos  : Gtkada.Style.Point;
      Item    : out Entity_Item;
      Newly_Created : out Boolean)
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
      Browser.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);

      if Item = null then
         declare
            Decl   : constant General_Location := Get_Declaration (Entity).Loc;
            Name   : constant String := Get_Name (Entity);
            Text   : Text_Item;
            S      : constant access Browser_Styles :=
              Browser.Get_View.Get_Styles;
         begin
            Item := new Entity_Item_Record;
            Item.Browser := General_Browser (Browser);
            Item.Entity.Replace_Element (Entity);

            Item.Initialize_Rect (Style => S.Item, Radius => 5.0);
            Setup_Titlebar
              (Item, Browser,
               Name  => Name,
               Left  => new Show_Ancestors_Button,
               Right => new Show_Children_Button);
            Text := Gtk_New_Text
              (S.Text_Font,
               Decl.File.Display_Base_Name & ':' & Image (Decl.Line));
            Item.Add_Child (Text, Margin => (2.0, 2.0, 2.0, 2.0));

            Item.Set_Position (At_Pos);
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
      Create_Or_Find_Entity
        (Browser, Entity, No_Position, Item, Newly_Added);
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

      Context : constant Draw_Context := Data.Browser.Get_View.Build_Context;

      use Items_Lists;
      C     : Items_Lists.Cursor;
      Total : Gdouble := 0.0;   --  total space for children
      Max   : Gdouble := 0.0;
      Box   : Item_Rectangle;
      MBox  : Model_Rectangle;
      Pos   : Gtkada.Style.Point;
      Child : Gtkada.Style.Point;
      It    : Entity_Item;
   begin
      --  If we are finishing processing the toplevel entity
      if Data.Parent = null
        and then not Data.Items.Is_Empty
      then
         --  Compute the size we need for the children

         C := Data.Items.First;
         while Has_Element (C) loop
            Element (C).Refresh_Layout (Context); --  Compute its size
            Box := Element (C).Bounding_Box;
            if Horizontal then
               Total := Total + Box.Height;
               Max := Gdouble'Max (Max, Box.Width);
            else
               Total := Total + Box.Width;
               Max := Gdouble'Max (Max, Box.Height);
            end if;
            Next (C);
         end loop;

         Total := Total + Space_Between_Items * Gdouble (Data.Items.Length);

         --  Now insert (if needed) the parent item, below or to the right of
         --  the existing model elements, and centered with regards to its
         --  future children

         Pos := Data.Item.Position;

         if Pos = No_Position then
            Browser_Model (Data.Browser.Get_View.Model).Add (Data.Item);

            Data.Item.Refresh_Layout (Context);  --  Compute its size
            Box := Data.Item.Bounding_Box;
            MBox := Data.Browser.Get_View.Model.Bounding_Box;

            if Horizontal then
               Child.Y := MBox.Y + MBox.Height;
               Pos.Y := Child.Y + (Total - MBox.Height) / 2.0;
               if Data.Link_From_Item then
                  Pos.X   := MBox.X;
                  Child.X := Pos.X + Box.Width + Space_Between_Layers;
               else
                  Child.X := MBox.X;
                  Pos.X   := Child.X + Max + Space_Between_Layers;
               end if;

            else
               Child.X := MBox.X + MBox.Width;
               Pos.X := Child.X + (Total - MBox.Width) / 2.0;
               if Data.Link_From_Item then
                  Pos.Y   := MBox.Y;
                  Child.Y := Pos.Y + Box.Height + Space_Between_Layers;
               else
                  Child.Y := MBox.Y;
                  Pos.Y   := Child.Y + Max + Space_Between_Layers;
               end if;
            end if;

            Data.Item.Set_Position (Pos);

         else
            Box := Data.Item.Bounding_Box;
            if Horizontal then
               Child.Y := Pos.Y + (Box.Height - Total) / 2.0;
               if Data.Link_From_Item then
                  Child.X := Pos.X + Box.Width + Space_Between_Layers;
               else
                  Child.X := Pos.X - Max - Space_Between_Layers;
               end if;
            else
               Child.X := Pos.X + (Box.Width - Total) / 2.0;
               if Data.Link_From_Item then
                  Child.Y := Pos.Y + Box.Height + Space_Between_Items;
               else
                  Child.Y := Pos.Y - Max - Space_Between_Items;
               end if;
            end if;
         end if;

         --  Reserve space for the children by moving existing items aside.

         if Horizontal then
            Reserve_Space
              (Data.Browser.Get_View,
               (Child.X, Child.Y, Max, Total),
               Direction => (if Data.Link_From_Item then Right else Left),
               Duration  => 0.3);
         else
            Reserve_Space
              (Data.Browser.Get_View,
               (Child.X, Child.Y, Total, Max),
               Direction => (if Data.Link_From_Item then Down else Up),
               Duration  => 0.3);
         end if;

         --  Now add the children, and create the links

         C := Data.Items.First;
         while Has_Element (C) loop
            It := Entity_Item (Element (C));

            It.Set_Position (Pos);  --  position of the parent initially
            Animate_Position (It, Child, Duration => 0.3)
               .Start (Data.Browser.Get_View);

            Browser_Model (Data.Browser.Get_View.Model).Add (It);
            if Data.Link_From_Item then
               Add_Link_If_Not_Present
                 (Data.Browser, Data.Item, It, Is_Renaming => False);
            else
               Add_Link_If_Not_Present
                 (Data.Browser, It, Data.Item, Is_Renaming => False);
            end if;

            Box := It.Bounding_Box;
            if Horizontal then
               Child.Y := Child.Y + Box.Height + Space_Between_Items;
            else
               Child.X := Child.X + Box.Width + Space_Between_Items;
            end if;

            Next (C);
         end loop;

         Data.Browser.Get_View.Model.Refresh_Layout;  --  for links
         Data.Browser.Get_View.Scroll_Into_View (Data.Item);
         Data.Browser.Get_View.Queue_Draw;
      end if;
   end Destroy;

   -----------------------------
   -- Add_Link_If_Not_Present --
   -----------------------------

   procedure Add_Link_If_Not_Present
     (Browser             : Call_Graph_Browser;
      Src, Dest           : Entity_Item;
      Is_Renaming         : Boolean)
   is
      Styles : constant access Browser_Styles := Browser.Get_View.Get_Styles;
      L                : GPS_Link;
   begin
      if not Browser.Has_Link (Src, Dest) then
         L := new GPS_Link_Record;
         if Is_Renaming then
            L.Default_Style := Styles.Link2;
            Initialize
              (L,
               From    => Src,
               To      => Dest,
               Routing => Curve,
               Label  => Gtk_New_Text (Styles.Title, "renames"),
               Anchor_From => (X => 1.0, others => <>),
               Anchor_To   => (X => 0.0, others => <>),
               Style       => L.Default_Style);
         else
            L.Default_Style := Styles.Link;
            Initialize
              (L,
               From    => Src,
               To      => Dest,
               Routing => Curve,
               Anchor_From => (X => 1.0, others => <>),
               Anchor_To   => (X => 0.0, others => <>),
               Style       => L.Default_Style);
         end if;
         Browser_Model (Browser.Get_View.Model).Add (L);
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
      pragma Unreferenced (Ref, Through_Dispatching, Is_Renaming);
      It : Entity_Item;
      Newly_Added : Boolean;
   begin
      if Data.Link_From_Item then
         Create_Or_Find_Entity
           (Data.Browser, Entity, Data.Item.Position, It, Newly_Added);
      else
         Create_Or_Find_Entity
           (Data.Browser, Parent, Data.Item.Position, It, Newly_Added);
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
      Create_Or_Find_Entity (Browser, Entity, No_Position, It, Newly_Added);
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
     (Self : not null access Show_Ancestors_Button;
      View : not null access GPS_Canvas_View_Record'Class)
   is
      It : constant Entity_Item := Entity_Item (Self.Get_Toplevel_Item);
   begin
      Examine_Ancestors_Call_Graph
        (Get_Kernel (It.Browser), It.Entity.Element);
      Highlight_Related_Items (View, It);
      It.Hide_Left_Arrow;
      View.Model.Item_Contents_Changed (It);
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self : not null access Show_Children_Button;
      View : not null access GPS_Canvas_View_Record'Class)
   is
      It : constant Entity_Item := Entity_Item (Self.Get_Toplevel_Item);
   begin
      Examine_Entity_Call_Graph
        (Get_Kernel (It.Browser), It.Entity.Element, Recursive => False);
      Highlight_Related_Items (View, It);
      It.Hide_Right_Arrow;
      View.Model.Item_Contents_Changed (It);
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
      if Node_Entity /= No_Root_Entity then
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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command  : Interactive_Command_Access;
   begin
      Call_Graph_Module_Id := new Module_ID_Record;
      Callgraph_Views.Register_Module (Kernel, ID => Call_Graph_Module_Id);

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

      Register_Command
        (Kernel, "called_by_browser",
         Class   => Get_Entity_Class (Kernel),
         Handler => Call_Graph_Command_Handler'Access);
   end Register_Module;
end Browsers.Call_Graph;
