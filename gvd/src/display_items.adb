------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Glib;                     use Glib;
with Gtk.Enums;                use Gtk.Enums;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;

with Debugger;             use Debugger;
with Language;             use Language;
with Items.Simples;        use Items.Simples;

with GNAT.Strings;         use GNAT.Strings;
with GVD.Canvas;           use GVD.Canvas;
with Default_Preferences;  use Default_Preferences;
with GVD.Preferences;      use GVD.Preferences;
with GVD.Types;
with Ada.Exceptions;       use Ada.Exceptions;
with GNATCOLL.Traces;      use GNATCOLL.Traces;
with GNATCOLL.VFS;         use GNATCOLL.VFS;

package body Display_Items is

   Me : constant Trace_Handle := Create ("Display_Items");

   package String_To_Items is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Display_Item, Ada.Strings.Hash_Case_Insensitive, "=", "=");
   use String_To_Items;

   ---------------------
   -- Local Constants --
   ---------------------

   --  The items are drawn with the following spacings:
   --
   --   _______________________
   --  |<->TITLE<->| |<->| |<->|
   --   -----------------------
   --  |                       |
   --  |                       |
   --   -----------------------
   --
   --  <->: Spacing
   --  Each of the two Buttons has a width and height of Buttons_Size.

   Typed_Aliases : constant Boolean := True;
   --  If True, then two items are aliases only if they have the same address
   --  *and* they are structurally equivalent. If False, only the addresses
   --  are checked.

   --  Aliases detection
   --  ==================
   --
   --  This package provides a complete aliases detection, ie when some items
   --  are found at the same location in memory. Each item has a uniq id,
   --  which most often is an address in memory, but can also be different for
   --  instance on the Java Virtual Machine.
   --  Every time a new item is inserted in the canvas, either as a result of
   --  a "graph print" or "graph display" command, or when the user clicks on
   --  an access type to dereference it, this package will test that there is
   --  not already an item on the canvas with the same Id.
   --  In every case, the new item is created (with possibly a link to it if it
   --  was a dereference). However, if there was already an item with the same
   --  id then the new item is set to be hidden (ie will not be displayed,
   --  nor any link to or from it).
   --  The links to and from an alias (hidden item) are automatically
   --  duplicated to reference the visible item, so that they are correctly
   --  visible and moved on the canvas. These temporary links have a special
   --  attribute Alias_Link set.
   --
   --  Just before the next update of the canvas, all these temporary links are
   --  removed, all aliases are cancelled and all items are made visible. Then
   --  we recompute the list of aliases before redrawing the canvas. It is
   --  worth noting that when we have an hidden item, we do not waste time
   --  reparsing its value.
   --
   --  Note also that for simplicity we do not create chains of aliases, ie
   --  an item is an alias to a second, which in turn in an alias to a third.
   --  Instead, both the first and the second will refer the same third. It is
   --  thus much easier to deal with aliases.
   --
   --  To improve the support for strings in Ada, an extra rule is added:
   --  X.all can not be an alias of X. It is always considered to be a
   --  different object. This is needed, otherwise it is mostly impossible to
   --  properly display a String parameter correctly.

   function Search_Item
     (Process : access Visual_Debugger_Record'Class;
      Id      : String;
      Name    : String) return Display_Item;
   --  Search for an item whose Id is Id in the canvas.
   --  If Name is not the empty string, the name must also match

   procedure Change_Visibility
     (Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access);
   pragma Unreferenced (Change_Visibility);
   --  Change the visibility status of a specific component in the item

   function Is_Alias_Of
     (Item : access Display_Item_Record'Class;
      Id   : String;
      Name : String;
      Deref_Name : String) return Boolean;
   --  Return True if Item is an alias of the entity with name Name and
   --  whose Id is Id.
   --  Deref_Name should be the dereferenced version of Name

   procedure Parse_Type (Item : access Display_Item_Record'Class);
   --  Parse the type of the entity associated with Item. If the type is
   --  already known, nothing is done

   procedure Parse_Value (Item : access Display_Item_Record'Class);
   --  Parse the value of the entity

   ----------------
   -- Parse_Type --
   ----------------

   procedure Parse_Type (Item : access Display_Item_Record'Class) is
   begin
      if Item.Is_A_Variable and then Item.Entity = null then
         begin
            Item.Entity := Parse_Type (Item.Debugger.Debugger, Item.Name.all);
         exception
            when E : Language.Unexpected_Type | Constraint_Error =>
               Trace (Me,
                      "Exception when getting type of entity: "
                      & Exception_Information (E));
               Item.Entity := null;
         end;

         if Item.Entity = null then
            Trace (Me, "Result of Parse_Type is null");
         end if;
      end if;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value (Item : access Display_Item_Record'Class) is
      Value_Found : Boolean;
   begin
      if Item.Entity /= null and then Item.Is_A_Variable then
         begin
            Parse_Value
              (Item.Debugger.Debugger,
               Item.Name.all,
               Item.Entity,
               Format      => Item.Format,
               Value_Found => Value_Found);
            Set_Valid (Item.Entity, Value_Found);

         exception
            when Language.Unexpected_Type | Constraint_Error =>
               Set_Valid (Item.Entity, False);
         end;
      elsif Item.Entity /= null then
         Set_Valid (Item.Entity, True);
      end if;
   end Parse_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item           : out Display_Item;
      Browser        : not null access Debugger_Data_View_Record'Class;
      Graph_Cmd      : String;
      Variable_Name  : String;
      Num            : Integer;
      Debugger       : access Visual_Debugger_Record'Class;
      Auto_Refresh   : Boolean := True;
      Is_Dereference : Boolean := False;
      Default_Entity : Items.Generic_Type_Access := null)
   is
      Styles : constant access Browser_Styles := Browser.Get_View.Get_Styles;
      Alias_Item : Display_Item;
   begin
      Item                := new Display_Item_Record;
      Item.Browser        := General_Browser (Browser);
      Item.Graph_Cmd      := new String'(Graph_Cmd);
      Item.Entity         := Default_Entity;
      Item.Is_A_Variable  := Default_Entity = null;
      Item.Num            := Num;
      Item.Debugger       := Visual_Debugger (Debugger);
      Item.Name           := new String'(Variable_Name);
      Item.Auto_Refresh   := Auto_Refresh;
      Item.Is_Dereference := Is_Dereference;

      --  We need the information on the type, so that we detect aliases only
      --  for structurally equivalent types. If we have an error at this level,
      --  the variable might not be known yet, and we will simply try to
      --  refresh over and over again until we can parse the type

      Parse_Type (Item);

      if Item.Entity /= null then
         Set_Valid (Item.Entity, False);
      end if;

      --  If an auto-updated similar item is on the canvas, we simply show
      --  and select it.

      if Item.Entity /= null then
         if Variable_Name /= "" then
            Item.Id :=
              new String'(Get_Uniq_Id (Debugger.Debugger, Variable_Name));
         end if;

         if Item.Is_A_Variable then
            if Auto_Refresh then
               --  Avoid creating the same item twice if it already exists in
               --  the canvas

               Alias_Item :=
                 Search_Item (Debugger, Item.Id.all, Variable_Name);

               --  Two structures are aliased only if they have the same
               --  address and the same structure. The latter is to handle
               --  cases like "struct A {struct B {int field}} where A, B and
               --  field have the same address and would be considered as
               --  aliases otherwise.
               if Alias_Item /= null
                 and then
                   (not Typed_Aliases
                    or else
                      Structurally_Equivalent (Alias_Item.Entity, Item.Entity))
               then
                  Browser.Get_View.Model.Add_To_Selection (Alias_Item);
                  Browser.Get_View.Scroll_Into_View
                    (Alias_Item, Duration => 0.3);
                  Destroy (Item, In_Model => Browser.Get_View.Model);
                  Item := Alias_Item;
                  return;
               end if;
            end if;
         end if;

         Parse_Value (Item);
      end if;

      Item.Initialize_Rect (Styles.Item);
      Browser_Model (Browser.Get_View.Model).Add (Item);
      Item.Set_Position (No_Position);

      Item.Update_Display;

      --  ??? Should be changed when preferences are changed
      Item.Set_Size_Range
        (Max_Width  => Gdouble (Max_Item_Width.Get_Pref),
         Max_Height => Gdouble (Max_Item_Height.Get_Pref));

      if Get_Detect_Aliases (Debugger) then
         Recompute_All_Aliases (Debugger, False);
      end if;
   end Gtk_New;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (Item : not null access Display_Item_Record'Class)
   is
      Close   : Close_Button;
   begin
      Item.Clear (In_Model => Item.Browser.Get_View.Model);

      if Item.Auto_Refresh then
         Item.Set_Style (Item.Browser.Get_View.Get_Styles.Item);
      else
         Item.Set_Style (Debugger_Data_View (Item.Browser).Freeze);
      end if;

      Gtk_New (Close);
      Item.Setup_Titlebar
        (Browser => Item.Browser,
         Name    => Integer'Image (Item.Num) & ": " & Item.Name.all,
         Buttons => (1  => Close));

      if Item.Entity /= null then
         Item.Add_Child
           (Item.Entity.Build_Display
              (Item.Name.all,
               Debugger_Data_View (Item.Browser),
               Get_Language (Item), Item.Mode));
      end if;
   end Update_Display;

   -------------------
   -- Get_Graph_Cmd --
   -------------------

   function Get_Graph_Cmd
     (Item : access Display_Item_Record) return String
   is
      Rect : Point;
   begin
      --  ??? Should memorize auto-refresh state ("graph print" vs "display")
      if Item.Graph_Cmd /= null then
         Rect := Item.Position;
         if Item.Is_Alias_Of = null then
            return Item.Graph_Cmd.all & " at"
              & Gdouble'Image (Rect.X)
              & "," & Gdouble'Image (Rect.Y)
              & " num" & Integer'Image (Item.Num);
         else
            return Item.Graph_Cmd.all & " at"
              & Gdouble'Image (Rect.X) & "," & Gdouble'Image (Rect.Y)
              & " num" & Integer'Image (Item.Num)
              & " alias_of" & Integer'Image (Item.Is_Alias_Of.Num);
         end if;
      else
         return "";
      end if;
   end Get_Graph_Cmd;

   -----------------
   -- Is_Alias_Of --
   -----------------

   function Is_Alias_Of
     (Item       : access Display_Item_Record'Class;
      Id         : String;
      Name       : String;
      Deref_Name : String) return Boolean is
   begin
      --  Do not detect aliases that are already aliases, so as to
      --  avoid chains of aliases.
      --  Note also that X.all can not be an alias of X, so as to properly
      --  display string parameters in Ada (they appear otherwise as access
      --  types, which, once dereferenced, would point to themselves).

      return Item.Id /= null
        and then Item.Auto_Refresh
        and then Item.Id.all = Id
        and then Item.Name.all /= Deref_Name
        and then Name /= Dereference_Name
        (Get_Language (Item.Debugger.Debugger), Item.Name.all);
   end Is_Alias_Of;

   ---------------
   -- Find_Item --
   ---------------

   function Find_Item
     (Canvas : not null access Debugger_Data_View_Record'Class;
      Num    : Integer) return Display_Item
   is
      Found : Display_Item;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if Display_Item (Item).Num = Num then
            Found := Display_Item (Item);
         end if;
      end On_Item;
   begin
      Canvas.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);
      return Found;
   end Find_Item;

   -----------------
   -- Search_Item --
   -----------------

   function Search_Item
     (Process : access Visual_Debugger_Record'Class;
      Id      : String;
      Name    : String) return Display_Item
   is
      Alias_Item : Display_Item := null;
      Deref_Name : constant String := Dereference_Name
        (Get_Language (Process.Debugger), Name);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
         It : constant Display_Item := Display_Item (Item);
      begin
         if Alias_Item = null  --  not found yet
           and then (Name = "" or else It.Name.all = Name)
           and then Is_Alias_Of (It, Id, Name, Deref_Name)
         then
            if It.Is_Alias_Of /= null then
               Alias_Item := It.Is_Alias_Of;
            else
               Alias_Item := It;
            end if;
         end if;
      end On_Item;

   begin
      --  Always search if we have a special name to look for, so as to avoid
      --  creating the same item multiple times
      if Name /= "" or else Get_Detect_Aliases (Process) then
         Process.Data.Get_View.Model.For_Each_Item
           (On_Item'Access, Filter => Kind_Item);
      end if;
      return Alias_Item;
   end Search_Item;

   ------------
   -- Update --
   ------------

   procedure Update (Item : not null access Display_Item_Record'Class) is
   begin
      if Item.Is_A_Variable
        and then Item.Entity = null
      then
         Parse_Type (Item);
      end if;

      if Item.Entity /= null then
         --  Parse the value

         if Item.Entity.all in Debugger_Output_Type'Class then
            Set_Value
              (Debugger_Output_Type (Item.Entity.all),
               Process_User_Command
                 (Item.Debugger,
                  Refresh_Command (Debugger_Output_Type (Item.Entity.all)),
                  Mode => GVD.Types.Internal));

         elsif Item.Name /= null then
            Parse_Value (Item);
         end if;
      end if;

      Item.Update_Display;
   end Update;

   -----------------
   -- Create_Link --
   -----------------

   procedure Create_Link
     (Browser    : not null access Debugger_Data_View_Record'Class;
      From, To   : access Display_Item_Record'Class;
      Name       : String;
      Alias_Link : Boolean := False)
   is
      Styles : constant access Browser_Styles := Get_Styles (Browser.Get_View);
      L : GVD_Link;
   begin
      if not Browser.Has_Link (From, To) then
         L := new GVD_Link_Record;
         L.Alias_Link := Alias_Link;
         L.Default_Style := (if Alias_Link then Styles.Link2 else Styles.Link);
         L.Name := To_Unbounded_String (Name);
         L.Initialize
           (From     => From,
            To       => To,
            Style    => L.Default_Style,
            Routing  => Curve,
            Label    => Gtk_New_Text (Styles.Label, Name));
         Browser_Model (Browser.Get_View.Model).Add (L);
      end if;
   end Create_Link;

   ----------------------
   -- Dereference_Item --
   ----------------------

   procedure Dereference_Item
     (Item      : access Display_Item_Record;
      Component : not null access Component_Item_Record'Class)
   is
      Link_Name : constant String := To_String (Component.Name);
      New_Name  : constant String :=
                   Dereference_Name
                         (Get_Language (Item.Debugger.Debugger),
                          To_String (Component.Name));
      Link      : constant String :=
                   Dereference_Name
                     (Get_Language (Item.Debugger.Debugger), Link_Name);

   begin
      --  The newly created item should have the same auto-refresh state as
      --  the one we are dereferencing

      if Item.Auto_Refresh then
         Process_User_Command
           (Item.Debugger,
            "graph display """ & New_Name & """ dependent on"
            & Integer'Image (Item.Num) & " link_name " & Link,
            Output_Command => True);
      else
         Process_User_Command
           (Item.Debugger,
            "graph print """ & New_Name & """ dependent on"
            & Integer'Image (Item.Num) & " link_name " & Link,
            Output_Command => True);
      end if;
   end Dereference_Item;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Item : access Display_Item_Record) return Language.Language_Access is
   begin
      --  ??? This is incorrect since it returns the current language of the
      --  compiler, not the one used to create the item. Seems that it has
      --  worked for several years though
      return Get_Language (Item.Debugger.Debugger);
   end Get_Language;

   -----------------------
   -- Change_Visibility --
   -----------------------

   procedure Change_Visibility
     (Item      : access Display_Item_Record'Class;
      Component : Generic_Type_Access) is
   begin
      Set_Visibility (Component, not Get_Visibility (Component.all));

      --  Redraw the canvas
      Refresh_Data_Window (Item.Debugger);
   end Change_Visibility;

   ----------------------
   -- Set_Auto_Refresh --
   ----------------------

   procedure Set_Auto_Refresh
     (Item         : access Display_Item_Record;
      Auto_Refresh : Boolean;
      Update_Value : Boolean := False)
   is
   begin
      Item.Auto_Refresh := Auto_Refresh;

      if Update_Value then
         --  If we moved back to the auto-refresh state, force an
         --  update of the value.

         Reset_Recursive (Item);
         Update (Item);
      end if;

      Item.Browser.Get_View.Model.Refresh_Layout;  --  for links
   end Set_Auto_Refresh;

   ----------------------
   -- Get_Auto_Refresh --
   ----------------------

   function Get_Auto_Refresh
     (Item : access Display_Item_Record) return Boolean is
   begin
      return Item.Auto_Refresh;
   end Get_Auto_Refresh;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Display_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
   begin
      if Self.Entity /= null then
         Free (Self.Entity);
      end if;

      GNAT.Strings.Free (Self.Name);
      GNAT.Strings.Free (Self.Id);

      GPS_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   -------------------------
   -- Remove_With_Aliases --
   -------------------------

   procedure Remove_With_Aliases
     (Item : access Display_Item_Record;
      Remove_Aliases : Boolean)
   is
      Canvas : constant Debugger_Data_View := Item.Debugger.Data;
      To_Remove : Item_Sets.Set;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
         It : constant Display_Item := Display_Item (Item);
      begin
         --  If It is an alias of Item, and it wasn't displayed explicitly
         --  by the user, then remove it from the canvas as well.  Also
         --  remove It if it is currently hidden (alias detection), and was
         --  linked to Item. The user probably expects it to be killed as
         --  well

         if It.Is_Alias_Of = Display_Item (Item)
           and then It.Is_Dereference
         then
            To_Remove.Include (Abstract_Item (It));

            --  If It is hidden, and was linked to Item
         elsif It.Is_Alias_Of /= null
           and then Canvas.Has_Link (GPS_Item (Item), GPS_Item (It))
         then
            To_Remove.Include (Abstract_Item (It));
         end if;

      end On_Item;

   begin
      --  Should recompute aliases (delete all the items that we aliased
      --  to this one, since the user was probably expecting them not to be
      --  visible any more).

      To_Remove.Include (Abstract_Item (Item));

      if Remove_Aliases then
         Canvas.Get_View.Model.For_Each_Item
           (On_Item'Access, Filter => Kind_Item);
      end if;

      if Remove_Aliases then
         Recompute_All_Aliases (Item.Debugger, Recompute_Values => False);
      end if;

      Canvas.Get_View.Model.Remove (To_Remove);
   end Remove_With_Aliases;

   ---------------------------
   -- Recompute_All_Aliases --
   ---------------------------

   procedure Recompute_All_Aliases
     (Process          : access Visual_Debugger_Record'Class;
      Recompute_Values : Boolean := True)
   is
      procedure Remove_Temporary
        (Item : not null access Abstract_Item_Record'Class);
      --  Remove all temporary links from the canvas

      procedure Recompute_Address
        (Item : not null access Abstract_Item_Record'Class);
      --  Recompute the address of the item, and identify which ones should be
      --  displayed and which ones should be hidden as aliases

      procedure Build_Aliases
        (Item : not null access Abstract_Item_Record'Class);
      --  Using the result of Recompute_Address, compute which items should be
      --  aliases

      procedure Make_Visible
        (Item : not null access Abstract_Item_Record'Class);
      --  Make the item visible

      procedure Update_Value
        (Item : not null access Abstract_Item_Record'Class);
      --  Update the value of a specific item in the canvas. The new value is
      --  read from the debugger, parsed, and redisplayed.
      --  Do nothing if the auto-refresh status of Item is set to false.

      To_Remove : Item_Sets.Set;
      To_Hide   : Item_Sets.Set;

      Addresses : String_To_Items.Map;
      --  Maps addresses to items, to identify aliases

      ------------------
      -- Make_Visible --
      ------------------

      procedure Make_Visible
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         Item.Show;
      end Make_Visible;

      ----------------------
      -- Remove_Temporary --
      ----------------------

      procedure Remove_Temporary
        (Item : not null access Abstract_Item_Record'Class)
      is
         Link : constant GVD_Link := GVD_Link (Item);
      begin
         if Link.Alias_Link then
            To_Remove.Include (Abstract_Item (Item));
         else
            Link.Show;  --  in case it was hidden before
         end if;
      end Remove_Temporary;

      -----------------------
      -- Recompute_Address --
      -----------------------

      procedure Recompute_Address
        (Item : not null access Abstract_Item_Record'Class)
      is
         It            : constant Display_Item := Display_Item (Item);
         Keeper : Display_Item;
      begin
         It.Show;
         It.Was_Alias := It.Is_Alias_Of /= null;
         It.Is_Alias_Of := null;

         --  If this is not an item associated with a variable, ignore it
         --  Only detect aliases if we have an auto_refresh item
         if It.Name = null
           or else not It.Auto_Refresh
           or else not Is_A_Variable (It)
         then
            return;
         end if;

         --  Else recompute the id for the item
         GNAT.Strings.Free (It.Id);

         declare
            Id : constant String :=
              Get_Uniq_Id (It.Debugger.Debugger, It.Name.all);
         begin
            if Id /= "" then
               It.Id := new String'(Id);

               if Addresses.Contains (Id) then
                  --  We want to hide dereferences when possible, and keep all
                  --  items explicitly display
                  Keeper := Addresses.Element (Id);
                  if Keeper.Is_Dereference and then not It.Is_Dereference then
                     Addresses.Include (Id, It);
                  end if;
               else
                  Addresses.Include (Id, It);
               end if;
            end if;
         end;
      end Recompute_Address;

      -------------------
      -- Build_Aliases --
      -------------------

      procedure Build_Aliases
        (Item : not null access Abstract_Item_Record'Class)
      is
         It : constant Display_Item := Display_Item (Item);
         Keeper : Display_Item;
         S      : Item_Sets.Set;

         procedure Duplicate_Links
           (Link : not null access Abstract_Item_Record'Class);
         --  Duplicate the links that go to an item that is being hidden

         procedure Duplicate_Links
           (Link : not null access Abstract_Item_Record'Class)
         is
            Src, Dest : Display_Item;
            Replace   : Boolean;
         begin
            if not GVD_Link (Link).Alias_Link then
               Src := Display_Item (GVD_Link (Link).Get_From);
               Dest := Display_Item (GVD_Link (Link).Get_To);
               Replace := False;

               if Src = It then
                  Src := It.Is_Alias_Of;
                  Replace := True;
               elsif Dest = It then
                  Dest := It.Is_Alias_Of;
                  Replace := True;
               end if;

               if Replace then
                  Create_Link
                    (Process.Data, Src, Dest,
                     Name       => To_String (GVD_Link (Link).Name),
                     Alias_Link => True);
               end if;
            end if;
         end Duplicate_Links;

         List : Items_Lists.List;
      begin
         if It.Id /= null and then Addresses.Contains (It.Id.all) then
            Keeper := Addresses.Element (It.Id.all);
            if It = Keeper then
               null;  --  nothing to do

            elsif It.Is_Dereference then
               Process.Data.Get_View.Model.Include_Related_Items (It, To_Hide);
               It.Is_Alias_Of := Keeper;

               S.Include (Abstract_Item (It));
               Process.Data.Get_View.Model.For_Each_Link
                 (Duplicate_Links'Access,
                  From_Or_To => S);

            else
               --  not a dereference, so Keeper is not a dereference either
               Create_Link
                 (Process.Data, It, Keeper, "<=>", Alias_Link => True);
            end if;
         end if;

         --  If we broke the alias, move the item back to some new coordinates
         if It.Is_Alias_Of = null
           and then It.Was_Alias
         then
            List.Append (Abstract_Item (It));
            Insert_And_Layout_Items
              (Process.Data.Get_View,
               Ref       => It,
               Items     => List,
               Direction => Right,
               Duration  => 0.3);
         end if;
      end Build_Aliases;

      ------------------
      -- Update_Value --
      ------------------

      procedure Update_Value
        (Item : not null access Abstract_Item_Record'Class)
      is
         It : constant Display_Item := Display_Item (Item);
      begin
         if It.Auto_Refresh and then It.Is_Alias_Of = null then
            Update (It);
         end if;
      end Update_Value;

   begin
      Process.Data.Get_View.Model.For_Each_Item
        (Remove_Temporary'Access, Filter => Kind_Link);
      Process.Data.Get_View.Model.Remove (To_Remove);
      To_Remove.Clear;

      --  First: Recompile all the addresses, and detect the aliases
      if Get_Detect_Aliases (Process) then
         Process.Data.Get_View.Model.For_Each_Item
           (Recompute_Address'Access, Filter => Kind_Item);
         Process.Data.Get_View.Model.For_Each_Item
           (Build_Aliases'Access, Filter => Kind_Item);
      else
         Process.Data.Get_View.Model.For_Each_Item
           (Make_Visible'Access, Filter => Kind_Item);
      end if;

      for L of To_Hide loop
         L.Hide;
      end loop;

      --  Then re-parse the value of each item and display them again
      if Recompute_Values then
         Process.Data.Get_View.Model.For_Each_Item
           (Update_Value'Access, Filter => Kind_Item);
      end if;
   end Recompute_All_Aliases;

   ---------------------
   -- Reset_Recursive --
   ---------------------

   procedure Reset_Recursive (Item : access Display_Item_Record'Class) is
   begin
      if Item.Entity /= null then
         Reset_Recursive (Item.Entity);
      end if;
   end Reset_Recursive;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : access Display_Item_Record) return String is
   begin
      if Item.Name = null then
         return "";
      end if;

      return Item.Name.all;
   end Get_Name;

   -------------
   -- Get_Num --
   -------------

   function Get_Num (Item : access Display_Item_Record) return Integer is
   begin
      return Item.Num;
   end Get_Num;

   ------------------
   -- Get_Debugger --
   ------------------

   function Get_Debugger
     (Item : access Display_Item_Record'Class) return Visual_Debugger is
   begin
      return Item.Debugger;
   end Get_Debugger;

   -------------------
   -- Is_A_Variable --
   -------------------

   function Is_A_Variable
     (Item : access Display_Item_Record'Class) return Boolean is
   begin
      return Item.Is_A_Variable;
   end Is_A_Variable;

   ----------------------
   -- Set_Display_Mode --
   ----------------------

   procedure Set_Display_Mode
     (Item : access Display_Item_Record'Class;
      Mode : Items.Display_Mode) is
   begin
      Item.Mode := Mode;
      Update (Item);
      Item.Browser.Get_View.Model.Refresh_Layout;
   end Set_Display_Mode;

   ----------------------
   -- Get_Display_Mode --
   ----------------------

   function Get_Display_Mode
     (Item : access Display_Item_Record) return Items.Display_Mode is
   begin
      return Item.Mode;
   end Get_Display_Mode;

   ----------------
   -- Set_Format --
   ----------------

   procedure Set_Format
     (Item   : access Display_Item_Record'Class;
      Format : Debugger.Value_Format) is
   begin
      if Format /= Item.Format then
         Item.Format := Format;
         Update (Item);
         Item.Browser.Get_View.Model.Refresh_Layout;
      end if;
   end Set_Format;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format
     (Item : access Display_Item_Record) return Debugger.Value_Format is
   begin
      return Item.Format;
   end Get_Format;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Item : access Display_Item_Record'Class)
      return Items.Generic_Type_Access is
   begin
      return Item.Entity;
   end Get_Entity;

end Display_Items;
