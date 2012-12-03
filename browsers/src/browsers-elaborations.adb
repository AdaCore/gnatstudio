------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

with Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Browsers.Canvas;           use Browsers.Canvas;
with Cairo;                     use Cairo;
with Default_Preferences;       use Default_Preferences;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Tools_Output;   use GPS.Kernel.Tools_Output;
with GPS.Intl;                  use GPS.Intl;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Canvas;             use Gtkada.Canvas;
with Gtkada.MDI;                use Gtkada.MDI;
with Pango.Layout;              use Pango.Layout;
with Traces;                    use Traces;

with Elaboration_Cycles;        use Elaboration_Cycles;
with Browsers.Elaborations.Cycle_Parser;

---------------------------
-- Browsers.Elaborations --
---------------------------

package body Browsers.Elaborations is

   type Elaboration_Browser_Module_Record is
     new Module_ID_Record with null record;

   Elaboration_Browser_Module : Module_ID;

   Output_Parser : aliased Cycle_Parser.Output_Parser_Fabric;
   --  Fabric to create output parser

   Last_Elaboration_Cycle : Elaboration_Cycles.Cycle;
   --  Last elaboration cycle reported by gnatbind

   Auto_Show_Preference : Boolean_Preference;
   --  Allow auto-display found elaboration cycles after each compilation

   --  Browser type
   type Elaboration_Browser_Record is
     new Browsers.Canvas.General_Browser_Record
   with record
      Cycle : Elaboration_Cycles.Cycle;
   end record;
   type Elaboration_Browser is access all Elaboration_Browser_Record'Class;

   --  Node to represent compilation unit in browser
   type Unit_Item_Record is new Browsers.Canvas.Browser_Item_Record with record
      Name : Unbounded_String;
   end record;
   type Unit_Item is access all Unit_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Unit_Item;
      Name    : String;
      Browser : access Browsers.Canvas.General_Browser_Record'Class);
   --  Open a new item in the browser that represents unit.

   procedure Initialize
     (Item    : access Unit_Item_Record'Class;
      Name    : String;
      Browser : access Browsers.Canvas.General_Browser_Record'Class);
   --  Internal initialization function

   overriding procedure Destroy (Item : in out Unit_Item_Record) is null;
   --  See doc for inherited subprograms

   overriding procedure Resize_And_Draw
     (Item             : access Unit_Item_Record;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprograms

   --  Node to represent Elaborate_All dependency in browser
   type Dependency_Item_Record is new Browsers.Canvas.Browser_Item_Record
   with record
      Lines  : Xref_List;
   end record;
   type Dependency_Item is access all Dependency_Item_Record'Class;

   procedure Gtk_New
     (Item    : out Dependency_Item;
      Dep     : Dependency;
      Browser : access Browsers.Canvas.General_Browser_Record'Class);
   --  Open a new item in the browser that represents Elaborate_All dependency

   procedure Initialize
     (Item    : access Dependency_Item_Record'Class;
      Dep     : Dependency;
      Browser : access Browsers.Canvas.General_Browser_Record'Class);
   --  Internal initialization function

   overriding procedure Resize_And_Draw
     (Item             : access Dependency_Item_Record;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprograms

   overriding procedure Destroy (Item : in out Dependency_Item_Record);
   --  See doc for inherited subprograms

   function Open_Elaboration_Browser_Child
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Create (or return an existing) browser, and insert it directly into
   --  the MDI.
   --  If a new browser is created, it is initially empty.
   --  If the browser already existed, it is raised.

   function Open_Elaboration_Browser
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Elaboration_Browser;
   --  Create (or return an existing) browser. The newly created browser
   --  is not inserted into the MDI.

   procedure On_Elaboration_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Browsers->Elaboration Cycles browser menu callback

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  compilation finished hook callback

   procedure Fill_Browser
     (Kernel : Kernel_Handle;
      Cycle  : Elaboration_Cycles.Cycle);
   --  Open browser and fill it with nodes correspond to Cycle data.

   function Get_Unit
     (Browser   : Elaboration_Browser;
      Unit_Name : String) return Unit_Item;
   --  Find or create unit in Browser

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Browser   : Elaboration_Browser;
      Unit_Name : String) return Unit_Item
   is
      Iter : Item_Iterator := Start (Get_Canvas (Browser));
      Item : Unit_Item;
   begin
      loop
         exit when Get (Iter) = null;

         if Get (Iter).all in Unit_Item_Record
           and then Unit_Item (Get (Iter)).Name = Unit_Name
         then
            Item := Unit_Item (Get (Iter));
            exit;
         end if;

         Next (Iter);
      end loop;

      if Item = null then
         Gtk_New (Item, Unit_Name, Browser);
         Put (Get_Canvas (Browser), Item);
      end if;

      return Item;
   end Get_Unit;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Unit_Item;
      Name    : String;
      Browser : access Browsers.Canvas.General_Browser_Record'Class) is
   begin
      Item := new Unit_Item_Record;
      Elaborations.Initialize (Item, Name, Browser);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Unit_Item_Record'Class;
      Name    : String;
      Browser : access Browsers.Canvas.General_Browser_Record'Class) is
   begin
      Item.Name := To_Unbounded_String (Name);

      Initialize (Item, Browser);
      Set_Title (Item, Name);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out Dependency_Item;
      Dep     : Dependency;
      Browser : access Browsers.Canvas.General_Browser_Record'Class) is
   begin
      Item := new Dependency_Item_Record;
      Elaborations.Initialize (Item, Dep, Browser);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access Dependency_Item_Record'Class;
      Dep     : Dependency;
      Browser : access Browsers.Canvas.General_Browser_Record'Class) is
   begin
      Add_Line (Item.Lines, After_Unit_Name (Dep));
      for J in reverse 1 .. Links_Count (Dep) loop
         declare
            Next : constant Link := Element (Dep, J);
         begin
            case Kind (Next) is
               when Withed =>
                  Add_Line (Item.Lines, "  withes:");
               when Body_With_Specification =>
                  Add_Line (Item.Lines, "  its body:");
            end case;
            Add_Line (Item.Lines, Unit_Name (Next));
         end;
      end loop;

      Initialize (Item, Browser);

      if Reason (Dep) = Elaborate_All_Desirable then
         Set_Title (Item, "implicit Elaborate All");
      else
         Set_Title (Item, "Elaborate All");
      end if;
   end Initialize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Item : in out Dependency_Item_Record) is
   begin
      Free (Item.Lines);
   end Destroy;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Unit_Item_Record;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class) is
   begin
      Resize_And_Draw
        (Browser_Item_Record (Item.all)'Access, Cr,
         Width, Height,
         Width_Offset,
         Height_Offset, Xoffset, Yoffset, Layout);
   end Resize_And_Draw;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Dependency_Item_Record;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      W, H, Y                        : Gint;
      Layout_H, Layout_W1, Layout_W2 : Gint;
   begin
      Get_Pixel_Size
        (Get_Browser (Item),
         Item.Lines,
         Layout_W1,
         Layout_W2,
         Layout_H,
         Layout);

      W := Gint'Max (Width, Layout_W1 + Layout_W2);
      H := Height + Layout_H;

      Resize_And_Draw
        (Browser_Item_Record (Item.all)'Access, Cr, W, H,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Margin + Yoffset;

      Display_Lines (Item, Cr, Item.Lines, Margin + Xoffset, Y, 0, Layout);
   end Resize_And_Draw;

   ------------------------------
   -- Open_Elaboration_Browser --
   ------------------------------

   function Open_Elaboration_Browser
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Elaboration_Browser
   is
      Browser    : constant Elaboration_Browser
        := new Elaboration_Browser_Record;
   begin
      Initialize
        (Browser, Kernel,
         Create_Toolbar  => True);

      --  Register menu with default actions
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Elaboration_Browser_Module,
         Context_Func    => Default_Browser_Context_Factory'Access);

      return Browser;
   end Open_Elaboration_Browser;

   ------------------------------------
   -- Open_Elaboration_Browser_Child --
   ------------------------------------

   function Open_Elaboration_Browser_Child
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : GPS_MDI_Child;
      Browser : Elaboration_Browser;
      Title   : constant String := -"Elaboration Cycles";
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Elaboration_Browser_Record'Tag));

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Open_Elaboration_Browser (Kernel);
         Gtk_New (Child, Browser,
                  Focus_Widget   => Gtk_Widget (Get_Canvas (Browser)),
                  Default_Width  => Gint (Default_Widget_Width.Get_Pref),
                  Default_Height => Gint (Default_Widget_Height.Get_Pref),
                  Group          => Group_Graphs,
                  Module         => Elaboration_Browser_Module);

         Set_Title (Child, Title);
         Put (Get_MDI (Kernel), Child);
         Set_Focus_Child (Child);
      end if;

      Add_Navigation_Location (Kernel, Title);

      return MDI_Child (Child);
   end Open_Elaboration_Browser_Child;

   ------------------
   -- Fill_Browser --
   ------------------

   procedure Fill_Browser
     (Kernel : Kernel_Handle;
      Cycle  : Elaboration_Cycles.Cycle)
   is
      use type Ada.Containers.Count_Type;

      Child   : constant Gtkada.MDI.MDI_Child
        := Open_Elaboration_Browser_Child (Kernel);
      Browser : constant Elaboration_Browser
        := Elaboration_Browser (Get_Widget (Child));
   begin
      Browser.Cycle := Cycle;

      Clear (Get_Canvas (Browser));

      for J in 1 .. Dependencies_Count (Cycle) loop
         declare
            Dep    : constant Dependency := Element (Cycle, J);
            Item_A : constant Unit_Item
              := Get_Unit (Browser, "Unit: " & After_Unit_Name (Dep));
            Item_B : constant Unit_Item
              := Get_Unit (Browser, "Unit: " & Before_Unit_Name (Dep));
         begin
            if Reason (Dep) in
              Pragma_Elaborate_All .. Elaborate_All_Desirable
            then
               declare
                  Link_A : constant Browser_Link := new Browser_Link_Record;
                  Link_B : constant Browser_Link := new Browser_Link_Record;
                  Over   : Dependency_Item;
               begin
                  Gtk_New (Over, Dep, Browser);
                  Put (Get_Canvas (Browser), Over);

                  Add_Link
                    (Get_Canvas (Browser),
                     Link_A,
                     Item_A,
                     Over);

                  Add_Link
                    (Get_Canvas (Browser),
                     Link_B,
                     Over,
                     Item_B);

                  Refresh (Over);
               end;
            else
               declare
                  Link   : constant Browser_Link := new Browser_Link_Record;
               begin
                  Add_Link
                    (Get_Canvas (Browser),
                     Link,
                     Item_A,
                     Item_B,
                     Descr => (Reason (Dep)'Img));
               end;
            end if;

            Refresh (Item_A);
            Refresh (Item_B);
         end;
      end loop;

      Layout (Browser);
      Refresh_Canvas (Get_Canvas (Browser));

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Fill_Browser;

   -----------------------------
   -- On_Compilation_Finished --
   -----------------------------

   procedure On_Compilation_Finished
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      use type Ada.Containers.Count_Type;

      Hook_Data : constant
        GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args :=
          GPS.Kernel.Standard_Hooks.Compilation_Finished_Hooks_Args (Data.all);

      Cycle   : Elaboration_Cycles.Cycle renames Last_Elaboration_Cycle;

      Show : constant Boolean := Get_Pref (Auto_Show_Preference);
   begin
      if not Show
        or else Hook_Data.Status = 0
        or else Dependencies_Count (Cycle) = 0
      then
         return;
      end if;

      Fill_Browser (Kernel_Handle (Kernel), Cycle);
   end On_Compilation_Finished;

   ----------------------------
   -- On_Elaboration_Browser --
   ----------------------------

   procedure On_Elaboration_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Fill_Browser (Kernel, Last_Elaboration_Cycle);
   end On_Elaboration_Browser;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Elaboration_Browser_Module := new Elaboration_Browser_Module_Record;

      Register_Module
        (Module      => Elaboration_Browser_Module,
         Kernel      => Kernel,
         Module_Name => "Elaboration_Browser");

      Register_Menu
        (Kernel      => Kernel,
         Parent_Path => '/' & (-"Tools") & '/' & (-"Browsers"),
         Text        => -"_Elaboration Cycles",
         Callback    => On_Elaboration_Browser'Access);

      GPS.Kernel.Hooks.Add_Hook
        (Kernel, GPS.Kernel.Compilation_Finished_Hook,
         GPS.Kernel.Hooks.Wrapper (On_Compilation_Finished'Access),
         Name => "gnatstack.compilation_finished");

      Register_Output_Parser (Output_Parser'Access, Line_By_Line + 20);

      Auto_Show_Preference := Create
        (Get_Preferences (Kernel),
         Name    => "Auto-Show-Elaboration-Cycles",
         Label   => -"Show elaboration cycles",
         Page    => -"Browsers",
         Doc     => -"Display elaboration cycles in browser after compilation",
         Default => True);
   end Register_Module;

   ---------------------------
   -- Set_Elaboration_Cycle --
   ---------------------------

   procedure Set_Elaboration_Cycle (Value : Elaboration_Cycles.Cycle) is
   begin
      Last_Elaboration_Cycle := Value;
   end Set_Elaboration_Cycle;

end Browsers.Elaborations;
