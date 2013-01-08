------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012-2013, AdaCore                   --
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
with Generic_Views;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Tools_Output;   use GPS.Kernel.Tools_Output;
with GPS.Intl;                  use GPS.Intl;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Canvas;             use Gtkada.Canvas;
with Gtkada.MDI;                use Gtkada.MDI;
with Pango.Layout;              use Pango.Layout;

with Elaboration_Cycles;        use Elaboration_Cycles;
with Browsers.Elaborations.Cycle_Parser;

package body Browsers.Elaborations is

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

   function Initialize
     (View   : access Elaboration_Browser_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   --  Initialize the view and returns the focus widget

   package Elaboration_Views is new Generic_Views.Simple_Views
     (Module_Name            => "Elaboration_Browser",
      View_Name              => -"Elaboration Circularities",
      Formal_View_Record     => Elaboration_Browser_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Graphs);
   subtype Elaboration_Browser is Elaboration_Views.View_Access;

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

   procedure Fill_Elaborate_All
     (Browser       : Elaboration_Browser;
      Item_After    : Unit_Item;
      Elaborate_All : Dependency);
   --  Put all units of Elaborate_All dependency in browser

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

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Elaboration_Browser_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget is
   begin
      Initialize (View, Kernel, Create_Toolbar  => True);

      --  Register menu with default actions
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Elaboration_Views.Get_Module,
         Context_Func    => Default_Browser_Context_Factory'Access);
      return Gtk_Widget (View);
   end Initialize;

   ------------------------
   -- Fill_Elaborate_All --
   ------------------------

   procedure Fill_Elaborate_All
     (Browser       : Elaboration_Browser;
      Item_After    : Unit_Item;
      Elaborate_All : Dependency)
   is
      function Kind_Image (Kind : Link_Kind) return String;

      ----------------
      -- Kind_Image --
      ----------------

      function Kind_Image (Kind : Link_Kind) return String is
      begin
         case Kind is
            when Withed =>
               return "with";
            when Body_With_Specification =>
               return "body";
         end case;
      end Kind_Image;

      Prev_Unit : Unit_Item := Item_After;
   begin
      for J in reverse 1 .. Links_Count (Elaborate_All) loop
         declare
            Next      : constant Link := Element (Elaborate_All, J);
            Next_Unit : constant Unit_Item
              := Get_Unit (Browser, "Unit: " & Unit_Name (Next));
            Link      : constant Browser_Link := new Browser_Link_Record;
         begin
            Add_Link
              (Get_Canvas (Browser),
               Link,
               Prev_Unit,
               Next_Unit,
               Descr => Kind_Image (Kind (Next)));
            Refresh (Next_Unit);
            Prev_Unit := Next_Unit;
         end;
      end loop;

      Refresh (Item_After);
   end Fill_Elaborate_All;

   ------------------
   -- Fill_Browser --
   ------------------

   procedure Fill_Browser
     (Kernel : Kernel_Handle;
      Cycle  : Elaboration_Cycles.Cycle)
   is
      use type Ada.Containers.Count_Type;
      Browser : constant Elaboration_Browser :=
        Elaboration_Views.Get_Or_Create_View (Kernel, Focus => True);
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
               Fill_Elaborate_All (Browser, Item_A, Dep);
            else
               declare
                  Link : constant Browser_Link := new Browser_Link_Record;
               begin
                  Add_Link
                    (Get_Canvas (Browser),
                     Link,
                     Item_A,
                     Item_B,
                     Descr => Image (Reason (Dep)));
               end;
            end if;

            Refresh (Item_A);
            Refresh (Item_B);
         end;
      end loop;

      Layout (Browser);
      Refresh_Canvas (Get_Canvas (Browser));
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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Elaboration_Views.Register_Module
        (Kernel,
         Menu_Name => -"Browsers/_Elaboration Circularities");

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
