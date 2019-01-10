------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012-2019, AdaCore                   --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Browsers.Canvas;           use Browsers.Canvas;
with Default_Preferences;       use Default_Preferences;
with Generic_Views;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Tools_Output;          use GPS.Tools_Output;
with GPS.Intl;                  use GPS.Intl;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.MDI;                use Gtkada.MDI;

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
     (View   : access Elaboration_Browser_Record'Class)
      return Gtk_Widget;
   --  Initialize the view and returns the focus widget

   package Elaboration_Views is new Generic_Views.Simple_Views
     (Module_Name            => "Elaboration_Browser",
      View_Name              => -"Elaboration Circularities",
      Formal_View_Record     => Elaboration_Browser_Record,
      Formal_MDI_Child       => Browser_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Default);
   subtype Elaboration_Browser is Elaboration_Views.View_Access;

   --  Node to represent compilation unit in browser
   type Unit_Item_Record is new GPS_Item_Record with record
      Name : Unbounded_String;
   end record;
   type Unit_Item is access all Unit_Item_Record'Class;

   overriding procedure Set_Context
     (Item    : not null access Unit_Item_Record;
      Context : in out Selection_Context) is null;

   type On_Compilation_Finished is new Compilation_Finished_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : On_Compilation_Finished;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Category, Target, Mode : String;
      Shadow, Background : Boolean;
      Status : Integer);
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
      function Strip_Unit_Kind (Unit_Name : String) return String;
      --  Strip (spec) and (body) from Unit_Name

      ---------------------
      -- Strip_Unit_Kind --
      ---------------------

      function Strip_Unit_Kind (Unit_Name : String) return String is
         Space : constant Natural := Ada.Strings.Fixed.Index
           (Unit_Name, " ", Ada.Strings.Backward);
      begin
         if Space in Unit_Name'Range then
            return Unit_Name (Unit_Name'First .. Space - 1);
         else
            return Unit_Name;
         end if;
      end Strip_Unit_Kind;

      Unit_Without_Kind : constant String := Strip_Unit_Kind (Unit_Name);
      Item : Unit_Item := null;
      S    : constant access Browser_Styles := Browser.Get_View.Get_Styles;

      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
      begin
         if Unit_Item (It).Name = Unit_Without_Kind then
            Item := Unit_Item (It);
         end if;
      end On_Item;

   begin
      Browser.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);

      if Item = null then
         Item         := new Unit_Item_Record;
         Item.Name    := To_Unbounded_String (Unit_Without_Kind);
         Item.Browser := General_Browser (Browser);

         Browser_Model (Browser.Get_View.Model).Add (Item);
         Item.Set_Position (No_Position);

         Item.Initialize_Rect (Style => S.Item, Radius => 5.0);
         Setup_Titlebar (Item, Browser, Name => "Unit: " & Unit_Without_Kind);
      end if;

      return Item;
   end Get_Unit;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Elaboration_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Browsers.Canvas.Initialize (View);
      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View);
      return Gtk_Widget (View.Get_View);
   end Initialize;

   ------------------
   -- Fill_Browser --
   ------------------

   procedure Fill_Browser
     (Kernel : Kernel_Handle;
      Cycle  : Elaboration_Cycles.Cycle)
   is
      Browser : constant Elaboration_Browser :=
        Elaboration_Views.Get_Or_Create_View (Kernel, Focus => True);
      Styles : constant access Browser_Styles :=
        Browser.Get_View.Get_Styles;

      procedure Fill_Elaborate_All
        (Item_After    : Unit_Item;
         Elaborate_All : Dependency);
      --  Put all units of Elaborate_All dependency in browser

      function Kind_Image (Kind : Link_Kind) return String;
      --  Return the label to use for a link of that kind.

      procedure Add_Link (It1, It2 : Unit_Item; Descr : String);

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

      --------------
      -- Add_Link --
      --------------

      procedure Add_Link (It1, It2 : Unit_Item; Descr : String) is
         Link         : GPS_Link;
         Offset       : Natural;
         Already_Have : Boolean;

         procedure Look_For_Links
           (Has_Exact   : out Boolean;
            Total_Count : out Natural);
         --  Look for links between It1 and It2, count total count of them.
         --  Also check if there is a link with label = Descr already.

         --------------------
         -- Look_For_Links --
         --------------------

         procedure Look_For_Links
           (Has_Exact   : out Boolean;
            Total_Count : out Natural)
         is

            procedure On_Link
              (Item : not null access Abstract_Item_Record'Class);

            -------------
            -- On_Link --
            -------------

            procedure On_Link
              (Item : not null access Abstract_Item_Record'Class)
            is
               Label : Container_Item;
            begin
               if Item.all in GPS_Link_Record'Class
                 and then GPS_Link (Item).Get_To = Abstract_Item (It2)
                 and then GPS_Link (Item).Get_From = Abstract_Item (It1)
               then
                  Total_Count := Total_Count + 1;
                  Label := GPS_Link (Item).Get_Label;
                  if Text_Item (Label).Get_Text = Descr then
                     Has_Exact := True;
                  end if;
               end if;
            end On_Link;

            S : Item_Sets.Set;

         begin
            Total_Count := 0;
            Has_Exact := False;
            S.Include (Abstract_Item (It1));
            Browser.Get_View.Model.For_Each_Link
              (On_Link'Access, From_Or_To => S);
         end Look_For_Links;

      begin
         Look_For_Links (Already_Have, Total_Count => Offset);

         if Already_Have then
            return;
         end if;

         Link := new GPS_Link_Record;
         Link.Default_Style := Styles.Link;

         Initialize
           (Link,
            From    => It1,
            To      => It2,
            Routing => Arc,
            Label   => Gtk_New_Text (Styles.Label, Descr),
            Style   => Link.Default_Style);

         if Offset mod 2 = 1 then
            Link.Set_Offset (Gdouble ((Offset + 1) / 2) * 10.0);
         else
            Link.Set_Offset (Gdouble (-Offset / 2 + 1) * 10.0);
         end if;

         Browser_Model (Browser.Get_View.Model).Add (Link);
      end Add_Link;

      ------------------------
      -- Fill_Elaborate_All --
      ------------------------

      procedure Fill_Elaborate_All
        (Item_After    : Unit_Item;
         Elaborate_All : Dependency)
      is
         Prev_Unit : Unit_Item := Item_After;
      begin
         for J in reverse 1 .. Links_Count (Elaborate_All) loop
            if Kind (Element (Elaborate_All, J)) = Withed then
               declare
                  Next      : constant Link := Element (Elaborate_All, J);
                  Next_Unit : constant Unit_Item :=
                    Get_Unit (Browser, Unit_Name (Next));
               begin
                  Add_Link (Prev_Unit, Next_Unit, Kind_Image (Kind (Next)));
                  Prev_Unit := Next_Unit;
               end;
            end if;
         end loop;
      end Fill_Elaborate_All;

   begin
      Browser.Cycle := Cycle;
      Browser_Model (Browser.Get_View.Model).Clear;

      for J in 1 .. Dependencies_Count (Cycle) loop
         declare
            Dep    : constant Dependency := Element (Cycle, J);
            Item_A : constant Unit_Item :=
              Get_Unit (Browser, After_Unit_Name (Dep));
            Item_B : constant Unit_Item :=
              Get_Unit (Browser, Before_Unit_Name (Dep));
         begin
            if Reason (Dep) in
              Pragma_Elaborate_All .. Elaborate_All_Desirable
            then
               Fill_Elaborate_All (Item_A, Dep);
            else
               Add_Link (Item_A, Item_B, Image (Reason (Dep)));
            end if;
         end;
      end loop;

      Browser.Refresh_Layout
        (Rescale => True,
         Space_Between_Items  => 40.0,
         Space_Between_Layers => 60.0);  --  long labels in this browser
   end Fill_Browser;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Compilation_Finished;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Category, Target, Mode : String;
      Shadow, Background : Boolean;
      Status : Integer)
   is
      pragma Unreferenced (Self, Category, Target, Mode, Shadow, Background);

      Cycle   : Elaboration_Cycles.Cycle renames Last_Elaboration_Cycle;
      Show : constant Boolean := Get_Pref (Auto_Show_Preference);

   begin
      if Show
        and then Status /= 0
        and then Dependencies_Count (Cycle) /= 0
      then
         Fill_Browser (Kernel_Handle (Kernel), Cycle);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Elaboration_Views.Register_Module (Kernel);

      Compilation_Finished_Hook.Add (new On_Compilation_Finished);

      Register_Output_Parser (Output_Parser'Access, "elaboration_cycles");

      Auto_Show_Preference := Create
        (Get_Preferences (Kernel),
         Path    => -"Browsers:Display",
         Name    => "Auto-Show-Elaboration-Cycles",
         Label   => -"Show elaboration cycles",
         Doc    => -"Display elaboration cycles in browser after compilation.",
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
