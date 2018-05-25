------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

with GNAT.Serial_Communications;

with Glib;                      use Glib;
with Glib.Values;               use Glib.Values;

with Gdk.RGBA;                  use Gdk.RGBA;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Generic_Views;
with Glib_Values_Utils;         use Glib_Values_Utils;

package body Serial_Ports_Views is

   type Serial_Ports_View_Record is new Generic_Views.View_Record with record
      Tree  : Gtk_Tree_View;
      Model : Gtk_Tree_Store;
   end record;

--     type View_Access is access all Serial_Ports_View_Record'Class;

   function Initialize
     (View : access Serial_Ports_View_Record'Class)
      return Gtk_Widget;
   --  Create a new log view

   procedure Refresh (View : access Serial_Ports_View_Record'Class);
   --  Refresh the contents of the log view

   package Serial_Port_View is new Generic_Views.Simple_Views
     (Module_Name        => "Serial_Ports_View",
      View_Name          => "Serial Ports View",
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Formal_View_Record => Serial_Ports_View_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Consoles,
      Position           => Position_Bottom,
      Initialize         => Initialize);
   subtype Serial_Ports_View_Access is Serial_Port_View.View_Access;
   use Serial_Port_View;

   type Refresh_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Refresh_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Refresh view

   Column_Value          : constant := 0;
   Column_Color          : constant := 1;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View : access Serial_Ports_View_Record'Class) return Gtk_Widget
   is
      Scroll       : Gtk_Scrolled_Window;
      Column_Types : constant GType_Array :=
        (Column_Value => GType_String,
         Column_Color => Gdk.RGBA.Get_Type);
      Col          : Gtk_Tree_View_Column;
      Text         : Gtk_Cell_Renderer_Text;
      Dummy        : Gint;

   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      View.Pack_Start (Scroll, Expand => True, Fill => True);

      Gtk_New (View.Model, Column_Types);
      Gtk_New (View.Tree, View.Model);

      Gtk_New (Col);
      Dummy := Append_Column (View.Tree, Col);
      View.Tree.Set_Headers_Visible (False);

      Gtk_New (Text);
      Col.Pack_Start (Text, False);
      Col.Set_Sort_Column_Id (Column_Value);
      Col.Add_Attribute (Text, "text", Column_Value);
      Col.Add_Attribute (Text, "foreground-rgba", Column_Color);

      Scroll.Add (View.Tree);

      View.Refresh;

      return Gtk_Widget (View.Tree);
   end Initialize;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Serial_Ports_View_Record'Class) is
      use GNAT.Serial_Communications;

      Row   : Gtk_Tree_Iter;
      Color : Glib.Values.GValue;
   begin
      View.Model.Clear;

      for K in 1 .. 255 loop
         declare
            Port : Serial_Port;
            Name : constant Port_Name := GNAT.Serial_Communications.Name (K);
         begin
            --  Test if serial port exists
            Open (Port, Port_Name (Name));
            Close (Port);

            --  This is a valid serial port
            View.Model.Append (Row, Null_Iter);
            Set_And_Clear
              (View.Model,
               Iter   => Row,
               Values =>
                 (Column_Value => As_String (String (Name))));

         exception
            when Serial_Error =>
               --  Not a valid serial port
               null;
         end;
      end loop;

      if View.Model.Get_Iter_First = Null_Iter then
         View.Model.Append (Row, Null_Iter);
         Init (Color, Gdk.RGBA.Get_Type);
         Gdk.RGBA.Set_Value
           (Color,
            Shade_Or_Lighten (Default_Style.Get_Pref_Fg, 0.6));

         Set_And_Clear
           (View.Model,
            Iter   => Row,
            Values =>
              (Column_Value => As_String ("not found"),
              Column_Color  => Color));
      end if;
   end Refresh;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Refresh_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);

      View : constant Serial_Ports_View_Access :=
        Serial_Port_View.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View = null then
         return Commands.Success;
      end if;

      View.Refresh;
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Serial_Port_View.Register_Module (Kernel);

      Register_Action
        (Kernel,
         "refresh serial ports",
         new Refresh_Command,
         "Refresh serial ports",
         Icon_Name => "gps-refresh-symbolic");
   end Register_Module;

end Serial_Ports_Views;
