------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with GNAT.Serial_Communications; use GNAT.Serial_Communications;

with Glib;                       use Glib;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Toolbar;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Combo_Tool_Button;   use Gtkada.Combo_Tool_Button;
with Gtkada.MDI;                 use Gtkada.MDI;

with GNATCOLL.Traces;            use GNATCOLL.Traces;

with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;

with Commands.Interactive;       use Commands, Commands.Interactive;
with Generic_Views;

package body Serial_Ports_Views is

   Me : constant Trace_Handle := Create ("GPS.VIEWS.SERIAL_PORTS");

   type Serial_Ports_View_Record is new Generic_Views.View_Record with record
      Combo : Gtkada_Combo_Tool_Button;
   end record;

   overriding procedure Create_Toolbar
     (View    : not null access Serial_Ports_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);

   function Initialize
     (View : access Serial_Ports_View_Record'Class)
      return Gtk_Widget;
   --  Create a new ports view

   procedure Refresh (View : access Serial_Ports_View_Record'Class);
   --  Refresh the list of available serial ports and fill toolbar combo

   package Serial_Port_View is new Generic_Views.Simple_Views
     (Module_Name        => "Serial_Ports_View",
      View_Name          => "Serial Ports",
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
   --  Refresh command

   function Name (Prefix : String; Number : Positive) return Port_Name;
   --  Returns port name based on prefix and number

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View : access Serial_Ports_View_Record'Class) return Gtk_Widget
   is

   begin
      Initialize_Vbox (View, Homogeneous => False);

      return Gtk_Widget (View);
   end Initialize;

   overriding procedure Create_Toolbar
     (View    : not null access Serial_Ports_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Gtk_New (View.Combo, "", Click_Pops_Up => True);

      View.Refresh;

      View.Append_Toolbar
        (Toolbar     => Toolbar,
         Item        => View.Combo,
         Right_Align => True,
         Homogeneous => False);
   end Create_Toolbar;

   ----------
   -- Name --
   ----------

   function Name (Prefix : String; Number : Positive) return Port_Name is
      N     : constant Natural := Number - 1;
      N_Img : constant String  := Natural'Image (N);
   begin
      return Port_Name (Prefix & N_Img (N_Img'First + 1 .. N_Img'Last));
   end Name;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Serial_Ports_View_Record'Class) is
      Empty : Boolean := True;

   begin
      Trace (Me, "Refresh");

      View.Combo.Clear_Items;

      for K in 1 .. 255 loop
         declare
            Port : Serial_Port;
            Name : constant Port_Name := GNAT.Serial_Communications.Name (K);
         begin
            --  Test if serial port exists
            Open (Port, Name);
            Close (Port);

            --  This is a valid serial port
            View.Combo.Add_Item (String (Name));
            Empty := False;

         exception
            when others =>
               --  Not a valid serial port
               null;
         end;
      end loop;

      -- ttyACM --
      for K in 1 .. 255 loop
         declare
            Port : Serial_Port;
            ACM_Name : constant Port_Name := Name ("/dev/ttyACM", K);
         begin
            --  Test if serial port exists
            Open (Port, ACM_Name);
            Close (Port);

            --  This is a valid serial port
            View.Combo.Add_Item (String (ACM_Name));
            Empty := False;

         exception
            when others =>
               --  Not a valid serial port
               null;
         end;
      end loop;

      if Empty then
         View.Combo.Add_Item ("null");
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
