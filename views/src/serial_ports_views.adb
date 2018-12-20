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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with GNAT.Serial_Communications; use GNAT.Serial_Communications;
with System;

with Glib;                       use Glib;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Enums;                  use Gtk.Enums;
with Glib.Main;
with Gtk.Menu;
with Gtk.Toolbar;
with Gtk.Tool_Button;            use Gtk.Tool_Button;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Combo_Tool_Button;   use Gtkada.Combo_Tool_Button;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;

with GNATCOLL.Traces;            use GNATCOLL.Traces;

with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;
with GPS.Kernel.Properties;
with GPS.Properties;             use GPS.Properties;

with Default_Preferences;        use Default_Preferences;
with Generic_Views;
with Interactive_Consoles;       use Interactive_Consoles;
with Histories;

package body Serial_Ports_Views is

   Me : constant Trace_Handle := Create ("GPS.VIEWS.SERIAL_PORTS");

   type Serial_Ports_View_Record is new Generic_Views.View_Record with record
      Status         : Gtk_Tool_Button;
      Ports_Refresh  : Gtk_Tool_Button;

      Ports          : Gtkada_Combo_Tool_Button;
      Rates          : Gtkada_Combo_Tool_Button;

      Console        : Interactive_Console;
      In_Destruction : Boolean := False;
      Is_Active      : Boolean := False;

      Timeout        : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  The handler for Read()
   end record;
   type Serial_Port_View is access all Serial_Ports_View_Record'Class;

   overriding procedure Create_Toolbar
     (View    : not null access Serial_Ports_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);

   overriding procedure Create_Menu
     (View    : not null access Serial_Ports_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   overriding procedure On_Destroy
     (View : not null access Serial_Ports_View_Record);

   function Initialize
     (View : access Serial_Ports_View_Record'Class)
      return Gtk_Widget;
   --  Create a new ports view

   procedure Refresh (View : access Serial_Ports_View_Record'Class);
   --  Refresh the list of available serial ports and fill toolbar combo

   procedure On_Port_Switched (Self : access Gtk_Widget_Record'Class);
   --  Called when the user selects a new port via the toolbar's combo

   procedure On_Rate_Switched (Self : access Gtk_Widget_Record'Class);
   --  Called when the user selects a new rate for a port
   --  via the toolbar's combo

   procedure On_Status_Clicked (Self : access Gtk_Widget_Record'Class);
   --  Called when status toolbar is pressed

   procedure On_Ports_Refresh_Clicked (Self : access Gtk_Widget_Record'Class);
   --  Called when refresh toolbar is pressed

   procedure Reopen (View : access Serial_Ports_View_Record'Class);
   --  Reopen selected port with selected rate

   procedure Open (View : access Serial_Ports_View_Record'Class);
   --  Open selected port with selected rate

   procedure Close (View : access Serial_Ports_View_Record'Class);
   --  Closes current port

   function Read (View : access Serial_Ports_View_Record'Class) return String;
   --  Reads from current port

   function Idle_Read (Kernel : Kernel_Handle) return Boolean;
   --  Is called by timeout for reading from a port

   function Interpret_Command_Handler
     (Console      : access Interactive_Console_Record'Class;
      Input        : String;
      View_Address : System.Address)
      return String;
   --  Send intput into port and return the output if any exists.

   package Serial_Ports_View is new Generic_Views.Simple_Views
     (Module_Name        => "Serial_Ports_View",
      View_Name          => "Serial Ports",
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Formal_View_Record => Serial_Ports_View_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Consoles,
      Position           => Position_Bottom,
      Initialize         => Initialize);

   function Name (Prefix : String; Number : Positive) return Port_Name;
   --  Returns port name based on prefix and number

   package Strings_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (String);

   package Timeout_Kernel is new Glib.Main.Generic_Sources (Kernel_Handle);

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Serial_Port_View);

   Autoconnect        : Boolean_Preference;
   Current_Port       : Serial_Port;

   -- Constants --

   Null_Port          : constant String := "null";
   Select_Port        : constant String := "select port";
   Port_Property_Name : constant String := "serial_port";
   Port_Rate_Prefix   : constant String := "serial_port_rate_";

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View : access Serial_Ports_View_Record'Class) return Gtk_Widget is
   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New
        (View.Console,
         View.Kernel,
         Handler             => Interpret_Command_Handler'Access,
         User_Data           => View.all'Address,
         Prompt              => "",
         History_List        => Get_History (View.Kernel),
         Key                 => "serial_port_console",
         Wrap_Mode           => Wrap_Char,
         ANSI_Support        => True,
         Empty_Equals_Repeat => True);
      View.Pack_Start (View.Console, Fill => True, Expand => True);
      Set_Font_And_Colors
        (View.Console.Get_View, Fixed_Font => True);

      Histories.Set_Max_Length
        (Get_History (View.Kernel).all, 100, "serial_port_console");
      Histories.Allow_Duplicates
        (Get_History (View.Kernel).all, "serial_port_console", True, True);

      View.Console.Set_Highlight_Color (Preference (Comments_Style));
      View.Console.Set_Sensitive (False);

      return Gtk_Widget (View);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   overriding procedure On_Destroy
     (View : not null access Serial_Ports_View_Record) is
   begin
      View.In_Destruction := True;
      View.Close;
   end On_Destroy;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Serial_Ports_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Gtk_New (View.Status);

      View.Status.Set_Label ("Open");
      View.Status.Set_Tooltip_Text ("Open/Close selected port");
      Widget_Callback.Object_Connect
        (View.Status, Gtk.Tool_Button.Signal_Clicked,
         On_Status_Clicked'Access, View);

      View.Append_Toolbar
        (Toolbar     => Toolbar,
         Item        => View.Status,
         Right_Align => False,
         Homogeneous => False);

      Gtk_New (View.Ports_Refresh);
      View.Ports_Refresh.Set_Icon_Name ("gps-refresh-symbolic");
      View.Ports_Refresh.Set_Tooltip_Text ("Refresh the list of serial ports");

      Widget_Callback.Object_Connect
        (View.Ports_Refresh, Gtk.Tool_Button.Signal_Clicked,
         On_Ports_Refresh_Clicked'Access, View);

      View.Append_Toolbar
        (Toolbar     => Toolbar,
         Item        => View.Ports_Refresh,
         Right_Align => True,
         Homogeneous => False);

      Gtk_New (View.Ports, "", Click_Pops_Up => True);

      Widget_Callback.Object_Connect
        (View.Ports,
         Gtkada.Combo_Tool_Button.Signal_Selection_Changed,
         On_Port_Switched'Access,
         Serial_Port_View (View));

      View.Append_Toolbar
        (Toolbar     => Toolbar,
         Item        => View.Ports,
         Right_Align => True,
         Homogeneous => False);

      Gtk_New (View.Rates, "", Click_Pops_Up => True);

      Widget_Callback.Object_Connect
        (View.Rates,
         Gtkada.Combo_Tool_Button.Signal_Selection_Changed,
         On_Rate_Switched'Access,
         Serial_Port_View (View));

      View.Append_Toolbar
        (Toolbar     => Toolbar,
         Item        => View.Rates,
         Right_Align => True,
         Homogeneous => False);

      View.Refresh;
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Serial_Ports_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Kernel : constant Kernel_Handle := View.Kernel;
   begin
      GPS.Kernel.Preferences.Append_Menu (Menu, Kernel, Autoconnect);
   end Create_Menu;

   ----------
   -- Name --
   ----------

   function Name (Prefix : String; Number : Positive) return Port_Name is
      N     : constant Natural := Number - 1;
      N_Img : constant String  := Natural'Image (N);
   begin
      return Port_Name (Prefix & N_Img (N_Img'First + 1 .. N_Img'Last));
   end Name;

   ----------------------
   -- On_Port_Switched --
   ----------------------

   procedure On_Port_Switched (Self : access Gtk_Widget_Record'Class)
   is
      View     : constant Serial_Port_View := Serial_Port_View (Self);
      Selected : constant String := View.Ports.Get_Selected_Item;
      Property : String_Property_Access;
   begin
      if View.In_Destruction
        or else Selected = ""
        or else Selected = Null_Port
        or else Selected = Select_Port
      then
         return;
      end if;

      --  Store selected port as a persistent property
      Property := new String_Property'(Value => new String'(Selected));
      GPS.Kernel.Properties.Set_Property
        (Kernel     => View.Kernel,
         Project    => GPS.Kernel.Project.Get_Project (View.Kernel),
         Name       => Port_Property_Name,
         Property   => Property,
         Persistent => True);

      --  Select port rate based on previous selection (from properties)
      declare
         Property : String_Property;
         Found    : Boolean;
      begin
         Get_Property
           (Property,
            GPS.Kernel.Project.Get_Project (View.Kernel),
            Name  => Port_Rate_Prefix & Selected,
            Found => Found);
         if Found then
            View.Rates.Select_Item (Property.Value.all);
         else
            View.Rates.Select_Item (Data_Rate'Image (Data_Rate'Last));
         end if;
      end;

      --  Reopen was called in On_Rate_Switched
   end On_Port_Switched;

   ----------------------
   -- On_Rate_Switched --
   ----------------------

   procedure On_Rate_Switched (Self : access Gtk_Widget_Record'Class) is
      View     : constant Serial_Port_View := Serial_Port_View (Self);
      Port     : constant String := View.Ports.Get_Selected_Item;
      Rate     : constant String := View.Rates.Get_Selected_Item;
      Property : String_Property_Access;
   begin
      if View.In_Destruction
        or else Port = ""
        or else Port = Null_Port
        or else Port = Select_Port
        or else Rate = ""
      then
         return;
      end if;

      --  Store selected rate for the port as a persistent property
      Property := new String_Property'(Value => new String'(Rate));
      GPS.Kernel.Properties.Set_Property
        (Kernel     => View.Kernel,
         Project    => GPS.Kernel.Project.Get_Project (View.Kernel),
         Name       => Port_Rate_Prefix & Port,
         Property   => Property,
         Persistent => True);

      View.Reopen;
   end On_Rate_Switched;

   -----------------------
   -- On_Status_Clicked --
   -----------------------

   procedure On_Status_Clicked (Self : access Gtk_Widget_Record'Class)
   is
      View : constant Serial_Port_View := Serial_Port_View (Self);
   begin
      if View.Is_Active then
         View.Close;
      else
         View.Open;
      end if;
   end On_Status_Clicked;

   ------------------------------
   -- On_Ports_Refresh_Clicked --
   ------------------------------

   procedure On_Ports_Refresh_Clicked (Self : access Gtk_Widget_Record'Class)
   is
   begin
      if Serial_Port_View (Self).In_Destruction then
         return;
      end if;

      Serial_Port_View (Self).Refresh;
   end On_Ports_Refresh_Clicked;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Serial_Ports_View_Record'Class) is
      Ports : Strings_Sets.Set;

      procedure Add_Ports;
      --  Add available ports into combo

      ---------------
      -- Add_Ports --
      ---------------

      procedure Add_Ports is
      begin
         for Item of Ports loop
            View.Ports.Add_Item (Item);
         end loop;
      end Add_Ports;

   begin
      if View.In_Destruction then
         return;
      end if;

      Trace (Me, "Refresh");

      View.Ports.Clear_Items;

      for K in 1 .. 255 loop
         declare
            Port : Serial_Port;
            Name : constant Port_Name := GNAT.Serial_Communications.Name (K);
         begin
            --  Test if serial port exists
            Open (Port, Name);
            Close (Port);

            --  This is a valid serial port
            Ports.Insert (String (Name));

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
            Ports.Insert (String (ACM_Name));

         exception
            when others =>
               --  Not a valid serial port
               null;
         end;
      end loop;

      if Ports.Is_Empty then
         View.Ports.Add_Item (Null_Port);
         View.Ports.Select_Item (Null_Port);
         View.Rates.Hide;

      else
         if not View.Rates.Has_Items then
            for Item in Data_Rate'Range loop
               View.Rates.Add_Item (Item'Image);
            end loop;
         end if;
         View.Rates.Show;

         declare
            Property : String_Property;
            Found    : Boolean;
         begin
            Get_Property
              (Property,
               GPS.Kernel.Project.Get_Project (View.Kernel),
               Name  => Port_Property_Name,
               Found => Found);
            if Found
              and then Ports.Contains (Property.Value.all)
            then
               Add_Ports;
               View.Ports.Select_Item (Property.Value.all);
            else
               View.Ports.Add_Item (Select_Port);
               Add_Ports;
               View.Ports.Select_Item (Select_Port);
            end if;
         end;
      end if;

      View.Reopen;
   end Refresh;

   -----------
   -- Close --
   -----------

   procedure Close (View : access Serial_Ports_View_Record'Class) is
      use type Glib.Main.G_Source_Id;
   begin
      if View.Timeout /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (View.Timeout);
         View.Timeout := Glib.Main.No_Source_Id;
      end if;

      if not View.Is_Active then
         return;
      end if;

      View.Is_Active := False;

      begin
         Close (Current_Port);
      exception
         when E : others =>
            Trace (Me, E);
      end;

      View.Console.Clear;
      View.Console.Set_Sensitive (False);
      View.Status.Set_Label ("Open");
   end Close;

   ----------
   -- Open --
   ----------

   procedure Open (View : access Serial_Ports_View_Record'Class) is
      Port : constant String := View.Ports.Get_Selected_Item;
      Rate : constant String := View.Rates.Get_Selected_Item;
   begin
      if View.In_Destruction
        or else View.Is_Active
        or else Port = ""
        or else Port = Null_Port
        or else Port = Select_Port
        or else Rate = ""
      then
         return;
      end if;

      Open (Current_Port, Port_Name (View.Ports.Get_Selected_Item));
      Set  (Current_Port, Data_Rate'Value (View.Rates.Get_Selected_Item),
            Block => False, Timeout => 0.01);

      View.Status.Set_Label ("Close");
      View.Console.Set_Sensitive (True);
      View.Console.Insert
        ("Connected to " & Port & " with " & Rate & " rate.");

      View.Is_Active := True;
      View.Timeout :=  Timeout_Kernel.Timeout_Add
        (250, Idle_Read'Access, View.Kernel);

   exception
      when E : others =>
         Trace (Me, E);
         View.Kernel.Messages_Window.Insert
           (Ada.Exceptions.Exception_Information (E), Mode => Error);
   end Open;

   ------------
   -- Reopen --
   ------------

   procedure Reopen (View : access Serial_Ports_View_Record'Class) is
   begin
      View.Close;

      if Autoconnect.Get_Pref then
         View.Open;
      end if;
   end Reopen;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Console      : access Interactive_Console_Record'Class;
      Input        : String;
      View_Address : System.Address)
      return String
   is
      use Ada.Streams;
      pragma Unreferenced (Console);

      View : constant Serial_Port_View := Convert (View_Address);
      Size : constant Stream_Element_Offset :=
        Input'Size / Stream_Element'Size;

      type Constrained_SEA_Access is
        access all Stream_Element_Array (1 .. Size);

      function As_Pointer is
        new Ada.Unchecked_Conversion
          (System.Address, Constrained_SEA_Access);

      Buffer_Access : constant Constrained_SEA_Access :=
        As_Pointer (Input'Address);

   begin
      if not View.Is_Active then
         return "";
      end if;

      Current_Port.Write (Buffer_Access.all);
      return View.Read;

   exception
      when E : others =>
         Trace (Me, E);
         return Ada.Exceptions.Exception_Information (E);
   end Interpret_Command_Handler;

   ----------
   -- Read --
   ----------

   function Read
     (View : access Serial_Ports_View_Record'Class)
      return String
   is
      use Ada.Streams;
      use Ada.Strings.Unbounded;

      pragma Unreferenced (View);

      Size : constant := 1024;

      type Constrained_String_Access is
        access all String (1 .. Size);

      function As_Pointer is
        new Ada.Unchecked_Conversion
          (System.Address, Constrained_String_Access);

      Buffer : Stream_Element_Array (1 .. Size);
      Last   : Stream_Element_Offset;
      Data   : Unbounded_String;
   begin
      loop
         Current_Port.Read (Buffer, Last);
         exit when Last < Buffer'First;
         Append (Data, As_Pointer (Buffer'Address) (1 .. Integer (Last)));
      end loop;

      return To_String (Data);

   exception
      when E : others =>
         Trace (Me, E);
         return Ada.Exceptions.Exception_Information (E);
   end Read;

   ---------------
   -- Idle_Read --
   ---------------

   function Idle_Read (Kernel : Kernel_Handle) return Boolean
   is
      View : constant Serial_Port_View := Serial_Port_View
        (Serial_Ports_View.Retrieve_View (Kernel, True));
   begin
      if View = null
        or else View.In_Destruction
        or else not View.Is_Active
      then
         return False;
      end if;

      declare
         Data : constant String := View.Read;
      begin
         if Data /= "" then
            View.Console.Insert (Data);
         end if;
      end;

      return True;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Idle_Read;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Serial_Ports_View.Register_Module (Kernel);

      Autoconnect := Kernel.Get_Preferences.Create_Invisible_Pref
        ("serial_ports_view-autoconnect", True,
         Label => "Autoconnect",
         Doc   =>
           "Reconnect automatically when the port or the rate has changed.");
   end Register_Module;

end Serial_Ports_Views;
