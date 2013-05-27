------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

with Commands.Interactive;     use Commands, Commands.Interactive;
with Gtk.Alignment;            use Gtk.Alignment;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Entry_Completion;  use Gtkada.Entry_Completion;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Search;        use GPS.Kernel.Search;
with GPS.Main_Window;          use GPS.Main_Window;
with Histories;                use Histories;

package body GPS.Search.GUI is

   type Global_Search_Module_Record is new Module_ID_Record with record
      Search : Gtkada_Entry;

      Default_Provider : GPS.Search.Search_Provider_Access;
      --  The default search provider

      Default_History : access History_Key;
      --  The default history (to be used for the default_provider).
      --  Do not free.

      History : access History_Key;
      --  The history that should be enhanced after the user types some text.
      --  Do not Free.

      Previous_Focus : Gtk_Widget;
      --  The widget that had the focus before we gave it to the search field
      --  for the last time
   end record;
   type Global_Search_Module is access all Global_Search_Module_Record'Class;

   Module : Global_Search_Module;

   type Global_Search_Command is new Interactive_Command with record
      Provider : GPS.Search.Search_Provider_Access;
      History  : access History_Key;
   end record;
   type Global_Search_Command_Access
      is access all Global_Search_Command'Class;
   overriding function Execute
      (Self    : access Global_Search_Command;
       Context : Interactive_Command_Context) return Command_Return_Type;
   --  Activate the global search field

   procedure On_Escape (Self : access Gtk_Widget_Record'Class);
   --  Called when "<escape>" has been called

   procedure On_Activate (Self : access Gtk_Widget_Record'Class);
   --  Called when the user activates one of the search proposals

   ---------------
   -- On_Escape --
   ---------------

   procedure On_Escape (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Module.Previous_Focus /= null then
         Module.Previous_Focus.Grab_Focus;
         Module.Previous_Focus := null;
      end if;

      --  Reset for when the user clicks in the field
      Module.Search.Set_Completion (Module.Default_Provider);

      Module.History := Module.Default_History;
   end On_Escape;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate (Self : access Gtk_Widget_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
   begin
      if Module.History /= null then
         Add_To_History
            (Get_History (S.Get_Kernel).all,
             Module.History.all,
             S.Get_Text);
      end if;

      Module.History := Module.Default_History;

      --  Reset for when the user clicks in the field
      Module.Search.Set_Completion (Module.Default_Provider);
   end On_Activate;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self    : access Global_Search_Command;
       Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Kernel : constant Kernel_Handle := Module.Get_Kernel;
   begin
      Module.Previous_Focus := Get_Current_Focus_Widget (Kernel);
      Module.History := Self.History;

      Create_New_Key_If_Necessary
         (Get_History (Kernel).all, Self.History.all, Strings);
      Set_Max_Length (Get_History (Kernel).all, 1, Self.History.all);

      Module.Search.Set_Completion (Self.Provider);

      --  Fill initial contents based on history
      Module.Search.Set_Text
         (Most_Recent (Get_History (Kernel), Self.History.all));

      --  Force the display of the popup, even if empty, to help the user
      --  see where the focus is.

      Module.Search.Popup;

      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Align   : Gtk_Alignment;
      Command : Global_Search_Command_Access;
   begin
      Module := new Global_Search_Module_Record;
      Register_Module
         (Module      => Module,
          Kernel      => Kernel,
          Module_Name => "Global_Search");
      Module.Default_Provider :=
         GPS.Kernel.Search.Registry.Get (Provider_Filenames);
      Module.Default_History := new History_Key'("global-search-entry");

      Gtk_New (Align, 0.0, 1.0, 0.0, 0.0);
      GPS_Window (Get_Main_Window (Kernel)).Toolbar_Box.Pack_End
         (Align, Expand => False);

      Gtk_New
         (Module.Search,
          Kernel              => Kernel,
          Name                => "global_search",
          Completion_In_Popup => True,
          Case_Sensitive      => True,
          Preview             => False,
          Completion          => Module.Default_Provider);
      Module.Search.Set_Name ("global-search");
      Align.Add (Module.Search);

      Widget_Callback.Connect (Module.Search, Signal_Escape, On_Escape'Access);
      Widget_Callback.Connect
         (Module.Search, Signal_Activate, On_Activate'Access);

      Command := new Global_Search_Command;
      Command.Provider := Module.Default_Provider;
      Command.History := Module.Default_History;
      Register_Action
         (Kernel, "Global Search", Command,
          Description =>
             "Activate the global search field in the main toolbar",
          Category => "Search");

      Command := new Global_Search_Command;
      Command.Provider := GPS.Kernel.Search.Registry.Get (Provider_Filenames);
      Command.History := new History_Key'("global-search-filenames-entry");
      Register_Action
         (Kernel, "Global Search in context: file names", Command,
          Description =>
             "Search amongst the source file names of the project",
          Category => "Search");
   end Register_Module;

end GPS.Search.GUI;
