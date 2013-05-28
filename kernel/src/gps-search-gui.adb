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
with Gdk.Types.Keysyms;        use Gdk.Types, Gdk.Types.Keysyms;
with Glib.Object;              use Glib.Object;
with Gtk.Alignment;            use Gtk.Alignment;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Entry_Completion;  use Gtkada.Entry_Completion;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Search;        use GPS.Kernel.Search;
with GPS.Kernel.Search.Filenames; use GPS.Kernel.Search.Filenames;
with GPS.Intl;                 use GPS.Intl;
with GPS.Main_Window;          use GPS.Main_Window;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;       use GNATCOLL.VFS_Utils;
with Histories;                use Histories;
with Remote;                   use Remote;

package body GPS.Search.GUI is

   type Command_Fallback is access function
      (Text : String) return GPS.Search.Search_Result_Access;

   type Global_Search_Command is new Interactive_Command with record
      Provider : GPS.Search.Search_Provider_Access;
      History  : access History_Key;
      Fallback : Command_Fallback;
   end record;
   type Global_Search_Command_Access
      is access all Global_Search_Command'Class;
   overriding function Execute
      (Self    : access Global_Search_Command;
       Context : Interactive_Command_Context) return Command_Return_Type;
   --  Activate the global search field

   type Global_Search_Entry is new Gtkada_Entry_Record with null record;
   overriding function Fallback
      (Self : not null access Global_Search_Entry;
       Text : String) return GPS.Search.Search_Result_Access;
   --  An entry used for the global search entry

   type Open_From_Project_Entry is new Gtkada_Entry_Record with null record;
   overriding function Fallback
      (Self : not null access Open_From_Project_Entry;
       Text : String) return GPS.Search.Search_Result_Access;
   --  An entry used for the Open From Project dialog

   type Global_Search_Module_Record is new Module_ID_Record with record
      Search          : access Global_Search_Entry;

      Default_Command : Global_Search_Command_Access;
      --  The command used to give the focus to the global search.
      --  Do not free.

      Current_Command : Global_Search_Command_Access;
      --  The command that was last used to give focus to the search (if any).
      --  Do not free.

      Previous_Focus : Gtk_Widget;
      --  The widget that had the focus before we gave it to the search field
      --  for the last time
   end record;
   type Global_Search_Module is access all Global_Search_Module_Record'Class;

   Module : Global_Search_Module;

   procedure On_Escape (Self : access Gtk_Widget_Record'Class);
   --  Called when "<escape>" has been called

   procedure On_Activate (Self : access Gtk_Widget_Record'Class);
   --  Called when the user activates one of the search proposals

   procedure On_Open_From_Project
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open From Path menu

   procedure Reset;
   --  Reset the global search entry after <escape> or a search is selected

   function Filenames_Fallback (Text : String) return Search_Result_Access;
   --  The fallbacks for the various commands. These are called if the user
   --  enters text which has no completion.

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      --  Reset for when the user clicks in the field
      if Module.Previous_Focus /= null then
         Module.Previous_Focus.Unref;
      end if;
      Module.Previous_Focus := null;
      Module.Current_Command := null;
      Module.Search.Set_Completion (Module.Default_Command.Provider);
   end Reset;

   ---------------
   -- On_Escape --
   ---------------

   procedure On_Escape (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Module.Previous_Focus /= null then
         Module.Previous_Focus.Grab_Focus;
      end if;

      Reset;
   end On_Escape;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate (Self : access Gtk_Widget_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
   begin
      if Module.Current_Command /= null then
         Add_To_History
            (Get_History (S.Get_Kernel).all,
             Module.Current_Command.History.all,
             S.Get_Text);
      end if;

      --  Give the focus back, so that if for instance we are executing
      --  a GPS action, it executes in the context of the original view, not
      --  that of the global search entry.

      if Module.Previous_Focus /= null then
         Module.Previous_Focus.Grab_Focus;
      end if;

      Reset;
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
      if Module.Previous_Focus /= null then
         Module.Previous_Focus.Ref;
      end if;

      Module.Current_Command := Global_Search_Command_Access (Self);

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

   ------------------------
   -- Filenames_Fallback --
   ------------------------

   function Filenames_Fallback (Text : String) return Search_Result_Access is
      F : constant Virtual_File := Create_From_Base (+Text);
   begin
      if F.Is_Regular_File then
         return GPS.Kernel.Search.Filenames.Build_Filenames_Result
            (Module.Get_Kernel, File => F);
      end if;
      return null;
   end Filenames_Fallback;

   --------------
   -- Fallback --
   --------------

   overriding function Fallback
      (Self : not null access Open_From_Project_Entry;
       Text : String) return GPS.Search.Search_Result_Access
   is
      pragma Unreferenced (Self);
   begin
      return Filenames_Fallback (Text);
   end Fallback;

   --------------
   -- Fallback --
   --------------

   overriding function Fallback
      (Self : not null access Global_Search_Entry;
       Text : String) return GPS.Search.Search_Result_Access
   is
      pragma Unreferenced (Self);
      Fallback : Command_Fallback;
   begin
      if Module.Current_Command /= null then
         Fallback := Module.Current_Command.Fallback;
         if Fallback /= null then
            return Fallback (Text);
         end if;
      end if;

      --  Else other fallbacks are possible. In fact, the order of
      --  fallbacks should be configurable...
      return null;
   end Fallback;

   --------------------------
   -- On_Open_From_Project --
   --------------------------

   procedure On_Open_From_Project
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Ignore : Gtk_Widget;
      Open_File_Dialog : Gtk_Dialog;
      Open_File_Entry  : Gtkada_Entry;
      Resp : Gtk_Response_Type;
      pragma Unreferenced (Widget, Ignore, Resp);

   begin
      Gtk_New (Open_File_Dialog,
               Title  => -"Open file from project",
               Parent => Get_Current_Window (Kernel),
               Flags  => Modal or Destroy_With_Parent);
      Open_File_Dialog.Set_Default_Size (600, 480);
      Open_File_Dialog.Set_Position (Win_Pos_Mouse);

      --  Do not use a combo box, so that users can easily navigate to the list
      --  of completions through the keyboard (C423-005)
      Open_File_Entry := new Open_From_Project_Entry;
      Initialize
         (Open_File_Entry,
          Kernel         => Kernel,
          Name           => "open_from_project",
          Completion_In_Popup => False,
          Completion     =>
             GPS.Kernel.Search.Registry.Get (Provider_Filenames),
          Case_Sensitive => Is_Case_Sensitive (Get_Nickname (Build_Server)));
      Open_File_Dialog.Get_Content_Area.Pack_Start
        (Open_File_Entry, Fill => True, Expand => True);

      Ignore := Open_File_Dialog.Add_Button
        (Stock_Cancel, Gtk_Response_Cancel);

      Open_File_Dialog.Show_All;

      --  The action is performed directly by the search_provider or the
      --  fallback
      Resp := Open_File_Dialog.Run;
      Open_File_Dialog.Destroy;
   end On_Open_From_Project;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      procedure Register_Provider
         (Name : String; Fallback : Command_Fallback);
      --  Register an action for a given search provider

      procedure Register_Provider
         (Name : String; Fallback : Command_Fallback)
      is
         Command : Global_Search_Command_Access;
      begin
         Command := new Global_Search_Command;
         Command.Provider := GPS.Kernel.Search.Registry.Get (Name);
         Command.History := new History_Key'
            ("global-search-entry-" & History_Key (Name));
         Command.Fallback := Fallback;
         Register_Action
            (Kernel, "Global Search in context: " & Name, Command,
             Description => Command.Provider.Documentation,
             Category => "Search");
      end Register_Provider;

      Align   : Gtk_Alignment;
      Command : Global_Search_Command_Access;
   begin
      Module := new Global_Search_Module_Record;
      Register_Module
         (Module      => Module,
          Kernel      => Kernel,
          Module_Name => "Global_Search");

      Command := new Global_Search_Command;
      Command.Provider := GPS.Kernel.Search.Registry.Get (Provider_Filenames);
      Command.History := new History_Key'("global-search-entry");
      Module.Default_Command := Command;
      Register_Action
         (Kernel, "Global Search", Command,
          Description =>
             "Activate the global search field in the main toolbar",
          Category => "Search");

      Register_Provider (Provider_Filenames, Filenames_Fallback'Access);
      Register_Provider (Provider_Actions, null);

      Gtk_New (Align, 0.0, 1.0, 0.0, 0.0);
      GPS_Window (Get_Main_Window (Kernel)).Toolbar_Box.Pack_End
         (Align, Expand => False);

      Module.Search := new Global_Search_Entry;
      Initialize
         (Module.Search,
          Kernel              => Kernel,
          Name                => "global_search",
          Completion_In_Popup => True,
          Case_Sensitive      => True,
          Preview             => False,
          Completion          => Module.Default_Command.Provider);
      Module.Search.Set_Name ("global-search");
      Align.Add (Module.Search);

      Widget_Callback.Connect (Module.Search, Signal_Escape, On_Escape'Access);
      Widget_Callback.Connect
         (Module.Search, Signal_Activate, On_Activate'Access);

      Register_Menu
        (Kernel,
         '/' & (-"File") & '/', -"Open _From Project...",  Stock_Open,
         On_Open_From_Project'Access, null,
         GDK_F3, Shift_Mask,
         Ref_Item => -"Open...", Add_Before => False);
   end Register_Module;

end GPS.Search.GUI;
