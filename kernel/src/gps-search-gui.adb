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

with Ada.Unchecked_Deallocation;
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
with GPS.Intl;                 use GPS.Intl;
with GPS.Main_Window;          use GPS.Main_Window;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;       use GNATCOLL.VFS_Utils;
with Histories;                use Histories;
with Remote;                   use Remote;

package body GPS.Search.GUI is

   Proposals_Per_Provider : constant := 5;
   --  Number of proposals from each provider in the Overall_Search_Provider

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

   type Global_Search_Module_Record is new Module_ID_Record with record
      Search          : Gtkada_Entry;

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

   type Provider_Array is array (Natural range <>) of Search_Provider_Access;
   type Provider_Array_Access is access Provider_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Provider_Array, Provider_Array_Access);

   type Overall_Search_Provider is new Kernel_Search_Provider with record
      Providers : Provider_Array_Access;
      Current_Provider : Integer := -1;
      Total_For_Current : Natural := 0;
   end record;
   type Overall_Search_Provider_Access
      is access all Overall_Search_Provider'Class;
   overriding procedure Set_Pattern
     (Self    : not null access Overall_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Overall_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Overall_Search_Provider) return String;

   procedure On_Escape (Self : access Gtk_Widget_Record'Class);
   --  Called when "<escape>" has been called

   procedure On_Activate (Self : access Gtk_Widget_Record'Class);
   --  Called when the user activates one of the search proposals

   procedure On_Open_From_Project
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  File->Open From Path menu

   procedure Reset;
   --  Reset the global search entry after <escape> or a search is selected

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Overall_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
   begin
      for P in Self.Providers'Range loop
         Self.Providers (P).Set_Pattern
            (Pattern, Limit => Natural'Min (Limit, Proposals_Per_Provider));
      end loop;

      Self.Current_Provider := Self.Providers'First;
      Self.Total_For_Current := 0;
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Overall_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
   begin
      if Self.Current_Provider > Self.Providers'Last then
         Result := null;
         Has_Next := False;
         return;
      end if;

      Self.Providers (Self.Current_Provider).Next (Result, Has_Next);

      if Result /= null then
         --  Make sure the primary sort key is the provider
         --  ??? Should disconnect the sorting for the display from the order
         --  in which we process the providers, since the former is controlled
         --  by the user and the latter by the application (we always want to
         --  run "file content" provider last, since it is slower.
         Result.Score := Result.Score
            + (Self.Providers'Last - Self.Current_Provider) * 1_000_000;

         Self.Total_For_Current := Self.Total_For_Current + 1;
      end if;

      if not Has_Next
         or else Self.Total_For_Current >= Proposals_Per_Provider
      then
         Self.Current_Provider := Self.Current_Provider + 1;
         Self.Total_For_Current := 0;
      end if;
   end Next;

   ------------------
   -- Display_Name --
   ------------------

   overriding function Display_Name
     (Self     : not null access Overall_Search_Provider) return String is
   begin
      if Self.Current_Provider > Self.Providers'Last then
         return "";
      else
         return Self.Providers (Self.Current_Provider).Display_Name;
      end if;
   end Display_Name;

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
      Gtk_New
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

      --  The action is performed directly by the search_provider
      Resp := Open_File_Dialog.Run;
      Open_File_Dialog.Destroy;
   end On_Open_From_Project;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Overall : constant Overall_Search_Provider_Access :=
         new Overall_Search_Provider;

      procedure Register_Provider (Name : String);
      --  Register an action for a given search provider

      procedure Register_Provider (Name : String) is
         Command : Global_Search_Command_Access;
         Tmp     : Provider_Array_Access;
      begin
         Command := new Global_Search_Command;
         Command.Provider := GPS.Kernel.Search.Registry.Get (Name);
         Command.History := new History_Key'
            ("global-search-entry-" & History_Key (Name));
         Register_Action
            (Kernel, "Global Search in context: " & Name, Command,
             Description => Command.Provider.Documentation,
             Category => "Search");

         Tmp := Overall.Providers;
         if Tmp = null then
            Overall.Providers := new Provider_Array (1 .. 1);
         else
            Overall.Providers := new Provider_Array (1 .. Tmp'Last + 1);
            Overall.Providers (Tmp'Range) := Tmp.all;
            Unchecked_Free (Tmp);
         end if;
         Overall.Providers (Overall.Providers'Last) := Command.Provider;
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
      Command.Provider := Search_Provider_Access (Overall);
      Command.History := new History_Key'("global-search-entry");
      Module.Default_Command := Command;
      Register_Action
         (Kernel, "Global Search", Command,
          Description =>
             "Activate the global search field in the main toolbar",
          Category => "Search");

      Register_Provider (Provider_Filenames);
      Register_Provider (Provider_Actions);

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
