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
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tool_Item;            use Gtk.Tool_Item;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Entry_Completion;  use Gtkada.Entry_Completion;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Search;        use GPS.Kernel.Search;
with GPS.Kernel.Search.Actions;
with GPS.Kernel.Search.Filenames;
with GPS.Intl;                 use GPS.Intl;
with GPS.Main_Window;          use GPS.Main_Window;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Histories;                use Histories;

package body GPS.Search.GUI is

   Proposals_Per_Provider : constant := 5;
   --  Number of proposals from each provider in the Overall_Search_Provider

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

   type Result_Array is array (Natural range <>) of Search_Result_Access;

   type Overall_Search_Provider is new Kernel_Search_Provider with record
      Pattern : Search_Pattern_Access;
      Provider : Search_Provider_Access;  --  the current one
      Current_Provider : Integer := -1;   --  index of current one

      Current : Result_Array (1 .. Proposals_Per_Provider);
      Current_Returned : Natural;
      Current_Index : Natural;
      --  The best proposals for the current provider.
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
   overriding function Documentation
     (Self : not null access Overall_Search_Provider) return String;

   procedure On_Escape (Self : access Gtk_Widget_Record'Class);
   --  Called when "<escape>" has been called

   procedure On_Activate (Self : access Gtk_Widget_Record'Class);
   --  Called when the user activates one of the search proposals

   procedure Reset;
   --  Reset the global search entry after <escape> or a search is selected

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access Overall_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Searches everywhere in GPS";
   end Documentation;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Overall_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
   begin
      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Current_Provider := 1;
      Self.Provider := Registry.Get (Self.Current_Provider);

      if Self.Provider /= null then
         Self.Provider.Set_Pattern
            (Self.Pattern,
             Limit => Natural'Min (Limit, Proposals_Per_Provider));
      end if;

      Self.Current_Returned := Self.Current'First - 1;
      Self.Current_Index := Self.Current'First - 1;
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Overall_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Insert_At : Integer;
   begin
      if Self.Provider = null then
         Result := null;
         Has_Next := False;
         return;
      end if;

      --  If we are in the process of processing the current provider

      if Self.Current_Returned < Self.Current'First then
         Self.Provider.Next (Result, Has_Next);

         if Result /= null then
            --  Make sure the primary sort key is the provider
            --  ??? Should disconnect the sorting for the display from the
            --  order in which we process the providers, since the former is
            --  controlled by the user and the latter by the application (we
            --  always want to run "file content" provider last, since it is
            --  slower.

            Result.Score := Result.Score
               + (100 - Self.Provider.Rank) * 1_000_000;

            --  ??? This doesn't take into account score modification that
            --  will be done by the entry_completion for instance to show
            --  most recent items first, or shorter items.

            Insert_At := Self.Current_Index + 1;
            for J in reverse Self.Current'First .. Self.Current_Index loop
               exit when Result.Score <= Self.Current (J).Score;
               Insert_At := J;
            end loop;

            if Insert_At > Self.Current'Last then
               Free (Result);

            elsif Self.Current_Index = Self.Current'Last then
               Free (Self.Current (Self.Current'Last));
               Self.Current (Insert_At + 1 .. Self.Current'Last) :=
                  Self.Current (Insert_At .. Self.Current'Last - 1);
               Self.Current (Insert_At) := Result;
            else
               Self.Current (Insert_At + 1 .. Self.Current_Index + 1) :=
                  Self.Current (Insert_At .. Self.Current_Index);
               Self.Current_Index := Self.Current_Index + 1;
               Self.Current (Insert_At) := Result;
            end if;
         end if;

         if Has_Next then
            --  Wait till we have finished processing this provider to make
            --  sure we have the ones with the top score
            Result := null;
            return;
         end if;

         Self.Current_Returned := Self.Current'First;
      end if;

      if Self.Current_Returned <= Self.Current_Index then
         Result := Self.Current (Self.Current_Returned);
         Self.Current_Returned := Self.Current_Returned + 1;
         Has_Next := True;
         return;
      end if;

      --  Move to next provider
      Self.Current_Provider := Self.Current_Provider + 1;
      Self.Provider := Registry.Get (Self.Current_Provider);

      if Self.Provider /= null then
         Self.Provider.Set_Pattern
            (Self.Pattern, Limit => Proposals_Per_Provider);
      end if;

      Self.Current_Index := Self.Current'First - 1;
      Self.Current_Returned := Self.Current'First - 1;
      Result := null;
      Has_Next := True;
   end Next;

   ------------------
   -- Display_Name --
   ------------------

   overriding function Display_Name
     (Self     : not null access Overall_Search_Provider) return String is
   begin
      if Self.Provider = null then
         return "";
      else
         return Self.Provider.Display_Name;
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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Overall : constant Overall_Search_Provider_Access :=
         new Overall_Search_Provider;
      Align   : Gtk_Alignment;
      Vbox    : Gtk_Vbox;
      Command : Global_Search_Command_Access;
      Item    : Gtk_Tool_Item;
      P       : Kernel_Search_Provider_Access;

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

      P := new GPS.Kernel.Search.Filenames.Filenames_Search_Provider;
      Register_Provider_And_Action (Kernel, P, Provider_Filenames);

      P := new GPS.Kernel.Search.Actions.Actions_Search_Provider;
      Register_Provider_And_Action (Kernel, P, Provider_Actions);

      Gtk_New (Item);
      Gtk_New (Align, 0.0, 1.0, 0.0, 0.0);
      Item.Add (Align);

      GPS_Window (Get_Main_Window (Kernel)).Toolbar.Insert (Item);

      Gtk_New
         (Module.Search,
          Kernel              => Kernel,
          Name                => "global_search",
          Completion_In_Popup => True,
          Case_Sensitive      => True,
          Preview             => False,
          Completion          => Module.Default_Command.Provider);
      Module.Search.Set_Name ("global-search");

      Gtk_New_Vbox (Vbox);
      Vbox.Pack_Start (Module.Search, Padding => 2);
      Align.Add (Vbox);

      Widget_Callback.Connect (Module.Search, Signal_Escape, On_Escape'Access);
      Widget_Callback.Connect
         (Module.Search, Signal_Activate, On_Activate'Access);

      Register_Menu
        (Kernel,
         '/' & (-"File") & '/', -"Open _From Project...",  Stock_Open,
         Callback => null,
         Action =>
           Lookup_Action (Kernel, Action_Name_Prefix & Provider_Filenames),
         Accel_Key => GDK_F3, Accel_Mods => Shift_Mask,
         Ref_Item => -"Open...", Add_Before => False);
   end Register_Module;

end GPS.Search.GUI;
