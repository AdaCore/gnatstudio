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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Default_Preferences;      use Default_Preferences;
with Gdk.Types.Keysyms;        use Gdk.Types, Gdk.Types.Keysyms;
with Glib.Object;              use Glib, Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;
with Gtk.Alignment;            use Gtk.Alignment;
with Gtk.Cell_Renderer;        use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tool_Item;            use Gtk.Tool_Item;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Spin_Button;          use Gtk.Spin_Button;

with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Entry_Completion;  use Gtkada.Entry_Completion;
with Gtkada.Handlers;          use Gtkada.Handlers;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Search;        use GPS.Kernel.Search;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Search.Actions;
with GPS.Kernel.Search.Filenames;
with GPS.Kernel.Search.Sources;
with GPS.Intl;                 use GPS.Intl;
with GPS.Main_Window;          use GPS.Main_Window;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Histories;                use Histories;
with System;

package body GPS.Search.GUI is
   Me : constant Trace_Handle := Create ("SEARCH");

   Pref_Proposals_Per_Provider : Integer_Preference;
   --  Preference for the number of proposals per provider

   Pref_Provider_Order : String_Preference;
   --  The preferred order of providers

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
   type Result_Array_Access is access all Result_Array;

   procedure Free (Self : in out Result_Array_Access);
   --  Free self and the search results

   type Overall_Search_Provider is new Kernel_Search_Provider with record
      Pattern : Search_Pattern_Access;
      Provider : Search_Provider_Access;  --  the current one
      Current_Provider : Integer := -1;   --  index of current one

      Current : Result_Array_Access;
      Current_Returned : Natural;
      Current_Index : Natural;
      --  The best proposals for the current provider.
   end record;
   type Overall_Search_Provider_Access
     is access all Overall_Search_Provider'Class;
   overriding procedure Free (Self : in out Overall_Search_Provider);
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
   overriding procedure Edit_Settings
     (Self : not null access Overall_Search_Provider;
      Box  : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback);
   overriding function Complete_Suffix
     (Self      : not null access Overall_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Search_Provider_Access);

   type Settings_Toggle_Record is new Gtk_Cell_Renderer_Toggle_Record
     with record
       Kernel    : Kernel_Handle;
       Model     : Gtk_List_Store;
       Data      : access Glib.Object.GObject_Record'Class;
       On_Change : On_Settings_Changed_Callback;
     end record;
   type Settings_Toggle is access all Settings_Toggle_Record'Class;

   procedure On_Toggle_Provider
     (Self : access Gtk_Cell_Renderer_Toggle_Record'Class;
      Path : UTF8_String);
   --  Called when a provider is enabled or disabled in the settings

   procedure On_Reorder_Provider
     (Toggle : access Glib.Object.GObject_Record'Class;
      Path   : Gtk_Tree_Path);
   --  Called when rows are reordered in the settings dialog

   procedure Update_Provider_Order
     (Self : access Settings_Toggle_Record'Class);
   --  Update the preference that lists the order of providers, based on the
   --  settings dialog.

   procedure On_Escape (Self : access Gtk_Widget_Record'Class);
   --  Called when "<escape>" has been called

   procedure On_Activate (Self : access Gtk_Widget_Record'Class);
   --  Called when the user activates one of the search proposals

   procedure Reset;
   --  Reset the global search entry after <escape> or a search is selected

   procedure Change_Proposals_Per_Provider
     (Spin : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when the user changes the number of proposals per provider
   --  through the settings.

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences change

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self : not null access Overall_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Searches everywhere in GPS. In the list of results, click on"
        & " the name of the context (file names, actions,...) to limit the"
        & " results to that context only, and display all possible matches"
        & " in that context.";
   end Documentation;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Result_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Result_Array, Result_Array_Access);
   begin
      if Self /= null then
         for S in Self'Range loop
            Free (Self (S));
         end loop;
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Overall_Search_Provider) is
   begin
      Free (Self.Current);
   end Free;

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

      Free (Self.Current);

      Self.Current := new Result_Array
        (1 .. Pref_Proposals_Per_Provider.Get_Pref);

      if Self.Provider /= null then
         Self.Provider.Count := 0;
         Self.Provider.Set_Pattern
            (Self.Pattern,
             Limit => Natural'Min (Limit, Self.Current'Last));
         Trace (Me, "Switching to provider: " & Self.Provider.Display_Name);
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

      if Self.Provider.Enabled
        and then Self.Current_Returned < Self.Current'First
      then
         Self.Provider.Next (Result, Has_Next);

         if Result /= null then
            Result.Provider.Count := Result.Provider.Count + 1;

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

      if Self.Provider.Enabled
        and then Self.Current_Returned <= Self.Current_Index
      then
         Result := Self.Current (Self.Current_Returned);
         Self.Current (Self.Current_Returned) := null;  --  belongs to caller
         Self.Current_Returned := Self.Current_Returned + 1;
         Has_Next := True;
         return;
      end if;

      --  Move to next provider
      Self.Current_Provider := Self.Current_Provider + 1;
      Self.Provider := Registry.Get (Self.Current_Provider);

      if Self.Provider /= null then
         Self.Provider.Count := 0;
         Self.Provider.Set_Pattern (Self.Pattern, Limit => Self.Current'Last);
         Trace (Me, "Switching to provider: " & Self.Provider.Display_Name);
      end if;

      Self.Current_Index := Self.Current'First - 1;
      Self.Current_Returned := Self.Current'First - 1;
      Result := null;
      Has_Next := True;
   end Next;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Overall_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      Suffix : Unbounded_String;
      Found : Boolean := False;
   begin
      Self.Set_Pattern (Pattern);

      while Self.Provider /= null loop

         declare
            Tmp : constant String := Self.Provider.Complete_Suffix (Pattern);
         begin
            if Tmp /= "" then
               if not Found then
                  Found := True;
                  Suffix := To_Unbounded_String (Tmp);
               else
                  declare
                     Current : constant String := To_String (Suffix);
                  begin
                     for S in Current'Range loop
                        if Tmp'First + S - Current'First > Tmp'Last
                          or else Current (S) /=
                             Tmp (Tmp'First + S - Current'First)
                        then
                           Suffix := To_Unbounded_String
                             (Current (Current'First .. S - 1));
                           exit;
                        end if;
                     end loop;
                  end;
               end if;
            end if;
         end;

         Self.Current_Provider := Self.Current_Provider + 1;
         Self.Provider := Registry.Get (Self.Current_Provider);
      end loop;

      return To_String (Suffix);
   end Complete_Suffix;

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

   -----------------------------------
   -- Change_Proposals_Per_Provider --
   -----------------------------------

   procedure Change_Proposals_Per_Provider
     (Spin : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      S : constant Gtk_Spin_Button := Gtk_Spin_Button (Spin);
   begin
      if S = null then
         raise Program_Error with "no spin button";
      end if;
      if Kernel = null then
         raise Program_Error with "no kernel";
      end if;
      Set_Pref
        (Pref_Proposals_Per_Provider,
         Get_Preferences (Kernel), Integer (S.Get_Value));
   end Change_Proposals_Per_Provider;

   ---------------------------
   -- Update_Provider_Order --
   ---------------------------

   procedure Update_Provider_Order
     (Self : access Settings_Toggle_Record'Class)
   is
      Iter     : Gtk_Tree_Iter := Self.Model.Get_Iter_First;
      Provider : Search_Provider_Access;
      Pref     : Unbounded_String;
   begin
      while Iter /= Null_Iter loop
         Provider := Convert (Get_Address (Self.Model, Iter, 2));
         if Provider /= null then
            if not Provider.Enabled then
               Append (Pref, "-");
            end if;

            Append (Pref, Provider.Display_Name & ";");
         end if;

         Self.Model.Next (Iter);
      end loop;

      Trace (Me, "Providers order changed: " & To_String (Pref));

      Set_Pref
        (Pref_Provider_Order, Get_Preferences (Self.Kernel), To_String (Pref));
   end Update_Provider_Order;

   -------------------------
   -- On_Reorder_Provider --
   -------------------------

   procedure On_Reorder_Provider
     (Toggle : access Glib.Object.GObject_Record'Class;
      Path   : Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);
      T : constant Settings_Toggle := Settings_Toggle (Toggle);
   begin
      Update_Provider_Order (T);
      T.On_Change (T.Data);
   end On_Reorder_Provider;

   ------------------------
   -- On_Toggle_Provider --
   ------------------------

   procedure On_Toggle_Provider
     (Self : access Gtk_Cell_Renderer_Toggle_Record'Class;
      Path : UTF8_String)
   is
      T        : constant Settings_Toggle := Settings_Toggle (Self);
      Iter     : Gtk_Tree_Iter;
      Provider : Search_Provider_Access;
   begin
      Iter := Get_Iter_From_String (T.Model, Path);

      if Iter /= Null_Iter then
         Provider := Convert (Get_Address (T.Model, Iter, 2));
         Provider.Enabled := not Provider.Enabled;

         --  Do the actual toggling
         T.Model.Set (Iter, 0, Provider.Enabled);

         Update_Provider_Order (T);
         T.On_Change (T.Data);
      end if;
   end On_Toggle_Provider;

   -------------------
   -- Edit_Settings --
   -------------------

   overriding procedure Edit_Settings
     (Self : not null access Overall_Search_Provider;
      Box  : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback)
   is
      Spin     : Gtk_Spin_Button;
      Label    : Gtk_Label;
      B        : Gtk_Box;
      P        : Integer;
      Provider : Kernel_Search_Provider_Access;
      Colnum   : Gint;
      Model    : Gtk_List_Store;
      Frame    : Gtk_Frame;
      View     : Gtk_Tree_View;
      Col      : Gtk_Tree_View_Column;
      Toggle   : Settings_Toggle;
      Text     : Gtk_Cell_Renderer_Text;
      Iter     : Gtk_Tree_Iter;
      Val      : GValue;
      pragma Unreferenced (Colnum);
   begin
      Gtk_New_Hbox (B, Homogeneous => False);
      Box.Pack_Start (B, Expand => False);

      Gtk_New (Label, -"Proposals per context:");
      B.Pack_Start (Label, Expand => False);

      Gtk_New (Spin, Min => 2.0, Max => 20.0, Step => 1.0);
      Spin.Set_Value (Gdouble (Pref_Proposals_Per_Provider.Get_Pref));
      B.Pack_Start (Spin, Expand => False);

      Kernel_Callback.Connect
        (Spin, Signal_Value_Changed,
         Change_Proposals_Per_Provider'Access, Self.Kernel);
      Spin.On_Value_Changed
        (Gtk.Spin_Button.Cb_GObject_Void (On_Change),
         Data, After => True);

      Gtk_New (Frame);
      Frame.Set_Label (-"Categories");
      Box.Pack_Start (Frame, Expand => True, Fill => True);

      Gtk_New_Vbox (B, Homogeneous => False);
      Frame.Add (B);

      Gtk_New
        (Label, "Drag categories to change the order in which results appear");
      Label.Set_Padding (5, 10);
      Label.Set_Alignment (0.0, 0.5);
      B.Pack_Start (Label, Expand => False);

      Gtk_New (Model,
               (0 => GType_Boolean,
                1 => GType_String,
                2 => GType_Pointer));
      Gtk_New (View, Model);
      View.Set_Headers_Visible (False);
      View.Set_Reorderable (True);
      B.Pack_Start (View, Expand => True, Fill => True);
      Unref (Model);

      Gtk_New (Col);
      Colnum := View.Append_Column (Col);
      Toggle := new Settings_Toggle_Record;
      Toggle.Model := Model;
      Toggle.Data := Data;
      Toggle.Kernel := Self.Kernel;
      Toggle.On_Change := On_Change;
      Gtk.Cell_Renderer_Toggle.Initialize (Toggle);
      Col.Pack_Start (Toggle, False);
      Col.Add_Attribute (Toggle, "active", 0);
      Set_Property
        (Toggle, Gtk.Cell_Renderer_Toggle.Activatable_Property, True);
      Toggle.On_Toggled (On_Toggle_Provider'Access);

      Gtk_New (Col);
      Colnum := View.Append_Column (Col);
      Gtk_New (Text);
      Col.Pack_Start (Text, True);
      Col.Add_Attribute (Text, "text", 1);

      P := 1;
      loop
         Provider := Kernel_Search_Provider_Access (Get (Registry, P));
         exit when Provider = null;

         Model.Append (Iter);
         Model.Set (Iter, 0, Provider.Enabled);
         Model.Set (Iter, 1, Provider.Display_Name);

         Init (Val, GType_Pointer);
         Set_Address (Val, Provider.all'Address);
         Model.Set_Value (Iter, 2, Val);
         Unset (Val);

         P := P + 1;
      end loop;

      --  Monitor drag-and-drop event. Do this after we have filled the tree.

      On_Row_Deleted
        (+Model, On_Reorder_Provider'Access, Toggle, After => True);

      --  Ask the settings for each of the providers.
      P := 1;
      loop
         Provider := Kernel_Search_Provider_Access (Registry.Get (P));
         exit when Provider = null;
         Provider.Edit_Settings (Box, Data, On_Change);
         P := P + 1;
      end loop;
   end Edit_Settings;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      Pref : constant Preference := Get_Pref (Data);
   begin
      if Pref = null
        or else Pref = Preference (Pref_Provider_Order)
      then
         declare
            Vals : String_List_Access := GNATCOLL.Utils.Split
              (Pref_Provider_Order.Get_Pref, On => ';');
            Rank : Positive := 1;
            P    : Positive;
            Provider : Search_Provider_Access;
            Enabled : Boolean;
         begin
            --  Reset all ranks

            P := 1;
            loop
               Provider := Get (Registry, P);
               exit when Provider = null;
               Provider.Rank := Positive'Last;
               P := P + 1;
            end loop;

            --  Then set the new values

            for V in Vals'Range loop
               if Vals (V)(Vals (V)'First) = '-' then
                  Provider := Get (Registry, Vals (V)
                                   (Vals (V)'First + 1 .. Vals (V)'Last));
                  Enabled := False;
               else
                  Provider := Get (Registry, Vals (V).all);
                  Enabled := True;
               end if;

               if Provider /= null then
                  Provider.Rank := Rank;
                  Provider.Enabled := Enabled;
                  Rank := Rank + 1;
               end if;
            end loop;

            --  And finally assign values for the providers when unset.

            P := 1;
            loop
               Provider := Get (Registry, P);
               exit when Provider = null;
               if Provider.Rank = Positive'Last then
                  Provider.Rank := Rank;
                  Rank := Rank + 1;
               end if;
               P := P + 1;
            end loop;

            Free (Vals);

            Registry.Sort_Providers;
         end;
      end if;
   end On_Preferences_Changed;

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

      Overall.Kernel := Kernel_Handle (Kernel);

      Command := new Global_Search_Command;
      Command.Provider := Search_Provider_Access (Overall);
      Command.History := new History_Key'("global-search-entry");
      Module.Default_Command := Command;
      Register_Action
         (Kernel, "Global Search", Command,
          Description =>
             "Activate the global search field in the main toolbar",
          Category => "Search");

      Pref_Proposals_Per_Provider := Create
        (Get_Preferences (Kernel),
         Name    => "Proposals_Per_Context",
         Label   => "",
         Page    => "",
         Doc     => "Number of proposals per context in the global search",
         Minimum => 2,
         Maximum => 20,
         Default => 5);

      Pref_Provider_Order := Create
        (Get_Preferences (Kernel),
         Name    => "Providers_Order",
         Label   => "",
         Page    => "",
         Doc     => "Order in which the search contexts are displayed in the"
         & " global search",
         Default =>
           Provider_Opened_Win & ";"
           & Provider_Filenames & ";"
           & Provider_Entities & ";"
           & Provider_Actions & ";"
           & Provider_Builds & ";"
           & Provider_Bookmarks & ";"
           & Provider_Sources & ";");

      P := new GPS.Kernel.Search.Filenames.Filenames_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      P := new GPS.Kernel.Search.Actions.Actions_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      P := new GPS.Kernel.Search.Sources.Sources_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

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

      Add_Hook (Kernel, Preference_Changed_Hook,
                Wrapper (On_Preferences_Changed'Access),
                Name => "search.preferences_changed");
   end Register_Module;

end GPS.Search.GUI;
