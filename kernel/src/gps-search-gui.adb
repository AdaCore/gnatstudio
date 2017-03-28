------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with GNAT.Strings;                  use GNAT.Strings;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.Utils;                use GNATCOLL.Utils;
with System;
with System.Address_Image;

with Glib;                          use Glib;
with Glib.Properties;               use Glib.Properties;
with Glib.Values;                   use Glib.Values;
with Gtk.Alignment;                 use Gtk.Alignment;
with Gtk.Cell_Renderer;             use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;        use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;      use Gtk.Cell_Renderer_Toggle;
with Gtk.Check_Button;              use Gtk.Check_Button;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Frame;                     use Gtk.Frame;
with Gtk.Label;                     use Gtk.Label;
with Gtk.List_Store;                use Gtk.List_Store;
with Gtk.Toggle_Button;             use Gtk.Toggle_Button;
with Gtk.Tree_Model;                use Gtk.Tree_Model;
with Gtk.Tree_View_Column;          use Gtk.Tree_View_Column;
with Gtk.Tree_View;                 use Gtk.Tree_View;
with Gtk.Spin_Button;               use Gtk.Spin_Button;
with Gtk.Widget;                    use Gtk.Widget;
with Gtkada.Entry_Completion;       use Gtkada.Entry_Completion;
with Gtkada.Handlers;               use Gtkada.Handlers;

with Commands.Interactive;          use Commands, Commands.Interactive;
with Default_Preferences;           use Default_Preferences;
with GPS.Kernel.Actions;            use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;              use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Modules;            use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;            use GPS.Kernel.Scripts;
with GPS.Kernel.Search.Actions;
with GPS.Kernel.Search.Filenames;
with GPS.Kernel.Search.Sources;
with GPS.Kernel.Search.Plugins;
with GPS.Kernel.Search.Preferences;
with GPS.Intl;                      use GPS.Intl;
with GPS.Main_Window;               use GPS.Main_Window;
with Histories;                     use Histories;

package body GPS.Search.GUI is
   Me : constant Trace_Handle := Create ("SEARCH");

   Pref_Proposals_Per_Provider : Integer_Preference;
   --  Preference for the number of proposals per provider

   Pref_Provider_Order : String_Preference;
   --  The preferred order of providers

   type Global_Search_Module_Record is new Module_ID_Record with record
      Search          : Gtkada_Entry;

      Registry        : Search_Provider_Registry_Access;
      --  List of all the providers of the Global Search.

      Default_Command : Global_Search_Command_Access;
      --  The command used to give the focus to the global search.
      --  Do not free.

      Current_Command : Global_Search_Command_Access;
      --  The command that was last used to give focus to the search (if any).
      --  Do not free.
   end record;
   type Global_Search_Module is access all Global_Search_Module_Record'Class;

   Module : constant Global_Search_Module := new Global_Search_Module_Record;

   procedure Free (Self : in out Result_Array_Access);
   --  Free self and the search results

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
      Path : Basic_Types.UTF8_String);
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

   procedure On_Entry_Changed (Self : access Gtk_Widget_Record'Class);
   --  Called when the user changes the text in the entry

   procedure Reset;
   --  Reset the global search entry after <escape> or a search is selected

   procedure Change_Proposals_Per_Provider
     (Spin : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Called when the user changes the number of proposals per provider
   --  through the settings.

   ----------------------------------
   -- Register_Provider_And_Action --
   ----------------------------------

   procedure Register_Provider_And_Action
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Provider   : not null access Kernel_Search_Provider'Class;
      Icon_Name  : String := "")
   is
      Command : Global_Search_Command_Access;
   begin
      Provider.Kernel := Kernel_Handle (Kernel);

      --  Register the provider's module
      Provider.Register_Module;

      --  Initialize the registry if it's the first time
      if Module.Registry = null then
         Module.Registry := new Search_Provider_Registry;
      end if;

      Module.Registry.Register (Provider);

      Command := new Global_Search_Command;
      Command.Provider := Search_Provider_Access (Provider);
      Command.History := new History_Key'
        ("global-search-entry-" & History_Key (Provider.Display_Name));
      Register_Action
        (Kernel, Action_Name_Prefix & Provider.Display_Name, Command,
         Description => Command.Provider.Documentation,
         Category    => "Search",
         Icon_Name   => Icon_Name);
   end Register_Provider_And_Action;

   -------------
   -- Scripts --
   -------------

   type Flags is mod 2 ** 16;
   Fuzzy          : constant Flags := 1;
   Substrings     : constant Flags := 2;
   Regexp         : constant Flags := 4;
   Case_Sensitive : constant Flags := 8;
   Whole_Word     : constant Flags := 16;

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences change

   function Create_Search_Instance
     (Script   : not null access Scripting_Language_Record'Class;
      Provider : access Search_Provider'Class)
      return Class_Instance;
   function Create_Search_Result_Instance
     (Script   : not null access Scripting_Language_Record'Class;
      Result   : Search_Result_Access)
      return Class_Instance;
   --  Create a new class instance for GPS.Search or GPS.Search_Result

   procedure Search_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   procedure Search_Result_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   procedure Search_Result_Setters
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Handlers for the shell commands

   type Python_Search_Provider is new Kernel_Search_Provider with record
      Name : GNAT.Strings.String_Access;
      Inst : aliased Instance_List;
   end record;
   type Python_Search_Provider_Access
      is access all Python_Search_Provider'Class;
   overriding procedure Free (Self : in out Python_Search_Provider);
   overriding procedure Set_Pattern
     (Self    : not null access Python_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Python_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Python_Search_Provider) return String
     is (Self.Name.all);

   type Python_Search_Result is new Kernel_Search_Result with record
      Inst : aliased Instance_List;
   end record;
   overriding procedure Execute
     (Self : not null access Python_Search_Result;
      Give_Focus : Boolean);
   overriding procedure Free (Self : in out Python_Search_Result);

   procedure Set_Search_Pattern
     (Data   : Callback_Data'Class;
      Num    : Positive;
      Search : Search_Pattern_Access);
   --  Update the search pattern stored in the instance's data

   type Provider_Property_Record is new Instance_Property_Record with record
      Provider : Search_Provider_Access;
      Pattern  : Search_Pattern_Access;
   end record;
   type Provider_Property is access all Provider_Property_Record'Class;
   overriding procedure Destroy (Prop : in out Provider_Property_Record);

   type Result_Property_Record is new Instance_Property_Record with record
      Result : Search_Result_Access;
   end record;
   type Result_Property is access all Result_Property_Record'Class;
   overriding procedure Destroy (Prop : in out Result_Property_Record);

   function Get_Search_Provider
     (Data : Callback_Data'Class; Num : Positive)
      return Search_Provider_Access;
   function Get_Search_Result
     (Data : Callback_Data'Class; Num : Positive)
      return Result_Property;
   --  Retrieve the provider or result information from the given parameter

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Overall_Search_Provider;
      Registry : not null Search_Provider_Registry_Access) is
   begin
      Self.Registry := Registry;
   end Initialize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Overall_Search_Provider) is
   begin
      Free (Self.Current);
      Free (Self.Registry);
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
      Self.Provider := Self.Registry.Get (Self.Current_Provider);

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

      --  If we are processing the current provider

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
      Self.Provider := Self.Registry.Get (Self.Current_Provider);

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
         Self.Provider := Self.Registry.Get (Self.Current_Provider);
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
      Module.Current_Command := null;
      Module.Search.Set_Completion (Module.Default_Command.Provider);
   end Reset;

   ---------------
   -- On_Escape --
   ---------------

   procedure On_Escape (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Reset;
   end On_Escape;

   ----------------------
   -- On_Entry_Changed --
   ----------------------

   procedure On_Entry_Changed (Self : access Gtk_Widget_Record'Class) is
      S : constant Gtkada_Entry := Gtkada_Entry (Self);
   begin
      if Module.Current_Command /= null then
         Add_To_History
           (Get_History (S.Get_Kernel).all,
            Module.Current_Command.History.all,
            S.Get_Text);
      end if;
   end On_Entry_Changed;

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
      Module.Search.Start_Searching;

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
      Path : Basic_Types.UTF8_String)
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
      H        : Gtk_Hbox;
      V        : Gtk_Vbox;
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
      Relative : Gtk_Check_Button;
      pragma Unreferenced (Colnum);
   begin
      Gtk_New_Hbox (H, Homogeneous => True);
      Box.Pack_Start (H, Expand => False);

      Gtk_New_Vbox (V, Homogeneous => False);
      H.Pack_Start (V, Expand => False, Padding => 3);

      Gtk_New_Hbox (B, Homogeneous => False);
      V.Pack_Start (B, Expand => False);

      Gtk_New (Relative, -"Display relative paths");
      Relative.Set_Tooltip_Text
        (-("Whether to display paths of project's sources as relative" &
           " to the project file."));
      V.Pack_Start (Relative, Expand => False);
      Associate (Get_History (Self.Kernel).all,
                 Key_Search_Displays_Relative_Paths,
                 Relative,
                 Default => False);
      Relative.On_Toggled
        (Gtk.Toggle_Button.Cb_GObject_Void (On_Change),
         Data, After => True);

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
      H.Pack_Start (Frame, Expand => True, Fill => True);

      Gtk_New_Vbox (B, Homogeneous => False);
      Frame.Add (B);

      Gtk_New (Label, "Drag categories to change the order");
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
         Provider := Kernel_Search_Provider_Access (Self.Registry.Get (P));
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
         Provider := Kernel_Search_Provider_Access (Self.Registry.Get (P));
         exit when Provider = null;
         Provider.Edit_Settings (V, Data, On_Change);
         P := P + 1;
      end loop;
   end Edit_Settings;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self, Kernel);
   begin
      if Pref = null
        or else Pref = Preference (Pref_Provider_Order)
      then
         declare
            Vals : String_List_Access := GNATCOLL.Utils.Split
              (Pref_Provider_Order.Get_Pref, On => ';');

            --  leave space for providers not part of the preference
            Rank : Positive := 2;
            P    : Positive;
            Provider : Search_Provider_Access;
            Enabled : Boolean;

            Unset : constant Positive := Positive'Last / 2;
            --  To find out which provides were not specified in the
            --  preference, and should preserve their current rank.
         begin
            --  Reset all ranks

            P := 1;
            loop
               Provider := Module.Registry.Get (P);
               exit when Provider = null;
               Provider.Rank :=
                  Unset + Positive'Min (Provider.Rank, Positive'Last / 2);
               P := P + 1;
            end loop;

            --  Then set the new values

            for V in Vals'Range loop
               if Vals (V)(Vals (V)'First) = '-' then
                  Provider := Module.Registry.Get (Vals (V)
                                   (Vals (V)'First + 1 .. Vals (V)'Last));
                  Enabled := False;
               else
                  Provider := Module.Registry.Get (Vals (V).all);
                  Enabled := True;
               end if;

               if Provider /= null then
                  Provider.Rank := Rank;
                  Provider.Enabled := Enabled;
                  Rank := Rank + 1;
               end if;
            end loop;

            --  And finally assign values for the providers when unset.
            --  Preserve the order they were in in the list

            P := 1;
            loop
               Provider := Module.Registry.Get (P);
               exit when Provider = null;
               if Provider.Rank >= Unset then
                  --  Preserve the old rank
                  Provider.Rank := Provider.Rank - Unset;
               end if;
               P := P + 1;
            end loop;

            Free (Vals);

            Module.Registry.Sort_Providers;
         end;
      end if;
   end Execute;

   ----------------------------
   -- Create_Search_Instance --
   ----------------------------

   function Create_Search_Instance
     (Script   : not null access Scripting_Language_Record'Class;
      Provider : access Search_Provider'Class)
      return Class_Instance
   is
      Search_Class : constant Class_Type :=
        New_Class (Get_Kernel (Script), "Search");
      Inst : Class_Instance;
   begin
      if Provider = null then
         return No_Class_Instance;
      end if;

      if Provider.all in Python_Search_Provider'Class then
         Inst := Get (Python_Search_Provider (Provider.all).Inst, Script);
         if Inst /= No_Class_Instance then
            return Inst;
         end if;
      end if;

      Inst := New_Instance (Script, Search_Class);
      Set_Data
        (Inst, "Search",
         Provider_Property_Record'
           (Provider => Search_Provider_Access (Provider),
            Pattern  => null));
      return Inst;
   end Create_Search_Instance;

   -----------------------------------
   -- Create_Search_Result_Instance --
   -----------------------------------

   function Create_Search_Result_Instance
     (Script   : not null access Scripting_Language_Record'Class;
      Result   : Search_Result_Access)
      return Class_Instance
   is
      Result_Class : Class_Type;
      Inst : Class_Instance := No_Class_Instance;
   begin
      if Result /= null then
         if Result.all in Python_Search_Result'Class then
            Inst := Get (Python_Search_Result (Result.all).Inst, Script);
            if Inst /= No_Class_Instance then
               return Inst;
            end if;
         end if;

         Result_Class := New_Class (Get_Kernel (Script), "Search_Result");
         Inst := New_Instance (Script, Result_Class);
         Set_Data
           (Inst, "Search_Result", Result_Property_Record'(Result => Result));

         if Result.all in Python_Search_Result'Class then
            Set (Python_Search_Result (Result.all).Inst, Inst);
         end if;
      end if;

      return Inst;
   end Create_Search_Result_Instance;

   -------------------------
   -- Get_Search_Provider --
   -------------------------

   function Get_Search_Provider
     (Data : Callback_Data'Class; Num : Positive)
      return Search_Provider_Access
   is
      Search_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), "Search");
      Inst : constant Class_Instance := Nth_Arg (Data, Num, Search_Class);
      Props : Provider_Property;
   begin
      if Inst /= No_Class_Instance then
         Props := Provider_Property
           (Instance_Property'(Get_Data (Inst, "Search")));
         if Props /= null then
            return Props.Provider;
         end if;
      end if;
      return null;
   end Get_Search_Provider;

   -----------------------
   -- Get_Search_Result --
   -----------------------

   function Get_Search_Result
     (Data : Callback_Data'Class; Num : Positive)
      return Result_Property
   is
      Result_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), "Search_Result");
      Inst : constant Class_Instance :=
        Nth_Arg (Data, Num, Result_Class, Allow_Null => True);
      Props : Result_Property;
   begin
      if Inst /= No_Class_Instance then
         Props := Result_Property
           (Instance_Property'(Get_Data (Inst, "Search_Result")));

         if Props = null then
            Set_Data
              (Inst, "Search_Result", Result_Property_Record'(Result => null));
            Props := Result_Property
              (Instance_Property'(Get_Data (Inst, "Search_Result")));
         end if;

         if Props.Result = null then
            --  Always ensure we create a proper search result for Ada.

            Props.Result := new Python_Search_Result'
              (Kernel   => Get_Kernel (Data),
               Inst     => Null_Instance_List,
               Score    => 100,
               Short    => new String'(""),
               Long     => null,
               Id       => new String'(""),
               Provider => null);
            --  Provider is set in the handler for "next", before returning
            --  the type to Ada.

            Set (Python_Search_Result (Props.Result.all).Inst, Inst);
         end if;

         return Props;
      end if;
      return null;
   end Get_Search_Result;

   ------------------------
   -- Set_Search_Pattern --
   ------------------------

   procedure Set_Search_Pattern
     (Data   : Callback_Data'Class;
      Num    : Positive;
      Search : Search_Pattern_Access)
   is
      Search_Class : constant Class_Type :=
        New_Class (Get_Kernel (Data), "Search");
      Inst : constant Class_Instance := Nth_Arg (Data, Num, Search_Class);
      Props : Provider_Property;
   begin
      if Inst /= No_Class_Instance then
         Props := Provider_Property
           (Instance_Property'(Get_Data (Inst, "Search")));
         if Props /= null then
            if Props.Pattern /= null then
               Free (Props.Pattern);
            end if;

            Props.Pattern := Search;
         end if;
      end if;
   end Set_Search_Pattern;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Prop : in out Provider_Property_Record) is
   begin
      Trace (Me, "Freeing search_pattern");
      if Prop.Pattern /= null then
         Free (Prop.Pattern);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Prop : in out Result_Property_Record) is
      Res : Search_Result_Access := Prop.Result;
   begin
      if Res /= null then
         --  Freeing the result will in turn free the instance, which will
         --  again call this function. So make this procedure reentrant.
         Prop.Result := null;

         Trace (Me, "Freeing search_result "
                & System.Address_Image (Res.all'Address));
         Free (Res);
      end if;
   end Destroy;

   -----------------------------
   -- Search_Commands_Handler --
   -----------------------------

   procedure Search_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, -"Use GPS.Search.lookup to create instances");

      elsif Command = "set_pattern" then
         declare
            Provider : constant Search_Provider_Access :=
              Get_Search_Provider (Data, 1);
            Pattern : constant String := Nth_Arg (Data, 2);
            Flag    : constant Flags := Flags (Nth_Arg (Data, 3, 0));
            Kind    : Search_Kind := Full_Text;
            P : Search_Pattern_Access;
         begin
            if (Flag and Fuzzy) /= 0 then
               Kind := GPS.Search.Fuzzy;
            elsif (Flag and Regexp) /= 0 then
               Kind := GPS.Search.Regexp;
            else
               Kind := GPS.Search.Full_Text;
            end if;

            if Provider /= null then
               P := Build
                 (Pattern        => Pattern,
                  Case_Sensitive => (Flag and Case_Sensitive) /= 0,
                  Whole_Word     => (Flag and Whole_Word) /= 0,
                  Kind           => Kind);
               Set_Search_Pattern (Data, 1, P);
               Provider.Set_Pattern (P);
            end if;
         end;

      elsif Command = "get" then
         declare
            Provider : constant Search_Provider_Access :=
              Get_Search_Provider (Data, 1);
            Result   : Search_Result_Access;
            Has_Next : Boolean;
         begin
            if Provider /= null then
               Provider.Next (Result, Has_Next);

               Set_Return_Value_As_List (Data, 2);
               Set_Return_Value (Data, Has_Next);
               Set_Return_Value
                 (Data,
                  Create_Search_Result_Instance (Get_Script (Data), Result));
            end if;
         end;

      elsif Command = "lookup" then
         declare
            Name     : constant String := Nth_Arg (Data, 1);
            Provider : constant Search_Provider_Access :=
                         Module.Registry.Get (Name);
         begin
            if Provider /= null then
               Set_Return_Value
                 (Data, Create_Search_Instance (Get_Script (Data), Provider));
            end if;
         end;

      elsif Command = "register" then
         declare
            Name : constant String := Nth_Arg (Data, 1);
            Inst : constant Class_Instance := Nth_Arg (Data, 2);
            Rank : constant Integer := Data.Nth_Arg (3, -1);
            Provider : Python_Search_Provider_Access;
         begin
            Provider := new Python_Search_Provider;
            Provider.Name := new String'(Name);
            Provider.Inst := Null_Instance_List;
            Provider.Rank := (if Rank <= 0 then 1000 else Rank);
            Provider.Kernel := Get_Kernel (Data);
            Set (Provider.Inst, Inst);

            Module.Registry.Register (Provider);
         end;
      end if;
   end Search_Commands_Handler;

   ------------------------------------
   -- Search_Result_Commands_Handler --
   ------------------------------------

   procedure Search_Result_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
   begin
      if Command = Constructor_Method then
         Set_Error_Msg (Data, -"Use GPS.Search.next to create instances");

      elsif Command = "show" then
         declare
            Result : constant Result_Property :=
              Get_Search_Result (Data, 1);
         begin
            if Result /= null then
               Result.Result.Execute (Give_Focus => True);
            end if;
         end;

      elsif Command = "short" then
         declare
            Result : constant Result_Property :=
              Get_Search_Result (Data, 1);
         begin
            if Result /= null
              and then Result.Result.Short /= null
            then
               Set_Return_Value (Data, Result.Result.Short.all);
            else
               Set_Return_Value (Data, String'(""));
            end if;
         end;

      elsif Command = "long" then
         declare
            Result : constant Result_Property :=
              Get_Search_Result (Data, 1);
         begin
            if Result /= null
              and then Result.Result.Long /= null
            then
               Set_Return_Value (Data, Result.Result.Long.all);
            else
               Set_Return_Value (Data, String'(""));
            end if;
         end;
      end if;
   end Search_Result_Commands_Handler;

   ---------------------------
   -- Search_Result_Setters --
   ---------------------------

   procedure Search_Result_Setters
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
   begin
      if Command = "short" then
         declare
            Result : constant Result_Property :=
              Get_Search_Result (Data, 1);
         begin
            if Result.Result.Id /= Result.Result.Short
              and then Result.Result.Short /= Result.Result.Long
            then
               Free (Result.Result.Short);
            end if;

            Result.Result.Short := new String'(Nth_Arg (Data, 2));
         end;

      elsif Command = "long" then
         declare
            Result : constant Result_Property :=
              Get_Search_Result (Data, 1);
         begin
            if Result.Result.Id /= Result.Result.Long
              and then Result.Result.Short /= Result.Result.Long
            then
               Free (Result.Result.Long);
            end if;

            Result.Result.Long := new String'(Nth_Arg (Data, 2));
         end;
      end if;
   end Search_Result_Setters;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Python_Search_Provider) is
   begin
      Free (Self.Name);
   end Free;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Python_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      Curs : Inst_Cursor := First (Self.Inst);
      Inst : Class_Instance;
      Result : Class_Instance;
      pragma Unreferenced (Limit);
   begin
      while Has_Element (Curs) loop
         Inst := Element (Self.Inst, Curs);
         declare
            Sub  : Subprogram_Type :=
              Get_Method (Inst, "set_pattern");
            Args : Callback_Data'Class :=
              Create (Get_Script (Inst), 2);
            P    : Flags := 0;
         begin
            Set_Nth_Arg (Args, 1, Pattern.Get_Text);

            case Pattern.Get_Kind is
            when GPS.Search.Full_Text =>
               P := P or Flags'(Substrings);
            when GPS.Search.Regexp =>
               P := P or Flags'(Regexp);
            when GPS.Search.Fuzzy | GPS.Search.Approximate =>
               P := P or Flags'(Fuzzy);
            end case;

            if Pattern.Get_Case_Sensitive then
               P := P or Flags'(Case_Sensitive);
            end if;

            if Pattern.Get_Whole_Word then
               P := P or Flags'(Whole_Word);
            end if;

            Set_Nth_Arg (Args, 2, Natural (P));

            Result := Execute (Sub, Args);
            Free (Args);
            Free (Sub);
         end;

         Next (Self.Inst, Curs);
      end loop;
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Python_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Curs : Inst_Cursor := First (Self.Inst);
      Inst : Class_Instance;
      R    : Result_Property;
   begin
      Result   := null;
      Has_Next := False;

      while Has_Element (Curs) loop
         Inst := Element (Self.Inst, Curs);
         declare
            Sub : Subprogram_Type := Get_Method (Inst, "get");
            Args : Callback_Data'Class := Create (Get_Script (Inst), 0);
            List : List_Instance'Class := Execute (Sub, Args);
         begin
            Has_Next := Nth_Arg (List, 1);

            --  If the result class is pure python, this will wrap it inside
            --  an Ada class, unless it has already been done when setting
            --  the 'short' and 'long' fields. This ensures the pure python
            --  instance can be used from Ada transparently.
            R := Get_Search_Result (List, 2);

            if R /= null then
               Result := R.Result;
               Result.Provider := Search_Provider_Access (Self);
               Self.Adjust_Score (Result);
            end if;

            Free (List);
            Free (Args);
            Free (Sub);
         end;

         Next (Self.Inst, Curs);
      end loop;
   end Next;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self : not null access Python_Search_Result;
      Give_Focus : Boolean)
   is
      pragma Unreferenced (Give_Focus);
      Curs : Inst_Cursor := First (Self.Inst);
      Inst : Class_Instance;
      Sub  : Subprogram_Type;
   begin
      while Has_Element (Curs) loop
         Inst := Element (Self.Inst, Curs);
         Sub := Get_Method (Inst, "show");
         if Sub /= null then
            declare
               Args : Callback_Data'Class :=
                 Create (Get_Script (Inst), 0);
               B    : constant Boolean := Execute (Sub, Args);
               pragma Unreferenced (B);
            begin
               Free (Args);
            end;
            Free (Sub);
         end if;
         Next (Self.Inst, Curs);
      end loop;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Python_Search_Result) is
      Curs : Inst_Cursor := First (Self.Inst);
      Inst : Class_Instance;
      Result    : Result_Property;
   begin
      while Has_Element (Curs) loop
         Inst := Element (Self.Inst, Curs);
         Result := Result_Property
           (Instance_Property'(Get_Data (Inst, "Search_Result")));

         if Result /= null then
            Result.Result := null;
         end if;
         Next (Self.Inst, Curs);
      end loop;

      Free (Self.Inst);
      Free (Search_Result (Self));
   end Free;

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
      P       : Kernel_Search_Provider_Access;
      Search_Class : constant Class_Type :=
        New_Class (Kernel.Scripts, "Search");
      Search_Result_Class : constant Class_Type :=
        New_Class (Kernel.Scripts, "Search_Result");

   begin
      Register_Module
         (Module      => Module,
          Kernel      => Kernel,
          Module_Name => "Global_Search");

      Overall.Kernel := Kernel_Handle (Kernel);
      Overall.Registry := Module.Registry;

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
         Label   => "Number of proposals",
         Path    => ":Omnisearch",
         Doc     => "Number of proposals per context in the global search",
         Minimum => 2,
         Maximum => 20,
         Default => 5);

      Pref_Provider_Order := Create
        (Get_Preferences (Kernel),
         Name    => "Providers_Order",
         Label   => "Search contexts order",
         Path    => ":Omnisearch",
         Doc     => "Order in which the search contexts are displayed in the"
         & " global search",
         Default =>
           Provider_Opened_Win & ";"
           & Provider_Filenames & ";"
           & Provider_Entities & ";"
           & Provider_Actions & ";"
           & Provider_Builds & ";"
           & Provider_Preferences & ";"
           & Provider_Bookmarks & ";"
           & Provider_Sources & ";"
           & Provider_Plugins & ";");

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all,
         Key_Search_Displays_Relative_Paths,
         Default_Value => False);

      P := new GPS.Kernel.Search.Filenames.Filenames_Search_Provider;
      Register_Provider_And_Action
        (Kernel, P, Icon_Name  => "gps-open-file-symbolic");

      P := new GPS.Kernel.Search.Actions.Actions_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      P := new GPS.Kernel.Search.Sources.Sources_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      P := new GPS.Kernel.Search.Sources.Current_File_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      P := new GPS.Kernel.Search.Preferences.Preferences_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      P := new GPS.Kernel.Search.Plugins.Plugins_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      Gtk_New (Align, 0.0, 1.0, 0.0, 0.0);
      Align.Set_Padding (0, 0, 0, 10);  --  to the right
      GPS_Window (Get_Main_Window (Kernel)).Toolbar_Box.Pack_End
        (Align, Expand => False);

      Gtk_New
         (Module.Search,
          Kernel              => Kernel,
          Name                => "global_search",
          Completion_In_Popup => True,
          Case_Sensitive      => False,
          Completion          => Module.Default_Command.Provider);
      Module.Search.Set_Name ("global-search");

      Gtk_New_Vbox (Vbox);
      Vbox.Pack_Start (Module.Search, Padding => 2);
      Align.Add (Vbox);

      Widget_Callback.Connect (Module.Search, Signal_Escape, On_Escape'Access);
      Widget_Callback.Connect
        (Module.Search, Signal_Activate, On_Activate'Access);

      Widget_Callback.Connect
        (Module.Search, Signal_Changed, On_Entry_Changed'Access);

      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Class   => Search_Class,
         Handler => Search_Commands_Handler'Access);
      Register_Command
        (Kernel.Scripts, "set_pattern",
         Params  => (1 => Param ("pattern"),
                     2 => Param ("flags", Optional => True)),
         Class   => Search_Class,
         Handler => Search_Commands_Handler'Access);
      Register_Command
        (Kernel.Scripts, "get",
         Class   => Search_Class,
         Handler => Search_Commands_Handler'Access);
      Register_Command
        (Kernel.Scripts, "lookup",
         Params        => (1 => Param ("name")),
         Static_Method => True,
         Class         => Search_Class,
         Handler       => Search_Commands_Handler'Access);
      Register_Command
        (Kernel.Scripts, "register",
         Params        => (1 => Param ("name"),
                           2 => Param ("provider"),
                           3 => Param ("rank", Optional => True)),
         Static_Method => True,
         Class         => Search_Class,
         Handler       => Search_Commands_Handler'Access);

      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Class   => Search_Result_Class,
         Handler => Search_Result_Commands_Handler'Access);
      Register_Command
        (Kernel.Scripts, "show",
         Class   => Search_Result_Class,
         Handler => Search_Result_Commands_Handler'Access);
      Register_Property
        (Kernel.Scripts, "short",
          Class   => Search_Result_Class,
          Getter  => Search_Result_Commands_Handler'Access,
          Setter  => Search_Result_Setters'Access);
      Register_Property
        (Kernel.Scripts, "long",
          Class   => Search_Result_Class,
          Getter  => Search_Result_Commands_Handler'Access,
          Setter  => Search_Result_Setters'Access);
   end Register_Module;

end GPS.Search.GUI;
