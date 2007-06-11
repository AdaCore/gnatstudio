-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.Strings;              use GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Combo;                 use Gtk.Combo;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.List;                  use Gtk.List;
with Gtk.List_Item;             use Gtk.List_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Spin_Button;           use Gtk.Spin_Button;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Toggle_Button;         use Gtk.Toggle_Button;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with Language_Handlers;         use Language_Handlers;
with Prj;
with Project_Viewers;           use Project_Viewers;
with Projects.Editor;           use Projects.Editor;
with Projects;                  use Projects;
with Scenario_Selectors;        use Scenario_Selectors;
with Snames;                    use Snames;
with String_Utils;              use String_Utils;
with Switch.M;                  use Switch.M;
with Traces;                    use Traces;
with Namet;                     use Namet;
with VFS;                       use VFS;

package body Switches_Editors is

   Me : constant Debug_Handle := Create ("Switches_Editors");

   -------------------
   -- Check buttons --
   -------------------

   type Switch_Check_Widget;
   type Switch_Check_Widget_Access is access all Switch_Check_Widget'Class;
   type Switch_Check_Widget is new Switch_Basic_Widget_Record with record
      Check         : Gtk.Check_Button.Gtk_Check_Button;
      Next_In_Group : Switch_Check_Widget_Access;
   end record;

   function Get_Switch (Switch : Switch_Check_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Check_Widget; List : in out GNAT.Strings.String_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Check_Widget; List : in out GNAT.Strings.String_List);

   ------------
   -- Fields --
   ------------

   type Switch_Field_Widget (Sep_Length, Switch_Length : Natural)
     is new Switch_Basic_Widget_Record (Switch_Length)
   with record
      Separator : String (1 .. Sep_Length);
      Field : Gtk.GEntry.Gtk_Entry;
   end record;
   type Switch_Field_Widget_Access is access all Switch_Field_Widget'Class;

   function Get_Switch (Switch : Switch_Field_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Field_Widget; List : in out GNAT.Strings.String_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Field_Widget; List : in out GNAT.Strings.String_List);

   ------------------
   -- Spin buttons --
   ------------------

   type Switch_Spin_Widget (Sep_Length, Switch_Length : Natural)
     is new Switch_Basic_Widget_Record (Switch_Length)
   with record
      Spin  : Gtk.Spin_Button.Gtk_Spin_Button;
      Default : Integer;
      --  Default value, for which no switch is needed on the command line
      Separator : String (1 .. Sep_Length);
   end record;
   type Switch_Spin_Widget_Access is access all Switch_Spin_Widget'Class;

   function Get_Switch (Switch : Switch_Spin_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out GNAT.Strings.String_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out GNAT.Strings.String_List);

   -------------------
   -- Popup buttons --
   -------------------

   type Switch_Popup_Widget (Label_Length : Natural) is new Gtk_Button_Record
   with record
      Popup  : Gtk_Dialog;
      Widget : Gtk_Widget;
      Label  : String (1 .. Label_Length);
   end record;
   type Switch_Popup_Widget_Access is access all Switch_Popup_Widget'Class;

   -------------------
   -- Combo buttons --
   -------------------

   type Switch_Combo_Widget (Sep_Length       : Natural;
                             Switch_Length    : Natural;
                             No_Digit_Length  : Natural;
                             No_Switch_Length : Natural)
     is new Switch_Basic_Widget_Record (Switch_Length)
   with record
      Separator         : String (1 .. Sep_Length);
      Combo             : Gtk_Combo;
      Default_No_Digit  : String (1 .. No_Digit_Length);
      Default_No_Switch : String (1 .. No_Switch_Length);
   end record;
   type Switch_Combo_Widget_Access is access all Switch_Combo_Widget'Class;

   function Get_Switch (Switch : Switch_Combo_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out GNAT.Strings.String_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out GNAT.Strings.String_List);

   procedure Page_Destroyed (Page : access Gtk_Widget_Record'Class);
   --  Free the memory occupied by Page

   ---------------------
   -- Combo list item --
   ---------------------

   type Combo_List_Item_Record (Value_Length : Natural) is new
     Gtk_List_Item_Record with
   record
      Value : String (1 .. Value_Length);
   end record;
   type Combo_List_Item is access all Combo_List_Item_Record'Class;

   ------------------
   -- Dependencies --
   ------------------

   type Dependency_Data is record
      Master_Status  : Boolean;
      Slave_Switch   : Switch_Check_Widget_Access;
      Slave_Activate : Boolean;
   end record;

   package Dependency_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Dependency_Data);

   procedure Check_Dependency
     (Check : access Gtk_Widget_Record'Class;
      Data  : Dependency_Data);
   procedure Check_Field_Dependency
     (Field : access Gtk_Widget_Record'Class;
      Data  : Dependency_Data);
   --  Callback to handle the dependencies between two items

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pages_Array, Page_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Strings.String_List, String_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String_List_Array, String_List_Array_Access);

   function Get_Switch_Widget
     (Page   : access Switches_Editor_Page_Record'Class;
      Switch : String) return Switch_Basic_Widget;
   --  Return the widget that edits a specific switch.
   --  Note: this only compare with the static part of the switch (e.g doesn't
   --  include the digits in "-O1")

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array);
   --  Fill the editor with the switches information for Files (or the
   --  default switches if Files is empty).

   function Close_Switch_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array;
      Scenario  : access Scenario_Selector_Record'Class) return Boolean;
   --  Called when the user has closed a switch editor for a specific file.
   --  This modifies the edited project to reflect the changes done in the
   --  dialog.
   --  File_Name is the name of the file whose switches we are changing, or ""
   --  if we are changing the default switches.
   --  Return True if the switches were modified

   procedure Browse_Directory (Field : access GObject_Record'Class);
   procedure Browse_File (Field : access GObject_Record'Class);
   --  Open a dialog to select a directory or a file

   procedure Revert_To_Default (Switches : access Gtk_Widget_Record'Class);
   --  Revert to the default switches in the editor

   function Normalize_Compiler_Switches
     (Page     : access Switches_Editor_Page_Record'Class;
      Switches : GNAT.Strings.String_List) return GNAT.Strings.String_List;
   --  Return an equivalent of Switches, but where concatenated switches have
   --  been separated (for instance, -gnatwue = -gnatwu -gnatwe).
   --  Nothing is done if the tool doesn't need this special treatment.
   --  The returned array should be freed. However, you no longer need to free
   --  the memory for the array that was passed as a parameter (we either
   --  return it directly, or reuse the strings from it for the output).

   procedure Activate_Dependencies
     (Page   : access Switches_Editor_Page_Record'Class;
      Editor : access Switches_Edit_Record'Class);
   --  Activate all the dependencies defined for page

   procedure Refresh_Page (Page : access Gtk_Widget_Record'Class);
   --  Recompute the value of the command line after a switch has been changed
   --  through the GUI

   procedure On_Cmd_Line_Changed (Page : access Gtk_Widget_Record'Class);
   --  Reset the GUI content of the page, based on the current command line

   function Get_Switches
     (Page : access Switches_Editor_Page_Record'Class;
      Normalize : Boolean) return GNAT.Strings.String_List;
   --  Return the list of parameters set on the command line.
   --  If normalize is True, then GNAT's switches will be split when possible
   --  ("gnatwue" => "gnatwu", "gnatwe")

   function Get_Switches
     (Switches : access Switches_Edit_Record'Class;
      Pkg_Name : String;
      Language : String;
      Files    : File_Array;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List;
   --  Return the list of switches for Files, found in the package Pkg_Name,
   --  for a specific language, and for a specific list of switches. The
   --  returned array must be freed by the caller.

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record'Class;
      Languages : GNAT.Strings.String_List;
      Show_Only : Boolean;
      File_Specific : Boolean);
   --  Same as the public version, except that the pages are never hidden, only
   --  shown depending on the languages.
   --  File_Specific should be True if the editor is open for a specific set
   --  of files, as opposed to a project-wide setting.

   procedure Append_Switch
     (Page   : access Switches_Editor_Page_Record'Class;
      Button : access Switch_Basic_Widget_Record'Class);
   --  Add a new switch to the page.

   procedure Editor_Destroyed (Editor : access Gtk_Widget_Record'Class);
   --  Callback when the editor is destroyed.

   procedure Popup_New_Page (Button : access Gtk_Widget_Record'Class);
   --  Popup the window registered in Switch_Popup_Widget.

   procedure Destroy_Popup (Button : access Gtk_Widget_Record'Class);
   --  Callback when the Switch_Popup_Widget is destroyed.

   function Filter_Matches
     (Page : access Switches_Editor_Page_Record'Class;
      Language : String) return Boolean;
   --  Return True if Page applies to Language.

   function Has_Supported_Language
     (Page                : access Switches_Editor_Page_Record'Class;
      Supported_Languages : GNAT.Strings.String_List) return Boolean;
   --  Return True if Page applies to one of the languages in
   --  Supported_Language

   function Get_Parameter
     (Switch          : String;
      Separator       : String;
      List            : GNAT.Strings.String_List;
      Index_In_List   : Natural;
      Skip_Next_Index : access Boolean) return String;
   --  Get the parameter for Switch, assuming it was first seen at position
   --  Index_In_List in List.

   function Switch_Matches
     (Switch        : String;
      Separator     : String;
      Candidate     : GNAT.Strings.String_Access) return Boolean;
   --  Whether Candidate matches Switch & Separator (properly take into
   --  account the case where Separator is a space, and the argument appears
   --  in a separate item in the argument_list

   -----------------------
   -- Get_Switch_Widget --
   -----------------------

   function Get_Switch_Widget
     (Page   : access Switches_Editor_Page_Record'Class;
      Switch : String) return Switch_Basic_Widget is
   begin
      if Page.Switches /= null then
         for S in Page.Switches'Range loop
            if Page.Switches (S).Switch = Switch then
               return Page.Switches (S);
            end if;
         end loop;
      end if;
      return null;
   end Get_Switch_Widget;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Switch : Switch_Check_Widget) return String is
   begin
      if Get_Active (Switch.Check) then
         return Switch.Switch;
      else
         return "";
      end if;
   end Get_Switch;

   -------------------
   -- Filter_Switch --
   -------------------

   procedure Filter_Switch
     (Switch : Switch_Check_Widget; List : in out GNAT.Strings.String_List) is
   begin
      for L in List'Range loop
         if List (L) /= null and then List (L).all = Switch.Switch then
            Free (List (L));
         end if;
      end loop;
   end Filter_Switch;

   ---------------------------
   -- Set_And_Filter_Switch --
   ---------------------------

   procedure Set_And_Filter_Switch
     (Switch : Switch_Check_Widget; List : in out GNAT.Strings.String_List)
   is
      use Widget_SList;
      Active : Boolean := False;
      Group  : Widget_SList.GSlist;
      Tmp    : Switch_Check_Widget_Access;
   begin
      if Switch.Check.all in Gtk_Radio_Button_Record'Class then
         Group := Get_Group (Gtk_Radio_Button (Switch.Check));

         --  First one in the group => Select it by default, so that if we
         --  have an item with switch="", this is properly handled:
         --   <radio label="...">
         --     <radio-entry label="..." switch="-switch" />
         --     <radio-entry label="..." switch="" />
         --   </radio>
         --  Since each radio button is prepended to the list in group, we need
         --  to initialize when we see the last in the group, since group is in
         --  the reverse order of calls to Set_And_Filter_Switch
         if Get_Data (Last (Group)) = Gtk_Widget (Switch.Check) then
            Tmp := Switch'Unrestricted_Access;
            while Tmp /= null loop
               if Tmp.Switch = "" then
                  Set_Active (Tmp.Check, True);
               end if;
               Tmp := Tmp.Next_In_Group;
            end loop;
         end if;
      end if;

      if Switch.Switch /= "" then
         for L in List'Range loop
            if List (L) /= null
              and then List (L).all = Switch.Switch
            then
               Active := True;
               Free (List (L));
            end if;
         end loop;

         Set_Active (Switch.Check, Active);
      end if;
   end Set_And_Filter_Switch;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Switch : Switch_Field_Widget) return String is
      Text : constant String := Get_Text (Switch.Field);
   begin
      if Text = "" then
         return "";
      else
         declare
            T : GNAT.Strings.String_Access := new String'(Text);
            Result : constant String := Switch.Switch & Switch.Separator
              & Argument_List_To_Quoted_String
              ((1 => T), Quote_Backslash => False);
         begin
            Free (T);
            return Result;
         end;
      end if;
   end Get_Switch;

   -------------------
   -- Filter_Switch --
   -------------------

   procedure Filter_Switch
     (Switch : Switch_Field_Widget; List : in out GNAT.Strings.String_List) is
   begin
      for L in List'Range loop
         if Switch_Matches (Switch.Switch, Switch.Separator, List (L)) then
            if Switch.Separator'Length /= 0
              and then Switch.Separator (Switch.Separator'First) = ' '
            then
               Free (List (L));

               if L < List'Last then
                  Free (List (L + 1));
               end if;
            else
               Free (List (L));
            end if;
         end if;
      end loop;
   end Filter_Switch;

   --------------------
   -- Switch_Matches --
   --------------------

   function Switch_Matches
     (Switch        : String;
      Separator     : String;
      Candidate     : GNAT.Strings.String_Access) return Boolean is
   begin
      if Candidate = null then
         return False;

      elsif Separator'Length > 0
        and then Separator (Separator'First) = ' '
      then
         return Candidate.all = Switch;

      else
         return Candidate'Length >= Switch'Length + Separator'Length
           and then Candidate
           (Candidate'First ..
              Candidate'First + Switch'Length + Separator'Length - 1) =
             Switch & Separator;
      end if;
   end Switch_Matches;

   -------------------
   -- Get_Parameter --
   -------------------

   function Get_Parameter
     (Switch          : String;
      Separator       : String;
      List            : GNAT.Strings.String_List;
      Index_In_List   : Natural;
      Skip_Next_Index : access Boolean) return String is
   begin
      if Separator'Length >= 1
        and then Separator (Separator'First) = ' '
      then
         if Index_In_List < List'Last
           and then List (Index_In_List + 1) /= null
         then
            Skip_Next_Index.all := True;
            return List (Index_In_List + 1).all;
         else
            Skip_Next_Index.all := False;
            return "";
         end if;

      else
         Skip_Next_Index.all := False;
         return List (Index_In_List)
           (List (Index_In_List)'First + Switch'Length + Separator'Length
            .. List (Index_In_List)'Last);
      end if;
   end Get_Parameter;

   ---------------------------
   -- Set_And_Filter_Switch --
   ---------------------------

   procedure Set_And_Filter_Switch
     (Switch : Switch_Field_Widget; List : in out GNAT.Strings.String_List)
   is
      Skip_Next_Index : aliased Boolean;
   begin
      for L in List'Range loop
         if Switch_Matches (Switch.Switch, Switch.Separator, List (L)) then
            Set_Text (Switch.Field,
                      Get_Parameter
                        (Switch          => Switch.Switch,
                         Separator       => Switch.Separator,
                         List            => List,
                         Index_In_List   => L,
                         Skip_Next_Index => Skip_Next_Index'Unchecked_Access));

            if Skip_Next_Index then
               Free (List (L + 1));
            end if;
            Free (List (L));
         end if;
      end loop;
   end Set_And_Filter_Switch;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Switch : Switch_Combo_Widget) return String is
      use type Widget_List.Glist;
      Children : Widget_List.Glist := Get_Children (Get_List (Switch.Combo));
      Item     : Combo_List_Item;
      Text     : constant String := Get_Text (Get_Entry (Switch.Combo));
   begin
      while Children /= Widget_List.Null_List loop
         Item := Combo_List_Item (Widget_List.Get_Data (Children));

         if Get_Text (Gtk_Label (Get_Child (Item))) = Text then
            if Item.Value = Switch.Default_No_Switch then
               return "";
            else
               return Switch.Switch & Switch.Separator & Item.Value;
            end if;
         end if;

         Children := Widget_List.Next (Children);
      end loop;

      return "";
   end Get_Switch;

   -------------------
   -- Filter_Switch --
   -------------------

   procedure Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out GNAT.Strings.String_List) is
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            Free (List (L));
            if L < List'Last
              and then Switch.Separator'Length > 0
              and then Switch.Separator (Switch.Separator'First) = ' '
            then
               Free (List (L + 1));
            end if;
         end if;
      end loop;
   end Filter_Switch;

   ---------------------------
   -- Set_And_Filter_Switch --
   ---------------------------

   procedure Set_And_Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out GNAT.Strings.String_List)
   is
      use type Widget_List.Glist;
      Item_Value : Integer := -2;
      Children   : Widget_List.Glist := Get_Children (Get_List (Switch.Combo));
      Item       : Combo_List_Item;
      Skip_Next_Index : aliased Boolean;
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            if Get_Parameter
              (Switch          => Switch.Switch,
               Separator       => Switch.Separator,
               List            => List,
               Index_In_List   => L,
               Skip_Next_Index => Skip_Next_Index'Unchecked_Access) = ""
            then
               Item_Value := -1;
            else
               Item_Value := L;
            end if;
            exit;
         end if;
      end loop;

      if Item_Value = -1 then
         while Children /= Widget_List.Null_List loop
            Item := Combo_List_Item (Widget_List.Get_Data (Children));
            if Item.Value = Switch.Default_No_Digit then
               Select_Child (Get_List (Switch.Combo), Gtk_Widget (Item));
               exit;
            end if;
            Children := Widget_List.Next (Children);
         end loop;

      elsif Item_Value = -2 then
         while Children /= Widget_List.Null_List loop
            Item := Combo_List_Item (Widget_List.Get_Data (Children));
            if Item.Value = Switch.Default_No_Switch then
               Select_Child (Get_List (Switch.Combo), Gtk_Widget (Item));
               exit;
            end if;
            Children := Widget_List.Next (Children);
         end loop;

      else
         declare
            Param : constant String := Get_Parameter
              (Switch          => Switch.Switch,
               Separator       => Switch.Separator,
               List            => List,
               Index_In_List   => Item_Value,
               Skip_Next_Index => Skip_Next_Index'Unchecked_Access);
         begin
            while Children /= Widget_List.Null_List loop
               Item := Combo_List_Item (Widget_List.Get_Data (Children));

               if Item.Value = Param then
                  Select_Child (Get_List (Switch.Combo), Gtk_Widget (Item));
                  exit;
               end if;

               Children := Widget_List.Next (Children);
            end loop;
         end;

         Free (List (Item_Value));
         if Skip_Next_Index then
            Free (List (Item_Value + 1));
         end if;
      end if;
   end Set_And_Filter_Switch;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Switch : Switch_Spin_Widget) return String is
      Val : constant Integer := Integer (Get_Value_As_Int (Switch.Spin));
   begin
      if Val /= Switch.Default  then
         return Switch.Switch & Switch.Separator & Image (Val);
      else
         return "";
      end if;
   end Get_Switch;

   -------------------
   -- Filter_Switch --
   -------------------

   procedure Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out GNAT.Strings.String_List) is
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            Free (List (L));
            if L < List'Last
              and then Switch.Separator'Length > 0
              and then Switch.Separator (Switch.Separator'First) = ' '
            then
               Free (List (L + 1));
            end if;
         end if;
      end loop;
   end Filter_Switch;

   ---------------------------
   -- Set_And_Filter_Switch --
   ---------------------------

   procedure Set_And_Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out GNAT.Strings.String_List)
   is
      Value           : Grange_Float := Grange_Float (Switch.Default);
      Skip_Next_Index : aliased Boolean;
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            declare
               Param : constant String := Get_Parameter
                 (Switch          => Switch.Switch,
                  Separator       => Switch.Separator,
                  List            => List,
                  Index_In_List   => L,
                  Skip_Next_Index => Skip_Next_Index'Unchecked_Access);
            begin
               Value := Grange_Float'Value (Param);
               Free (List (L));

               if Skip_Next_Index then
                  Free (List (L + 1));
               end if;

               Set_Value (Switch.Spin, Value);
            exception
               when Constraint_Error =>
                  --  Do not free that element in the list, it might be used
                  --  for other coalesce switches (for instance, -gnaty3
                  --  indicates an indentation level, whereas -gnatya checks
                  --  casing).
                  null;
            end;
         end if;
      end loop;
   end Set_And_Filter_Switch;

   ------------------
   -- Refresh_Page --
   ------------------

   procedure Refresh_Page (Page : access Gtk_Widget_Record'Class) is
      P    : constant Switches_Editor_Page := Switches_Editor_Page (Page);
      Found : Boolean;
   begin
      --  Don't do anything if the callbacks were blocked, to avoid infinite
      --  loops while we are updating the command line, and it is updating
      --  the buttons, that are updating the command line,...

      if P.Block_Refresh then
         return;
      end if;

      declare
         Current            : GNAT.Strings.String_List :=
                                Get_Switches (P, Normalize => True);
         Coalesce_Switches  : GNAT.Strings.String_List
           (P.Coalesce_Switches'Range) := (others => null);
         Tmp                : GNAT.Strings.String_Access;

      begin
         P.Block_Refresh := True;
         Set_Text (P.Cmd_Line, "");

         if P.Switches = null then
            Insert (P.Kernel,
                    "No switches defined for " & P.Title.all,
                    Mode => GPS.Kernel.Console.Error);
         else
            --  Find out all selected switches through the widgets

            for S in P.Switches'Range loop
               declare
                  Text : constant String := Get_Switch (P.Switches (S).all);
               begin
                  Found := False;
                  if Text /= "" then
                     --  For "coalesce switches", we cannot add them
                     --  immediately, since we have to coalesce them first.
                     for C in P.Coalesce_Switches'Range loop
                        if Text'Length >= P.Coalesce_Switches (C)'Length
                          and then P.Coalesce_Switches (C).all = Text
                          (Text'First .. P.Coalesce_Switches (C)'Length - 1
                           + Text'First)
                        then
                           Tmp := Coalesce_Switches (C);

                           if Coalesce_Switches (C) = null then
                              Coalesce_Switches (C) := new String'(Text);
                           else
                              Coalesce_Switches (C) := new String'
                                (Coalesce_Switches (C).all
                                 & Text (P.Coalesce_Switches (C)'Length
                                         + Text'First .. Text'Last));
                              Free (Tmp);
                           end if;

                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Append_Text (P.Cmd_Line, Text & ' ');
                     end if;
                  end if;

                  Filter_Switch (P.Switches (S).all, Current);
               end;
            end loop;
         end if;

         --  Remove from current all switches that have been coalesced
         for Cur in Current'Range loop
            if Current (Cur) /= null then
               for C in P.Coalesce_Switches'Range loop
                  if Current (Cur)'Length >=
                    P.Coalesce_Switches (C)'Length
                    and then P.Coalesce_Switches (C).all = Current (Cur)
                    (Current (Cur)'First ..
                      Current (Cur)'First + P.Coalesce_Switches (C)'Length - 1)
                  then
                     --  Check if we have a check button with this switch...
                     --  If not, leave the switch as is on the command line.
                     --  This is the case for instance if the user puts
                     --  -gnatwY on the command line, but we do not have a
                     --  check button for it.
                     for S in P.Switches'Range loop
                        if P.Switches (S).Switch = Current (Cur).all then
                           Free (Current (Cur));
                           exit;
                        end if;
                     end loop;
                     exit;
                  end if;
               end loop;
            end if;
         end loop;

         --  Remove the old instances of common switch from the command line,
         --  and add the new ones

         for C in Coalesce_Switches'Range loop
            --  Add the coalesced switches to the command line. For instance,
            --  this will add "-gnatwuv" instead of "-gnatwu -gnatwv".
            --  However, if we have defined an alias for that new switch, we
            --  use that alias directly.

            Found := False;
            if Coalesce_Switches (C) /= null then
               for Alias in P.Coalesce_Switches'Range loop
                  if P.Coalesce_Switches_Default (Alias).all =
                    Coalesce_Switches (C).all
                  then
                     Append_Text
                       (P.Cmd_Line, P.Coalesce_Switches (Alias).all & ' ');
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Append_Text
                    (P.Cmd_Line, Coalesce_Switches (C).all & ' ');
               end if;
            end if;
         end loop;

         --  Now add all the switches that do not have an associated widget

         for K in Current'Range loop
            if Current (K) /= null then
               Append_Text (P.Cmd_Line, Current (K).all & " ");
            end if;
         end loop;

         P.Block_Refresh := False;

         Free (Current);
      end;
   end Refresh_Page;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Page      : access Switches_Editor_Page_Record'Class;
      Normalize : Boolean) return GNAT.Strings.String_List
   is
      Str : constant String := Get_Text (Page.Cmd_Line);
      Null_Argument_List : GNAT.Strings.String_List (1 .. 0);
      List               : GNAT.Strings.String_List_Access;
      First_Non_Blank    : Natural := Str'First;
   begin
      Skip_Blanks (Str, First_Non_Blank);
      if First_Non_Blank <= Str'Last then
         List :=
           GNAT.OS_Lib.Argument_String_To_List
             (Str (First_Non_Blank .. Str'Last));

         if Normalize then
            declare
               Ret : constant GNAT.Strings.String_List :=
                 Normalize_Compiler_Switches (Page, List.all);
            begin
               Unchecked_Free (List);
               return Ret;
            end;
         else
            declare
               Ret : constant GNAT.Strings.String_List := List.all;
            begin
               Unchecked_Free (List);
               return Ret;
            end;
         end if;
      end if;
      return Null_Argument_List;
   end Get_Switches;

   ---------------------------------
   -- Normalize_Compiler_Switches --
   ---------------------------------

   function Normalize_Compiler_Switches
     (Page     : access Switches_Editor_Page_Record'Class;
      Switches : GNAT.Strings.String_List) return GNAT.Strings.String_List
   is
      Output : GNAT.Strings.String_List_Access;
      Found  : Boolean;
      S      : GNAT.Strings.String_Access;

      procedure Expand_Or_Append (Switch : String);
      --  Add either switch or its expansion to Output

      procedure Expand_Or_Append (Switch : String) is
         Exp   : GNAT.Strings.String_List_Access;
         Found : Boolean := False;
      begin
         for C in Page.Expansion_Switches'Range loop
            Exp := Page.Expansion_Switches (C);
            if Switch = Exp (Exp'First).all then
               Append (Output, Clone (Exp (Exp'First + 1 .. Exp'Last)));
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            Append (Output, (1 => new String'(Switch)));
         end if;
      end Expand_Or_Append;

   begin
      for Index in Switches'Range loop
         Found := False;

         --  For Ada switches, use the functions provided by GNAT that provide
         --  the splitting of composite switches like "-gnatwue" into
         --  "-gnatwu -gnatwe"

         if Filter_Matches (Page, Ada_String) then
            declare
               Arr : constant GNAT.Strings.String_List :=
                 Normalize_Compiler_Switches (Switches (Index).all);
               --  Do not free Arr, this refers to internal strings in GNAT!
            begin
               --  If the switch wasn't already as simple as possible, or
               --  wasn't recognized at all.
               if Arr'Length > 1 then
                  for A in Arr'Range loop
                     Expand_Or_Append (Arr (A).all);
                  end loop;
                  Found := True;
               end if;
            end;
         end if;

         --  Check expansion switches with no parameter, if any
         if not Found then
            Expand_Or_Append (Switches (Index).all);
         end if;

         S := Switches (Index);
         Free (S);
      end loop;

      if Output = null then
         return (1 .. 0 => null);

      else
         declare
            O : constant GNAT.Strings.String_List := Output.all;
         begin
            Unchecked_Free (Output);
            return O;
         end;
      end if;
   end Normalize_Compiler_Switches;

   -------------------------
   -- On_Cmd_Line_Changed --
   -------------------------

   procedure On_Cmd_Line_Changed (Page : access Gtk_Widget_Record'Class) is
      P : constant Switches_Editor_Page := Switches_Editor_Page (Page);
   begin
      if P.Block_Refresh then
         return;
      end if;

      if P.Switches /= null then
         declare
            Arg : GNAT.Strings.String_List :=
                    Get_Switches (P, Normalize => True);
         begin
            P.Block_Refresh := True;

            for S in P.Switches'Range loop
               Set_And_Filter_Switch (P.Switches (S).all, Arg);
            end loop;

            Free (Arg);
            P.Block_Refresh := False;
         end;
      end if;
   end On_Cmd_Line_Changed;

   -------------------
   -- Append_Switch --
   -------------------

   procedure Append_Switch
     (Page   : access Switches_Editor_Page_Record'Class;
      Button : access Switch_Basic_Widget_Record'Class)
   is
      S : Widget_Array_Access := Page.Switches;
   begin
      if S = null then
         Page.Switches := new Widget_Array (1 .. 1);
      else
         Page.Switches := new Widget_Array (1 .. S'Length + 1);
         Page.Switches (S'Range) := S.all;
         Unchecked_Free (S);
      end if;

      Page.Switches (Page.Switches'Last) := Switch_Basic_Widget (Button);
   end Append_Switch;

   -----------------
   -- Create_Spin --
   -----------------

   procedure Create_Spin
     (Page              : access Switches_Editor_Page_Record;
      Box               : access Gtk.Box.Gtk_Box_Record'Class;
      Label             : String;
      Switch            : String;
      Min, Max, Default : Integer;
      Tip               : String := "";
      Label_Size_Group  : Gtk.Size_Group.Gtk_Size_Group := null;
      Separator         : String := "")
   is
      Hbox  : Gtk_Box;
      Adj   : Gtk_Adjustment;
      S     : constant Switch_Spin_Widget_Access := new Switch_Spin_Widget
        (Separator'Length, Switch'Length);
      L     : Gtk_Label;

   begin
      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Box, Hbox, False, False);
      S.Separator := Separator;
      S.Switch    := Switch;
      S.Default   := Default;

      Gtk_New (L, Label);
      Set_Alignment (L, 0.0, 0.5);
      Pack_Start (Hbox, L, False, False, 0);

      if Label_Size_Group /= null then
         Add_Widget (Label_Size_Group, L);
      end if;

      Gtk_New (Adj, Gdouble (Default), Gdouble (Min), Gdouble (Max),
               1.0, 10.0, 10.0);
      Gtk_New (S.Spin, Adj, 1.0, 0);
      Pack_Start (Hbox, S.Spin, True, True, 0);
      Widget_Callback.Object_Connect
        (S.Spin, Signal_Changed, Refresh_Page'Access, Page);

      if Tip /= "" then
         Set_Tip (Page.Tips, S.Spin, '(' & Switch & ") " & ASCII.LF & Tip);
      else
         Set_Tip (Page.Tips, S.Spin, '(' & Switch & ')');
      end if;

      Append_Switch (Page, S);
   end Create_Spin;

   ----------------------
   -- Browse_Directory --
   ----------------------

   procedure Browse_Directory
     (Field : access GObject_Record'Class)
   is
      F   : constant Gtk_Entry := Gtk_Entry (Field);
      Dir : constant Virtual_File := Select_Directory
        (Base_Directory    => Create (Get_Text (F)),
         Parent            => Gtk_Window (Get_Toplevel (F)),
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs));
   begin
      if Dir /= VFS.No_File then
         Set_Text (F, Full_Name (Dir).all);
      end if;
   end Browse_Directory;

   -----------------
   -- Browse_File --
   -----------------

   procedure Browse_File
     (Field : access GObject_Record'Class)
   is
      F    : constant Gtk_Entry := Gtk_Entry (Field);
      VF   : constant Virtual_File := Create (Get_Text (F));
      File : constant Virtual_File := Select_File
        (Base_Directory    => Dir (VF),
         Default_Name      => Base_Name (VF),
         Parent            => Gtk_Window (Get_Toplevel (F)),
         Kind              => Open_File,
         File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
         Pattern_Name      => -"All files;Ada files;C/C++ files",
         Use_Native_Dialog => Get_Pref (Use_Native_Dialogs));
   begin
      if File /= VFS.No_File then
         Set_Text (F, Full_Name (File).all);
      end if;
   end Browse_File;

   ------------------
   -- Create_Field --
   ------------------

   procedure Create_Field
     (Page             : access Switches_Editor_Page_Record;
      Box              : access Gtk.Box.Gtk_Box_Record'Class;
      Label            : String;
      Switch           : String;
      Tip              : String := "";
      As_Directory     : Boolean := False;
      As_File          : Boolean := False;
      Label_Size_Group : Gtk.Size_Group.Gtk_Size_Group := null;
      Separator        : String := " ")
   is
      Hbox   : Gtk_Box;
      S      : constant Switch_Field_Widget_Access := new Switch_Field_Widget
        (Separator'Length, Switch'Length);
      L      : Gtk_Label;
      Button : Gtk_Button;

   begin
      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Box, Hbox, False, False);
      S.Separator := Separator;
      S.Switch    := Switch;

      Gtk_New (L, Label);
      Set_Alignment (L, 0.0, 0.5);
      Pack_Start (Hbox, L, False, False, 0);

      if Label_Size_Group /= null then
         Add_Widget (Label_Size_Group, L);
      end if;

      Gtk_New (S.Field);
      Pack_Start (Hbox, S.Field, True, True, 0);
      Widget_Callback.Object_Connect
        (S.Field, Signal_Changed, Refresh_Page'Access, Page);

      if As_File then
         Gtk_New (Button, -"Browse");
         Pack_Start (Hbox, Button, Expand => False);
         Object_Callback.Object_Connect
           (Button, Signal_Clicked, Browse_File'Access,
            Slot_Object => S.Field);

      elsif As_Directory then
         Gtk_New (Button, -"Browse");
         Pack_Start (Hbox, Button, Expand => False);
         Object_Callback.Object_Connect
           (Button, Signal_Clicked,
            Browse_Directory'Access,
            Slot_Object => S.Field);
      end if;

      if Tip /= "" then
         Set_Tip (Page.Tips, S.Field, '(' & Switch & ") " & ASCII.LF & Tip);
      else
         Set_Tip (Page.Tips, S.Field, '(' & Switch & ')');
      end if;

      Append_Switch (Page, S);
   end Create_Field;

   ------------------
   -- Create_Radio --
   ------------------

   procedure Create_Radio
     (Page    : access Switches_Editor_Page_Record;
      Box     : access Gtk.Box.Gtk_Box_Record'Class;
      Buttons : Radio_Switch_Array)
   is
      S, Previous : Switch_Check_Widget_Access;
      Last        : Gtk_Radio_Button;
   begin
      for B in Buttons'Range loop
         S := new Switch_Check_Widget (Buttons (B).Switch'Length);

         if Previous /= null then
            Previous.Next_In_Group := S;
         end if;
         Previous := S;

         Gtk_New (Last, Group => Last, Label => -Buttons (B).Label.all);
         S.Check  := Gtk_Check_Button (Last);
         S.Switch := Buttons (B).Switch.all;
         Pack_Start (Box, Last, False, False);
         Widget_Callback.Object_Connect
           (Last, Gtk.Toggle_Button.Signal_Toggled, Refresh_Page'Access, Page);

         if Buttons (B).Tip /= null then
            Set_Tip (Page.Tips, Last,
                     '(' & Buttons (B).Switch.all & ") " & ASCII.LF
                     & Buttons (B).Tip.all);
         else
            Set_Tip (Page.Tips, Last, '(' & Buttons (B).Switch.all & ')');
         end if;

         Append_Switch (Page, S);
      end loop;
   end Create_Radio;

   ------------------
   -- Create_Check --
   ------------------

   procedure Create_Check
     (Page   : access Switches_Editor_Page_Record;
      Box    : access Gtk.Box.Gtk_Box_Record'Class;
      Label  : String;
      Switch : String;
      Tip    : String := "")
   is
      S : constant Switch_Check_Widget_Access := new Switch_Check_Widget
        (Switch'Length);
   begin
      Gtk_New (S.Check, Label);
      S.Switch := Switch;
      Pack_Start (Box, S.Check, False, False);
      Set_Active (S.Check, False);
      Widget_Callback.Object_Connect
        (S.Check, Signal_Toggled, Refresh_Page'Access, Page);

      if Tip /= "" then
         Set_Tip (Page.Tips, S.Check, '(' & Switch & ") " & ASCII.LF & Tip);
      else
         Set_Tip (Page.Tips, S.Check, '(' & Switch & ')');
      end if;

      Append_Switch (Page, S);
   end Create_Check;

   ------------------
   -- Create_Combo --
   ------------------

   function Create_Combo
     (Page                 : access Switches_Editor_Page_Record;
      Label                : String;
      Switch               : String;
      Default_No_Switch    : String;
      Default_No_Digit     : String;
      Buttons              : Combo_Switch_Array;
      Tip                  : String := "";
      Label_Size_Group     : Gtk.Size_Group.Gtk_Size_Group := null;
      Separator            : String := "")
     return Gtk.Widget.Gtk_Widget
   is
      L     : Gtk_Label;
      S     : constant Switch_Combo_Widget_Access := new Switch_Combo_Widget
        (Sep_Length       => Separator'Length,
         Switch_Length    => Switch'Length,
         No_Digit_Length  => Default_No_Digit'Length,
         No_Switch_Length => Default_No_Switch'Length);
      Hbox  : Gtk_Box;
      Item  : Combo_List_Item;
   begin
      Gtk_New_Hbox (Hbox, Homogeneous => False);

      if Label /= "" then
         Gtk_New (L, Label);
         Pack_Start (Hbox, L, Expand => False);
         Set_Alignment (L, 0.0, 0.5);

         if Label_Size_Group /= null then
            Add_Widget (Label_Size_Group, L);
         end if;
      end if;

      Gtk_New (S.Combo);

      Pack_Start (Hbox, S.Combo, Expand => True, Fill => True);
      S.Separator         := Separator;
      S.Default_No_Switch := Default_No_Switch;
      S.Default_No_Digit  := Default_No_Digit;

      for B in Buttons'Range loop
         Item := new Combo_List_Item_Record (Buttons (B).Value'Length);
         Gtk.List_Item.Initialize (Item, -Buttons (B).Label.all);
         Item.Value := Buttons (B).Value.all;
         Show (Item);
         Add (Get_List (S.Combo), Item);
      end loop;

      S.Switch := Switch;
      Append_Switch (Page, S);

      Widget_Callback.Object_Connect
        (Get_Entry (S.Combo), Signal_Changed, Refresh_Page'Access, Page);

      if Tip /= "" then
         Set_Tip (Page.Tips, S.Combo, '(' & Switch & ") " & ASCII.LF & Tip);
      else
         Set_Tip (Page.Tips, S.Combo, '(' & Switch & ')');
      end if;

      return Gtk_Widget (Hbox);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Gtk_Widget (Hbox);
   end Create_Combo;

   --------------------
   -- Popup_New_Page --
   --------------------

   procedure Popup_New_Page (Button : access Gtk_Widget_Record'Class) is
      B : constant Switch_Popup_Widget_Access :=
        Switch_Popup_Widget_Access (Button);
      Tmp : Gtk_Widget;
      Response : Gtk_Response_Type;
      pragma Unreferenced (Tmp, Response);
   begin
      Gtk_New (Dialog => B.Popup,
               Title  => B.Label,
               Parent => Gtk_Window (Get_Toplevel (B)),
               Flags  => Modal);
      Tmp := Add_Button (B.Popup, Stock_Ok, Gtk_Response_OK);
      Pack_Start (Get_Vbox (B.Popup), B.Widget);
      Show_All (B.Popup);

      Response := Run (B.Popup);

      Remove (Get_Vbox (B.Popup), B.Widget);
      Destroy (B.Popup);
      B.Popup := null;
   end Popup_New_Page;

   -------------------
   -- Destroy_Popup --
   -------------------

   procedure Destroy_Popup (Button : access Gtk_Widget_Record'Class) is
      B : constant Switch_Popup_Widget_Access :=
        Switch_Popup_Widget_Access (Button);
   begin
      Unref (B.Widget);
   end Destroy_Popup;

   ------------------
   -- Create_Popup --
   ------------------

   function Create_Popup
     (Label  : String;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Hbox : Gtk_Box;
      L    : Gtk_Label;
      B    : Switch_Popup_Widget_Access;
   begin
      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Gtk_New (L, Label & ": ");
      Set_Alignment (L, 0.0, 0.5);
      Pack_Start (Hbox, L, Expand => True, Fill => True);

      Gtk_New (L, "...");
      --  ??? Should indicate the number of selected switches in the popup
      Set_Alignment (L, 1.0, 0.5);
      Pack_End (Hbox, L, Expand => True, Fill => True);

      B        := new Switch_Popup_Widget (Label'Length);
      Gtk.Button.Initialize (B, "");
      Add (B, Hbox);
      B.Widget := Gtk_Widget (Widget);
      Ref (Widget);
      B.Label := Label;

      Widget_Callback.Connect (B, Signal_Clicked, Popup_New_Page'Access);
      Widget_Callback.Connect (B, Signal_Destroy, Destroy_Popup'Access);

      return Gtk_Widget (B);
   end Create_Popup;

   ----------------------------
   -- Check_Field_Dependency --
   ----------------------------

   procedure Check_Field_Dependency
     (Field : access Gtk_Widget_Record'Class;
      Data  : Dependency_Data)
   is
      Has_Text : constant Boolean := Get_Text (Gtk_Entry (Field)) /= "";
   begin
      if (Has_Text and then Data.Master_Status)
        or else (not Has_Text and then not Data.Master_Status)
      then
         Set_Sensitive (Data.Slave_Switch.Check, False);
         Set_Active (Data.Slave_Switch.Check, Data.Slave_Activate);
      else
         Set_Sensitive (Data.Slave_Switch.Check, True);
         Set_Active (Data.Slave_Switch.Check, not Data.Slave_Activate);
      end if;
   end Check_Field_Dependency;

   ----------------------
   -- Check_Dependency --
   ----------------------

   procedure Check_Dependency
     (Check : access Gtk_Widget_Record'Class;
      Data  : Dependency_Data) is
   begin
      if Get_Active (Gtk_Check_Button (Check)) = Data.Master_Status then
         Set_Sensitive (Data.Slave_Switch.Check, False);
         Set_Active (Data.Slave_Switch.Check, Data.Slave_Activate);

      else
         Set_Sensitive (Data.Slave_Switch.Check, True);
         Set_Active (Data.Slave_Switch.Check, not Data.Slave_Activate);
      end if;
   end Check_Dependency;

   --------------
   -- Get_Page --
   --------------

   function Get_Page
     (Editor : access Switches_Edit_Record'Class;
      Title  : String) return Switches_Editor_Page is
   begin
      if Editor.Pages /= null then
         for Num in Editor.Pages'Range loop
            if Editor.Pages (Num) /= null
              and then Editor.Pages (Num).Title.all = Title
            then
               return Editor.Pages (Num);
            end if;
         end loop;
      end if;

      return null;
   end Get_Page;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (Page           : access Switches_Editor_Page_Record;
      Master_Page    : String;
      Master_Switch  : String;
      Master_Status  : Boolean;
      Slave_Page     : String;
      Slave_Switch   : String;
      Slave_Activate : Boolean := True) is
   begin
      Page.Dependencies := new Dependency_Description'
        (Next          => Page.Dependencies,
         Master_Page   => new String'(Master_Page),
         Master_Switch => new String'(Master_Switch),
         Master_Status => Master_Status,
         Slave_Page    => new String'(Slave_Page),
         Slave_Switch  => new String'(Slave_Switch),
         Slave_Status  => Slave_Activate);
   end Add_Dependency;

   ---------------------------
   -- Activate_Dependencies --
   ---------------------------

   procedure Activate_Dependencies
     (Page   : access Switches_Editor_Page_Record'Class;
      Editor : access Switches_Edit_Record'Class)
   is
      Master_Page, Slave_Page : Switches_Editor_Page;
      S1, S2  : Switch_Basic_Widget;
      Dep : Dependency_Description_Access := Page.Dependencies;
   begin
      while Dep /= null loop
         Master_Page := Get_Page (Editor, Dep.Master_Page.all);
         Slave_Page  := Get_Page (Editor, Dep.Slave_Page.all);

         if Master_Page /= null and then Slave_Page /= null then
            S1 := Get_Switch_Widget (Master_Page, Dep.Master_Switch.all);
            S2 := Get_Switch_Widget (Slave_Page, Dep.Slave_Switch.all);
            if S1 = null
              or else S2 = null
              or else (S1.all not in Switch_Check_Widget'Class
                       and then S1.all not in Switch_Field_Widget'Class)
              or else S2.all not in Switch_Check_Widget'Class
            then
               Insert
                 (Page.Kernel,
                  "Can only add dependencies between check button switches "
                  & Master_Page.Title.all & ' ' & Dep.Master_Switch.all
                  & ' ' & Slave_Page.Title.all & ' ' & Dep.Slave_Switch.all,
                  Mode => GPS.Kernel.Console.Error);
            else
               if S1.all in Switch_Check_Widget'Class then
                  Dependency_Callback.Connect
                    (Switch_Check_Widget_Access (S1).Check, Signal_Toggled,
                     Check_Dependency'Access,
                     (Dep.Master_Status,
                      Switch_Check_Widget_Access (S2),
                      Dep.Slave_Status));
                  Check_Dependency
                    (Switch_Check_Widget_Access (S1).Check,
                     (Dep.Master_Status,
                      Switch_Check_Widget_Access (S2),
                      Dep.Slave_Status));
               else
                  Dependency_Callback.Connect
                    (Switch_Field_Widget_Access (S1).Field, Signal_Changed,
                     Check_Field_Dependency'Access,
                     (Dep.Master_Status,
                      Switch_Check_Widget_Access (S2),
                      Dep.Slave_Status));
                  Check_Field_Dependency
                    (Switch_Field_Widget_Access (S1).Field,
                     (Dep.Master_Status,
                      Switch_Check_Widget_Access (S2),
                      Dep.Slave_Status));
               end if;
            end if;
         end if;

         Dep := Dep.Next;
      end loop;
   end Activate_Dependencies;

   -------------------------
   -- Add_Coalesce_Switch --
   -------------------------

   procedure Add_Coalesce_Switch
     (Page              : access Switches_Editor_Page_Record'Class;
      Switch            : String;
      Default_As_String : String := "") is
   begin
      Append (Page.Coalesce_Switches, (1 => new String'(Switch)));
      Append (Page.Coalesce_Switches_Default,
              (1 => new String'(Default_As_String)));
   end Add_Coalesce_Switch;

   --------------------------
   -- Add_Custom_Expansion --
   --------------------------

   procedure Add_Custom_Expansion
     (Page : access Switches_Editor_Page_Record'Class;
      Switch  : String;
      Default : Cst_Argument_List)
   is
      Tmp : String_List_Array_Access := Page.Expansion_Switches;
   begin
      Page.Expansion_Switches :=  new String_List_Array (1 .. Tmp'Length + 1);
      Page.Expansion_Switches (Tmp'Range) := Tmp.all;
      Unchecked_Free (Tmp);
      Page.Expansion_Switches (Page.Expansion_Switches'Last) :=
        new GNAT.Strings.String_List (Default'First .. Default'Last + 1);

      --  Duplicate the strings, which are freed in Page_Destroyed.
      Page.Expansion_Switches (Page.Expansion_Switches'Last)(Default'First) :=
        new String'(Switch);
      for D in Default'Range loop
         Page.Expansion_Switches (Page.Expansion_Switches'Last)(D + 1) :=
           new String'(Default (D).all);
      end loop;
   end Add_Custom_Expansion;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Page            : out Switches_Editor_Page;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title           : String;
      Project_Package : String;
      Attribute_Index : String := "";
      Lines, Cols     : Glib.Guint;
      Tips            : access Gtk.Tooltips.Gtk_Tooltips_Record'Class) is
   begin
      Page := new Switches_Editor_Page_Record;
      Page.Kernel := Kernel_Handle (Kernel);
      Gtk.Table.Initialize (Page, Lines + 1, Cols, False);
      Set_Row_Spacings (Page, 0);
      Set_Col_Spacings (Page, 0);

      Page.Lang_Filter := null;

      Page.Attribute_Index := new String'(Attribute_Index);
      Page.Title := new String'(Title);
      Page.Pkg   := new String'(To_Lower (Project_Package));
      Page.Coalesce_Switches  := new GNAT.Strings.String_List (1 .. 0);
      Page.Coalesce_Switches_Default := new GNAT.Strings.String_List (1 .. 0);
      Page.Expansion_Switches := new String_List_Array (1 .. 0);
      Page.Tips               := Gtk_Tooltips (Tips);

      Gtk_New (Page.Cmd_Line);
      Set_Editable (Page.Cmd_Line, True);
      Widget_Callback.Object_Connect
        (Page.Cmd_Line, Signal_Changed, On_Cmd_Line_Changed'Access, Page);
      Attach (Page, Page.Cmd_Line, 0, Cols, Lines, Lines + 1,
              Expand or Fill, 0, 5, 0);
   end Gtk_New;

   ------------------
   -- Add_Language --
   ------------------

   procedure Add_Language
     (Page : access Switches_Editor_Page_Record;
      Language_Filter : String) is
   begin
      Append (Page.Lang_Filter, (1 => new String'(Language_Filter)));
      To_Lower (Page.Lang_Filter (Page.Lang_Filter'Last).all);
   end Add_Language;

   --------------------
   -- Filter_Matches --
   --------------------

   function Filter_Matches
     (Page : access Switches_Editor_Page_Record'Class;
      Language : String) return Boolean
   is
   begin
      if Page.Lang_Filter = null then
         return True;
      end if;

      for F in Page.Lang_Filter'Range loop
         if Language = Page.Lang_Filter (F).all then
            return True;
         end if;
      end loop;

      return False;
   end Filter_Matches;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Switches_Edit;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tab    : Gtk_Label;

   begin
      Editor := new Switches_Edit_Record;
      Gtk.Notebook.Initialize (Editor);
      Set_Scrollable (Editor);

      Editor.Kernel := Kernel_Handle (Kernel);
      Editor.Pages := new Pages_Array (1 .. Switches_Page_Count (Kernel));

      for P in Editor.Pages'Range loop
         Editor.Pages (P) := Get_Nth_Switches_Page (Kernel, P);

         --  ??? Since the kernel will always return the same page, is this
         --  really necessary ? Especially since we might end up connecting to
         --  it several times in fact.
         Widget_Callback.Connect
           (Editor.Pages (P), Signal_Destroy, Page_Destroyed'Access);

         Gtk_New (Tab, Editor.Pages (P).Title.all);
         Append_Page (Editor, Editor.Pages (P), Tab);
      end loop;

      --  Then once all the pages have been created, setup the dependencies
      for P in Editor.Pages'Range loop
         Activate_Dependencies (Editor.Pages (P), Editor);
      end loop;

      Set_Current_Page (Editor, 0);

      Widget_Callback.Connect
        (Editor, Signal_Destroy, Editor_Destroyed'Access);
   end Gtk_New;

   --------------------
   -- Page_Destroyed --
   --------------------

   procedure Page_Destroyed (Page : access Gtk_Widget_Record'Class) is
      P : constant Switches_Editor_Page := Switches_Editor_Page (Page);
   begin
      Free (P.Lang_Filter);
      Free (P.Attribute_Index);
      Free (P.Title);
      Free (P.Pkg);
      Free (P.Coalesce_Switches);
      Free (P.Coalesce_Switches_Default);

      for C in P.Expansion_Switches'Range loop
         Free (P.Expansion_Switches (C));
      end loop;

      Unchecked_Free (P.Expansion_Switches);
      Unchecked_Free (P.Switches);
   end Page_Destroyed;

   ----------------------
   -- Editor_Destroyed --
   ----------------------

   procedure Editor_Destroyed (Editor : access Gtk_Widget_Record'Class) is
      Edit : constant Switches_Edit := Switches_Edit (Editor);
   begin
      --  When a Notebook is destroyed, it destroys its children whatever their
      --  ref counting is. So we need to remove the children first before
      --  destroying the Editor, who inherits its parent's behavior.
      for P in Edit.Pages'Range loop
         Remove (Edit, Edit.Pages (P));
      end loop;

      Unchecked_Free (Edit.Pages);
   end Editor_Destroyed;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record;
      Languages : GNAT.Strings.String_List) is
   begin
      Set_Visible_Pages
        (Editor, Languages, Show_Only => False, File_Specific => False);
   end Set_Visible_Pages;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record'Class;
      Languages : GNAT.Strings.String_List;
      Show_Only : Boolean;
      File_Specific : Boolean)
   is
      Current : Gint := Get_Current_Page (Editor);
   begin
      for P in Editor.Pages'Range loop
         if (Editor.Pages (P).Lang_Filter = null
             or else Has_Supported_Language (Editor.Pages (P), Languages))
           and then (not File_Specific
                     or else Editor.Pages (P).Pkg.all /= Ide_Package)
         then
            Show (Editor.Pages (P));
         elsif not Show_Only then
            Hide (Editor.Pages (P));
         end if;
      end loop;

      --  Work around an apparent bug in gtk+: when the contents of a page is
      --  hidden, and the shown again, it is always displayed on top of the
      --  current page in the notebook. We thus see the contents of two or more
      --  pages at the same time...
      if Current = -1 then
         Current := 0;
      end if;
      Set_Current_Page (Editor, Current);
   end Set_Visible_Pages;

   -----------------------
   -- Revert_To_Default --
   -----------------------

   procedure Revert_To_Default
     (Switches : access Gtk_Widget_Record'Class)
   is
      S : constant Switches_Edit := Switches_Edit (Switches);
   begin
      for P in S.Pages'Range loop
         declare
            List : GNAT.Strings.String_List := Get_Switches
              (S,
               S.Pages (P).Pkg.all,
               S.Pages (P).Attribute_Index.all,
               Files => (1 .. 0 => VFS.No_File),
               Use_Initial_Value => True);
         begin
            Set_Text (S.Pages (P).Cmd_Line,
                      Argument_List_To_String (List));
            Free (List);
         end;
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Revert_To_Default;

   ----------------------------
   -- Has_Supported_Language --
   ----------------------------

   function Has_Supported_Language
     (Page                : access Switches_Editor_Page_Record'Class;
      Supported_Languages : GNAT.Strings.String_List) return Boolean is
   begin
      if Page.Lang_Filter /= null then
         for L in Page.Lang_Filter'Range loop
            for PL in Supported_Languages'Range loop
               if To_Lower (Page.Lang_Filter (L).all) =
                 To_Lower (Supported_Languages (PL).all)
               then
                  return True;
               end if;
            end loop;
         end loop;
      else
         --  No filter => always save those switches
         return True;
      end if;
      return False;
   end Has_Supported_Language;

   ----------------------
   -- Generate_Project --
   ----------------------

   function Generate_Project
     (Switches           : access Switches_Edit_Record'Class;
      Project            : Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array;
      Files              : File_Array) return Boolean
   is
      Changed : Boolean := False;

      procedure Change_Switches
        (Page      : access Switches_Editor_Page_Record'Class;
         File_Name : Virtual_File);
      --  Changes the switches for a specific package and tool.

      procedure Process_File (File_Name : Virtual_File);
      --  Generate the switches for a specific file (or the default switches if
      --  File_Name is the empty string). Return True if the project was
      --  changed.

      ---------------------
      -- Change_Switches --
      ---------------------

      procedure Change_Switches
        (Page      : access Switches_Editor_Page_Record'Class;
         File_Name : Virtual_File)
      is
         Language : constant Name_Id := Get_String (Page.Attribute_Index.all);
         Value    : Prj.Variable_Value;
         Is_Default_Value : Boolean;
         Rename_Prj : Project_Type;
         To_Remove : Boolean := False;
      begin
         Rename_Prj := Find_Project_Of_Package (Project, Page.Pkg.all);

         --  Language not supported => Ignore the attribute.
         --  We shouldn't remove it, since it might have been added by another
         --  page for a different language (Compiler'Switches is modified by
         --  several pages, for instance).

         if not Has_Supported_Language (Page, Languages) then
            return;
         end if;

         --  Check if we in fact have the initial value. We much check the
         --  normalized switches (after expansion), since otherwise -gnatwue
         --  and -gnatweu would appear to be different. Note that this test
         --  doesn't handle the case where "-O" and "-O1" are the same.

         declare
            Args : GNAT.Strings.String_List := Normalize_Compiler_Switches
              (Page, Get_Switches (Page, Normalize => False));
         begin
            if Project = No_Project then
               Is_Default_Value := False;

            else
               --  If the switches are exactly the ones set by default for the
               --  language, remove the file-specific attribute

               Get_Switches
                 (Project          => Rename_Prj,
                  In_Pkg           => Page.Pkg.all,
                  File             => VFS.No_File,
                  Language         => Language,
                  Value            => Value,
                  Is_Default_Value => Is_Default_Value);
               declare
                  Default_Args : GNAT.Strings.String_List :=
                                   Normalize_Compiler_Switches
                                     (Page,
                                      To_Argument_List
                                        (Get_Tree (Project), Value));
               begin
                  Is_Default_Value := Is_Equal (Default_Args, Args);

                  if not Is_Default_Value and then Active (Me) then
                     Trace (Me, "Switches are not the default value");
                  end if;

                  Free (Default_Args);
               end;

               if Is_Default_Value then
                  To_Remove := File_Name /= VFS.No_File;

               else
                  --  If the switches are the ones already set for the file,
                  --  no change has been done in the dialog

                  Get_Switches
                    (Project          => Rename_Prj,
                     In_Pkg           => Page.Pkg.all,
                     File             => File_Name,
                     Language         => Language,
                     Value            => Value,
                     Is_Default_Value => Is_Default_Value);
                  declare
                     Default_Args : GNAT.Strings.String_List :=
                       Normalize_Compiler_Switches
                         (Page, To_Argument_List (Get_Tree (Project), Value));
                  begin
                     Is_Default_Value := Is_Equal (Default_Args, Args);

                     if not Is_Default_Value and then Active (Me) then
                        Trace (Me, "Switches changed by user");
                     end if;

                     Free (Default_Args);
                  end;
               end if;
            end if;

            if To_Remove then
               if File_Name /= VFS.No_File then
                  Trace (Me, "Removing file-specific switches for "
                         & Base_Name (File_Name));
                  Delete_Attribute
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          => Build
                       (Page.Pkg.all, Get_String (Name_Switches)),
                     Attribute_Index    => Base_Name (File_Name));
                  Changed := True;
               end if;

            elsif not Is_Default_Value then
               if File_Name /= VFS.No_File then
                  if Args'Length /= 0 then
                     Trace (Me, "Changing switches for "
                            & Base_Name (File_Name));
                     Update_Attribute_Value_In_Scenario
                       (Project            => Rename_Prj,
                        Scenario_Variables => Scenario_Variables,
                        Attribute          =>
                          Build (Page.Pkg.all, Get_String (Name_Switches)),
                        Values             => Args,
                        Attribute_Index    => Base_Name (File_Name),
                        Prepend            => False);
                     Changed := True;
                  else
                     Trace (Me, "Removing switches for "
                            & Base_Name (File_Name));
                     Delete_Attribute
                       (Project            => Rename_Prj,
                        Scenario_Variables => Scenario_Variables,
                        Attribute          =>
                          Build (Page.Pkg.all, Get_String (Name_Switches)),
                        Attribute_Index    => Base_Name (File_Name));
                     Changed := True;
                  end if;

               elsif Args'Length /= 0 then
                  Trace (Me, "Changing default switches for "
                         & Page.Pkg.all & " " & Page.Attribute_Index.all);
                  Update_Attribute_Value_In_Scenario
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          =>
                      Build (Page.Pkg.all, Get_String (Name_Default_Switches)),
                     Values             => Args,
                     Attribute_Index    => Page.Attribute_Index.all,
                     Prepend            => False);
                  Changed := True;

               else
                  Trace (Me, "Removing default switches for "
                         & Page.Pkg.all & " " & Page.Attribute_Index.all);
                  Delete_Attribute
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          =>
                      Build (Page.Pkg.all, Get_String (Name_Default_Switches)),
                     Attribute_Index    => Page.Attribute_Index.all);
                  Changed := True;
               end if;
            end if;

            Free (Args);
         end;
      end Change_Switches;

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (File_Name : Virtual_File) is
      begin
         for P in Switches.Pages'Range loop
            Change_Switches (Switches.Pages (P), File_Name);
         end loop;
      end Process_File;

   begin
      pragma Assert (Project /= No_Project);

      if Files'Length = 0 then
         Process_File (VFS.No_File);
      else
         for F in Files'Range loop
            Process_File (Files (F));
         end loop;
      end if;

      return Changed;
   end Generate_Project;

   -------------------------
   -- Close_Switch_Editor --
   -------------------------

   function Close_Switch_Editor
     (Switches     : access Switches_Edit_Record'Class;
      Project      : Project_Type;
      Files        : VFS.File_Array;
      Scenario     : access Scenario_Selector_Record'Class) return Boolean
   is
      Saved     : GNAT.Strings.String_List := Get_Current_Scenario
        (Scenario_Variables (Switches.Kernel));
      Scenar    : Scenario_Iterator := Start (Scenario);
      Modified  : Boolean := False;
      Languages : GNAT.Strings.String_List := Get_Languages (Project);

   begin
      --  No scenario variables ?
      while not At_End (Scenar) loop
         declare
            Cur : GNAT.Strings.String_List := Current (Scenar);
         begin
            Set_Environment (Scenario_Variables (Switches.Kernel), Cur);
            Modified := Modified or Generate_Project
              (Switches           => Switches,
               Languages          => Languages,
               Scenario_Variables => Scenario_Variables (Switches.Kernel),
               Project            => Project,
               Files              => Files);
            Free (Cur);
         end;

         Next (Scenar);
      end loop;

      Free (Languages);

      Set_Environment (Scenario_Variables (Switches.Kernel), Saved);
      Free (Saved);

      --  ??? Need this to update the icon in the project explorer...
      if Modified then
         Recompute_View (Switches.Kernel);
      end if;

      return Modified;
   end Close_Switch_Editor;

   ------------------
   -- Set_Switches --
   ------------------

   procedure Set_Switches
     (Editor : access Switches_Edit_Record; Project : Project_Type) is
   begin
      Fill_Editor (Editor, Project, Files => (1 .. 0 => VFS.No_File));
   end Set_Switches;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Switches          : access Switches_Edit_Record'Class;
      Pkg_Name          : String;
      Language          : String;
      Files             : File_Array;
      Use_Initial_Value : Boolean := True) return GNAT.Strings.String_List is
   begin
      if Files'Length = 0 then
         return Get_Switches
           (Switches.Kernel,
            Switches.Project, Pkg_Name, VFS.No_File,
            Language, Use_Initial_Value => Use_Initial_Value);
      else
         --  ??? Should we merge all the switches ?
         return Get_Switches
           (Switches.Kernel,
            Switches.Project, Pkg_Name, Files (Files'First),
            Language, Use_Initial_Value => Use_Initial_Value);
      end if;
   end Get_Switches;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array) is
   begin
      Switches.Project := Project;

      --  Project might be null when we are in the project wizard. In this
      --  case, we fall back on switches for the default project.

      if Project /= No_Project then
         if Files'Length = 0 then
            declare
               Langs : GNAT.Strings.String_List := Get_Languages (Project);
            begin
               Set_Visible_Pages (Switches, Langs);
               Free (Langs);
            end;

         else
            for F in Files'Range loop
               declare
                  Lang : aliased String := Get_Language_From_File
                    (Get_Language_Handler (Switches.Kernel), Files (F));
               begin
                  To_Lower (Lang);

                  Set_Visible_Pages
                    (Editor    => Switches,
                     Languages => (1 => Lang'Unchecked_Access),
                     Show_Only => F /= Files'First,
                     File_Specific => True);
               end;
            end loop;
         end if;
      end if;

      --  Set the switches for all the pages

      for P in Switches.Pages'Range loop
         declare
            List : GNAT.Strings.String_List := Get_Switches
              (Switches,
               Switches.Pages (P).Pkg.all,
               Switches.Pages (P).Attribute_Index.all,
               Files,
               Use_Initial_Value => False);
         begin
            --  Force a space, so that at least some text is inserted, and thus
            --  we correctly initialize the widgets.

            Set_Text (Switches.Pages (P).Cmd_Line,
                      ' ' & Argument_List_To_String (List));
            Free (List);

            --  Now that the default widget values have been set, recompute
            --  the command line to find out the status of the switches that
            --  haven't been initialized yet, for instance a radio button with
            --  no corresponding command line.
            Refresh_Page (Switches.Pages (P));
         end;
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Fill_Editor;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_Switches_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Modified : Boolean;
      pragma Unreferenced (Command, Modified);
   begin
      Assert (Me, Has_Project_Information (Context.Context),
              "Project unknown when editing switches");

      if Has_File_Information (Context.Context) then
         Modified := Edit_Switches_For_Files
           (Get_Kernel (Context.Context),
            Project_Information (Context.Context),
            (1 => File_Information (Context.Context)));
      else
         Modified := Edit_Switches_For_Files
           (Get_Kernel (Context.Context),
            Project_Information (Context.Context),
            (1 .. 0 => VFS.No_File));
      end if;
      return Success;
   end Execute;

   -----------------------------
   -- Edit_Switches_For_Files --
   -----------------------------

   function Edit_Switches_For_Files
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Files        : File_Array) return Boolean
   is
      Switches  : Switches_Edit;
      Dialog    : Gtk_Dialog;
      Button, Button_OK    : Gtk_Widget;
      B         : Gtk_Button;
      Box       : Gtk_Box;
      Selector  : Scenario_Selector;
      Modified  : Boolean;
      pragma Unreferenced (Button_OK);

   begin
      if Files'Length > 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for multiple files",
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      elsif Files'Length = 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for specific file",
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      else
         Gtk_New (Dialog,
                  Title  => (-"Editing default switches for project ")
                    & Project_Name (Project),
                  Parent => Get_Current_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);
      end if;

      Gtk_New_Hbox (Box, Homogeneous => False);
      Pack_Start (Get_Vbox (Dialog), Box, Fill => True, Expand => True);

      Gtk_New (Switches, Kernel);
      Switches.Kernel := Kernel_Handle (Kernel);
      Pack_Start (Box, Switches, Fill => True, Expand => True);

      Gtk_New (Selector, Kernel);
      Pack_Start (Box, Selector, Expand => False);

      Show_All (Dialog);

      --  Unrestricted_Access is safe, since Switches is a local variable
      --  destroyed when the dialog is destroyed at the end of this procedure.

      Switches.Files := Files'Unrestricted_Access;

      Fill_Editor (Switches, Project, Files);

      Button_OK := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

      if Files'Length /= 0 then
         Gtk_New_From_Stock (B, Stock_Revert_To_Saved);
         Pack_Start (Get_Action_Area (Dialog), B);
         Widget_Callback.Object_Connect
           (B, Signal_Clicked, Revert_To_Default'Access,
            Slot_Object => Switches);
         Show_All (B);
      end if;

      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Show_All (Button);

      --  Note: if the dialog is no longer modal, then we need to create a copy
      --  of the context for storing in the callback, since the current context
      --  will be automatically freed by the kernel at some point in the life
      --  of this dialog.

      if Run (Dialog) = Gtk_Response_OK then
         Modified := Close_Switch_Editor
           (Switches, Project, Files, Selector);
      else
         Modified := False;
      end if;

      Destroy (Dialog);
      return Modified;
   end Edit_Switches_For_Files;

end Switches_Editors;
