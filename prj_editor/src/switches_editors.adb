-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Gtk.Adjustment;       use Gtk.Adjustment;
with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Check_Button;     use Gtk.Check_Button;
with Gtk.Combo;            use Gtk.Combo;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Handlers;         use Gtk.Handlers;
with Gtk.Label;            use Gtk.Label;
with Gtk.List;             use Gtk.List;
with Gtk.List_Item;        use Gtk.List_Item;
with Gtk.Notebook;         use Gtk.Notebook;
with Gtk.Radio_Button;     use Gtk.Radio_Button;
with Gtk.Size_Group;       use Gtk.Size_Group;
with Gtk.Spin_Button;      use Gtk.Spin_Button;
with Gtk.Stock;            use Gtk.Stock;
with Gtk.Tooltips;         use Gtk.Tooltips;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Window;           use Gtk.Window;
with Gtkada.Handlers;      use Gtkada.Handlers;

with GNAT.OS_Lib;          use GNAT.OS_Lib;
with GNAT.Case_Util;       use GNAT.Case_Util;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with Prj;
with Projects.Editor;      use Projects, Projects.Editor;
with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Intl;           use Glide_Intl;
with Language_Handlers;    use Language_Handlers;
with String_Utils;         use String_Utils;
with Basic_Types;          use Basic_Types;
with Scenario_Selectors;   use Scenario_Selectors;
with Projects;             use Projects;
with Project_Viewers;      use Project_Viewers;
with VFS;                  use VFS;

with Types;                use Types;
with Snames;               use Snames;
with Switch.M;             use Switch.M;

with Ada.Exceptions;       use Ada.Exceptions;
with Traces;               use Traces;

package body Switches_Editors is

   Me : constant Debug_Handle := Create ("Switches_Editors");

   -------------------
   -- Check buttons --
   -------------------

   type Switch_Check_Widget is new Switch_Basic_Widget_Record with record
      Check : Gtk.Check_Button.Gtk_Check_Button;
   end record;
   type Switch_Check_Widget_Access is access all Switch_Check_Widget'Class;

   function Get_Switch (Switch : Switch_Check_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Check_Widget; List : in out Argument_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Check_Widget; List : in out Argument_List);

   ------------------
   -- Spin buttons --
   ------------------

   type Switch_Spin_Widget is new Switch_Basic_Widget_Record with record
      Spin  : Gtk.Spin_Button.Gtk_Spin_Button;
      Default : Integer;
      --  Default value, for which no switch is needed on the command line
   end record;
   type Switch_Spin_Widget_Access is access all Switch_Spin_Widget'Class;

   function Get_Switch (Switch : Switch_Spin_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out Argument_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out Argument_List);

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

   type Switch_Combo_Widget (Switch_Length : Natural;
                             No_Digit_Length : Natural;
                             No_Switch_Length : Natural)
      is new Switch_Basic_Widget_Record (Switch_Length) with
   record
      Combo             : Gtk_Combo;
      Default_No_Digit  : String (1 .. No_Digit_Length);
      Default_No_Switch : String (1 .. No_Switch_Length);
   end record;
   type Switch_Combo_Widget_Access is access all Switch_Combo_Widget'Class;

   function Get_Switch (Switch : Switch_Combo_Widget) return String;
   procedure Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out Argument_List);
   procedure Set_And_Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out Argument_List);

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
   --  Callback to handle the dependencies between two items

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Widget_Array, Widget_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pages_Array, Page_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.OS_Lib.String_List, String_List_Access);
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

   procedure Revert_To_Default (Switches : access Gtk_Widget_Record'Class);
   --  Revert to the default switches in the editor

   function Normalize_Compiler_Switches
     (Page     : access Switches_Editor_Page_Record'Class;
      Switches : Argument_List) return Argument_List;
   --  Return an equivalent of Switches, but where concatenated switches have
   --  been separated (for instance, -gnatwue = -gnatwu -gnatwe).
   --  Nothing is done if the tool doesn't need this special treatment.
   --  The returned array should be freed. However, you no longer need to free
   --  the memory for the array that was passed as a parameter (we either
   --  return it directly, or reuse the strings from it for the output).

   procedure Refresh_Page (Page : access Gtk_Widget_Record'Class);
   --  Recompute the value of the command line after a switch has been changed
   --  through the GUI

   procedure On_Cmd_Line_Changed (Page : access Gtk_Widget_Record'Class);
   --  Reset the GUI content of the page, based on the current command line

   function Get_Switches
     (Page : access Switches_Editor_Page_Record'Class;
      Normalize : Boolean) return Argument_List;
   --  Return the list of parameters set on the command line.
   --  If normalize is True, then GNAT's switches will be split when possible
   --  ("gnatwue" => "gnatwu", "gnatwe")

   function Get_Switches
     (Switches : access Switches_Edit_Record'Class;
      Pkg_Name : String;
      Language : Name_Id;
      Files    : VFS.File_Array) return Argument_List;
   --  Return the list of switches for Files, found in the package Pkg_Name,
   --  for a specific language, and for a specific list of switches. The
   --  returned array must be freed by the caller.

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record'Class;
      Languages : Argument_List;
      Show_Only : Boolean);
   --  Same as the public version, except that the pages are never hidden, only
   --  shown depending on the languages

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

   -----------------------
   -- Get_Switch_Widget --
   -----------------------

   function Get_Switch_Widget
     (Page   : access Switches_Editor_Page_Record'Class;
      Switch : String) return Switch_Basic_Widget is
   begin
      for S in Page.Switches'Range loop
         if Page.Switches (S).Switch = Switch then
            return Page.Switches (S);
         end if;
      end loop;
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
     (Switch : Switch_Check_Widget; List : in out Argument_List) is
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
     (Switch : Switch_Check_Widget; List : in out Argument_List)
   is
      Active : Boolean := False;
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L).all = Switch.Switch
         then
            Active := True;
            Free (List (L));
         end if;
      end loop;

      Set_Active (Switch.Check, Active);
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
               return Switch.Switch & Item.Value;
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
     (Switch : Switch_Combo_Widget; List : in out Argument_List) is
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            Free (List (L));
         end if;
      end loop;
   end Filter_Switch;

   ---------------------------
   -- Set_And_Filter_Switch --
   ---------------------------

   procedure Set_And_Filter_Switch
     (Switch : Switch_Combo_Widget; List : in out Argument_List)
   is
      use type Widget_List.Glist;
      Item_Value : Integer := -2;
      Children   : Widget_List.Glist := Get_Children (Get_List (Switch.Combo));
      Item       : Combo_List_Item;
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            begin
               if List (L)'Last <
                 List (L)'First + Switch.Switch'Length
               then
                  Item_Value := -1;

               else
                  Item_Value := L;
               end if;
            exception
               when Constraint_Error =>
                  Item_Value := -1;
            end;
            exit;
         end if;
      end loop;

      while Children /= Widget_List.Null_List loop
         Item := Combo_List_Item (Widget_List.Get_Data (Children));

         if Item_Value = -1 then
            if Item.Value = Switch.Default_No_Digit then
               Select_Child (Get_List (Switch.Combo), Gtk_Widget (Item));
               exit;
            end if;

         elsif Item_Value = -2 then
            if Item.Value = Switch.Default_No_Switch then
               Select_Child (Get_List (Switch.Combo), Gtk_Widget (Item));
               exit;
            end if;

         elsif Item.Value =
           List (Item_Value) (List (Item_Value)'First + Switch.Switch'Length
                              .. List (Item_Value)'Last)
         then
            Select_Child (Get_List (Switch.Combo), Gtk_Widget (Item));
            exit;
         end if;

         Children := Widget_List.Next (Children);
      end loop;

      if Item_Value >= 0 then
         Free (List (Item_Value));
      end if;
   end Set_And_Filter_Switch;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Switch : Switch_Spin_Widget) return String is
      Val : constant Integer := Integer (Get_Value_As_Int (Switch.Spin));
   begin
      if Val /= Switch.Default  then
         return Switch.Switch & Image (Val);
      else
         return "";
      end if;
   end Get_Switch;

   -------------------
   -- Filter_Switch --
   -------------------

   procedure Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out Argument_List) is
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            Free (List (L));
         end if;
      end loop;
   end Filter_Switch;

   ---------------------------
   -- Set_And_Filter_Switch --
   ---------------------------

   procedure Set_And_Filter_Switch
     (Switch : Switch_Spin_Widget; List : in out Argument_List)
   is
      Value  : Grange_Float := Grange_Float (Switch.Default);
   begin
      for L in List'Range loop
         if List (L) /= null
           and then List (L)'Length >= Switch.Switch'Length
           and then List (L) (List (L)'First .. List (L)'First
                              + Switch.Switch'Length - 1) = Switch.Switch
         then
            begin
               Value := Grange_Float'Value
                 (List (L) (List (L)'First + Switch.Switch'Length
                            .. List (L)'Last));
               Free (List (L));
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
      P : constant Switches_Editor_Page := Switches_Editor_Page (Page);
   begin
      --  Don't do anything if the callbacks were blocked, to avoid infinite
      --  loops while we are updating the command line, and it is updating
      --  the buttons, that are updating the command line,...

      if P.Block_Refresh then
         return;
      end if;

      declare
         Current : Argument_List := Get_Switches (P, Normalize => True);
         Coalesce_Switches : Argument_List (P.Coalesce_Switches'Range) :=
           (others => new String'(""));
         Tmp : GNAT.OS_Lib.String_Access;
      begin
         P.Block_Refresh := True;
         Set_Text (P.Cmd_Line, "");

         Assert (Me, P.Switches /= null,
                 "No switches defined for " & P.Title.all);

         for S in P.Switches'Range loop
            declare
               Text : constant String := Get_Switch (P.Switches (S).all);
               Found : Boolean := False;
            begin
               if Text /= "" then
                  --  For "coalesce switches", we cannot add them immediately,
                  --  since we have to coalesce them first.
                  for C in P.Coalesce_Switches'Range loop
                     if Text'Length >= P.Coalesce_Switches (C)'Length
                       and then P.Coalesce_Switches (C).all =
                       Text (Text'First .. P.Coalesce_Switches (C)'Length - 1
                             + Text'First)
                     then
                        Tmp := Coalesce_Switches (C);
                        Coalesce_Switches (C) := new String'
                          (Coalesce_Switches (C).all
                           & Text (P.Coalesce_Switches (C)'Length
                                   + Text'First .. Text'Last));
                        Free (Tmp);
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

         --  Remove the old instances of common switch from the command line,
         --  and add the new ones

         for C in Coalesce_Switches'Range loop
            --  Add the coalesced switch to the command line. As a special
            --  case, if the result is in fact the default value of the switch,
            --  we try to minimize the length of the command line (ie instead
            --  of putting -gnaty3ab we put -gnaty if these are equivalent)

            if Coalesce_Switches (C).all /= "" then
               declare
                  Cmd : constant String :=
                    P.Coalesce_Switches (C).all & Coalesce_Switches (C).all;
               begin
                  if Cmd = P.Coalesce_Switches_Default (C).all then
                     Append_Text
                       (P.Cmd_Line, P.Coalesce_Switches (C).all & ' ');
                  else
                     Append_Text (P.Cmd_Line, Cmd & ' ');
                  end if;
               end;
            end if;

            for Cur in Current'Range loop
               if Current (Cur) /= null
                 and then Current (Cur)'Length >=
                   P.Coalesce_Switches (C)'Length
                 and then P.Coalesce_Switches (C).all = Current (Cur)
                 (Current (Cur)'First ..
                  Current (Cur)'First + P.Coalesce_Switches (C)'Length - 1)
               then
                  Free (Current (Cur));
               end if;
            end loop;
         end loop;


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
     (Page : access Switches_Editor_Page_Record'Class;
      Normalize : Boolean) return Argument_List
   is
      Str : constant String := Get_Text (Page.Cmd_Line);
      Null_Argument_List : Argument_List (1 .. 0);
      List               : Argument_List_Access;
   begin
      if Str /= "" then
         List := Argument_String_To_List (Str);

         if Normalize then
            declare
               Ret : constant Argument_List :=
                 Normalize_Compiler_Switches (Page, List.all);
            begin
               Unchecked_Free (List);
               return Ret;
            end;
         else
            declare
               Ret : constant Argument_List := List.all;
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
      Switches : Argument_List) return Argument_List
   is
      Output : Argument_List_Access;
      Found  : Boolean;
      S      : GNAT.OS_Lib.String_Access;
      Exp    : Argument_List_Access;

   begin
      for Index in Switches'Range loop
         Found := False;

         --  For Ada switches, use the functions provided by GNAT that provide
         --  the splitting of composite switches like "-gnatwue" into
         --  "-gnatwu -gnatwe"

         if Page.Lang.all = Ada_String then
            declare
               Arr : constant Argument_List :=
                 Normalize_Compiler_Switches (Switches (Index).all);
               --  Do not free Arr, this refers to internal strings in GNAT!
            begin
               --  If the switch was already as simple as possible, or wasn't
               --  recognized at all.
               if Arr'Length > 1 then
                  Append (Output, Clone (Arr));
                  S := Switches (Index);
                  Free (S);
                  Found := True;
               end if;
            end;
         end if;

         --  Check expansion switches with no parameter, if any
         if not Found then
            for C in Page.Expansion_Switches'Range loop
               Exp := Page.Expansion_Switches (C);
               if Switches (Index).all = Exp (Exp'First).all then
                  Append (Output, Clone (Exp (Exp'First + 1 .. Exp'Last)));
                  Found := True;
                  S := Switches (Index);
                  Free (S);
                  exit;
               end if;
            end loop;
         end if;

         if not Found then
            Append (Output, (1 => Switches (Index)));
         end if;
      end loop;

      if Output = null then
         return (1 .. 0 => null);

      else
         declare
            O : constant Argument_List := Output.all;
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

      declare
         Arg : Argument_List := Get_Switches (P, Normalize => True);
      begin
         P.Block_Refresh := True;

         for S in P.Switches'Range loop
            Set_And_Filter_Switch (P.Switches (S).all, Arg);
         end loop;

         Free (Arg);
         P.Block_Refresh := False;
      end;
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
      Tip               : String := "")
   is
      Hbox  : Gtk_Box;
      Adj   : Gtk_Adjustment;
      S     : Switch_Spin_Widget_Access := new Switch_Spin_Widget
        (Switch'Length);
      L     : Gtk_Label;
   begin
      Gtk_New_Hbox (Hbox, False, 0);
      Pack_Start (Box, Hbox, False, False);
      S.Switch := Switch;
      S.Default := Default;

      Gtk_New (L, Label);
      Pack_Start (Hbox, L, False, False, 0);

      Gtk_New (Adj, Gdouble (Default), Gdouble (Min), Gdouble (Max),
               1.0, 10.0, 10.0);
      Gtk_New (S.Spin, Adj, 1.0, 0);
      Pack_Start (Hbox, S.Spin, True, True, 0);
      Widget_Callback.Object_Connect
        (S.Spin, "changed",
         Widget_Callback.To_Marshaller (Refresh_Page'Access), Page);

      if Tip /= "" then
         Set_Tip (Page.Tips, S.Spin, '(' & Switch & ") " & ASCII.LF & Tip);
      else
         Set_Tip (Page.Tips, S.Spin, '(' & Switch & ')');
      end if;

      Append_Switch (Page, S);
   end Create_Spin;

   ------------------
   -- Create_Radio --
   ------------------

   procedure Create_Radio
     (Page    : access Switches_Editor_Page_Record;
      Box     : access Gtk.Box.Gtk_Box_Record'Class;
      Buttons : Radio_Switch_Array)
   is
      S    : Switch_Check_Widget_Access;
      Last : Gtk_Radio_Button;
   begin
      for B in Buttons'Range loop
         S := new Switch_Check_Widget (Buttons (B).Switch'Length);

         Gtk_New (Last, Group => Last, Label => -Buttons (B).Label.all);
         S.Check := Gtk_Check_Button (Last);
         S.Switch := Buttons (B).Switch.all;
         Pack_Start (Box, Last, False, False);
         Widget_Callback.Object_Connect
           (Last, "toggled",
            Widget_Callback.To_Marshaller (Refresh_Page'Access), Page);

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
      S : Switch_Check_Widget_Access := new Switch_Check_Widget
        (Switch'Length);
   begin
      Gtk_New (S.Check, Label);
      S.Switch := Switch;
      Pack_Start (Box, S.Check, False, False);
      Set_Active (S.Check, False);
      Widget_Callback.Object_Connect
        (S.Check, "toggled",
         Widget_Callback.To_Marshaller (Refresh_Page'Access), Page);

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
      Label_Size_Group     : Gtk.Size_Group.Gtk_Size_Group := null)
     return Gtk.Widget.Gtk_Widget
   is
      L     : Gtk_Label;
      S     : Switch_Combo_Widget_Access := new Switch_Combo_Widget
        (Switch'Length, Default_No_Switch'Length, Default_No_Digit'Length);
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
        (Get_Entry (S.Combo), "changed",
         Widget_Callback.To_Marshaller (Refresh_Page'Access), Page);

      if Tip /= "" then
         Set_Tip (Page.Tips, S.Combo, '(' & Switch & ") " & ASCII.LF & Tip);
      else
         Set_Tip (Page.Tips, S.Combo, '(' & Switch & ')');
      end if;

      return Gtk_Widget (Hbox);

   exception
      when E : others =>
         Trace (Me, "Create_Combo: Unexpected exception "
                & Exception_Information (E));
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

      Widget_Callback.Connect
        (B, "clicked", Widget_Callback.To_Marshaller (Popup_New_Page'Access));
      Widget_Callback.Connect
        (B, "destroy", Widget_Callback.To_Marshaller (Destroy_Popup'Access));

      return Gtk_Widget (B);
   end Create_Popup;

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
     (Master_Page    : Switches_Editor_Page;
      Master_Switch  : String;
      Master_Status  : Boolean;
      Slave_Page     : access Switches_Editor_Page_Record'Class;
      Slave_Switch   : String;
      Slave_Activate : Boolean := True)
   is
      S1, S2  : Switch_Basic_Widget;
   begin
      if Master_Page /= null then
         S1 :=  Get_Switch_Widget (Master_Page, Master_Switch);
         S2 := Get_Switch_Widget (Slave_Page, Slave_Switch);
         Assert (Me, S1 /= null
                 and then S2 /= null
                 and then S1.all in Switch_Check_Widget'Class
                 and then S2.all in Switch_Check_Widget'Class,
                 "Can only add dependencies between check button switches "
                 & Master_Page.Title.all & ' ' & Master_Switch
                 & ' ' & Slave_Page.Title.all & ' ' & Slave_Switch);

         Dependency_Callback.Connect
           (Switch_Check_Widget_Access (S1).Check, "toggled",
            Dependency_Callback.To_Marshaller (Check_Dependency'Access),
            (Master_Status, Switch_Check_Widget_Access (S2), Slave_Activate));
      end if;
   end Add_Dependency;

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
        new GNAT.OS_Lib.String_List (Default'First .. Default'Last + 1);

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
      Title           : String;
      Project_Package : String;
      Language        : String;
      Lines, Cols     : Glib.Guint;
      Tips            : access Gtk.Tooltips.Gtk_Tooltips_Record'Class) is
   begin
      Page := new Switches_Editor_Page_Record;
      Gtk.Table.Initialize (Page, Lines + 1, Cols, False);
      Set_Row_Spacings (Page, 0);
      Set_Col_Spacings (Page, 0);

      Page.Lang  := new String'(Language);
      To_Lower (Page.Lang.all);
      Page.Title := new String'(Title);
      Page.Pkg   := new String'(Project_Package);
      Page.Coalesce_Switches  := new GNAT.OS_Lib.String_List (1 .. 0);
      Page.Coalesce_Switches_Default := new GNAT.OS_Lib.String_List (1 .. 0);
      Page.Expansion_Switches := new String_List_Array (1 .. 0);
      Page.Tips               := Gtk_Tooltips (Tips);

      Gtk_New (Page.Cmd_Line);
      Set_Editable (Page.Cmd_Line, True);
      Widget_Callback.Object_Connect
        (Page.Cmd_Line, "changed",
         Widget_Callback.To_Marshaller (On_Cmd_Line_Changed'Access), Page);
      Attach (Page, Page.Cmd_Line, 0, Cols, Lines, Lines + 1,
              Expand or Fill, 0, 5, 0);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Switches_Edit;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tab    : Gtk_Label;

   begin
      Editor := new Switches_Edit_Record;
      Gtk.Notebook.Initialize (Editor);

      Editor.Pages := new Pages_Array (1 .. Switches_Page_Count (Kernel));

      for P in Editor.Pages'Range loop
         Editor.Pages (P) := Get_Nth_Switches_Page (Kernel, Editor, P);

         Widget_Callback.Connect
           (Editor.Pages (P), "destroy",
            Widget_Callback.To_Marshaller (Page_Destroyed'Access));

         Gtk_New (Tab, Editor.Pages (P).Title.all);
         Append_Page (Editor, Editor.Pages (P), Tab);
      end loop;

      Set_Current_Page (Editor, 0);

      Widget_Callback.Connect
        (Editor, "destroy",
         Widget_Callback.To_Marshaller (Editor_Destroyed'Access));
   end Gtk_New;

   --------------------
   -- Page_Destroyed --
   --------------------

   procedure Page_Destroyed (Page : access Gtk_Widget_Record'Class) is
      P : Switches_Editor_Page := Switches_Editor_Page (Page);
   begin
      Free (P.Lang);
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
   begin
      Unchecked_Free (Switches_Edit (Editor).Pages);
   end Editor_Destroyed;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record; Languages : Argument_List) is
   begin
      Set_Visible_Pages (Editor, Languages, Show_Only => False);
   end Set_Visible_Pages;

   -----------------------
   -- Set_Visible_Pages --
   -----------------------

   procedure Set_Visible_Pages
     (Editor    : access Switches_Edit_Record'Class;
      Languages : Argument_List;
      Show_Only : Boolean)
   is
      Visible : Boolean;
      Current : Gint := Get_Current_Page (Editor);
   begin
      for P in Editor.Pages'Range loop
         Visible := False;

         for L in Languages'Range loop
            if To_Lower (Languages (L).all) = Editor.Pages (P).Lang.all then
               Visible := True;
               exit;
            end if;
         end loop;

         if Visible then
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
            List : Argument_List := Get_Switches
              (S,
               S.Pages (P).Pkg.all,
               Get_String (S.Pages (P).Lang.all),
               Files => (1 .. 0 => VFS.No_File));
         begin
            Set_Text (S.Pages (P).Cmd_Line,
                      Argument_List_To_String (List));
            Free (List);
         end;
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Revert_To_Default;

   ----------------------
   -- Generate_Project --
   ----------------------

   function Generate_Project
     (Switches           : access Switches_Edit_Record'Class;
      Project            : Project_Type;
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
         Language : constant Name_Id := Get_String (Page.Lang.all);
         Args     : Argument_List := Get_Switches (Page, Normalize => False);
         Value    : Prj.Variable_Value;
         Is_Default_Value : Boolean;
         Rename_Prj : Project_Type;
      begin
         Rename_Prj := Find_Project_Of_Package (Project, Page.Pkg.all);

         if Project = No_Project then
            Is_Default_Value := False;

         else
            Get_Switches
              (Project          => Rename_Prj,
               In_Pkg           => Page.Pkg.all,
               File             => File_Name,
               Language         => Language,
               Value            => Value,
               Is_Default_Value => Is_Default_Value);

            --  Check if we in fact have the initial value
            declare
               Default_Args : Argument_List := To_Argument_List (Value);
            begin
               Is_Default_Value := Is_Equal (Default_Args, Args);
               Free (Default_Args);
            end;
         end if;

         if not Is_Default_Value then
            if File_Name /= VFS.No_File then
               if Args'Length /= 0 then
                  Update_Attribute_Value_In_Scenario
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          =>
                       Build (Page.Pkg.all, Get_String (Name_Switches)),
                     Values             => Args,
                     Attribute_Index    => Base_Name (File_Name),
                     Prepend            => False);
               else
                  Delete_Attribute
                    (Project            => Rename_Prj,
                     Scenario_Variables => Scenario_Variables,
                     Attribute          =>
                       Build (Page.Pkg.all, Get_String (Name_Switches)),
                     Attribute_Index    => Base_Name (File_Name));
               end if;

            elsif Args'Length /= 0 then
               Update_Attribute_Value_In_Scenario
                 (Project            => Rename_Prj,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          =>
                    Build (Page.Pkg.all, Get_String (Name_Default_Switches)),
                  Values             => Args,
                  Attribute_Index    => Get_String (Language),
                  Prepend            => False);

            else
               Delete_Attribute
                 (Project            => Rename_Prj,
                  Scenario_Variables => Scenario_Variables,
                  Attribute          =>
                    Build (Page.Pkg.all, Get_String (Name_Default_Switches)),
                  Attribute_Index    => Get_String (Language));
            end if;

            Changed := True;
         end if;

         Free (Args);
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
      Saved : Argument_List := Get_Current_Scenario
        (Scenario_Variables (Switches.Kernel));
      Scenar : Scenario_Iterator := Start (Scenario);
      Modified : Boolean := False;
   begin
      while not At_End (Scenar) loop
         declare
            Cur : Argument_List := Current (Scenar);
         begin
            Set_Environment (Scenario_Variables (Switches.Kernel), Cur);
            Modified := Modified or Generate_Project
              (Switches           => Switches,
               Scenario_Variables => Scenario_Variables (Switches.Kernel),
               Project            => Project,
               Files              => Files);
            Free (Cur);
         end;

         Next (Scenar);
      end loop;

      Set_Environment (Scenario_Variables (Switches.Kernel), Saved);
      Free (Saved);

      return Modified;

      --  if Modified then
      --     Recompute_View (Switches.Kernel);
      --  end if;
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
     (Switches : access Switches_Edit_Record'Class;
      Pkg_Name : String;
      Language : Name_Id;
      Files    : File_Array) return Argument_List
   is
      Value      : Prj.Variable_Value;
      Is_Default : Boolean;
   begin
      if Switches.Project = No_Project then
         if Pkg_Name = Builder_Package then
            return Clone (Default_Builder_Switches);
         elsif Pkg_Name = Compiler_Package then
            return Clone (Default_Compiler_Switches);
         elsif Pkg_Name = Linker_Package then
            return Clone (Default_Linker_Switches);
         else
            return (1 .. 0 => null);
         end if;

      else
         if Files'Length = 0 then
            Get_Switches (Switches.Project, Pkg_Name, VFS.No_File,
                          Language, Value, Is_Default);
         else
            --  ??? Should we merge all the switches ?
            Get_Switches (Switches.Project, Pkg_Name, Files (Files'First),
                          Language, Value, Is_Default);
         end if;
      end if;
      return To_Argument_List (Value);
   end Get_Switches;

   -----------------
   -- Fill_Editor --
   -----------------

   procedure Fill_Editor
     (Switches  : access Switches_Edit_Record'Class;
      Project   : Project_Type;
      Files     : File_Array)
   is
   begin
      Switches.Project := Project;

      --  Project might be null when we are in the project wizard. In this
      --  case, we fall back on switches for the default project.

      if Project /= No_Project then
         if Files'Length = 0 then
            declare
               Langs : Argument_List := Get_Languages (Project);
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
                  Show_Only => F /= Files'First);
               end;
            end loop;
         end if;
      end if;

      --  Set the switches for all the pages

      for P in Switches.Pages'Range loop
         declare
            List : Argument_List := Get_Switches
              (Switches,
               Switches.Pages (P).Pkg.all,
               Get_String (Switches.Pages (P).Lang.all),
               Files);
         begin
            Set_Text (Switches.Pages (P).Cmd_Line,
                      Argument_List_To_String (List));
            Free (List);
         end;
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Fill_Editor;

   -------------------
   -- Edit_Switches --
   -------------------

   procedure Edit_Switches
     (Item    : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File      : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Modified : Boolean;
      pragma Unreferenced (Item, Modified);
   begin
      Assert (Me, Has_Project_Information (File),
              "Project unknown when editing switches");

      if Has_File_Information (File) then
         Modified := Edit_Switches_For_Files
           (Get_Kernel (Context),
            Project_Information (File),
            (1 => File_Information (File)));
      else
         Modified := Edit_Switches_For_Files
           (Get_Kernel (Context),
            Project_Information (File),
            (1 .. 0 => VFS.No_File));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Edit_Switches;

   -----------------------------
   -- Edit_Switches_For_Files --
   -----------------------------

   function Edit_Switches_For_Files
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Files        : File_Array) return Boolean
   is
      Switches  : Switches_Edit;
      Dialog    : Gtk_Dialog;
      Button    : Gtk_Widget;
      B         : Gtk_Button;
      Box       : Gtk_Box;
      Selector  : Scenario_Selector;
      Modified  : Boolean;

   begin
      if Files'Length > 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for multiple files",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      elsif Files'Length = 1 then
         Gtk_New (Dialog,
                  Title  => -"Editing switches for specific file",
                  Parent => Get_Main_Window (Kernel),
                  Flags  => Modal or Destroy_With_Parent);

      else
         Gtk_New (Dialog,
                  Title  => (-"Editing default switches for project ")
                    & Project_Name (Project),
                  Parent => Get_Main_Window (Kernel),
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

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);

      if Files'Length /= 0 then
         Gtk_New_From_Stock (B, Stock_Revert_To_Saved);
         Pack_Start (Get_Action_Area (Dialog), B);
         Widget_Callback.Object_Connect
           (B, "clicked",
            Widget_Callback.To_Marshaller (Revert_To_Default'Access),
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
