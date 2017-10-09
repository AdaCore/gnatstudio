------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with String_Utils;            use String_Utils;

with Gtk.Enums;               use Gtk.Enums;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_View;           use Gtk.Text_View;
with Pango.Enums;             use Pango.Enums;

with Default_Preferences;     use Default_Preferences;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Search;              use GPS.Search;
with GPS.Kernel.Custom.GUI;   use GPS.Kernel.Custom.GUI;

package body GPS.Kernel.Search.Plugins is

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self    : not null access Plugins_Search_Provider) return String is
      pragma Unreferenced (Self);
   begin
      return "Search amongst the GPS plugins, and display the associated page "
        & "in the preferences editor dialog.";
   end Documentation;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Plugins_Search_Provider) is
   begin
      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Plugins_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      --  If Self.Pattern has been allocated by the provider itself, free
      --  it before allocating a new pattern.
      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      --  Set Self.Pattern to Approximate if Pattern.Kind = Fuzzy
      Self.Pattern := Pattern.Build_If_Needed
        (Kind     => Fuzzy,
         New_Kind => Approximate,
         Built    => Self.Pattern_Needs_Free);

      Self.Iter :=  Get_First_Reference (Self.Kernel.Get_Preferences);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Plugins_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Page                   : constant Preferences_Page :=
                                 Get_Page (Self.Iter);
      Is_Plugins_Page        : Boolean := False;
      Displayed_In_Assistant : Boolean := False;
   begin
      if Page /= null then
         Is_Plugins_Page := Page.all in Plugin_Preferences_Page_Record'Class;
         Displayed_In_Assistant := Page.Get_Page_Type = Assistant_Page;
      end if;

      if Is_Plugins_Page and then not Displayed_In_Assistant then
         declare
            Plugin_Page            : constant Plugin_Preferences_Page :=
                                       Plugin_Preferences_Page (Page);
            Doc                    : constant String :=
                                       Plugin_Page.Get_Documentation;
            Doc_First_Line         : constant String :=
                                       Get_Surrounding_Line
                                         (Doc, Doc'First, Doc'First);
            Plugin_Label           : constant String :=
                                       Plugin_Page.Get_Plugin_Label;
            Name_Context           : Search_Context;
            Doc_Context            : Search_Context;
            Short                  : GNAT.Strings.String_Access;
            Long                   : GNAT.Strings.String_Access;
         begin
            Result := null;
            Name_Context := Self.Pattern.Search_Best_Match (Plugin_Label);

            --  Try to match the plugin's name
            if Name_Context /= GPS.Search.No_Match then
               Short := new String'
                 (Self.Pattern.Highlight_Match
                    (Buffer  => Plugin_Label,
                     Context => Name_Context));
               Long := new String'(Doc_First_Line);

               Result := Plugins_Search_Provider'Class
                 (Self.all).Create_Plugins_Search_Result
                 (Plugin_Page, Short, Long, Name_Context.Score);

               Self.Adjust_Score (Result);
            end if;

            --  If the name did not match, try to match the first line of the
            --  plugin's documentation.
            if Result = null then
               Doc_Context := Self.Pattern.Search_Best_Match (Doc_First_Line);

               if Doc_Context /= GPS.Search.No_Match then
                  Short := new String'(Plugin_Label);
                  Long := new String'
                    (Self.Pattern.Highlight_Match
                       (Buffer  => Doc_First_Line,
                        Context => Doc_Context));

                  Result := Plugins_Search_Provider'Class
                    (Self.all).Create_Plugins_Search_Result
                    (Plugin_Page, Short, Long, Doc_Context.Score);

                  Self.Adjust_Score (Result);
               end if;
            end if;
         end;
      end if;

      Next (Self.Iter);
      Has_Next := Get_Page (Self.Iter) /= null;
   end Next;

   ----------------------------------
   -- Create_Plugins_Search_Result --
   ----------------------------------

   function Create_Plugins_Search_Result
     (Self        : not null access Plugins_Search_Provider;
      Plugin_Page : not null GPS.Kernel.Custom.GUI.Plugin_Preferences_Page;
      Short       : GNAT.Strings.String_Access;
      Long        : GNAT.Strings.String_Access;
      Score       : Natural) return GPS.Search.Search_Result_Access is
   begin
      return new Plugins_Search_Result'
        (Kernel      => Self.Kernel,
         Provider    => Self,
         Score       => Score,
         Short       => Short,
         Long        => Long,
         Id          => new String'(Long.all),
         Plugin_Page => Plugin_Page);
   end Create_Plugins_Search_Result;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Plugins_Search_Result) is
   begin
      Free (Kernel_Search_Result (Self));
   end Free;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Plugins_Search_Result;
      Give_Focus : Boolean)
   is
      Success        : Boolean;
      pragma Unreferenced (Give_Focus, Success);
   begin
      Success := Execute_Action
        (Self.Kernel, "open Preferences", Synchronous => True,
         Error_Msg_In_Console        => True);

      --  Display the plugin page in the preferences editor dialog
      Self.Kernel.Get_Preferences.Get_Editor.Display_Page
        (Self.Plugin_Page.Get_Name);
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self       : not null access Plugins_Search_Result)
      return Gtk.Widget.Gtk_Widget
   is
      View      : Gtk_Text_View;
      Buffer    : Gtk_Text_Buffer;
      Underline : Gtk_Text_Tag;
      Bold      : Gtk_Text_Tag;
      Iter      : Gtk_Text_Iter;
   begin
      Gtk_New (View);
      Buffer := View.Get_Buffer;

      View.Set_Editable (False);
      View.Set_Wrap_Mode (Wrap_Word);
      View.Modify_Font (Default_Style.Get_Pref_Font);

      Bold := Buffer.Create_Tag;
      Set_Property (Bold, Gtk.Text_Tag.Weight_Property, Pango_Weight_Bold);

      Underline := Buffer.Create_Tag;
      Set_Property
        (Underline, Gtk.Text_Tag.Weight_Property, Pango_Weight_Bold);
      Set_Property
        (Underline, Gtk.Text_Tag.Underline_Property,
         Pango_Underline_Single);

      Buffer.Get_End_Iter (Iter);
      Buffer.Insert (Iter, Self.Plugin_Page.Get_Documentation);

      return Gtk.Widget.Gtk_Widget (View);
   end Full;

end GPS.Kernel.Search.Plugins;
