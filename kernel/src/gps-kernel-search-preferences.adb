------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Glib.Convert;            use Glib.Convert;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Search;              use GPS.Search;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_View;           use Gtk.Text_View;
with Pango.Enums;             use Pango.Enums;

package body GPS.Kernel.Search.Preferences is

   procedure Search_Match_In_Label
     (Self     : not null access Preferences_Search_Provider;
      Pref     : not null Preference;
      Result   : out GPS.Search.Search_Result_Access);
   --  Search if the current pattern matches with the given preference's label.
   --  Return null if no match was found and a non-null result otherwise.

   procedure Search_Match_In_Doc
     (Self     : not null access Preferences_Search_Provider;
      Pref     : not null Preference;
      Result   : out GPS.Search.Search_Result_Access);
   --  Search if the current pattern matches with the given preference's
   --  documentation.
   --  Return null if no match was found and a non-null result otherwise.

   ---------------------------
   -- Search_Match_In_Label --
   ---------------------------

   procedure Search_Match_In_Label
     (Self     : not null access Preferences_Search_Provider;
      Pref     : not null Preference;
      Result   : out GPS.Search.Search_Result_Access)
   is
      Label             : constant String := Get_Label (Pref);
      Label_Context     : Search_Context;
      Short             : GNAT.Strings.String_Access;
      Long              : GNAT.Strings.String_Access;
   begin
      Result := null;
      Label_Context := Self.Pattern.Search_Best_Match (Label);

      --  if a match was found
      if Label_Context /= GPS.Search.No_Match then
         declare
            Doc : constant String := Get_Doc (Pref);
         begin
            Short := new String'
              (Escape_Text (Get_Page_Name (Pref)) &
                 Self.Pattern.Highlight_Match
                 (Buffer  => Pref.Get_Label,
                  Context => Label_Context));
            Long := new String'
              (Get_Surrounding_Line (Doc, Doc'First, Doc'First));

            Result := Preferences_Search_Provider'Class
              (Self.all).Create_Preferences_Search_Result
              (Pref, Short, Long, Label_Context.Score);

            Self.Adjust_Score (Result);
         end;
      end if;
   end Search_Match_In_Label;

   -------------------------
   -- Search_Match_In_Doc --
   -------------------------

   procedure Search_Match_In_Doc
     (Self     : not null access Preferences_Search_Provider;
      Pref     : not null Preference;
      Result   : out GPS.Search.Search_Result_Access)
   is
      Doc               : constant String := Get_Doc (Pref);
      Short             : GNAT.Strings.String_Access;
      Long              : GNAT.Strings.String_Access;
      Doc_Context       : Search_Context;
   begin
      Result := null;

      Doc_Context := Self.Pattern.Search_Best_Match (Doc);

      --  If a match was found
      if Doc_Context /= GPS.Search.No_Match then
         Short := new String'
           (Escape_Text (Get_Page_Name (Pref)) & Pref.Get_Label);
         Long := new String'
           (Self.Pattern.Highlight_Match
              (Buffer  => Get_Surrounding_Line
                   (Doc,
                    Doc_Context.Start.Index,
                    Doc_Context.Finish.Index),
               Context => Doc_Context));

         Result := Preferences_Search_Provider'Class
           (Self.all).Create_Preferences_Search_Result
           (Pref, Short, Long, Doc_Context.Score);

         Self.Adjust_Score (Result);
      end if;
   end Search_Match_In_Doc;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self    : not null access Preferences_Search_Provider) return String is
      pragma Unreferenced (Self);
   begin
      return "Search amongst the GPS preferences, and display the page "
      & "containing it.";
   end Documentation;

   -----------------------------
   -- Set_Search_Among_Hidden --
   -----------------------------

   procedure Set_Search_Among_Hidden
     (Self  : not null access Preferences_Search_Provider;
      Value : Boolean) is
   begin
      Self.Search_Among_Hidden := Value;
   end Set_Search_Among_Hidden;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Preferences_Search_Provider) is
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
     (Self    : not null access Preferences_Search_Provider;
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
     (Self     : not null access Preferences_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Manager                : constant Preferences_Manager :=
                                 Self.Kernel.Get_Preferences;
      Pref                   : constant Preference :=
                                 Get_Pref (Self.Iter, Manager => Manager);
      Page_Name              : constant String := Pref.Get_Page_Name;
      Page                   : constant Preferences_Page :=
                                 Manager.Get_Registered_Page (Page_Name);
      Displayed_In_Assistant : constant Boolean := (Page /= null
        and then Page.Get_Page_Type = Assistant_Page);
   begin
      Result := null;

      if not Displayed_In_Assistant and then
        (Self.Search_Among_Hidden or else Page_Name /= "")
      then
         --  Try to match the preference's label first
         Search_Match_In_Label (Self   => Self,
                                Pref   => Pref,
                                Result => Result);

         --  If no match was found for the label, try with the documentation
         if Result = null then
            Search_Match_In_Doc (Self   => Self,
                                 Pref   => Pref,
                                 Result => Result);
         end if;
      end if;

      Next (Self.Iter);
      Has_Next := Get_Pref (Self.Iter, Manager => Manager) /= null;
   end Next;

   --------------------------------------
   -- Create_Preferences_Search_Result --
   --------------------------------------

   function Create_Preferences_Search_Result
     (Self  : not null access Preferences_Search_Provider;
      Pref  : not null Default_Preferences.Preference;
      Short : GNAT.Strings.String_Access;
      Long  : GNAT.Strings.String_Access;
      Score : Natural) return GPS.Search.Search_Result_Access
   is
      Name  : constant String := Get_Name (Pref);
   begin
      return new Preferences_Search_Result'
        (Kernel   => Self.Kernel,
         Provider => Self,
         Score    => Score,
         Short    => Short,
         Long     => Long,
         Id       => new String'(Name),
         Pref     => Pref);
   end Create_Preferences_Search_Result;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Preferences_Search_Result;
      Give_Focus : Boolean)
   is
      Success : Boolean;
      pragma Unreferenced (Give_Focus, Success);
   begin
      Success := Execute_Action
         (Self.Kernel, "open Preferences", Synchronous => True,
          Error_Msg_In_Console => True);

      --  Display the preference in the preferences editor dialog
      Self.Kernel.Get_Preferences.Get_Editor.Display_Pref
        (Pref      => Self.Pref,
         Highlight => False);
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self       : not null access Preferences_Search_Result)
      return Gtk.Widget.Gtk_Widget is
      View      : Gtk_Text_View;
      Buffer    : Gtk_Text_Buffer;
      Underline : Gtk_Text_Tag;
      Bold      : Gtk_Text_Tag;
      Iter      : Gtk_Text_Iter;
   begin
      if Self.Pref /= null then
         Gtk_New (View);
         Buffer := View.Get_Buffer;

         View.Set_Editable (False);
         View.Set_Wrap_Mode (Wrap_Word);

         Bold := Buffer.Create_Tag;
         Set_Property (Bold, Gtk.Text_Tag.Weight_Property, Pango_Weight_Bold);

         Underline := Buffer.Create_Tag;
         Set_Property
           (Underline, Gtk.Text_Tag.Weight_Property, Pango_Weight_Bold);
         Set_Property
           (Underline, Gtk.Text_Tag.Underline_Property,
            Pango_Underline_Single);

         Buffer.Get_End_Iter (Iter);
         Buffer.Insert (Iter, "Name: " & Get_Name (Self.Pref) & ASCII.LF);
         Buffer.Insert
           (Iter, ASCII.LF & "Page: " & Get_Page_Name (Self.Pref) & ASCII.LF);
         Buffer.Insert (Iter, ASCII.LF & Get_Doc (Self.Pref));

         return Gtk.Widget.Gtk_Widget (View);
      end if;

      return null;
   end Full;

end GPS.Kernel.Search.Preferences;
