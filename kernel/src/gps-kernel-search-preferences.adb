------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Default_Preferences;     use Default_Preferences;
with Commands;                use Commands;
with Commands.Interactive;    use Commands.Interactive;

with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Search;              use GPS.Search;

package body GPS.Kernel.Search.Preferences is

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
      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Iter :=  Get_First_Reference (Self.Kernel.Get_Preferences);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Preferences_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean) is
      Pref : Preference;
      C    : Search_Context;
      S    : GNAT.Strings.String_Access;
   begin
      Result := null;

      Pref := Get_Pref (Self.Iter);

      --  Don't try to match the hidden preferences
      if Get_Page (Pref) /= "" then
         declare
            Page  : constant String := Get_Page (Pref);
            Doc   : constant String := Get_Doc (Pref);
         begin
            C := Self.Pattern.Start (Doc);
            if C /= GPS.Search.No_Match then
               S := new String'
                 (Self.Pattern.Highlight_Match (Doc, Context => C));
               Result := new Preferences_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => C.Score,
                  Short    => S,
                  Long     => new String'(Page),
                  Id       => S,
                  Pref     => Pref);

               Self.Adjust_Score (Result);
            end if;
         end;
      end if;

      Next (Self.Kernel.Get_Preferences, Self.Iter);
      Has_Next := Get_Pref (Self.Iter) /= null;
   end Next;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Preferences_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      Suffix      : Unbounded_String;
      Suffix_Last : Natural := 0;
      Pref        : Preference;
      C           : Search_Context;
   begin
      Self.Set_Pattern (Pattern);

      loop
         Pref := Get_Pref (Self.Iter);
         exit when Pref = null;

         declare
            Page : constant String := Get_Page (Pref);
         begin
            C := Self.Pattern.Start (Page);
            if C /= GPS.Search.No_Match then
               Self.Pattern.Compute_Suffix (C, Page, Suffix, Suffix_Last);
               exit when Suffix_Last = 0;
            end if;
         end;

         Next (Self.Kernel.Get_Preferences, Self.Iter);
      end loop;

      return Slice (Suffix, 1, Suffix_Last);
   end Complete_Suffix;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Preferences_Search_Result) is
   begin
      Free (Kernel_Search_Result (Self));
   end Free;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Preferences_Search_Result;
      Give_Focus : Boolean) is
      Open_Preferences_Action  : Action_Record_Access;
      Open_Preferences_Command : Interactive_Command_Access;
      pragma Unreferenced (Give_Focus);
   begin
      --  Open the preferences dialog
      Open_Preferences_Action := Lookup_Action
        (Self.Kernel, "open Preferences");
      Open_Preferences_Command := Get_Command (Open_Preferences_Action);
      Launch_Foreground_Command (Self.Kernel, Open_Preferences_Command, False);

      --  Display the preference in the preferences editor view
      Self.Kernel.Get_Preferences.Get_Editor.Display_Pref (Self.Pref);
   end Execute;

end GPS.Kernel.Search.Preferences;
