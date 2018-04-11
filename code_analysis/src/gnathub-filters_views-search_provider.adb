------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

separate (GNAThub.Filters_Views)
package body Search_Provider is

   use Ada.Strings.Unbounded;
   use GPS.Search;
   use GPS.Kernel.Search;

   -- Severity_Search_Result --
   type Severity_Search_Result is
     new GPS.Kernel.Search.Kernel_Search_Result with record
      Severity : GNAThub.Severity_Access;
   end record;
   overriding procedure Execute
     (Self       : not null access Severity_Search_Result;
      Give_Focus : Boolean);

   -- Rule_Search_Result --
   type Rule_Search_Result is
     new GPS.Kernel.Search.Kernel_Search_Result with record
      Rule : GNAThub.Rule_Access;
   end record;
   overriding procedure Execute
     (Self       : not null access Rule_Search_Result;
      Give_Focus : Boolean);

   ------------------
   -- Copy_Pattern --
   ------------------

   procedure Copy_Pattern
     (Self    : not null access Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class) is
   begin
      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      --  Set Self.Pattern to Approximate if Pattern.Kind = Fuzzy
      Self.Pattern := Pattern.Build_If_Needed
        (Kind     => Fuzzy,
         New_Kind => Approximate,
         Built    => Self.Pattern_Needs_Free);
   end Copy_Pattern;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Severity_Search_Result;
      Give_Focus : Boolean)
   is
      pragma Unreferenced (Give_Focus);
      View : constant Views.View_Access := Views.Retrieve_View (Self.Kernel);

   begin
      if View /= null then
         View.Severities_Editor.Highlight (Self.Severity);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Rule_Search_Result;
      Give_Focus : Boolean)
   is
      pragma Unreferenced (Give_Focus);
      View : constant Views.View_Access := Views.Retrieve_View (Self.Kernel);

   begin
      if View /= null then
         View.Rules_Editor.Highlight (Self.Rule);
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Provider) is
   begin
      if Self.Pattern_Needs_Free then
         Free (Self.Pattern);
      end if;

      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Severities_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      use GNAThub.Severities_Ordered_Sets;
      Severity : Severity_Access;
      Context  : Search_Context;
      View     : constant Views.View_Access :=
        Views.Retrieve_View (Self.Kernel);
   begin
      Result   := null;
      Has_Next := False;

      if View = null
        or else not Has_Element (Self.Cursor)
      then
         return;
      end if;

      Severity := Element (Self.Cursor);

      if Is_Severity_Visible (Severity, Gtk.Widget.Gtk_Widget (View)) then
         declare
            Name : constant String := To_String (Get_Name (Severity.all));
         begin
            Context := Self.Pattern.Search_Best_Match (Name);

            if Context /= No_Match then
               Result := new Severity_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => Context.Score,
                  Short    => new String'
                    (Self.Pattern.Highlight_Match
                         (Buffer  => Name,
                          Context => Context)),
                  Long     => new String'("severity - " & Name),
                  Id       => new String'(Name),
                  Severity => Severity);

               Self.Adjust_Score (Result);
            end if;
         end;
      end if;

      Next (Self.Cursor);
      Has_Next := Has_Element (Self.Cursor);
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Rules_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      use GNAThub.Rule_Sets;
      Rule    : Rule_Access;
      Context : Search_Context;
      View     : constant Views.View_Access :=
        Views.Retrieve_View (Self.Kernel);
   begin
      Result   := null;
      Has_Next := False;

      if View = null
        or else not Has_Element (Self.Cursor)
      then
         return;
      end if;

      Rule := Element (Self.Cursor);

      if Is_Rule_Visible (Rule, Gtk.Widget.Gtk_Widget (View)) then
         declare
            Name : constant String := To_String (Rule.Name);
         begin
            Context := Self.Pattern.Search_Best_Match (Name);

            if Context /= No_Match then
               Result := new Rule_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => Context.Score,
                  Short    => new String'
                    (Self.Pattern.Highlight_Match
                         (Buffer  => Name,
                          Context => Context)),
                  Long     => new String'("rule - " & Name),
                  Id       => new String'(Name),
                  Rule     => Rule);

               Self.Adjust_Score (Result);
            end if;
         end;
      end if;

      Next (Self.Cursor);
      Has_Next := Has_Element (Self.Cursor);
   end Next;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Severities_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      if Views.Retrieve_View (Self.Kernel) = null then
         Self.Cursor := GNAThub.Severities_Ordered_Sets.No_Element;
      else
         Self.Copy_Pattern (Pattern);
         Self.Cursor := GNAThub_Module.Severities.First;
      end if;
   end Set_Pattern;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Rules_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      if Views.Retrieve_View (Self.Kernel) = null then
         Self.Cursor := GNAThub.Rule_Sets.No_Element;
      else
         Self.Copy_Pattern (Pattern);
         Self.Cursor := GNAThub_Module.Rules.First;
      end if;
   end Set_Pattern;

end Search_Provider;
