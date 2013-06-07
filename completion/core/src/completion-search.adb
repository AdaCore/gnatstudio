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

with Ada_Semantic_Tree;              use Ada_Semantic_Tree;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Generics;     use Ada_Semantic_Tree.Generics;
with Engine_Wrappers;                use Engine_Wrappers;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks;      use GPS.Kernel.Standard_Hooks;
with GNAT.Strings;                   use GNAT.Strings;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Widget;                     use Gtk.Widget;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;

package body Completion.Search is

   type Entity_Search_Result is new Kernel_Search_Result with record
      Proposal : Root_Proposal_Access;
   end record;
   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self  : not null access Entity_Search_Result)
     return Gtk.Widget.Gtk_Widget;
   overriding procedure Free (Self : in out Entity_Search_Result);

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Entity_Search_Result) is
   begin
      Free (Self.Proposal.all);
      Unchecked_Free (Self.Proposal);
   end Free;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean)
   is
      Loc : constant File_Location :=
         Get_Location (Self.Proposal.all, Self.Kernel.Databases);
   begin
      Open_File_Editor
         (Self.Kernel,
          Loc.File_Path,
          Loc.Line,
          Loc.Column,
          Focus => Give_Focus);
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self  : not null access Entity_Search_Result)
     return Gtk.Widget.Gtk_Widget
   is
      Label : Gtk_Label;
   begin
      Gtk_New (Label, Self.Proposal.Get_Documentation (Self.Kernel));
      Label.Set_Use_Markup (True);
      Label.Modify_Font (View_Fixed_Font.Get_Pref);
      return Gtk_Widget (Label);
   end Full;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self    : not null access Entities_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Searches amonst entities defined in the project";
   end Documentation;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Self : in out Entities_Search_Provider) is
   begin
      Free (Self.List);
      Unchecked_Free (Self.Iter);
      Free (Kernel_Search_Provider (Self));  --  inherited
   end Free;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Entities_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
      Expression : Parsed_Expression;
      Text : String_Access;
   begin
      --  ??? Could use current context to restrict the visibility

      Free (Self.List);
      Unchecked_Free (Self.Iter);

      Self.Pattern := Search_Pattern_Access (Pattern);

      Text := new String'(Pattern.Get_Text);
      Expression := Parse_Expression_Backward (Text);
      Self.List := Find_Declarations
        (Context =>
           (From_Database,
            Null_Instance_Info,
            Get_Construct_Database (Self.Kernel)),
         From_Visibility  => Null_Visibility_Context,
         Is_Partial       => True,
         Expression       => Expression);

      Self.Iter := new Engine_Wrappers.Entity_Iterator'
        (I => Ada_Semantic_Tree.First (Self.List));

      --  Since Find_Declarations returns a sorted list already, we want to
      --  preserve that sorting.
      Self.Score := 1_000;

      Free (Expression);
      Free (Text);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Proposal : Root_Proposal_Access;
      L : String_Access;
      Loc : File_Location;
      C : GPS.Search.Search_Context;
   begin
      Result := null;

      if Self.Iter.At_End then
         Has_Next := False;
         return;
      end if;

      Proposal := new Root_Proposal'Class'
         (Self.Iter.Get_Proposal);

      if Proposal /= null then
         declare
            Label : constant String :=
              Proposal.Get_Label (Self.Kernel.Databases);
         begin
            C := Self.Pattern.Start (Label);
            if C /= GPS.Search.No_Match then
               Loc := Get_Location (Proposal.all, Self.Kernel.Databases);

               L := new String'
                 (Loc.File_Path.Display_Base_Name
                  & ":" & Image (Loc.Line, Min_Width => 0)
                  & ":" & Image (Integer (Loc.Column), Min_Width => 0));

               Result := new Entity_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => Self.Score + C.Score,
                  Short    => new String'
                    (Self.Pattern.Highlight_Match (Label, Context => C)),
                  Long     => L,
                  Id       => new String'(Label & ":" & L.all),
                  Proposal => Proposal);
               Self.Adjust_Score (Result);

               if Self.Score > 1 then
                  Self.Score := Self.Score - 1;
               end if;
            end if;
         end;
      end if;

      Self.Iter.Next (Self.Kernel.Databases);
      Has_Next := not Self.Iter.At_End;
   end Next;

end Completion.Search;
