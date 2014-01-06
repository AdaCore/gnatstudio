------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2014, AdaCore                     --
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

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada_Semantic_Tree;              use Ada_Semantic_Tree;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks;      use GPS.Kernel.Standard_Hooks;
with GNAT.Strings;                   use GNAT.Strings;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Widget;                     use Gtk.Widget;
with GNATCOLL.Symbols;               use GNATCOLL.Symbols;
with GNATCOLL.Utils;                 use GNATCOLL.Utils;
with Language.Tree.Database;         use Language.Tree.Database;
with Xref;                           use Xref;
with GPS.Kernel.Xref;                use GPS.Kernel.Xref;

package body Completion.Search is

   type Entity_Search_Result is new Kernel_Search_Result with record
      Entity : Entity_Persistent_Access :=
        Null_Entity_Persistent_Access;
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
      if Self.Entity /= Null_Entity_Persistent_Access then
         Unref (Self.Entity);
         Self.Entity := Null_Entity_Persistent_Access;
      end if;
      Free (Kernel_Search_Result (Self));  --  inherited
   end Free;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Entity_Search_Result;
      Give_Focus : Boolean)
   is
      Construct : constant Simple_Construct_Information :=
        Get_Construct (Self.Entity);
      File : constant Structured_File_Access := Get_File (Self.Entity);
   begin
      Open_File_Editor
        (Self.Kernel,
         Get_File_Path (File),
         Construct.Sloc_Entity.Line,
         Visible_Column_Type (Construct.Sloc_Entity.Column),
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
      Entity : General_Entity;
      Construct : constant Simple_Construct_Information :=
        Get_Construct (Self.Entity);
      File : constant Structured_File_Access := Get_File (Self.Entity);
   begin
      Entity := Xref.Get_Entity
        (Self.Kernel.Databases,
         Name  => Get (Construct.Name).all,
         Loc   => (File => Get_File_Path (File),
                   Line => Construct.Sloc_Entity.Line,
                   Column => Visible_Column_Type
                     (Construct.Sloc_Entity.Column)));

      Gtk_New (Label, Documentation
                 (Self.Kernel.Databases,
                  Self.Kernel.Get_Language_Handler,
                  Entity));
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
      Free (Self.Iter);
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
   begin
      --  ??? Could use current context to restrict the visibility

      Free (Self.Iter);

      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Iter := Start
        (Self.Kernel.Databases.Constructs, Prefix => "", Is_Partial => True);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Entities_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      L : GNAT.Strings.String_Access;
      C : GPS.Search.Search_Context;
      Entity : Entity_Access;
   begin
      Result := null;

      if At_End (Self.Iter) then
         Has_Next := False;
         return;

      elsif not Is_Valid (Self.Iter) then
         Has_Next := True;
         Next (Self.Iter);
         return;
      end if;

      Entity := Get (Self.Iter);

      if Entity /= Null_Entity_Access then
         declare
            Construct : constant access Simple_Construct_Information :=
              Get_Construct (Entity);
            File : constant Structured_File_Access := Get_File (Self.Iter);
            Name : constant String := Get (Construct.Name).all;
         begin
            C := Self.Pattern.Start (Name);
            if C /= GPS.Search.No_Match then
               L := new String'
                 (Get_File_Path (File).Display_Base_Name
                  & ":" & Image (Construct.Sloc_Entity.Line, Min_Width => 0)
                  & ":"
                  & Image (Construct.Sloc_Entity.Column, Min_Width => 0));

               Result := new Entity_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => C.Score,
                  Short    => new String'
                    (Self.Pattern.Highlight_Match (Name, Context => C)),
                  Long     => L,
                  Id       => new String'(Name & ":" & L.all),
                  Entity   => To_Entity_Persistent_Access (Entity));
               Self.Adjust_Score (Result);
            end if;
         end;
      end if;

      Next (Self.Iter);
      Has_Next := not At_End (Self.Iter);
   end Next;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Entities_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      Suffix      : Unbounded_String;
      Suffix_Last : Natural := 0;
      C : GPS.Search.Search_Context;
      Entity : Entity_Access;
   begin
      Self.Set_Pattern (Pattern);

      while not At_End (Self.Iter) loop
         if not Is_Valid (Self.Iter) then
            Next (Self.Iter);
         else
            Entity := Get (Self.Iter);
            if Entity /= Null_Entity_Access then
               declare
                  Construct : constant access Simple_Construct_Information :=
                    Get_Construct (Entity);
                  Name : constant String := Get (Construct.Name).all;
               begin
                  C := Self.Pattern.Start (Name);
                  if C /= GPS.Search.No_Match then
                     Self.Pattern.Compute_Suffix
                       (C, Name, Suffix, Suffix_Last);
                     exit when Suffix_Last = 0;
                  end if;
               end;
            end if;
         end if;

         Next (Self.Iter);
      end loop;

      return Slice (Suffix, 1, Suffix_Last);
   end Complete_Suffix;

end Completion.Search;
