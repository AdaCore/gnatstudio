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

with GNAT.Strings;        use GNAT.Strings;
with GPS.Kernel.Actions;  use GPS.Kernel.Actions;
with GPS.Search;          use GPS.Search;

package body GPS.Kernel.Search.Actions is

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
     (Self    : not null access Actions_Search_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Search amongst the GPS commands, and execute the selected one";
   end Documentation;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Actions_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Iter := Start (Self.Kernel);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Actions_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      Action : constant Action_Record_Access := Get (Self.Iter);
      C : Search_Context;
   begin
      Result := null;

      if Action /= null then

         --  Do not complete on menu names
         if Action.Name (Action.Name'First) /= '/' then
            C := Self.Pattern.Start (Action.Name.all);
            if C /= GPS.Search.No_Match then
               Result := Build_Actions_Result
                  (Self.Kernel,
                   Name  => Action.Name.all,
                   Score => C.Score,
                   Short => Self.Pattern.Highlight_Match
                      (Action.Name.all, Context => C));
            end if;
         end if;

         Has_Next := True;
         Next (Self.Kernel, Self.Iter);
      else
         Has_Next := False;
      end if;
   end Next;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Actions_Search_Result) is
   begin
      GNAT.Strings.Free (Self.Name);
      Free (Kernel_Search_Result (Self));
   end Free;

   --------------------------
   -- Build_Actions_Result --
   --------------------------

   function Build_Actions_Result
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Name   : String;
       Score  : Natural := 100;
       Short  : String := "")
      return GPS.Search.Search_Result_Access
   is
      S : constant GNAT.Strings.String_Access :=
         new String'((if Short = "" then Name else Short));
   begin
      return new Actions_Search_Result'
         (Kernel   => Kernel_Handle (Kernel),
          Score    => Score,
          Short    => S,
          Long     => null,
          Id       => S,
          Name     => new String'(Name));
   end Build_Actions_Result;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Actions_Search_Result;
      Give_Focus : Boolean)
   is
      A : constant Action_Record_Access :=
         Lookup_Action (Self.Kernel, Self.Name.all);
      Dummy : Boolean;
      pragma Unreferenced (Dummy, Give_Focus);
   begin
      if A /= null then
         Dummy := Execute_In_Background
            (Kernel   => Self.Kernel,
             Action   => A);
      end if;
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self : not null access Actions_Search_Result) return String
   is
      Action : constant Action_Record_Access :=
         Lookup_Action (Self.Kernel, Self.Name.all);
   begin
      if Action /= null and then Action.Description /= null then
         --  ??? Should include name, category,...
         return Action.Description.all;
      end if;
      return "";
   end Full;

end GPS.Kernel.Search.Actions;
