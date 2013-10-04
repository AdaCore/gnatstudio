------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNATCOLL.JSON;     use GNATCOLL.JSON;

package body Docgen3.Backend.HTML.Source_Code is

   procedure Append_Text_Object
     (Self  : in out Source_Code_Printer'Class;
      Class : String;
      Text  : String);

   ----------------------------
   -- Annotated_Comment_Text --
   ----------------------------

   not overriding procedure Annotated_Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Comment_Text;

   ----------------------------
   -- Annotated_Keyword_Text --
   ----------------------------

   not overriding procedure Annotated_Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Keyword_Text;

   ------------------------
   -- Append_Text_Object --
   ------------------------

   procedure Append_Text_Object
     (Self  : in out Source_Code_Printer'Class;
      Class : String;
      Text  : String)
   is
      Slice_First : Natural := Text'First;
      Slice_Last  : Natural;
      LF_Pattern  : constant String (1 .. 1) := (1 => ASCII.LF);
      Object      : JSON_Value;

   begin
      while Slice_First <= Text'Last loop
         Slice_Last := Index (Text (Slice_First .. Text'Last), LF_Pattern);

         if Slice_Last >= Slice_First then
            if Slice_First < Slice_Last then
               Object := Create_Object;
               Object.Set_Field ("class", Class);
               Object.Set_Field ("text", Text (Slice_First .. Slice_Last - 1));
               Append (Self.Line, Object);
            end if;

            Append (Self.Result, Create (Self.Line));
            Self.Line := Empty_Array;

         else
            Object := Create_Object;
            Object.Set_Field ("class", Class);
            Object.Set_Field ("text", Text (Slice_First .. Text'Last));
            Append (Self.Line, Object);

            Slice_Last := Text'Last;
         end if;

         Slice_First := Slice_Last + 1;
      end loop;
   end Append_Text_Object;

   -------------------------
   -- Aspect_Keyword_Text --
   -------------------------

   not overriding procedure Aspect_Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Keyword_Text;

   -----------------
   -- Aspect_Text --
   -----------------

   not overriding procedure Aspect_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Normal_Text;

   ----------------
   -- Block_Text --
   ----------------

   not overriding procedure Block_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

   --------------------
   -- Character_Text --
   --------------------

   not overriding procedure Character_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("character", Self.Buffer (First.Index .. Last.Index));
   end Character_Text;

   ------------------
   -- Comment_Text --
   ------------------

   not overriding procedure Comment_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("comment", Self.Buffer (First.Index .. Last.Index));
   end Comment_Text;

   --------------
   -- End_File --
   --------------

   not overriding procedure End_File
     (Self     : in out Source_Code_Printer;
      Text     : out Ada.Strings.Unbounded.Unbounded_String;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      --  Append last line

      if Length (Self.Line) /= 0 then
         Append (Self.Result, Create (Self.Line));
         Self.Line := Empty_Array;
      end if;

      Text := Write (Create (Self.Result), False);

      --  Cleanup

      Self.Buffer := null;
      Self.Result := Empty_Array;
   end End_File;

   ---------------------
   -- Identifier_Text --
   ---------------------

   not overriding procedure Identifier_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("identifier", Self.Buffer (First.Index .. Last.Index));
   end Identifier_Text;

   ------------------
   -- Keyword_Text --
   ------------------

   not overriding procedure Keyword_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("keyword", Self.Buffer (First.Index .. Last.Index));
   end Keyword_Text;

   -----------------
   -- Normal_Text --
   -----------------

   not overriding procedure Normal_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("text", Self.Buffer (First.Index .. Last.Index));
   end Normal_Text;

   -----------------
   -- Number_Text --
   -----------------

   not overriding procedure Number_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("number", Self.Buffer (First.Index .. Last.Index));
   end Number_Text;

   -------------------
   -- Operator_Text --
   -------------------

   not overriding procedure Operator_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

   -----------------------------
   -- Partial_Identifier_Text --
   -----------------------------

   not overriding procedure Partial_Identifier_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

   ----------------
   -- Start_File --
   ----------------

   not overriding procedure Start_File
     (Self     : in out Source_Code_Printer;
      Buffer   : not null GNAT.Strings.String_Access;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Buffer := Buffer;
      Self.Current_Line := 1;
   end Start_File;

   -----------------
   -- String_Text --
   -----------------

   not overriding procedure String_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean)
   is
      pragma Unreferenced (Continue);

   begin
      Self.Append_Text_Object
        ("string", Self.Buffer (First.Index .. Last.Index));
   end String_Text;

   ---------------
   -- Type_Text --
   ---------------

   not overriding procedure Type_Text
     (Self     : in out Source_Code_Printer;
      First    : Language.Source_Location;
      Last     : Language.Source_Location;
      Continue : in out Boolean) renames Identifier_Text;

end Docgen3.Backend.HTML.Source_Code;
