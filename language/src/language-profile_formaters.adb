------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Language.Profile_Formaters is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Self : in out Text_Profile_Formater;
      Show_Param_Names : Boolean := True)
   is
   begin
      Self.Show_Param_Names := Show_Param_Names;
   end Configure;

   -------------------
   -- Add_Parameter --
   -------------------

   overriding procedure Add_Parameter
     (Self    : access Text_Profile_Formater;
      Name    : String;
      Mode    : String;
      Of_Type : String;
      Default : String)
   is
      Mode_Image : constant String := Trim (Mode, Ada.Strings.Right);
   begin
      if not Self.Has_Parameter then
         Append (Self.Text, "(");
         Self.Has_Parameter := True;
      else
         Append (Self.Text, "; ");
      end if;

      if Self.Show_Param_Names then
         Append (Self.Text, Trim (Name, Ada.Strings.Right));
      end if;

      if Of_Type /= "" then
         if Self.Show_Param_Names then
            Append (Self.Text, " : ");
         end if;

         --  Do not display "in" when also hiding parameter names, since the
         --  goal is to save as much space as possible
         if Mode_Image /= ""
           and then (Self.Show_Param_Names
                     or else Mode_Image /= "in")
         then
            Append (Self.Text, Mode_Image);
            Append (Self.Text, " ");
         end if;

         Append (Self.Text, Trim (Of_Type, Ada.Strings.Right));
      end if;

      if Default /= "" then
         Append (Self.Text, " :=");
         Append (Self.Text, Default);
      end if;
   end Add_Parameter;

   ----------------
   -- Add_Result --
   ----------------

   overriding procedure Add_Result
     (Self    : access Text_Profile_Formater;
      Mode    : String;
      Of_Type : String)
   is
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ")");
         Self.Has_Parameter := False;
      end if;
      Append (Self.Text, " return ");
      Append (Self.Text, Mode);
      Append (Self.Text, Of_Type);
   end Add_Result;

   ------------------
   -- Add_Variable --
   ------------------

   overriding procedure Add_Variable
     (Self    : access Text_Profile_Formater;
      Mode    : String;
      Of_Type : String) is
   begin
      Append (Self.Text, " ");
      Append (Self.Text, Mode);
      Append (Self.Text, Of_Type);
   end Add_Variable;

   -----------------
   -- Add_Aspects --
   -----------------

   overriding procedure Add_Aspects
     (Self : access Text_Profile_Formater;
      Text : String) is
   begin
      --  No aspects in text format for now
      null;
   end Add_Aspects;

   ------------------
   -- Add_Comments --
   ------------------

   overriding procedure Add_Comments
     (Self : access Text_Profile_Formater;
      Text : String) is
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ")");
         Self.Has_Parameter := False;
      end if;
      if Length (Self.Text) = 0 then
         Append (Self.Text, Text);
      else
         Self.Text := Text & ASCII.LF & ASCII.LF & Self.Text;
      end if;
   end Add_Comments;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : access Text_Profile_Formater) return String
   is
   begin
      if Self.Has_Parameter then
         Append (Self.Text, ")");
         Self.Has_Parameter := False;
      end if;

      return To_String (Self.Text);
   end Get_Text;

end Language.Profile_Formaters;
