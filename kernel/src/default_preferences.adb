-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Gint_Xml;          use Gint_Xml;

package body Default_Preferences is

   Default_Preferences : Gint_Xml.Node_Ptr;
   --  All the preferences and their default values.
   --  This is a global variable, but is only modified at elaboration time when
   --  the preferences are declared.

   procedure Create_Default_Pref (Name : String; Default : String);
   --  Create a new node in the default_preferences tree for the preference
   --  Name.
   --  Default is the default value for that property

   -----------------------
   -- Find_Default_Pref --
   -----------------------

   function Find_Default_Pref (Name : String) return Gint_Xml.Node_Ptr is
      Node : Node_Ptr;
   begin
      Node := Find_Tag (Default_Preferences.Child, Name);
      pragma Assert (Node /= null);
      return Node;
   end Find_Default_Pref;

   -------------------------
   -- Create_Default_Pref --
   -------------------------

   procedure Create_Default_Pref (Name : String; Default : String) is
      N : Node_Ptr;
   begin
      if Default_Preferences = null then
         Default_Preferences := new Gint_Xml.Node;
         Default_Preferences.Tag := new String' ("Preferences");
      end if;

      N := Find_Tag (Default_Preferences.Child, Name);

      if N = null then
         N := new Node;
         N.Tag := new String' (Name);
         Add_Child (Default_Preferences, N);
      else
         Gint_Xml.Free (N.Value);
      end if;
      N.Value := new String' (Default);
   end Create_Default_Pref;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property (Name : String; Default : Glib.Guint)
      return Glib.Properties.Property_Uint is
   begin
      Create_Default_Pref (Name, Guint'Image (Default));
      return Build (Name);
   end Register_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property (Name : String; Default : Boolean)
      return Glib.Properties.Property_Boolean is
   begin
      Create_Default_Pref (Name, Boolean'Image (Default));
      return Build (Name);
   end Register_Property;

end Default_Preferences;
