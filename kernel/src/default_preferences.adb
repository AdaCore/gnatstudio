-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.XML;
with Gdk.Color;         use Gdk.Color;
with Gdk.Font;          use Gdk.Font;
with Basic_Types;       use Basic_Types;

package body Default_Preferences is

   Internal_Color_Type : constant GType := GType'Last;
   Internal_Font_Type  : constant GType := GType'Last - 1;
   --  Internal types to represent the colors and fonts. We cannot use
   --  Gdk.Color.Gdk_Color_Type or Gdk.Font.Get_Type, since gdk hasn't been
   --  initialized when the preferences are registered.

   type Property_Description is record
      Description     : String_Access;
      GUI_Description : String_Access;
      Page            : String_Access;
      Prop_Type       : Glib.GType;
      Editable        : Boolean;
   end record;

   package Property_XML is new Glib.XML (Property_Description);
   use Property_XML;
   --  ??? We could register the description as attributes of the XML nodes.

   Default_Preferences : Node_Ptr;
   --  All the preferences and their default values.
   --  This is a global variable, but is only modified at elaboration time when
   --  the preferences are declared.

   procedure Create_Default_Pref
     (Name, Default, Description, GUI_Description, Page : String;
      Prop_Type : Glib.GType;
      Editable  : Boolean);
   --  Create a new node in the default_preferences tree for the preference
   --  Name.
   --  Default is the default value for that property

   function Find_Default_Pref (Name : String) return Node_Ptr;
   --  Return the node for the preference Name

   -----------------------
   -- Find_Default_Pref --
   -----------------------

   function Find_Default_Pref (Name : String) return Node_Ptr is
      Node : Node_Ptr;
   begin
      Node := Find_Tag (Default_Preferences.Child, Name);
      pragma Assert (Node /= null);
      return Node;
   end Find_Default_Pref;

   -----------------------
   -- Find_Default_Pref --
   -----------------------

   function Find_Default_Pref (Name : String) return String is
      N : constant Node_Ptr := Find_Default_Pref (Name);
   begin
      if N /= null then
         return N.Value.all;
      else
         return "";
      end if;
   end Find_Default_Pref;

   -------------------------
   -- Create_Default_Pref --
   -------------------------

   procedure Create_Default_Pref
     (Name, Default, Description, GUI_Description, Page : String;
      Prop_Type : Glib.GType;
      Editable  : Boolean)
   is
      N : Node_Ptr;
   begin
      if Default_Preferences = null then
         Default_Preferences := new Node;
         Default_Preferences.Tag := new String' ("Preferences");
      end if;

      N := Find_Tag (Default_Preferences.Child, Name);

      if N = null then
         N := new Node;
         N.Tag := new String' (Name);
         Add_Child (Default_Preferences, N);
      else
         Property_XML.Free (N.Value);
      end if;

      N.Value := new String' (Default);

      Free (N.Specific_Data.Description);
      Free (N.Specific_Data.GUI_Description);
      Free (N.Specific_Data.Page);
      N.Specific_Data :=
        (Description     => new String' (Description),
         GUI_Description => new String' (GUI_Description),
         Page            => new String' (Page),
         Prop_Type       => Prop_Type,
         Editable        => Editable);
   end Create_Default_Pref;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property
     (Name            : String;
      Default         : Glib.Gint;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Glib.Properties.Property_Int is
   begin
      Create_Default_Pref
        (Name, Gint'Image (Default),
         Description, GUI_Description, Page, GType_Int, Editable);
      return Build (Name);
   end Register_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property
     (Name            : String;
      Default         : Glib.Guint;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Glib.Properties.Property_Uint is
   begin
      Create_Default_Pref
        (Name, Guint'Image (Default),
         Description, GUI_Description, Page, GType_Uint, Editable);
      return Build (Name);
   end Register_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property
     (Name            : String;
      Default         : Boolean;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Glib.Properties.Property_Boolean is
   begin
      Create_Default_Pref
        (Name, Boolean'Image (Default),
         Description, GUI_Description, Page, GType_Boolean, Editable);
      return Build (Name);
   end Register_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property
     (Name            : String;
      Default         : String;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Glib.Properties.Property_String is
   begin
      Create_Default_Pref
        (Name, Default,
         Description, GUI_Description, Page, GType_String, Editable);
      return Build (Name);
   end Register_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property
     (Name            : String;
      Default         : String;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Property_Color is
   begin
      Create_Default_Pref
        (Name, Default,
         Description, GUI_Description, Page, Internal_Color_Type, Editable);
      return Build (Name);
   end Register_Property;

   -----------------------
   -- Register_Property --
   -----------------------

   function Register_Property
     (Name            : String;
      Default         : String;
      Description     : String;
      GUI_Description : String;
      Page            : String;
      Editable        : Boolean := True)
      return Property_Font is
   begin
      Create_Default_Pref
        (Name, Default,
         Description, GUI_Description, Page, Internal_Font_Type, Editable);
      return Build (Name);
   end Register_Property;

   ------------------------------
   -- Save_Default_Preferences --
   ------------------------------

   procedure Save_Default_Preferences (File_Name : String) is
   begin
      Print (Default_Preferences, File_Name => File_Name);
   end Save_Default_Preferences;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Name : String) return String is
      N : constant Node_Ptr := Find_Default_Pref (Name);
   begin
      if N /= null then
         return N.Specific_Data.Description.all;
      else
         return "";
      end if;
   end Get_Description;

   -------------------------
   -- Get_GUI_Description --
   -------------------------

   function Get_GUI_Description (Name : String) return String is
      N : constant Node_Ptr := Find_Default_Pref (Name);
   begin
      if N /= null then
         return N.Specific_Data.GUI_Description.all;
      else
         return "";
      end if;
   end Get_GUI_Description;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (Name : String) return String is
      N : constant Node_Ptr := Find_Default_Pref (Name);
   begin
      if N /= null then
         return N.Specific_Data.Page.all;
      else
         return "General";
      end if;
   end Get_Page;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Name : String) return Glib.GType is
      N : constant Node_Ptr := Find_Default_Pref (Name);
   begin
      if N /= null then
         case N.Specific_Data.Prop_Type is
            when Internal_Color_Type =>
               N.Specific_Data.Prop_Type := Gdk_Color_Type;
            when Internal_Font_Type =>
               N.Specific_Data.Prop_Type := Gdk.Font.Get_Type;
            when others =>
               null;
         end case;

         return N.Specific_Data.Prop_Type;
      else
         return GType_Int;
      end if;
   end Get_Type;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable (Name : String) return Boolean is
      N : constant Node_Ptr := Find_Default_Pref (Name);
   begin
      if N /= null then
         return N.Specific_Data.Editable;
      else
         return True;
      end if;
   end Is_Editable;

   -------------------------
   -- Get_All_Preferences --
   -------------------------

   function Get_All_Preferences return Basic_Types.String_Array is
      Count : Natural := 0;
      N : Node_Ptr := Default_Preferences.Child;
   begin
      while N /= null loop
         Count := Count + 1;
         N := N.Next;
      end loop;

      declare
         Prefs : String_Array (1 .. Count);
      begin
         N := Default_Preferences.Child;
         Count := 1;

         while N /= null loop
            Prefs (Count) := new String' (N.Tag.all);
            Count := Count + 1;
            N := N.Next;
         end loop;

         return Prefs;
      end;
   end Get_All_Preferences;

end Default_Preferences;
