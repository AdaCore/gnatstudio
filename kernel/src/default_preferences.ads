-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  This package handles the default value for all the preferences in Glide.
--  Note that all the preferences must be registered through this package
--  before being available to the user.
--  This package is separated from Glide_Kernel.Preferences for elaboration
--  circularity reaons.

with Glib.Properties;
with Gint_Xml;

package Default_Preferences is

   type Property_Color is new Glib.Property;
   type Property_Font  is new Glib.Property;


   function Register_Property (Name : String; Default : Glib.Guint)
      return Glib.Properties.Property_Uint;
   function Register_Property (Name : String; Default : Boolean)
      return Glib.Properties.Property_Boolean;
   function Register_Property (Name : String; Default : String)
      return Glib.Properties.Property_String;
   function Register_Property (Name : String; Default : String)
      return Property_Color;
   function Register_Property (Name : String; Default : String)
      return Property_Font;
   --  Register a new property and its default value.
   --  The returned value should be used later on to retrieve the value of the
   --  property.

   function Find_Default_Pref (Name : String) return Gint_Xml.Node_Ptr;
   --  Return the node in the default preferences that matches Name

end Default_Preferences;
