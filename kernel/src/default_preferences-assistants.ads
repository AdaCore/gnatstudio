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

--  This packages provides utility subprograms to display a preferences
--  assistant, which helps the user to set some critical preferences in a
--  step-by-step approach when using GPS for the first time.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Widget;            use Gtk.Widget;
with Default_Preferences;   use Default_Preferences;
with GPS.Kernel;            use GPS.Kernel;
with GNATCOLL.Traces;       use GNATCOLL.Traces;

package Default_Preferences.Assistants is

   Auto_Run_Assistant : constant Trace_Handle :=
     Create ("GPS.INTERNAL.AUTO_RUN_ASSISTANT", GNATCOLL.Traces.Off);

   type Preferences_Assistant_Page is tagged private;
   type Preferences_Assistant_Page_Array is
     array (Integer range <>) of Preferences_Assistant_Page;
   --  Type representing a preferences assistant page

   function Create
     (Pref_Page : not null access Preferences_Page_Record'Class;
      Label     : String;
      Message   : String) return Preferences_Assistant_Page;
   --  Return a new preferences assistant page which can then be displayed
   --  in a preferences assistant via the Display_Preferences_Assistant
   --  procedure.
   --
   --  Pref_Page refers to the preferences page that will be shown in the
   --  center area of the newly created page.
   --
   --  Label is used for the preferences assistant page title (e.g: "Set
   --  the color theme"), which is displayed at the top of the page.
   --
   --  Message is displayed at the bottom of the page and should indicate where
   --  the preferences shown on this page can be modified later (e.g: "The
   --  color theme can be modified later via the .... menu").

   procedure Display_Preferences_Assistant
     (Kernel : not null access Kernel_Handle_Record'Class;
      Pages  : Preferences_Assistant_Page_Array);
   --  Display a preferences assistant, guiding the user through the given
   --  preferences assistant pages.

private

   type Preferences_Assistant_Page is tagged record
      Pref_Page : Preferences_Page;
      --  The preferences page displayed in the center area

      Label     : Unbounded_String;
      --  The label used for the page's title displayed at the top

      Message   : Unbounded_String;
      --  The message displayed at the bottom
   end record;

end Default_Preferences.Assistants;
