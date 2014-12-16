------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

--  This package provides base class for pythin to extend gnatdoc with
--  custom tags.

with GNATdoc.Customization.Markup_Generators;
use GNATdoc.Customization.Markup_Generators;

package GNATdoc.Customization.Tag_Handlers is

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class);
   --  Register the script classes and commands.

   function Is_Supported (Tag : String) return Boolean;
   --  Check is there is custom tag code for given Tag

   procedure On_Match
     (Writer : in out Markup_Generator;
      Tag    : String;
      Attrs  : String;
      Value  : String);
   --  Lookup custom tag code for given Tag and execute it if found.

end GNATdoc.Customization.Tag_Handlers;
