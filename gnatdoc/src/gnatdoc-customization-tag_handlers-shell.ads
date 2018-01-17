------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

private with GNATCOLL.Scripts;

package GNATdoc.Customization.Tag_Handlers.Shell is

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class);
   --  Register the script classes and commands.

private

   type Python_Inline_Tag_Handler is
     new Abstract_Inline_Tag_Handler with record
      Instance : GNATCOLL.Scripts.Class_Instance;
   end record;

   overriding function Name
     (Self : Python_Inline_Tag_Handler) return String;
   --  Returns name of the tag

   overriding function Has_Parameter
     (Self : Python_Inline_Tag_Handler) return Boolean;
   --  Returns True when first word after tag should be processed as tag's
   --  argument.

   overriding procedure To_Markup
     (Self      : in out Python_Inline_Tag_Handler;
      Parameter : String;
      Writer    : in out Markup_Generator);
   --  Called to process tag

end GNATdoc.Customization.Tag_Handlers.Shell;
