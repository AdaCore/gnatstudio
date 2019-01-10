------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2015-2019, AdaCore                  --
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

package GNATdoc.Customization.Tag_Handlers.Images is

   type Image_Tag_Handler is new Abstract_Inline_Tag_Handler with private;

private

   type Image_Tag_Handler is new Abstract_Inline_Tag_Handler with null record;

   overriding function Name (Self : Image_Tag_Handler) return String;
   --  Returns tag's name ('image')

   overriding function Has_Parameter
     (Self : Image_Tag_Handler) return Boolean;
   --  Returns True, image tag has argument - name of the image file.

   overriding procedure To_Markup
     (Self      : in out Image_Tag_Handler;
      Parameter : String;
      Writer    : in out Markup_Generator);
   --  Process 'image' tag.

end GNATdoc.Customization.Tag_Handlers.Images;
