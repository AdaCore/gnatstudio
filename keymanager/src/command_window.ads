------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  This package provides a command window widget.
--  This window is opened on request of one of the GPS module. There can be
--  only one such window activate at any one time. All key events are
--  redirected to this window.
--  The window is automatically closed when the user presses Enter or a
--  non-graphical key like the movement keys are any key with a modifier.
--
--  This is similar to the Emacs mini-buffer.

with GPS.Kernel;

package Command_Window is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

end Command_Window;
