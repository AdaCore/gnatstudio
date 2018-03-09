------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2018, AdaCore                   --
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

with Language;               use Language;
with Language.Ada;
with Case_Handling;
with Libadalang.Analysis;
with GPS.Core_Kernels;
private with Utils.Command_Lines;
private with Pp.Command_Lines;

package LAL.Ada_Languages is

   type Ada_Language is new Language_Root with private;

   procedure Initialize
     (Self    : in out Ada_Language'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel;
      Context : Libadalang.Analysis.Analysis_Context);

private

   type Ada_Language is new Language.Ada.Ada_Language with record
      Kernel          : GPS.Core_Kernels.Core_Kernel;
      Context         : Libadalang.Analysis.Analysis_Context;
      Pp_Command_Line : Utils.Command_Lines.Command_Line
        (Pp.Command_Lines.Descriptor'Access);
   end record;

   overriding procedure Format_Buffer
     (Lang                : access Ada_Language;
      Buffer              : String;
      Replace             : Replace_Text_Callback;
      From, To            : Natural := 0;
      Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null);

end LAL.Ada_Languages;
