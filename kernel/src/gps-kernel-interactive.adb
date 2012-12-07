------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtkada.MDI;             use Gtkada.MDI;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Histories;              use Histories;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Widget;             use Gtk.Widget;

package body GPS.Kernel.Interactive is

   --------------------------------
   -- Create_Interactive_Console --
   --------------------------------

   function Create_Interactive_Console
     (Kernel              : access Kernel_Handle_Record'Class;
      Title               : String := "";
      History             : History_Key := "interactive";
      Create_If_Not_Exist : Boolean := True;
      Module              : GPS.Kernel.Abstract_Module_ID := null;
      Force_Create        : Boolean := False;
      Accept_Input        : Boolean := True;
      ANSI_Support        : Boolean := False;
      Manage_Prompt       : Boolean := True) return Interactive_Console
   is
      Console : Interactive_Console;
      Child   : MDI_Child;
      NChild  : GPS_MDI_Child;
      Create  : Boolean;
   begin
      Create := Force_Create;
      if not Create then
         Child := Find_MDI_Child_By_Name (Get_MDI (Kernel), Title);
         Create := (Child = null
                    or else Get_Widget (Child).all not in
                      Interactive_Console_Record'Class)
           and then Create_If_Not_Exist;
      end if;

      if Create then
         Gtk_New
           (Console, "", null,
            System.Null_Address,
            History_List => Get_History (Kernel),
            Key          => History,
            Wrap_Mode    => Wrap_Char,
            Manage_Prompt => Manage_Prompt,
            ANSI_Support => ANSI_Support,
            Highlight    => Message_Highlight.Get_Pref);
         Set_Font_And_Colors (Get_View (Console), Fixed_Font => True);
         Set_Max_Length   (Get_History (Kernel).all, 100, History);
         Allow_Duplicates (Get_History (Kernel).all, History, True, True);

         NChild := new GPS_Console_MDI_Child_Record;

         if Module /= null then
            GPS.Kernel.MDI.Initialize
              (NChild, Console,
               Group               => Group_Consoles,
               Focus_Widget        => Gtk_Widget (Get_View (Console)),
               Module              => Module_ID (Module),
               Desktop_Independent => True);

         else
            GPS.Kernel.MDI.Initialize
              (NChild, Console,
               Group               => Group_Consoles,
               Focus_Widget        => Gtk_Widget (Get_View (Console)),
               Module              => null,
               Desktop_Independent => False);
         end if;

         Set_Title (NChild, Title, Title);
         Put
           (Get_MDI (Kernel), NChild, Initial_Position => Position_Bottom);
         Raise_Child (NChild);

         if Console /= null then
            Enable_Prompt_Display (Console, Accept_Input);
         end if;

      elsif Child /= null then
         Highlight_Child (Child);
         Console := Interactive_Console (Get_Widget (Child));
         Enable_Prompt_Display (Console, Accept_Input);
      end if;

      return Console;
   end Create_Interactive_Console;

end GPS.Kernel.Interactive;
