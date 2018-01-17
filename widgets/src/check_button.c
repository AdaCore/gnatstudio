/*----------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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
----------------------------------------------------------------------------*/

#include <gtk/gtk.h>

void (* clicked_orig) (GtkButton *button);

void gtkada_check_button_install_handler
  (GType type, void (* clicked) (GtkButton *button))
{
   GObjectClass* objklass = g_type_class_ref(type);
   clicked_orig = GTK_BUTTON_CLASS(objklass)->clicked;
   GTK_BUTTON_CLASS(objklass)->clicked = clicked;
   g_type_class_unref (objklass);
}

void gtkada_check_button_clicked (GtkButton *button)
{
   clicked_orig (button);
}

void gtkada_check_button_force_state (GtkToggleButton *button, int state)
{
   gtk_toggle_button_set_active (button, state);
}

