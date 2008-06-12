/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                   Copyright (C) 2008, AdaCore                     *
 *                                                                   *
 * GPS is free  software;  you can redistribute it and/or modify  it *
 * under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is  distributed in the hope that it will be  useful, *
 * but  WITHOUT ANY WARRANTY;  without even the  implied warranty of *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *
 * General Public License for more details. You should have received *
 * a copy of the GNU General Public License along with this program; *
 * if not,  write to the  Free Software Foundation, Inc.,  59 Temple *
 * Place - Suite 330, Boston, MA 02111-1307, USA.                    *
 *********************************************************************/

#include <gtk/gtkbutton.h>
#include <gtk/gtktogglebutton.h>

void (* clicked_orig) (GtkButton *button);

void gtkada_check_button_install_handler
  (GtkButton *button, void (* clicked) (GtkButton *button))
{
   clicked_orig = GTK_BUTTON_GET_CLASS (button)->clicked;
   GTK_BUTTON_GET_CLASS (button)->clicked = clicked;
}

void gtkada_check_button_clicked (GtkButton *button)
{
   clicked_orig (button);
}

void gtkada_check_button_force_state (GtkToggleButton *button, int state)
{
   button->active = state;
}

