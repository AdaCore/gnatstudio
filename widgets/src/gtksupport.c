#include <glib.h>
#include <gtk/gtk.h>

void
ada_g_signal_emit_by_name_int_int (gpointer     instance,
                                   const gchar *detailed_signal,
                                   gint arg1,
                                   gint arg2)
{
  g_signal_emit_by_name (instance, detailed_signal, arg1, arg2);
}

void
ada_gtk_tree_store_set_ptr_ptr (GtkTreeStore *tree_store,
                               GtkTreeIter  *iter,
                               gint          col1,
                               void         *val1,
                               gint          col2,
                               void         *val2)
{
  gtk_tree_store_set (tree_store, iter, col1, val1, col2, val2, -1);
}

void
ada_gtk_tree_store_set_ptr_int (GtkTreeStore *tree_store,
                               GtkTreeIter  *iter,
                               gint          col1,
                               void         *val1,
                               gint          col2,
                               gint          val2)
{
  gtk_tree_store_set (tree_store, iter, col1, val1, col2, val2, -1);
}

void
ada_gtk_tree_store_set_int_int_int (GtkTreeStore *tree_store,
                                   GtkTreeIter  *iter,
                                   gint          col1,
                                   gint          val1,
                                   gint          col2,
                                   gint          val2,
                                   gint          col3,
                                   gint          val3)
{
  gtk_tree_store_set
    (tree_store, iter, col1, val1, col2, val2, col3, val3, -1);
}

void
ada_gtk_tree_store_set_ptr_int_int (GtkTreeStore *tree_store,
                                   GtkTreeIter  *iter,
                                   gint          col1,
                                   void         *val1,
                                   gint          col2,
                                   gint          val2,
                                   gint          col3,
                                   gint          val3)
{
  gtk_tree_store_set
    (tree_store, iter, col1, val1, col2, val2, col3, val3, -1);
}

void
ada_gtk_tree_store_set_ptr_int_ptr (GtkTreeStore *tree_store,
                                   GtkTreeIter  *iter,
                                   gint          col1,
                                   void         *val1,
                                   gint          col2,
                                   gint          val2,
                                   gint          col3,
                                   void         *val3)
{
  gtk_tree_store_set
    (tree_store, iter, col1, val1, col2, val2, col3, val3, -1);
}

void
ada_gtk_tree_store_set_ptr_ptr_int (GtkTreeStore *tree_store,
                                   GtkTreeIter  *iter,
                                   gint          col1,
                                   void         *val1,
                                   gint          col2,
                                   void         *val2,
                                   gint          col3,
                                   gint          val3)
{
  gtk_tree_store_set
    (tree_store, iter, col1, val1, col2, val2, col3, val3, -1);
}

void
ada_gtk_tree_store_set_int_ptr_int (GtkTreeStore *tree_store,
                                   GtkTreeIter  *iter,
                                   gint          col1,
                                   void         *val1,
                                   void         *col2,
                                   void         *val2,
                                   gint          col3,
                                   gint          val3)
{
  gtk_tree_store_set
    (tree_store, iter, col1, val1, col2, val2, col3, val3, -1);
}
