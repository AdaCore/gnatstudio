#include <gtk/gtktreestore.h>
#include <gtk/gtktreesortable.h>
#include <gtk/gtkenums.h>

gint ada_gtk_tree_view_freeze_sort (GtkTreeStore* tree) {
  gint save = tree->sort_column_id;
  tree->sort_column_id = -2;
  return save;
}

void ada_gtk_tree_view_thaw_sort (GtkTreeStore* tree, gint id) {
  gtk_tree_sortable_set_sort_column_id
    (GTK_TREE_SORTABLE (tree), id, tree->order);  
}
