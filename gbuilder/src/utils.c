#include <glib-object.h>

guint ada_properties_count (gpointer klass) {
  return G_OBJECT_CLASS (klass)->n_property_specs;
}

GParamSpec* ada_nth_property (gpointer klass, guint num) {
  return G_OBJECT_CLASS (klass)->property_specs [num];
}

/******************************************
 ** GObjectClass                         **
 ******************************************/

const char* ada_object_class_name (gpointer klass) {
  return G_OBJECT_CLASS_NAME (klass);
}

