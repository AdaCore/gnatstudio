generic
   type Element_Type is private;
   --  with procedure Delete_Element (el : Element_Type);

package Simple_Vector is

   -------------------------------------------------------------------------

   type Node;
   type Node_Access is access Node;
   type Node is
      record
         data : Element_Type;
         next : Node_Access;
      end record;


   -------------------------------------------------------------------------

   procedure Append (Item : in Element_Type; Root : in out Node_Access);
   --  Appends new item at the beginning of the list. If root is null then
   --  a new list is created.

   procedure Append (Root : in out Node_Access; Item : in Element_Type);
   --  Appends new item to the end of the list. If root is null then a new
   --  list is created.

   function Get_Element_At (Root : in Node_Access; Index : in Integer)
         return Element_Type;
   --  Returns element with specified Index. If such element is not
   --  available then exception is raised. Starting index is 1 for the
   --  first element.

   function Size (Root : in Node_Access) return Integer;
   --  Returns size of the Vector

   procedure Release_Vector (Root : in out Node_Access);

   Element_Not_Found : exception;

   --  Other methods are welcome

   -------------------------------------------------------------------------

end Simple_Vector;
