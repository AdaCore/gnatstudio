--  A generic simple linked list, with an efficient sorting function.

with Unchecked_Deallocation;

generic
   type Data_Type (<>) is private;

package Generic_List is

   type List is private;

   type Comparison is access
     function (Arg1, Arg2 : Data_Type) return Boolean;

   procedure Prepend
     (L    : in out List;
      Item : Data_Type);
   --  Add an item at the beginning of a list. The cost is O(1).

   procedure Append
     (L    : in out List;
      Item : Data_Type);
   --  Add an item at the end of a list. The cost is O(n) !

   function Is_Empty (L : List) return Boolean;
   --  True if L does not contain any element.

   --  function Is_Last (L : List) return Boolean;

   procedure Sort
     (L        : in out List;
      Inferior : Comparison);
   --  Sorts a List. The cost is O(n*log(n)).
   --  Inferior is a Comparison that returns True if Arg1 is strictly
   --  inferior to Arg2.

   procedure Free (L : in out List);
   --  Free memory associated to L.

   function Tail (L : List) return List;
   --  Return the list following the first element.
   --  Raise List_Empty if L is empty.

   function Head (L : List) return Data_Type;
   --  Return the first element contained in the list.
   --  Raise List_Empty if L is empty;

   List_Empty : exception;

private

   type List_Node;
   type List is access List_Node;

   type Data_Access is access Data_Type;

   type List_Node is record
      Element : Data_Access;
      Next    : List;
   end record;

   procedure Free is new Unchecked_Deallocation (Data_Type, Data_Access);
   procedure Free_Node is new Unchecked_Deallocation (List_Node, List);

end Generic_List;
