generic
   type Element is private;

package Unit is

   type Access_Type is limited private;

private
   type Record_Type;
   type Access_Type is access Record_Type;

end Unit;
