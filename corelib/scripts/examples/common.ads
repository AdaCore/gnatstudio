
--  Shell registration for the examples

with Scripts;

package Common is

   function Register_Scripts_And_Functions return Scripts.Scripts_Repository;
   --  Register the various scripting languages and the functions we export
   --  to them

end Common;
