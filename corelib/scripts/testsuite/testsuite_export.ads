
--  Shell registration for the examples

with Scripts;

package Testsuite_Export is

   procedure Register_Functions (Repo : Scripts.Scripts_Repository);
   --  Register the various scripting languages and the functions we export
   --  to them

end Testsuite_Export;
