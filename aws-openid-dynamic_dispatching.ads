--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  AWS.OpenID.Manual_Dispatching,
  AWS.Services.Dispatchers.URI,
  AWS.Status;

generic
   Host_Name       : String;
   Log_In_Page     : String := "login";
   Return_To_Page  : String := "return_to";
   Logged_In_Page  : String := "logged_in";
   Logged_Out_Page : String := "logged_out";
package AWS.OpenID.Dynamic_Dispatching is
   package Handlers is
     new AWS.OpenID.Manual_Dispatching (Host_Name       => Host_Name,
                                        Log_In_Page     => Log_In_Page,
                                        Return_To_Page  => Return_To_Page,
                                        Logged_In_Page  => Logged_In_Page,
                                        Logged_Out_Page => Logged_Out_Page);

   procedure Register
     (Dispatcher : in out AWS.Services.Dispatchers.URI.Handler);

   function Is_Authenticated (Request : in AWS.Status.Data) return Boolean
     renames Handlers.Is_Authenticated;

   Not_Authenticated : exception renames Handlers.Not_Authenticated;

   function Authenticated_As (Request : in AWS.Status.Data) return String
     renames Handlers.Authenticated_As;
end AWS.OpenID.Dynamic_Dispatching;
