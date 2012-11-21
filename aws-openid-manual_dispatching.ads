--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  AWS.Response,
  AWS.Status;

private with
  Security.OpenID;
pragma Elaborate (Security.OpenID);

generic
   Host_Name       : String;
   Log_In_Page     : String := "log_in";
   Return_To_Page  : String := "return_to";
   Logged_In_Page  : String := "logged_in";
   Log_Out_Page    : String := "log_out";
   Logged_Out_Page : String := "logged_out";
package AWS.OpenID.Manual_Dispatching is
   Provider_Parameter_Name : constant String := "openid";

   package Log_In is
      URI : constant String := "/" & Log_In_Page;
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Log_In;

   package Validate is
      URI : constant String := "/" & Return_To_Page;
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Validate;

   package Logged_In is
      URI : constant String := "/" & Logged_In_Page;
   end Logged_In;

   package Log_Out is
      URI : constant String := "/" & Log_Out_Page;
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Log_Out;

   package Logged_Out is
      URI : constant String := "/" & Logged_Out_Page;
   end Logged_Out;

   function Is_Authenticated (Request : in AWS.Status.Data) return Boolean;

   Not_Authenticated : exception;

   function Authenticated_As (Request : in AWS.Status.Data) return String;
end AWS.OpenID.Manual_Dispatching;
