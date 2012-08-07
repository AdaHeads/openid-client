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
   Host_Name         : String;
   Logged_In_URI     : String := "/logged_in";
   Token_Lifetime    : Duration := 3600.0;
package AWS.OpenID.Manual_Dispatching is
   Provider_Parameter_Name : constant String := "openid";
   Token_Cookie_Name       : constant String := "token";

   package Log_In is
      URI : constant String := "/login";
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Log_In;

   package Validate is
      URI : constant String := "/return_to";
      function Service (Request : in AWS.Status.Data) return AWS.Response.Data;
   end Validate;

   function Is_Authenticated (Request : in AWS.Status.Data) return Boolean;

   Not_Authenticated : exception;

   function Authenticated_As (Request : in AWS.Status.Data) return String;
end AWS.OpenID.Manual_Dispatching;
