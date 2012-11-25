with
  AWS.Response;
with
  Security.OpenID;

private
package AWS.OpenID.Error_Messages is
   function Invalid_URL (URL : in String) return AWS.Response.Data;
   function Invalid_End_Point (URL : in String) return AWS.Response.Data;
   function Provider_Off_Line (URL : in String) return AWS.Response.Data;
   function Authentication_Failed return AWS.Response.Data;
end AWS.OpenID.Error_Messages;
