-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with AWS.Dispatchers.Callback;
with AWS.Response;
with AWS.Status;

generic

   Authentication_Failed : AWS.Response.Callback;
   --  Is called when the OpenID authentication has failed.

   Host_Name             : String;
   --  The hostname. This is is appended to Protocol and the resulting string
   --  prefixed to the various Log* strings to create the URL's needed to
   --  handle an OpenID authentication request.

   Invalid_End_Point     : AWS.Response.Callback;
   --  Is called when the OpenID end point is bad.

   Invalid_URL           : AWS.Response.Callback;
   --  Is called when the OpenID provider URL isn't valid.

   Log_In_Page           : String := "/log_in";
   --  This is where the OpenID login procedure starts.

   Logged_In_Page        : String := "/logged_in";
   --  We're redirected to this page when the OpenID authentication succeeded.

   Log_Out_Page          : String := "/log_out";
   --  This is where we go to log out.

   Logged_Out_Page       : String := "/logged_out";
   --  We're redirected here when a log out request succeeded.

   Protocol              : String := "https://";
   --  Protocol is prefixed to Host_Name to build the full URL.

   Provider_Off_Line     : AWS.Response.Callback;
   --  Is called when the OpenID provider is offline.

   Return_To_Page        : String := "/return_to";
   --  The page we return to after the OpenID authentication is completed.

package AWS.OpenID.Manual_Dispatching is

   Not_Authenticated : exception;

   package Log_In is
      URI : constant String := Log_In_Page;

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --  TODO: write comment

      Callback : constant AWS.Dispatchers.Callback.Handler
        := AWS.Dispatchers.Callback.Create (Service'Access);
   end Log_In;

   package Validate is
      URI : constant String := Return_To_Page;

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --  Upon successful completion of OpenID login, add the user to the
      --  authentication database and forward to the Logged_In.URI. On
      --  failure ???????

      Callback : constant AWS.Dispatchers.Callback.Handler
        := AWS.Dispatchers.Callback.Create (Service'Access);
   end Validate;

   package Logged_In is
      URI : constant String := Logged_In_Page;
   end Logged_In;

   package Log_Out is
      URI : constant String := Log_Out_Page;

      function Service
        (Request : in AWS.Status.Data)
         return AWS.Response.Data;
      --  Delete the user from the authentication database and forward to
      --  Logged_Out.URI.

      Callback : constant AWS.Dispatchers.Callback.Handler
        := AWS.Dispatchers.Callback.Create (Service'Access);
   end Log_Out;

   package Logged_Out is
      URI : constant String := Logged_Out_Page;
   end Logged_Out;

   function Authenticated_As
     (Request : in AWS.Status.Data)
      return String;
   --  Return the identity of the user.

   function Is_Authenticated
     (Request : in AWS.Status.Data)
      return Boolean;
   --  Return True if the user is authenticated.

end AWS.OpenID.Manual_Dispatching;
