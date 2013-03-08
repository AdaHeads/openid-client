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

with AWS.Status;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.MIME;

with AWS.OpenID.Error_Messages;
with AWS.OpenID.Manual_Dispatching;

with Configuration;

package body OpenID_Handler is

   use AWS.OpenID.Error_Messages;

   package OpenID is new AWS.OpenID.Manual_Dispatching
     (Authentication_Failed => Authentication_Failed'Access,
      Invalid_End_Point     => Invalid_End_Point'Access,
      Invalid_URL           => Invalid_URL'Access,
      Provider_Off_Line     => Provider_Off_Line'Access,
      Host_Name             => Configuration.Host_Name);

   function Index
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is (AWS.Response.File (Content_Type => AWS.MIME.Text_HTML,
                          Filename     => "index.html"));

   function Logged_In
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is (AWS.Response.File (Content_Type => AWS.MIME.Text_HTML,
                          Filename     => "logged_in.html"));

   function Logged_Out
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is (AWS.Response.File (Content_Type => AWS.MIME.Text_HTML,
                          Filename     => "logged_out.html"));

   function Not_Authenticated
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is (AWS.Response.File (Content_Type => AWS.MIME.Text_HTML,
                          Filename     => "not_authenticated.html"));

   function Style_CSS
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is (AWS.Response.File (Content_Type => AWS.MIME.Text_HTML,
                          Filename     => "style.css"));

   ----------------------
   --  Get_Dispatcher  --
   ----------------------

   function Get_Dispatcher
     return AWS.Services.Dispatchers.URI.Handler
   is
   begin
      return D : AWS.Services.Dispatchers.URI.Handler do
         D.Register_Default_Callback
           (AWS.Dispatchers.Callback.Create (Not_Authenticated'Access));

         D.Register (URI => "/", Action => Index'Access);
         D.Register (URI => "/index", Action => Index'Access);
         D.Register (URI => "/index.html", Action => Index'Access);

         D.Register (URI    => OpenID.Log_In.URI,
                     Action => OpenID.Log_In.Callback);

         D.Register (URI    => OpenID.Validate.URI,
                     Action => OpenID.Validate.Callback);

         D.Register (URI    => OpenID.Log_Out.URI,
                     Action => OpenID.Log_Out.Callback);

         D.Register (URI    => OpenID.Logged_In.URI,
                     Action => Logged_In'Access);

         D.Register (URI    => OpenID.Logged_Out.URI,
                     Action => Logged_Out'Access);

         D.Register (URI    => "/style.css",
                     Action => Style_CSS'Access);
      end return;
      --  return D;
   end Get_Dispatcher;

   --------------
   --  Whoops  --
   --------------

   procedure Whoops
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     AWS.Exceptions.Data;
      Answer : in out AWS.Response.Data)
   is
      pragma Unreferenced (E, Log, Error);
   begin
      Answer := AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "error.html",
                                   Status_Code   => AWS.Messages.S500);
   end Whoops;

end OpenID_Handler;
