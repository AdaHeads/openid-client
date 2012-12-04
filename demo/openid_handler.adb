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

with
  Ada.Exceptions,
  Ada.Strings.Unbounded;
with
  AWS.Messages,
  AWS.OpenID.Log,
  AWS.OpenID.Manual_Dispatching,
  AWS.Status;
with
  Configuration;

package body OpenID_Handler is
   package OpenID is new AWS.OpenID.Manual_Dispatching
                           (Host_Name       => Configuration.Host_Name,
                            Logged_Out_Page => "");

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      AWS.OpenID.Log.Info
        (Message => "Request URI: <" & AWS.Status.URI (Request) & ">");

      if AWS.Status.URI (Request) = OpenID.Log_In.URI then
         return OpenID.Log_In.Service (Request);
      elsif AWS.Status.URI (Request) = OpenID.Validate.URI then
         return OpenID.Validate.Service (Request);
      elsif AWS.Status.URI (Request) = OpenID.Log_Out.URI then
         return OpenID.Log_Out.Service (Request);
      elsif AWS.Status.URI (Request) = "/error" then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "error.html");
      elsif AWS.Status.URI (Request) = "/style" then
         return AWS.Response.File (Content_Type  => "text/css",
                                   Filename      => "style.css");
      elsif AWS.Status.URI (Request) = "/favicon.ico" then
         AWS.OpenID.Log.Info
           (Message => "Redirecting to <http://www.jacob-sparre.dk/icon>");

         return AWS.Response.Moved ("http://www.jacob-sparre.dk/icon");
      elsif OpenID.Is_Authenticated (Request) then
         return AWS.Response.URL ("http://www.jacob-sparre.dk/?" &
                                  OpenID.Authenticated_As (Request));
      elsif AWS.Status.URI (Request) = OpenID.Logged_Out.URI then
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "index.html");
      else
         return AWS.Response.File (Content_Type  => "text/html",
                                   Filename      => "not_authenticated.html");
      end if;
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "OpenID demo failed at URI <" &
                       AWS.Status.URI (Request) & "> with exception " &
                       Ada.Exceptions.Exception_Name (E) & ": " &
                       Ada.Exceptions.Exception_Message (E));
         raise;
   end Service;
end OpenID_Handler;
