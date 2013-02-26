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

with AWS.Messages;

package body AWS.OpenID.Error_Messages is

   -----------------------------
   --  Authentication_Failed  --
   -----------------------------

   function Authentication_Failed
     return AWS.Response.Data
   is
   begin
      return AWS.Response.Build
        (Content_Type => "text/html; charset=iso-8859-1",
         Status_Code  => AWS.Messages.S403,
         Message_Body => "<html><head><title>Authentication failed" &
           "</title></head><body></body><h1>" &
           "Authentication failed</h1><p>Your OpenID " &
           "provider failed to authenticate you " &
           "properly.</p></html>");
   end Authentication_Failed;

   -------------------------
   --  Invalid_End_Point  --
   -------------------------

   function Invalid_End_Point
     (URL : in String)
      return AWS.Response.Data
   is
   begin
      return AWS.Response.Build
        (Content_Type => "text/html; charset=iso-8859-1",
         Status_Code  => AWS.Messages.S403,
         Message_Body =>
           "<html><head><title>Invalid end-point</title>" &
           "</head><body></body><h1>Invalid end-point" &
           "</h1><p><q><code>" & URL & "</code></q> " &
           "does not refer to a valid end-point.</p>" &
           "</html>");
   end Invalid_End_Point;

   -------------------
   --  Invalid_URL  --
   -------------------

   function Invalid_URL
     (URL : in String)
      return AWS.Response.Data
   is
   begin
      return AWS.Response.Build
        (Content_Type => "text/html; charset=iso-8859-1",
         Status_Code  => AWS.Messages.S403,
         Message_Body => "<html><head><title>Invalid URL</title>" &
           "</head><body></body><h1>Invalid URL</h1>" &
           "<p><q><code>" & URL & "</code></q> is not " &
           "a valid URL.</p></html>");
   end Invalid_URL;

   -------------------------
   --  Provider_Off_Line  --
   -------------------------

   function Provider_Off_Line
     (URL : in String)
      return AWS.Response.Data
   is
   begin
      return AWS.Response.Build
        (Content_Type => "text/html; charset=iso-8859-1",
         Status_Code  => AWS.Messages.S403,
         Message_Body =>
           "<html><head><title>Provider off-line</title>" &
           "</head><body></body><h1>Provider off-line" &
           "</h1><p>The OpenID provider at <q><code>" &
           URL & "</code></q> seems to be off-line at " &
           "the moment.</p></html>");
   end Provider_Off_Line;

end AWS.OpenID.Error_Messages;
