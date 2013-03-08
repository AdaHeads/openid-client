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

with AWS.Response;
with AWS.Status;

with AWS.OpenID.Security;

package AWS.OpenID.Authentication_Database is

   Not_Authenticated : exception;

   procedure Delete_Identity
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  Delete an identity from the authentication database.

   function Identity
     (Request : in AWS.Status.Data)
      return String;
   --  Return the identity string from the authentication database.

   function Is_Authenticated
     (Request  : in AWS.Status.Data)
      return Boolean;
   --  Return True if the user is authenticated.

   procedure Load
     (File_Name : in String);
   --  Load File_Name into the authentication database.

   procedure Register_Identity
     (Source   : in     AWS.OpenID.Security.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  Register a new identity in the authentication database.

   procedure Save
     (File_Name : in String);
   --  Save the authentication database in File_Name.

end AWS.OpenID.Authentication_Database;
