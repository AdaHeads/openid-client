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

--  with Security.OpenID;
with AWS.OpenID.Security;

package Authentication_Database is

   Not_Authenticated : exception;

   procedure Delete_Identity
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  TODO: write comment

   function Identity
     (Request : in AWS.Status.Data)
      return String;
   --  TODO: write comment

   function Is_Authenticated
     (Request  : in AWS.Status.Data)
      return Boolean;
   --  TODO: write comment

   procedure Load
     (File_Name : in String);
   --  TODO: write comment

   procedure Register_Identity
     (Source   : in     AWS.OpenID.Security.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  TODO: write comment

   procedure Save
     (File_Name : in String);
   --  TODO: write comment

end Authentication_Database;
