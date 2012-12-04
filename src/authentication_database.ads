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
   AWS.Response,
   AWS.Status;
with
  Security.OpenID;

package Authentication_Database is
   Not_Authenticated : exception;

   procedure Register_Identity
     (Source   : in     Security.OpenID.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);

   function Is_Authenticated (Request  : in AWS.Status.Data) return Boolean;

   function Identity (Request : in AWS.Status.Data) return String;

   procedure Delete_Identity (Request  : in     AWS.Status.Data;
                              Response : in out AWS.Response.Data);

   procedure Save (File_Name : in     String);
   procedure Load (File_Name : in     String);
end Authentication_Database;
