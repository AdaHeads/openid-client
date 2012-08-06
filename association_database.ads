with
  Security.OpenID;

package Association_Database is
   procedure Clean_Up;

   procedure Insert (Item : in     Security.OpenID.Association);

   function Has (Handle : in Security.OpenID.Association_Handle) return Boolean;

   function Look_Up (Handle : in Security.OpenID.Association_Handle)
     return Security.OpenID.Association;
end Association_Database;
