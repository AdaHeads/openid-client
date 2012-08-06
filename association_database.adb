with
  Ada.Containers.Hashed_Maps,
  Ada.Exceptions,
  Ada.Strings.Unbounded,
  Ada.Strings.Unbounded.Hash;
with
  Yolk.Log;

package body Association_Database is
   package Maps is
     new Ada.Containers.Hashed_Maps
           (Key_Type        => Security.OpenID.Association_Handle,
            Hash            => Ada.Strings.Unbounded.Hash,
            Equivalent_Keys => Ada.Strings.Unbounded."=",
            Element_Type    => Security.OpenID.Association,
            "="             => Security.OpenID."=");

   protected Database is
      procedure Clean_Up;
      procedure Insert (Item : in     Security.OpenID.Association);
      function Look_Up (Handle : in Security.OpenID.Association_Handle)
        return Security.OpenID.Association;
   private
      Associations : Maps.Map := Maps.Empty_Map;
   end Database;

   protected body Database is
      procedure Clean_Up is
      begin
         raise Program_Error;
      end Clean_Up;

      procedure Insert (Item : in     Security.OpenID.Association) is
         Key : constant Security.OpenID.Association_Handle
                 := Security.OpenID.Handle (Item);
      begin
         Maps.Insert (Container => Associations,
                      Key       => Key,
                      New_Item  => Item);
      exception
         when others =>
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Failed to insert <" &
                          Security.OpenID.To_String (Item) &
                          "> into the association database with the handle <" &
                          Ada.Strings.Unbounded.To_String (Key) & ">.");
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Current database capacity: " &
                          Maps.Capacity (Associations)'Img);
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Hash value: " &
                          Ada.Strings.Unbounded.Hash (Key)'Img);
            raise;
      end Insert;

      function Look_Up (Handle : in Security.OpenID.Association_Handle)
        return Security.OpenID.Association is
      begin
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Looking <" & Ada.Strings.Unbounded.To_String (Handle) &
                       "> up in the association database.");
         return Maps.Element (Container => Associations,
                              Key       => Handle);
      end Look_Up;
   end Database;

   procedure Clean_Up is
      pragma Inline (Clean_Up);
   begin
      Database.Clean_Up;
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Clean_Up: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Clean_Up;

   procedure Insert (Item : in     Security.OpenID.Association) is
      pragma Inline (Insert);
   begin
      Database.Insert (Item => Item);
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Insert: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Insert;

   function Look_Up (Handle : in Security.OpenID.Association_Handle)
     return Security.OpenID.Association is
      pragma Inline (Look_Up);
   begin
      return Database.Look_Up (Handle => Handle);
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Look_Up: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Look_Up;
end Association_Database;
