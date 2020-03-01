type 'a typ =
  | TyTop of 'a
  | TyBottom of 'a
  | TyVar of string * 'a
  | TyInt of 'a
  | TyBool of 'a