(**************************************************************************)
(*                                                                        *)
(* Omega: a solver of quantifier-free problems in Presburger Arithmetic   *)
(*                                                                        *)
(* Pierre Cr�gut (CNET, Lannion, France)                                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

Grammar vernac vernac :=
  omega_sett [ "Set" "Omega" "Time" "." ] -> [(OmegaFlag "+time")]
| omega_sets [ "Set" "Omega" "System" "." ] -> [(OmegaFlag "+system")]
| omega_seta [ "Set" "Omega" "Action" "." ] -> [(OmegaFlag "+action")]
| omega_unst [ "Unset" "Omega" "Time" "." ] -> [(OmegaFlag "-time")]
| omega_unss [ "Unset" "Omega" "System" "." ] -> [(OmegaFlag "-system")]
| omega_unsa [ "Unset" "Omega" "Action" "." ] -> [(OmegaFlag "-action")]
| omega_swit [ "Switch" "Omega" "Time" "." ] -> [(OmegaFlag "time")]
| omega_swis [ "Switch" "Omega" "System" "." ] -> [(OmegaFlag "system")]
| omega_swia [ "Switch" "Omega" "Action" "." ] -> [(OmegaFlag "action")]
| omega_set  [ "Set" "Omega" stringarg($id) "." ] -> [(OmegaFlag $id)].


Grammar tactic simple_tactic :=
  omega [ "Omega" ] -> [(Omega)].

Syntax tactic level 0:
  omega [(Omega)] -> ["Omega"].
