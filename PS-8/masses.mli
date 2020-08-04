(*
                       Point masses with forces
                          CS51 Problem Set 8
                         -.-. ... ..... .----
 *)

open Points ;;

(* Bound the locations of the masses within a frame of fixed size *)
val cFRAMESIZE : int

(* Class: mass 
   Arguments: initx : float	 initial x
              inity : float      ...and y positions of the mass
              m : float          the "physical" mass of the mass for computing 
                                 force effects
                                                        
   A mass is a point that has a mass value associated with it and can
   have forces act upon it. Masses can move based on the forces, but
   store their most recent position so that that position can be
   restored. *)
                   
class mass : float -> float -> float -> object
  (* A mass is located at a point *)
  inherit point 

  (* Masses have distinct integer ids *)
  method get_id : int

  (* The mass itself *)
  val mass : float

  (*** Movement of the mass ***)
	       
  (* move p -- Moves mass directly to the position of p, storing
     previous position for possible later restoring, clipping to frame
     boundary while we're at it. *)
  method move : point -> unit
  (* restore_pos -- Restore position of mass to the single previous stored
     position. Multiple restore points are not supported. That is, if
     you move from position p1 to p2 and then move again to position
     p3, restore_pos will get you back to p2, but restoring again will
     not get you back to p1. Rather, it will return you to p3. *)
  method restore_pos : unit
                                                          
  (*** Forces on the mass ***)

  (* set_force p -- Sets force on the mass to the vector given by p *)
  method set_force : point -> unit
  (* reset_force -- Resets the force on the mass to 0 *)
  method reset_force : unit
  (* get_force -- Returns the force on the mass *)
  method get_force : point
  (* add_force p -- Adds to the existing force on the mass a force
     specified by p *)
  method add_force : point -> unit
  (* apply_force -- Applies the current force to the mass, moving it
     accordingly, and resetting the force to 0 *)
  method apply_force : unit
  (* scale_force factor -- Scales the current force by the provided
     multiplicative factor *)
  method scale_force : float -> unit

  (*** I/O methods ***)

  (* Draws an indicator of the mass location on the canvas *)
  method reveal : unit
  (* Prints a textual description of the object on stdout *)
  method describe : unit
end
