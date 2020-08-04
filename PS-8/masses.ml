(*
                          CS51 Problem Set 8
                       Point masses with forces
                         -.-. ... ..... .----
 *)

open Points ;;
     
let cFRAMESIZE = 500 ;;

class mass =
  let currid = ref 0 in

  fun (initx : float) (inity : float) (m : float) ->
    object (this)
           
      (* A mass is located at a point *)
      inherit point initx inity as super
                                     
      (* Unique identifier for the mass *)
      val id = currid := !currid + 1; !currid
      method get_id = id
                        
      (* The mass itself *)
      val mass = m

      (*..............................................................
      Your part goes here: Provide the implementations of the move and
      restore_pos methods. The move method should differ from the
      corresponding method for points in that the coordinates that the
      mass moves to should be "clipped" to stay within a square frame,
      as defined by cFRAMESIZE above. That is, the move will stay
      within 0 and cFRAMESIZE in both dimensions.
      ..............................................................*)

      (* Forces on the mass *)
      val frc : point = new point 0. 0.    (* accumulator for forces *)
      method set_force (p: point) : unit =
        frc#move p
      method reset_force : unit =
        this#set_force (new point 0. 0.)
      method get_force = frc
      method add_force p =
        frc#move (frc#plus p)
      method apply_force =
        this#move (this#plus (this#get_force#scale (1. /. mass)));
        this#reset_force
      method scale_force factor =
        this#set_force (this#get_force#scale factor)
                       
      (* I/O methods *)
      method reveal =
        let x, y = this#round in
        Graphics.draw_circle x y 3

      method describe =
        let x, y = this#pos in
        Printf.printf "Mass %d: %f, %f\n%!" this#get_id x y
    end

(*======================================================================
Reflection on the problem set

     Please fill out the information about time spent and your
     reflection thereon in the file points.ml.
 *)
