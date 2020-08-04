
type image ;;

val create_image : int -> int -> float list list -> image ;;

val create_depiction : image -> unit ;;

val filter : (float -> float) -> image -> image ;;

val invert : image -> image ;;

val threshold : float -> image -> image ;;

val dither : image -> image ;;

val error_diffuse : image -> image ;;
