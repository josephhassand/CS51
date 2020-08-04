open Image ;;



let mona =
  create_image 250 360 Monalisa.image in create_depiction mona;
  create_depiction (threshold 0.75 mona);
  create_depiction (dither mona);
  create_depiction (error_diffuse mona) ;;
