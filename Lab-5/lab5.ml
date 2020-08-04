(*
                              CS51 Lab 5
           Variants, algebraic types, and pattern matching
 *)

(*
Objective:

In this lab you'll practice concepts of algebraic data types,
including product types (like tuples and records) and sum types (like
variants), and the expressive power that arises from combining both. A
theme will be the requirement of and checking for consistency with
invariants.

NOTE: Since we ask that you define types in this lab, you must
complete certain exercises before this will compile with the testing
framework on the course grading server. Exercises 1, 2, 6, and 9 are
required for full compilation. If you want to "peek" at the right type
definitions, you can check out <http://url.cs51.io/lab5-1>.  *)

(*======================================================================
Part 1: Colors as an algebraic data type

In this lab you'll use algebraic data types to create several data
structures.

Ultimately, you'll define several types and structures that allow you
to create a family tree. To do this, you need to create a type to
store a set of biographical information about a person, like
name, birthdate, and favorite color. This set of data is
different from the enrollment data from the prior lab, so you'll need
to create a new type.

You might be tempted to do something simple like

  type person = { name : string;
                  favorite : string;
                  birthday : string } ;;

Let's consider why this may not be appropriate by evaluating the type
for each record field individually.

First, it seems reasonable for a name to be a string, so let's declare
that complete and move on.

The "favorite" field is more problematic. Although we named it such
for simplicity, it doesn't convey very well that we intended for this
field to represent a person's favorite *color*. This could be resolved
with some documentation, but is not enforced at any level other than
hope. Next, it's very likely that many persons would select one of a
subset of colors. Let's fix this issue first.

........................................................................
Exercise 1: Define a new type, called `color_label`, whose value can
be any of the following: red, crimson, orange, yellow, green,
blue, indigo, or violet.
......................................................................*)

type color_label =
  | Red
  | Crimson
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet
;;

(* This is a good start, but doesn't allow for definition of all of
the colors that, say, a computer display might be able to
present. Let's make it more usable.

One of the most commonly used methods of representing color in digital
devices is as an "RGB" value: a triplet of values to represent red,
green, and blue components that, through additive mixing, produce the
wide array of colors our devices render.

Commonly, each of the red, green, and blue values are made up of a
single 8-bit (1-byte) integer. Since one byte represents 256 discrete
values, there are over 16.7 million (256 * 256 * 256) possible colors
that can be represented with this method.

The three components that make up an RGB color are referred to as
"channels". In this 8-bit-per-channel model, a value of 0 represents
no color and a value of 255 represents the full intensity of that
color. Some examples:

     R  |  G  |  B  | Color
    ----|-----|-----|------------
    255 |   0 |   0 | Red
      0 |  64 |   0 | Dark green
      0 | 255 | 255 | Cyan
    164 |  16 |  52 | Crimson
    255 | 165 |   0 | Orange
    255 | 255 |   0 | Yellow
     75 |   0 | 130 | Indigo
    240 | 130 | 240 | Violet

........................................................................
Exercise 2: Define a color type that supports either `Simple` colors
(from the `color_label` type you defined previously) or `RGB` colors,
which would incorporate a tuple of values for the three color
channels. You'll want to use `Simple` and `RGB` as the value
constructors in this new variant type.
......................................................................*)

type color =
  | Simple of color_label
  | RGB of int * int * int ;;

(* There is an important assumption about the RGB values that
determine whether a color is valid or not. The RGB type presupposes an
*invariant*, that is, a condition that we assume to be true in order
for the type to be valid:

  The red, green, and blue channels must be a non-negative
  8-bit int. Therefore, each channel must be in the range [0, 255].

Since OCaml, unlike some other languages, does not have native support
for unsigned 8-bit integers, you should ensure the invariant remains
true in your code. (You might think to use the OCaml `char` type --
which is an 8-bit character -- but this would be an abuse of the
type. In any case, thinking about invariants will be useful practice
for upcoming problem sets.)

We'll want a function to validate the invariant for RGB color
values. There are several approaches to building such functions,
which differ in their types, and for which we'll use different naming
conventions:

* valid_rgb : color -> bool -- Returns true if the color argument
  is valid, false otherwise.

* validated_rgb : color -> color -- Returns its argument unchanged if
  it is a valid color, and raises an appropriate exception otherwise.

* validate_rgb : color -> unit -- Returns unit; raises an appropriate
  exception if its argument is not a valid color.

The name prefixes "valid_", "validated_", and "validate_" are intended
to be indicative of the different approaches to validation.

In this lab, we'll use the "validated_" approach and naming
convention, though you may want to think about the alternatives. In
the next lab, we use the "valid_" alternative.

........................................................................
Exercise 3: Write a function `validated_rgb` that accepts a color and
returns that color unchanged if it's valid. However, if its argument
is not a valid color (that is, the invariant is violated), it raises
an `Invalid_color` exception (defined below) with a useful message.
......................................................................*)

exception Invalid_color of string ;;

let validated_rgb (input : color) : color =
  match input with
  | Simple color -> input
  | RGB (r,g,b) -> if (r >= 0 && r <= 255) && (b >= 0 && b <= 255) &&
                      (g >= 0 && g <= 255) then input else
      raise (Invalid_color "Your input was wrong");;

(*......................................................................
Exercise 4: Write a function `make_color` that accepts three integers
for the channel values and returns a value of the color type. Be sure
to verify the invariant.
......................................................................*)

let make_color (r: int) (b: int) (g: int) : color =
  if (r >= 0 && r <= 255) && (b >= 0 && b <= 255) &&
     (g >= 0 && g <= 255) then RGB(r,b,g) else
    raise (Invalid_color "Your input was wrong");;

(*......................................................................
Exercise 5: Write a function `convert_to_rgb` that accepts a color and
returns a 3-tuple of ints representing that color. This is trivial for
`RGB` colors, but not quite so easy for the hard-coded `Simple`
colors. Fortunately, we've already provided RGB values for simple
colors in the table above.
......................................................................*)

let convert_to_rgb (input: color) : int * int * int =
  match input with
  | RGB (a,b,c) -> (a,b,c)
  | Simple Red -> (255, 0, 0)
  | Simple Crimson -> (164,  16,  52)
  | Simple Orange  -> (255, 165,   0)
  | Simple Yellow  -> (255, 255,   0)
  | Simple Green   -> (  0, 255,   0)
  | Simple Blue    -> (  0,   0, 255)
  | Simple Indigo  -> ( 75,   0, 130)
  | Simple Violet  -> (240, 130, 240) ;;

(*======================================================================
Part 2: Dates as a record type

Now let's move on to the last data type that will be used in the
biographical data type: the date field.

Above, we naively proposed a string for the date field. Does this make
sense for this field? Arguably not, since it will make comparison and
calculation extremely difficult.

Dates are frequently needed data in programming, and OCaml (like many
languages) supports them through a library module, the `Date` module.
Normally, we would reduce duplication of code by relying on that
module (the edict of irredundancy), but for the sake of practice
you'll develop your own simple version.

........................................................................
Exercise 6: Create a type `date` that supports values for years,
months, and days. First, consider what types of data each value should
be. Then, consider the implications of representing the overall data
type as a tuple or a record.
......................................................................*)

type date = {year: int; month: int; day: int } ;;

(* After you've thought it through, look up the `Date` module in the
OCaml documentation to see how this was implemented there. If you
picked differently, why did you choose that way? Why might the `Date`
module have implemented this data type as it did?

........................................................................
Exercise 7: Change your `date` data type, above, to implement it in a
manner identical to the `Date` module, but only with fields for year,
month, and day. If no changes are required...well, that was easy.
........................................................................

Like the color type, above, date values obey invariants. In fact, the
invariants for this type are more complex: we must ensure that days
fall within an allowable range depending on the month, and even on the
year.

The invariants are as follows:

- For our purposes, we'll only support non-negative years.

- January, March, May, July, August, October, and December have 31
  days.

- April, June, September, and November have 30 days.

- February has 28 days in common years, 29 days in leap years.

- Leap years are years that can be divided by 4, but not by 100,
  unless by 400.

You may find Wikipedia's leap year algorithm pseudocode useful:
<https://en.wikipedia.org/wiki/Leap_year#Algorithm>

........................................................................
Exercise 8: Create a `validated_date` function that raises
`Invalid_date` if the invariant is violated, and returns the date if
valid.
......................................................................*)

exception Invalid_date of string ;;

let leap (input:date) : bool =
  if (input.year mod 4 = 0 && input.year mod 100 <> 0) || input.year mod 400 = 0
  then true else false;;

let validated_date (input : date) : date =
  if input.year < 0 then raise (Invalid_date "Invalid year") else
  let max_days = match input.month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 ->  30
    | 2 -> if leap input then 29
          else 28
    | _ -> raise (Invalid_date "Invalid month") in
  if input.day > max_days then raise (Invalid_date "Day too large for month")
  else if input.day < 1 then raise (Invalid_date "Can't have negative days")
  else input ;;

(*======================================================================
Part 3: Family trees as an algebraic data type

Now, combine all of these different types to define a person record,
with a name, a favorite color, and a birthdate.

........................................................................
Exercise 9: Define a `person` record type. Use the field names `name`,
`favorite`, and `birthdate`.
......................................................................*)

type person = {name: string; favorite: color; birthdate: date} ;;

(* As a final augmentation to our modeling of individual persons,
we'll extend the model to allow for family trees. In this simple case,
we'll ignore issues of multiple parents and such. Instead, we'll just
add a further field `children` to the `person` type to store a list of
children. We'll call this extended type `family`. *)

type family = { person : person; children : family list } ;;

(* Let's now write a series of functions to build these family trees.

........................................................................
Exercise 10: Write a function that accepts a name, a color, and a
date, and returns a `family` consisting of a single person with no
children. If you completed the validity functions that ensure the
invariants are preserved for color and date, use them here as well.
......................................................................*)

let new_family (name: string) (color: color) (date: date) : family =
  { person = {name; favorite = validated_rgb color; birthdate = validated_date date};
    children = []}
;;

(*......................................................................
Exercise 11: Write a function `add_child` that takes two values of
type `family` and returns a new family with the latter added as a
child of the former.
......................................................................*)

let add_child ({person;children}: family) (child: family) : family =
  {person; children = child :: children}

(* Using the functions you've defined, the following code should build
a small family tree: *)

let the_ks =
  let kris = new_family "Kris"
                        (Simple Blue)
                        {year = 1955; month = 11; day = 5} in
  let kim = new_family "Kim"
                       (Simple Yellow)
                       {year = 1980; month = 10; day = 21} in
  let kourtney = new_family "Kourtney"
                            (Simple Red)
                            {year = 1979; month = 4; day = 18} in
  let north = new_family "North"
                         (Simple Indigo)
                         {year = 2013; month = 6; day = 15} in
  kris
  |> add_child (kourtney
                |> add_child north)
  |> add_child kim ;;

(*......................................................................
Exercise 12: Write a function `family_size` that returns a count of
the number of people in a family.
......................................................................*)

let rec count_people ({children; _} : family) : int =
  let open List in
  1 + fold_left (+) 0 (map count_people children) ;;
