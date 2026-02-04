(* A few utility functions. *)
module a02.Strings

/// Returns 's' as a string prefixed with spaces so that the result is at least
/// 'width' characters long.
let prefixSpace s (width: int) =
    let str = string s

    if width <= 0 || str.Length >= width then
        str
    else
        let diff = width - str.Length
        let prefix = String.init diff (fun _ -> " ")
        prefix + str

let rec _formatList lst delimiter =
    match lst with
    | [] -> ""
    | [ head ] -> string head
    | [ first; last ] -> string first + delimiter + string last
    | head :: neck :: tail -> string head + delimiter + string neck + _formatList tail delimiter

/// Prints the full contents of a list.
/// 'delimiter' refers to the string that will be placed in-between elements,
/// such as ", " or "\n".
let formatList lst delimiter =
    match lst with
    | [] -> "[]"
    | [ head ] -> "[" + string head + "]"
    | _ -> "[" + _formatList lst delimiter + "]"