(* A few utility functions. *)
module a02.Strings

open System

/// Returns 'message' as a string prefixed with spaces so that the result is at least
/// 'width' characters long.
/// The prefix string of spaces is interned.
let prefixSpace (message: 'a) (width: int) =
    let stringVersion = string message

    if width <= 0 || stringVersion.Length >= width then
        stringVersion
    else
        let diff = width - stringVersion.Length
        if diff <= 0 then
            stringVersion
        else
            let prefix = String.Intern(String.replicate diff " ")
            prefix + stringVersion

let rec _formatList lst delimiter =
    match lst with
    | [] -> ""
    | [ head ] -> string head
    | [ first; last ] -> string first + delimiter + string last
    | head :: neck :: tail -> string head + delimiter + string neck + _formatList tail delimiter

/// Formats the full contents of a list for printing.
/// 'delimiter' refers to the string that will be placed in-between elements,
/// such as ", " or "\n".
let formatList lst delimiter =
    match lst with
    | [] -> "[]"
    | [ head ] -> "[" + string head + "]"
    | _ -> "[" + _formatList lst delimiter + "]"