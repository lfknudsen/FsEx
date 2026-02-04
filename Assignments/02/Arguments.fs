module a02.Arguments

/// Struct for holding command-line argument information.
type Config() =
    class
        let mutable _continueOnFailure = true
        let mutable _verbose = true
        let mutable _printFailedTestIndices = false
        let mutable _showOnlyFailures = false

        member this.continueOnFailure = _continueOnFailure
        member this.verbose = _verbose
        member this.printFailedTestIndices = _printFailedTestIndices
        member this.showOnlyFailures = _showOnlyFailures


        member this.setContinue b =
            _continueOnFailure <- b
            this

        member this.setVerbose b =
            _verbose <- b
            this

        member this.setPrintFailedTestIndices b =
            _printFailedTestIndices <- b
            this

        member this.setShowOnlyFailures b =
            _showOnlyFailures <- b
            this
    end

let parameters (config: Config) =
    [ ('c', "continue", config.setContinue, true, "Continue when assertions fail.")
      ('b',
       "break",
       config.setContinue,
       false,
       "Stop if an assertion fails and print the exception message.")
      ('v', "verbose", config.setVerbose, true, "Show the results of the individual assertions.")
      ('q', "quiet", config.setVerbose, false, "Show only the cumulative results.")
      ('l',
       "list",
       config.setPrintFailedTestIndices,
       true,
       "Print a list of the failed test cases at the end.") ]

let rec _printParameters (parameters: ('a * 'b * 'c * 'd * 'e) list) =
    match parameters with
    | [] -> ()
    | (alias, command, _, _, description) :: tail ->
        System.Console.WriteLine(
            "-" + string alias + "\n" + "--" + string command + ":\t\t" + description
        )

        _printParameters tail

let printHelp config =
    System.Console.WriteLine("Command line arguments:\n")
    _printParameters (parameters config)
    config


/// Parse command-line arguments and return the result as a Config struct.
let rec _parseArgs args config : Config =
    match args with
    | [] -> config
    | head :: tail ->
        match head with
        | "-c"
        | "--continue" -> _parseArgs tail (config.setContinue true)
        | "-b"
        | "--break" -> _parseArgs tail (config.setContinue false)
        | "-v"
        | "--verbose" -> _parseArgs tail (config.setVerbose true)
        | "-q"
        | "--quiet" -> _parseArgs tail (config.setVerbose false)
        | "-l"
        | "--list" -> _parseArgs tail (config.setPrintFailedTestIndices true)
        | "-f"
        | "--failures" -> _parseArgs tail (config.setShowOnlyFailures true)
        | "-h"
        | "--help" -> printHelp config
        | _ -> _parseArgs tail config

/// Parse command-line arguments and return the result as a Config struct.
let parseArgs args = _parseArgs args (Config())
