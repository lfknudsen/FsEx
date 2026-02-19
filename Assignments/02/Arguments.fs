module a02.Arguments

open System

/// Encapsulates parameter information for printing help information.
/// More readable than (char * string * string).
type private Param =
    struct
        /// The single-character initial (e.g. v)
        val short : char
        /// The long-form parameter name excl. leading dashes (e.g. verbose)
        val long : string
        /// The help-description for what this parameter does.
        val desc : string

        new (short, long, desc) = {
                short = short;
                long = long;
                desc = desc
        }

        override this.ToString() =
            "\t-" + string this.short + "\n\t--" + string this.long + ":\t\t" + string this.desc + "\n"
    end

type private Params =
    class
        static member parameters = [
            Param('c', "continue",
                "Continue when assertions fail.")
            Param('b', "break",
                "Stop if an assertion fails and print the exception message.")
            Param('v', "verbose",
                "Show the results of the individual assertions.")
            Param('q', "quiet",
                "Override all other options, and print nothing.")
            Param('l', "list",
                "Print a list of the failed test cases at the end.")
            Param('f', "failures",
                "Only show failed test cases. Implies -v.")
            Param('h', "help",
                "Print this help text and exit.")
        ]

        /// Pretty print usage information.
        static member printHelp =
            Console.WriteLine("Usage: dotnet run -- [<Options>...]\n\nOptions (all case-insensitive):\n")

            let rec _printParameters (parameters : Param list) =
                match parameters with
                | [] -> ()
                | head :: tail ->
                    Console.WriteLine(head)
                    _printParameters tail

            _printParameters Params.parameters

        static member printInvalidArgument arg =
            Console.WriteLine("Invalid argument '" + arg +
                              "'\nExecute with argument -h or --help to see usage information.")
    end

/// Data structure for holding command-line argument information.
type private ConfigBuilder() =
    class
        let mutable _continueOnFailure = true
        let mutable _verbose = true
        let mutable _quiet = false
        let mutable _printFailedTestIndices = false
        let mutable _showOnlyFailures = false

        let _parse (args : string array) =
            let upper = args.Length - 1
            for i = 0 to upper do
                match args[i].ToLower() with
                    | "-c"
                    | "--continue" ->   _continueOnFailure <- true
                    | "-b"
                    | "--break" ->      _continueOnFailure <- false
                    | "-v"
                    | "--verbose" ->    _verbose <- false
                    | "-q"
                    | "--quiet" ->      _quiet <- true
                    | "-l"
                    | "--list" ->       _printFailedTestIndices <- true
                    | "-f"
                    | "--failures" ->   _showOnlyFailures <- true
                    | "-h"
                    | "--help" ->
                        Params.printHelp
                        exit 0
                    | _ ->
                        Params.printInvalidArgument args[i]
                        exit -1

        member private this.setFields (args : string array) =
            _parse args
            this

        /// If true, a failed assertion does not throw an exception.
        /// Default is true. Override with --break or -b.
        member this.continueOnFailure = _continueOnFailure

        /// If true, print all results, not just the cumulative final results.
        /// Default is true. Override with --verbose or -v.
        member this.verbose = _verbose

        /// If true, print absolutely nothing, overriding any other flags.
        /// Default is false. Override with --quiet or -q.
        member this.quiet = _quiet

        /// If true, end by printing a list of test numbers which failed.
        /// Defaults to false. Override during execution with -l
        member this.printFailedTests = _printFailedTestIndices

        /// Print only the results of tests which failed.
        /// Implies -v.
        /// Defaults to false. Override during execution with -f.
        member this.showOnlyFailures = _showOnlyFailures

        /// Parse command-line arguments and return the result as a new Config instance.
        /// If -h or --help is encountered, will print usage information and exit with
        /// code 0.
        /// If an unrecognised argument is encountered, will print an error message and
        /// exit with code -1.
        static member parse(args : string array) : ConfigBuilder =
            if args.Length > 0 then
                ConfigBuilder().setFields(args)
            else
                ConfigBuilder()

    end

/// Data structure for holding command-line argument information.
type Config =
    struct
        val continueOnFailure : bool
        val verbose : bool
        val quiet : bool
        val printFailedTestNumbers : bool
        val showOnlyFailures : bool

        private new (c : ConfigBuilder) =
            {
                continueOnFailure = c.continueOnFailure
                verbose = c.verbose
                quiet = c.quiet
                printFailedTestNumbers = c.printFailedTests
                showOnlyFailures = c.showOnlyFailures
            }

        new (args : string array) =
            Config(ConfigBuilder.parse args)

        static member parse (args : string array) =
            Config(args)

        override this.ToString() =
            "-c: " + string this.continueOnFailure + "\n" +
            "-v: " + string this.verbose + "\n" +
            "-q: " + string this.quiet + "\n" +
            "-l: " + string this.printFailedTestNumbers + "\n" +
            "-f: " + string this.showOnlyFailures + "\n"
    end
