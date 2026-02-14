module a02.State

open System

/// Holds the state of this programme.
type TestState(config: Arguments.Config, testNumberDigits: int) =
    class
        /// The number of successful tests.
        let mutable _successes = 0u

        /// The last test number to be executed. When all tests are done,
        /// this represents the total number of tests.
        let mutable _lastTestNumber = 0u

        /// Contains the test numbers of failed tests. Will be printed at the
        /// end if 'config.printFailedTestNumbers' is true.
        let mutable _failedTests = []

        /// Holds the name of the current section of tests. Used to delineate
        /// tests to make results easier to understand.
        let mutable _region = ""

        /// If true, the next call to TestState.print will also print the
        /// name of the current region.
        let mutable _regionHasChangedSinceLastPrint = false

        /// Used for aligning test numbers displayed when printing individual
        /// test results. If the current test number has fewer digits
        /// than this, whitespace will be prefixed so that it reaches this
        /// amount of characters.
        let _testNumberDigits = testNumberDigits

        /// The number of successful tests.
        member this.successes = _successes

        /// The last test number to be executed. When all tests are done,
        /// this represents the total number of tests.
        member this.lastTestNumber = _lastTestNumber

        new (config : Arguments.Config) =
            TestState(config, 4)

        /// Updates test number and prints it to the standard output.
        /// If the region has been updated since last, the new region name
        /// is also printed.
        member this.nextTest() =
            _lastTestNumber <- _lastTestNumber + 1u

        member private this.printRegion() =
            if _regionHasChangedSinceLastPrint then
                Console.WriteLine("--- " + _region + ":")
                _regionHasChangedSinceLastPrint <- false

        /// Called when a test succeeds.
        member this.onSuccess() =
            if (not config.quiet) && config.verbose && (not config.showOnlyFailures) then
                this.printRegion()
                let num = Strings.prefixSpace _lastTestNumber _testNumberDigits
                Console.Write("{0}: ", num)
                Console.ForegroundColor <- ConsoleColor.Green
                Console.Write("SUCCESS\n")
                Console.ResetColor()
            _successes <- _successes + 1u

        /// Called when a test fails.
        /// Prints the failure-string if verbose printing is turned on.
        /// Depending on configuration, it may also throw an exception
        /// or add the failing test number to a list.
        member this.onFailure(msg: string) =
            if (not config.quiet) && (config.verbose || config.showOnlyFailures) then
                this.printRegion()
                let num = Strings.prefixSpace _lastTestNumber _testNumberDigits
                Console.Write("{0}: ", num)
                Console.ForegroundColor <- ConsoleColor.Red
                Console.Write("FAILED\n")
                Console.ResetColor()

            if not config.continueOnFailure then
                failwith (
                    "Assertion failure "
                    + if _region.Length > 0 then
                          "in region '" + _region + "'\n"
                      else
                          ""
                    + "at test number "
                    + string _lastTestNumber
                    + ":\n"
                    + msg
                )
            if config.printFailedTestNumbers then
                _failedTests <- _failedTests @ [ _lastTestNumber ]

        /// Call to start a new section of tests.
        /// The new region name will be printed before the next test's results.
        member this.setRegion(name: string) =
            _region <- name
            _regionHasChangedSinceLastPrint <- true

        member this.printResults() =
            if (not config.quiet) then
                if config.verbose && (not config.showOnlyFailures) && _lastTestNumber > 0u then
                    Console.WriteLine()
                else if config.showOnlyFailures && (_successes <> _lastTestNumber) && _lastTestNumber > 0u then
                    Console.WriteLine()

                if _successes = _lastTestNumber then
                    Console.Write("All {0} tests were successful.\n", _successes)
                else
                    Console.Write("{0}/{1} tests completed successfully.\n",
                                      _successes,
                                      _lastTestNumber)
                    if config.printFailedTestNumbers then
                        Console.Write("Failed test numbers:\n{0}\n",
                                      (Strings.formatList _failedTests ", "))

    end
