module a02.State

open System

/// Holds the state of this programme and encapsulates functionality
/// related to its members.
/// Mostly used to be able to print results in a useful manner.
type TestState(config: Arguments.Config) =
    class
        /// The number of successful tests.
        let mutable _successes = 0u

        /// The last test number to be executed. When all tests are done,
        /// this represents the total number of tests.
        let mutable _lastTestNumber = 0u

        /// Contains the test numbers of failed tests.
        let mutable _failedTests = []

        /// Holds the name of the current section of tests. Used to delineate
        /// tests to make results easier to understand.
        let mutable _region = ""

        /// If true, the next call to TestState.print will also print the
        /// name of the current region.
        let mutable _regionHasChangedSinceLastPrint = false

        /// The number of successful tests.
        member this.successes = _successes

        /// The last test number to be executed. When all tests are done,
        /// this represents the total number of tests.
        member this.lastTestNumber = _lastTestNumber

        /// Unused.
        member this.failedTests = _failedTests

        /// Holds the name of the current section of tests. Used to delineate
        /// tests for better readability/understandability.
        member this.currentRegion = _region

        /// If true, a failed assertion does not throw an exception.
        /// Default is true. Override with --break or -b.
        member this.continueOnFailure = config.continueOnFailure

        /// If true, print all results, not just the cumulative final results.
        /// Default is true. Override with --quiet or -q.
        member this.verbose = config.verbose

        member this.printFailedTests = config.printFailedTestNumbers

        member this.nextTest() =
            _lastTestNumber <- _lastTestNumber + 1u
            Strings.prefixSpace _lastTestNumber 4 |> this.print

        /// Called when a test succeeds.
        member this.onSuccess() =
            this.print ": SUCCESS\n"
            _successes <- _successes + 1u

        /// Called when a test fails.
        /// Prints the failure-string if verbose printing is turned on.
        /// Depending on configuration, it may also throw an exception
        /// or add the failing test number to a list.
        member this.onFailure(msg: string) =
            this.print ": FAILED\n"

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
            else if (config.printFailedTestNumbers || config.showOnlyFailures) then
                _failedTests <- _failedTests @ [ _lastTestNumber ]
            ()

        /// Call to start a new section of tests.
        member this.setRegion(name: string) =
            _region <- name
            _regionHasChangedSinceLastPrint <- true

        /// Call to print the given 'msg' if 'this.verbose' is true.
        /// Used to print individual test results.
        member this.print msg =
            if this.verbose then
                if _regionHasChangedSinceLastPrint then
                    Console.WriteLine("--- " + _region + ":")
                    _regionHasChangedSinceLastPrint <- false

                Console.Write(string msg)

    end
