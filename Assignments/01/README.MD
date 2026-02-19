# Assignment 1 tests

To use this project with your `Assignment1.fs`, put it into the same directory
as this file.

Run these tests with
```shell
dotnet run
```

`dotnet test` won't do anything.\
If the Assignment solution as a whole is opened, a run configuration file
allows you to execute it directly from Rider.

Supports the command-line argument `--continue` or `-c`:
```shell
dotnet run -- -c
```

```shell
dotnet run -- -continue
```

Makes the programme continue running if a test fails.\
If this is omitted, a failed test will immediately throw an exception.